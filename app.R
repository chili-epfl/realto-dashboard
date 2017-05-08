## app.R ##
# devtools::install_github(c("ramnathv/htmlwidgets", "rstudio/dygraphs"))
# install_github ('ramnathv/rCharts')
# install_github("dgrapov/networkly")
library(shiny)
library(shinydashboard)
library(magrittr)
library(RPostgreSQL)
library(ggplot2)
library(dygraphs)
library(xts)
library(DT)
library(dplyr)
library(scales)
library(reshape2)
library(googleVis)
library(reshape)
library(cluster)
library(igraph)
library(networkD3) # social net and sankey
# library(rCharts) # for sankey diagram

source("./eventcount.R")
source("./password.R")
source("./preparePlots.R")

drv <- dbDriver("PostgreSQL")
con <- dbConnect(    drv,     host = "icchilisrv1.epfl.ch",           user = "shiny",    dbname = "realto",    password = password  )

source("./UI.R")
ui <- dashboardPage(header, sidebar, body)
########################################## server #################################
server <- function(input, output) {
  professionq = function(x) { paste0("p.name LIKE '",x ,"'") }
  localeq = function(x) { paste0("u.locale LIKE '", x, "'") }
  uroleq = function(x) { paste0("u.role_id LIKE '", x, "'") }
  users_sub = function(activityLang, userRole, activityProf){    
    conditions = c(
      (if (activityLang != 'all') localeq(activityLang) else NULL),
      (if (userRole != 'all') uroleq(userRole) else NULL),
      (if (activityProf != 'all') professionq(activityProf) else NULL)
    )
    conditions = paste(conditions, collapse=' AND ')
    conditions = if(nchar(conditions) > 0) paste0("WHERE ", conditions) else ''
    paste('WITH users_sub as (select u.*, p.name from users u LEFT JOIN professions p ON u.profession_id = p._id ', conditions,  ")", sep=" ")
  }
  
  #----------------------- tab: activity ----------------------------
  # ----all activity over time  
  
  activitySql = reactive({    
    paste(users_sub(input$activityLang, input$userRole, input$activityProf), "SELECT a.date::DATE, count(a.*) AS n FROM user_activity_logs_cleaner_m a RIGHT JOIN users_sub u ON a.user_id = u._id GROUP BY date::date")
  })
  output$activitySql = reactive({ activitySql() })
  activityData = reactive({pall = dbGetQuery(con, activitySql())
  if (nrow(pall) == 0) {
    c("empty", "empty")
  } else {
    xts(na.omit(pall)[, -1], order.by = as.Date(na.omit(pall)[, 1]))
  }
  })
  
  output$rollerActivities <- renderDygraph({
    a = activityData()
    if (class(a) == "character" && a == "empty") {
      NA
    } else {
      dygraph(a, main = "All", group = "1") %>%
        dyRoller(rollPeriod = as.numeric(input$rollPeriod)) %>%
        dyOptions(fillGraph = TRUE, fillAlpha = 0.2) %>%
        dyRangeSelector
    }
  })
  #----------------------- tab: posts ----------------------------
  # ---------------  new postsover time
  postsql = reactive({ 
    typeq = if(input$postType != 'all') paste0(" WHERE p.deleted = FALSE AND (LD.deleted IS NULL OR LD.deleted = TRUE) AND p.post_type LIKE '", input$postType, "'") else ""
    paste(users_sub(input$activityLangpost, input$userRolepost, input$activityProfpost),
          "select p.CREATED_AT::date as n, count(p.*) FROM posts p INNER JOIN users_sub u on u._id = p.owner_id LEFT JOIN learning_doc_entries LD on LD._id = p.ld_id ", typeq, " GROUP BY p.created_at::date", sep=' ')
  })
  output$postsql = reactive({ postsql() })
  postdata = reactive({
    postcgroup = dbGetQuery(con, postsql())
    xts(postcgroup[, -1], order.by = as.Date(postcgroup[, 1]))
  })
  
  output$rollerPost = renderDygraph({
    pqxts = postdata()
    dygraph(pqxts) %>% dyRangeSelector %>% dyRoller(rollPeriod = as.numeric(input$rollPeriodpost))
  })
  
  #----------------------- tab:  uniqueUsers ----------------------------
  #---------- 1. weekly unique users
  uniqueUsersSql = reactive({
    paste0(
      users_sub(input$activityLangUnique, input$userRoleUnique, input$activityProfUnique),
      ", b AS (SELECT a.user_id, date_trunc('",
      input$uniqueUsersGran,
      "', a.DATE) AS DATE FROM user_activity_logs_cleaner_m a INNER JOIN users_sub u on u._id = a.user_id )
      SELECT DATE, count(distinct(user_id)) FROM b GROUP BY DATE ORDER BY DATE desc", sep=' '
    )
  })
  output$uniqueSql = reactive({ uniqueUsersSql()})
  uniqueUsersData = reactive({
    month = dbGetQuery(con, uniqueUsersSql())
    xts(month[, -1], order.by = as.Date(month[, 1]))
  })
  
  output$uniqueUsersPlot <- renderDygraph({
    mxts = uniqueUsersData()
    dygraph(mxts) %>% dyRangeSelector %>% dyRoller(rollPeriod = as.numeric(input$uniqueUsersSmoothing))
  })
  #------------- 2. CUMULATIVE REGISTERED USERS COUNT ----------
  cumulUsersSql = reactive({
    a = input$uniqueUsersPlot_date_window  
    paste0(
      users_sub(input$activityLangUnique, input$userRoleUnique, input$activityProfUnique),
      "SELECT created_at::DATE as date from users_sub WHERE created_at::DATE > '",a[[1]],"' AND created_at::DATE < '", a[[2]], 
      "' ORDER BY DATE"
    )
  })
  output$cumulUsersSql = reactive({ cumulUsersSql()})
  cumulUsersData = reactive({    dbGetQuery(con, cumulUsersSql())  })
  
  output$cumulUsersPlot = renderPlot({
    p=cumulUsersData()
    ggplot(p, aes(date)) + stat_bin(aes(y = cumsum(..count..)), geom="step", col='blue') + labs(y='Registered User Count')+
      scale_x_date(date_breaks = "2 month", date_minor_breaks = "1 week", labels=date_format("%b%Y"))#+theme(axis.text.x = element_text(angle = 90, hjust = 1))  
  })
  
  #---------------  users count V.S. POSTS COUNT -----------------------
  # posts users 
  postsUsersSql = reactive({ 
    typeq = if(input$postType != 'all') paste0(" WHERE p.post_type LIKE '", input$postType, "' AND ") else " WHERE "
    a = input$rollerPost_date_window  
    ret = paste0(users_sub(input$activityLangpost, input$userRolepost, input$activityProfpost),
                 " select count(p.*) as n FROM posts p INNER JOIN users_sub u on u._id = p.owner_id LEFT JOIN learning_doc_entries LD on LD._id = p.ld_id ", typeq, 
                 " p.deleted = FALSE AND (LD.deleted IS NULL or LD.deleted = FALSE) AND p.created_at::DATE > '",a[[1]],"' AND p.created_at::DATE < '", a[[2]],"' GROUP BY u._id")
    return(ret)
  })
  
  output$postsUsersSql = reactive({postsUsersSql()})
  postsUsersData = reactive({ dbGetQuery(con, postsUsersSql())  })
  
  output$postUsers = renderPlot({
    p = postsUsersData()
    ggplot(p, aes(n)) + geom_histogram(binwidth=5,fill="blue",) + 
      scale_x_continuous('Number of posts', breaks=seq(0,1000,5))+labs(x="Age", y='Number of users')
  })
  
  
  #------------------- users Post Sequence  ------------------  
  #  what each user does in each week
  usersPostSequenceSql = reactive({ 
    typeq = if(input$postType != 'all') paste0(" WHERE p.post_type LIKE '", input$postType, "' AND ") else " WHERE "
    a = input$rollerPost_date_window  
    ret = paste0("WITH u_week_post as ( WITH u_week_post_count as (", users_sub(input$activityLangpost, input$userRolepost, input$activityProfpost),
                 " select date_trunc('",input$posSeq_time_window,"',p.CREATED_AT) as time, p.owner_id, p.post_type, count(p.*)
                 FROM posts p INNER JOIN users_sub u on u._id = p.owner_id LEFT JOIN learning_doc_entries LD on LD._id = p.ld_id ", typeq, 
                 " p.post_type NOT LIKE 'learningJournal' AND p.deleted = FALSE AND (LD.deleted IS NULL or LD.deleted = FALSE) AND p.created_at::DATE > '",a[[1]],"' AND p.created_at::DATE < '", a[[2]],
                 "' GROUP BY p.owner_id, p.post_type,time ORDER BY owner_id, time desc,post_type",") 
                 SELECT  owner_id,time, string_agg((post_type), ', ') AS post_type
                 FROM   u_week_post_count  GROUP  BY owner_id,time) select u.first_name, u.last_name,upw.* from u_week_post upw LEFT JOIN users u ON upw.owner_id=u._id")
    return(ret)
  })
  
  output$usersPostSequenceSql = reactive({usersPostSequenceSql()})
  usersPostSequenceData= reactive({ dbGetQuery(con, usersPostSequenceSql())  })
  
  output$usersPostSequencePlot = renderPlot({
    p = usersPostSequenceData()
    getPostSequencePlot(p, input)
  })
  
  
  
  #----------------------- tab:  most Active users ----------------------------
  #   mostActiveUsersSql =reactive({  "WITH postcnt AS (SELECT count(*) AS n, owner_id AS uid FROM posts GROUP BY uid),
  #                         ldcnt AS (SELECT count(*) AS n, apprentice_id AS uid FROM learning_doc_entries WHERE deleted = FALSE GROUP BY uid)
  #                         SELECT u._id as personalflow, u.first_name, u.last_name, coalesce(max(p.n),0) as postsn, coalesce(max(l.n),0) as ldocsn FROM users u FULL JOIN postcnt p ON u._id = p.uid FULL JOIN ldcnt l ON u._id = l.uid GROUP BY u._id;"
  #   })
  
  
  posts_comment_lds_query= ", AllPostCnt AS (SELECT count(*) AS n, owner_id AS uid FROM posts where deleted = FALSE GROUP BY uid),
  StandardPostCnt AS (SELECT count(*) AS n, owner_id AS uid FROM posts where post_type LIKE 'standard' AND deleted = FALSE GROUP BY uid),
  LDPostCnt AS (SELECT count(*) AS n, owner_id AS uid FROM posts where post_type IN ( 'learnDoc','standardLd','learningJournal') AND deleted = FALSE GROUP BY uid),
  ActivitySub AS (SELECT count(*) AS n, owner_id AS uid FROM posts where post_type IN ( 'activitySubmission') AND deleted = FALSE GROUP BY uid),
  Activity AS (SELECT count(*) AS n, owner_id AS uid FROM posts where post_type IN ( 'activity') AND deleted = FALSE GROUP BY uid),
  ldcnt AS (SELECT count(*) AS n, apprentice_id AS uid FROM learning_doc_entries WHERE deleted = FALSE GROUP BY uid),
  commentsCnt AS (SELECT count(*) AS n, owner_id AS uid FROM post_comments where deleted = FALSE GROUP BY uid)
  SELECT u._id as personalflow, u.first_name, u.last_name,
  coalesce(max(l.n),0) as learning_document ,  coalesce(max(a.n),0) as all_post_types,  coalesce(max(s.n),0) as satndard_posts  ,coalesce(max(ldp.n),0) as ld_posts ,
  coalesce(max(asp.n),0) as activity_submission ,coalesce(max(act.n),0) as activity  ,coalesce(max(cmn.n),0) as comments_n
  FROM users_sub u 
  FULL JOIN AllPostCnt a ON u._id = a.uid   FULL JOIN StandardPostCnt s ON u._id = s.uid   FULL JOIN ldcnt l ON u._id = l.uid
  FULL JOIN LDPostCnt ldp ON u._id = ldp.uid   FULL JOIN ActivitySub asp ON u._id = asp.uid   FULL JOIN Activity act ON u._id = act.uid 
  FULL JOIN commentsCnt cmn ON u._id = cmn.uid   GROUP BY u._id,u._id,u.first_name, u.last_name;;"
 
   mostActiveUsersSql =reactive({ paste( users_sub('all','all','all'),posts_comment_lds_query)})
  output$mostActiveUsersSql = reactive({ mostActiveUsersSql()})
  mostactiveUData = reactive({    p=dbGetQuery(con, mostActiveUsersSql());  p=filter(p, !is.na(first_name))})
  # columns are:
  # standard_post_n, learning_document ,activity ,activity_submission, comments_n, all_post_types, ld_posts , 
  output$mostActiveTable <- DT::renderDataTable({
    p = mostactiveUData()
    p$satndard_posts = paste0("<a href=https://www.realto.ch/userFlow/", p$personalflow,">",      as.numeric(p$satndard_posts),"</a>"  )
    p$learning_document = paste0("<a href=https://www.realto.ch/learnDoc?apprentice=",p$personalflow,  ">", as.numeric(p$learning_document),  "</a>"    )
    p$comment=p$comments_n; p$comments_n=NULL
    p$personalflow = NULL
    p$all_post_types = NULL
    p$ld_posts=NULL
    p
  }, escape = F, server = F)
  
  #----------------------- tab: platform usage clusters ----------------------------
  #--------------------- clusters of platform usagegb
  UsersclusterSql =reactive({ paste( users_sub(input$userClustLang, input$userClustRole, input$userClustProf),
                                     posts_comment_lds_query)})
  output$UsersclusterSql = reactive({ UsersclusterSql()})
  UsersclusterData = reactive({    p=dbGetQuery(con, UsersclusterSql());  p=filter(p, !is.na(first_name))})
  
  # columns are:
  # standard_post_n, learning_document ,activity ,activity_submission, comments_n, all_post_types, ld_posts , 
  
  output$usageClustersPlot = renderPlot({
    p=UsersclusterData()
    getUsageClusterPlot(p, input)
  })
  
  output$usageBarPlot = renderPlot({
    p=UsersclusterData()
    getUsageBarPlot(p, input)
  })
  
  output$usageTable<- DT::renderDataTable({
    p = UsersclusterData()
    p$satndard_posts = paste0("<a href=https://www.realto.ch/userFlow/", p$personalflow,">",      as.numeric(p$satndard_posts),"</a>"  )
    p$learning_document = paste0("<a href=https://www.realto.ch/learnDoc?apprentice=",p$personalflow,  ">", as.numeric(p$learning_document),  "</a>"    )
    p$comment=p$comments_n; p$comments_n=NULL
    p$personalflow = NULL
    p$all_post_types = NULL
    p$ld_posts=NULL
    p
  }, escape = F, server = F)
  
  #--------------------- tab:  social network -----------------------
  socialNetSql =reactive({ paste(
    users_sub(input$socialLang, 'all', input$socialProf),
    ",links as(select  a.owner_id as from_uid , b.owner_id as to_uid, b.flow_id as to_flow_id, b.post_type as to_post_type ,'comment' AS link_type
    from post_comments a LEFT JOIN posts b on a.post_id=b._id
    UNION ALL
    select  a.user_id as from_uid , b.owner_id as to_uid, b.flow_id as to_flow_id, b.post_type as to_post_type ,'like' AS link_type
    from post_likes a LEFT JOIN posts b on a.post_id=b._id      
  ),
    network as ( SELECT  links.*, u.first_name as from_first_name, u.last_name as from_last_name, u2.first_name as to_first_name, u2.last_name as to_last_name
    from links  INNER JOIN  users_sub u on links.from_uid   = u._id  
    INNER JOIN  users_sub u2 on  links.to_uid = u2._id )
    select net.from_first_name,net.from_last_name, net.to_first_name , net.to_last_name, count(net.*) as weight 
    from network net where link_type IN (", input$socialLinkType,")",
    "group by net.from_first_name,net.from_last_name, net.to_first_name , net.to_last_name"                                    
  )})
  output$socialNetSql = reactive({ socialNetSql()})
  
  socialNetData = reactive({  dbGetQuery(con, socialNetSql())})
  
  # columns are: from_first_name,from_last_name, to_first_name , to_last_name,  weight
  #   output$socialNetPlot_sankey2 = renderChart2({
  #     p=socialNetData()
  #     getSocialNetPlot_sankey2(p, input)
  #   })
  output$socialNetPlot_sankey = renderSankeyNetwork({
    p=socialNetData()
    getSocialNetPlot_sankey(p, input)
  })
  
  
  output$socialNetPlot_force = renderForceNetwork({
    p=socialNetData()
    getSocialNetPlot_force(p, input)
  })
  
  
  #----------------------- tab:  most Active flows ----------------------------
  # most active flows table
  flowSql = reactive({"WITH p_n AS (SELECT flow_id, count(*) AS n FROM posts GROUP BY flow_id),
            p_wk AS (SELECT flow_id, count(*) AS n FROM posts WHERE (created_at > CURRENT_DATE - INTERVAL '7 days')
            GROUP BY flow_id),
            ff AS (SELECT f._id, coalesce(max(p_n.n),0) AS alltime, coalesce(max(p_wk.n),0) AS last7 FROM flows f FULL JOIN p_n ON f._id = p_n.flow_id FULL JOIN p_wk ON f._id = p_wk.flow_id WHERE f.type LIKE 'group' OR f.type LIKE 'school' AND f.deleted = FALSE GROUP BY f._id)
            SELECT flows._id AS id, flows.name, alltime, last7 FROM ff LEFT JOIN flows ON ff._id = flows._id;"})
  
  output$flowSql = reactive({flowSql()})
  flowData = reactive({    dbGetQuery(con, flowSql())  })
  
  output$flowsTable <- DT::renderDataTable({
    p = flowData()#dbGetQuery(con, flowSql)
    p$name = paste0("<a href=https://www.realto.ch/userFlow/",
                    p,">",p$name,"</a>")
    p$id = NULL
    p
  }, escape = F, server = F, caption = "Number of posts per flow, all time, or in the last 7 days")
  }


#======================== Run the application
shinyApp(ui = ui, server = server)

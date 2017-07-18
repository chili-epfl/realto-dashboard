#----- load libraries
list.of.packages <- c("shiny","shinydashboard","magrittr","RPostgreSQL","ggplot2","dygraphs","xts",
                      "DT","dplyr","plyr","scales","reshape2","googleVis","reshape","cluster",
                      "igraph", "networkD3", "rCharts","pracma","gridExtra") 

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages) 
lapply(list.of.packages, require, character.only=T)

# devtools::install_github(c("ramnathv/htmlwidgets", "rstudio/dygraphs"))
# devtools::install_github ('ramnathv/rCharts')
# install_github("dgrapov/networkly")
# devtools::install_github("igraph/rigraph")

source("./eventcount.R")
source("./password.R")
source("./preparePlots.R")
source("./Regularity.R")
source("./socialNetworkAnalysis.R")
drv <- dbDriver("PostgreSQL")
con <- dbConnect(    drv,     host = "icchilisrv1.epfl.ch",           user = "shiny",    dbname = "realto",    password = password  )
source("./UI.R")

ui <- dashboardPage(header, sidebar, body)
########################################## server #################################
server <- function(input, output) {
   #---- dropdowns for profession list 
      professions_list_fromDB= reactive({ c('all',dbGetQuery(con, "select distinct(name) from professions ORDER BY name"))  })
      output$activityProf_dropdown =     renderUI({selectInput("activityProf","Professions", professions_list_fromDB()) })
      output$activityProfpost_dropdown = renderUI({selectInput("activityProfpost","Professions", professions_list_fromDB()) })
      output$activityProfUnique_dropdown = renderUI({selectInput("activityProfUnique","Professions", professions_list_fromDB()) })
      output$userClustProf_dropdown = renderUI({selectInput("userClustProf","Professions", professions_list_fromDB()) })
      output$socialProf_dropdown = renderUI({selectInput("socialProf","Professions", professions_list_fromDB()) })
      
    #---- dropdowns for school names
      schools_list_fromDB_sql= reactive({ c('all',dbGetQuery(con, "select distinct(name) from schools ORDER BY name"))  })
      output$activitySchool_dropdown = renderUI({selectInput("activitySchool","Schools", schools_list_fromDB_sql()) })
      output$activitySchoolpost_dropdown= renderUI({selectInput("activitySchoolpost","Schools", schools_list_fromDB_sql()) })
      output$activitySchoolUnique_dropdown= renderUI({selectInput("activitySchoolUnique","Schools", schools_list_fromDB_sql()) })
      output$userClustSchool_dropdown= renderUI({selectInput("userClustSchool","Schools", schools_list_fromDB_sql()) })
      output$socialSchool_dropdown= renderUI({selectInput("socialSchool","Schools", schools_list_fromDB_sql()) })
    
    #---- dropdowns for flow names
      flows_names_members=  dbGetQuery(con, "select distinct(name), members, owner_id from flows where type='group' and deleted='f' ORDER BY name ")
      getBigFlowNames <-  function(flows_members) {
        # flows_members=read.csv('/home/mina/Dropbox/Mina/RealtoResearchDashboard/flowsmembers.csv', sep='|' , stringsAsFactors = F)
        #---- count number of members in each flow
        flows_members$members_count=sapply(flows_members$members, function(x) { length(strsplit(substring(x,2,nchar(x)-1),',')[[1]])})
        #---- return names of flows with more than 5 members
        bigFlows=filter(flows_members,members_count>=5)
        return(bigFlows)
      }
      flows_names_members=getBigFlowNames(flows_names_members)
      output$regularity_flow_dropdown = renderUI({selectInput("flowName_regularity","Flow name", c(flows_names_members$name)) })
      
    #----
      professionq = function(x) { paste0("p.name LIKE '",x ,"'") }
      schoolq = function(x) { paste0("s.name LIKE '",x ,"'") }
      localeq = function(x) { paste0("u.locale LIKE '", x, "'") }
      uroleq  = function(x) { paste0("u.role_id LIKE '", x, "'") }
    
    users_sub = function(lang, role, prof,school){    
      conditions = c(
        (if (lang != 'all') localeq(lang) else NULL),
        (if (role != 'all') uroleq(role) else NULL),
        (if (prof != 'all') professionq(prof) else NULL),
        (if (school != 'all') schoolq(school) else NULL)
      )
      conditions = paste(conditions, collapse=' AND ')
      conditions = if(nchar(conditions) > 0) paste0("WHERE ", conditions) else ''
      paste('WITH users_sub as (select u.*, p.name as professionname, s.name as schoolname from users u LEFT JOIN professions p ON u.profession_id = p._id LEFT JOIN schools s on u.school_id=s._id ', conditions,  ")", sep=" ")
    }
    
    #=================================== tab: activity  #============================================= 
    # ----all activity over time  
    
    activitySql = reactive({    
      paste(users_sub(input$activityLang, input$userRole, input$activityProf, input$activitySchool), "SELECT a.date::DATE, count(a.*) AS n FROM user_activity_logs_cleaner_m a RIGHT JOIN users_sub u ON a.user_id = u._id GROUP BY date::date")
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
  #============================================= tab: posts  #============================================= 
      # ---------------  1. new postsover time
      postsql = reactive({ 
        typeq = if(input$postType != 'all') paste0(" WHERE p.deleted = FALSE AND (LD.deleted IS NULL OR LD.deleted = TRUE) AND p.post_type LIKE '", input$postType, "'") else ""
        paste(users_sub(input$activityLangpost, input$userRolepost, input$activityProfpost, input$activitySchoolpost),
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
      #------------------- 2. users Post Sequence  ------------------  
        #  what each user does in each week
        usersPostSequenceSql = reactive({ 
          typeq = if(input$postType != 'all') paste0(" WHERE p.post_type LIKE '", input$postType, "' AND ") else " WHERE "
          a = input$rollerPost_date_window  
          ret = paste0("WITH u_week_post as ( WITH u_week_post_count as (", users_sub(input$activityLangpost, input$userRolepost, input$activityProfpost, input$activitySchoolpost),
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
    #---------------  3. users count V.S. POSTS COUNT -----------------------
      # posts users 
      postsUsersSql = reactive({ 
        typeq = if(input$postType != 'all') paste0(" WHERE p.post_type LIKE '", input$postType, "' AND ") else " WHERE "
        a = input$rollerPost_date_window  
        ret = paste0(users_sub(input$activityLangpost, input$userRolepost, input$activityProfpost, input$activitySchoolpost),
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
 #============================================= tab:  uniqueUsers  #============================================= 
    #---------- 1. weekly unique users
    uniqueUsersSql = reactive({
      paste0(
        users_sub(input$activityLangUnique, input$userRoleUnique, input$activityProfUnique, input$activitySchoolUnique),
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
    #------------- 2. Cumulative registered users count ----------
    cumulUsersSql = reactive({
      a = input$uniqueUsersPlot_date_window  
      paste0(
        users_sub(input$activityLangUnique, input$userRoleUnique, input$activityProfUnique, input$activitySchoolUnique),
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
    
    #=================================== tab:  most Active users  #============================================= 
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
   
     mostActiveUsersSql =reactive({ paste( users_sub('all','all','all','all'),posts_comment_lds_query)})
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
    
    #=================================== tab: platform usage clusters  #============================================= 
    #--------------------- clusters of platform usagegb
    UsersclusterSql =reactive({ paste( users_sub(input$userClustLang, input$userClustRole, input$userClustProf, input$userClustSchool),
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
      users_sub(input$socialLang, 'all', input$socialProf, input$socialSchool),
      ",links as(select  a.owner_id as from_uid , b.owner_id as to_uid, b.flow_id as to_flow_id, b.post_type as to_post_type ,'comment' AS link_type
      from post_comments a LEFT JOIN posts b on a.post_id=b._id
      UNION ALL
      select  a.user_id as from_uid , b.owner_id as to_uid, b.flow_id as to_flow_id, b.post_type as to_post_type ,'like' AS link_type
      from post_likes a LEFT JOIN posts b on a.post_id=b._id      
    ),
      network as ( SELECT  links.*, u.first_name as from_first_name, u.last_name as from_last_name,u.role_id as from_role,
                                    u2.first_name as to_first_name, u2.last_name as to_last_name, u2.role_id as to_role
      from links  INNER JOIN  users_sub u on links.from_uid   = u._id  
      INNER JOIN  users_sub u2 on  links.to_uid = u2._id )
      select net.from_first_name,net.from_last_name, net.from_role, net.to_first_name , net.to_last_name, net.to_role, count(net.*) as weight
      from network net where link_type IN (", input$socialLinkType,")",
      "group by net.from_first_name,net.from_last_name, net.from_role, net.to_first_name , net.to_last_name, net.to_role"                                    
    )})
    
    
#     WITH users_sub as (select u.*, p.name as professionname, s.name as schoolname from users u LEFT JOIN professions p ON u.profession_id = p._id LEFT JOIN schools s on u.school_id=s._id ),
#     links as (
#       select a.owner_id as from_uid , b.owner_id as to_uid, b.flow_id as to_flow_id, b.post_type as to_post_type ,
#       'comment' AS link_type from post_comments a LEFT JOIN posts b on a.post_id=b._id UNION ALL select a.user_id as from_uid , 
#       b.owner_id as to_uid, b.flow_id as to_flow_id, b.post_type as to_post_type ,'like' AS link_type from post_likes a 
#       LEFT JOIN posts b on a.post_id=b._id 
#     ), 
#     network as ( 
#       SELECT links.*, u.first_name as from_first_name, u.last_name as from_last_name, u.role_id as from_role,
#       u2.first_name as to_first_name, u2.last_name as to_last_name , u2.role_id as to_role
#       from links INNER JOIN users_sub u on links.from_uid = u._id INNER JOIN users_sub u2 on links.to_uid = u2._id 
#     )
#     select net.from_first_name,net.from_last_name, net.from_role, net.to_first_name , net.to_last_name, net.to_role, count(net.*) as weight 
#     from network net where link_type IN ( 'comment', 'like' ) 
#     group by net.from_first_name,net.from_last_name, net.from_role, net.to_first_name , net.to_last_name, net.to_role
#     
#     
    
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
      net=prepareNetwork(p, input$socialRoleType)
      plotSocialNetPlot_force (net, input)
    })
    
    output$socialNetAttributesTable <- DT::renderDataTable({
        p = socialNetData()
        netAttributes=data.frame()
        for(role in c('all', 'apprentice', 'teacher', 'supervisor')){
          net=prepareNetwork(p,  role)
          attributes= getNetworkAttributes(net, role)
          netAttributes=rbind(netAttributes, attributes)
        }
        netAttributes
        
      }, escape = F, server = F, caption = "attributes of networks")
    
  #============================================= tab: Regularity  #============================================= 
  getFlowMembersList <- function(selectedFlowName , flows_list_fromDB){
        currentFlow=filter(flows_list_fromDB,name==selectedFlowName)
        members=currentFlow$members;
        owner=currentFlow$owner_id
       (members=(strsplit(substring(members,2,nchar(members)-1),',')[[1]]))
        members=c(members,owner)
        members_str=paste(members, collapse = '\',\'')
        members_str=paste0('(\'',members_str, '\')')
       return(members_str)
    }
    output$flows_Names_members_table<- DT::renderDataTable({
      p = flows_names_members
      p
    }, escape = F, server = F)
    regularityUsersActions_Sql = reactive({paste ('with actions as (SELECT user_id, type, date ,EXTRACT(YEAR FROM date) as year,  EXTRACT(DOY FROM date) as dayofyear,
                                           EXTRACT(WEEK FROM date) as weekofyear, EXTRACT(DAY FROM date) as dayofmonth, 
                                           EXTRACT(ISODOW FROM date) as dyofweek, EXTRACT(HOUR FROM date) as hourofDay 
                                           from user_activity_logs_cleaner_m where user_id IN ', getFlowMembersList(input$flowName_regularity , flows_names_members), 
                                           'AND type NOT IN (\'login\',\'logout\',\'viewPost\') order by user_id, date),
                                           u as (SELECT _id, first_name, last_name, role_id  from users where _id IN',getFlowMembersList(input$flowName_regularity , flows_names_members),')',
                                           ' select u.* , actions.* from u INNER JOIN actions on actions.user_id=u._id')
    })
    output$regularityUsersActions_Sql = reactive({ regularityUsersActions_Sql()})
    regularityUsersActionsData = reactive({  dbGetQuery(con, regularityUsersActions_Sql()) })
    
    #-----WSB
    output$WSB_reg_WeeklyProfilePlot = renderPlot({
      p = regularityUsersActionsData()
      getRegularity_WSB(p)
      })
    #----- peak on week day
    output$CWD_reg_WeeklyHistPlot= renderPlot({
      p = regularityUsersActionsData()
      getRegularity_peakWeekDay(p)
    })
    #----- peak on day hour
    output$CDH_reg_DailyHistPlot= renderPlot({
      p = regularityUsersActionsData()
      getRegularity_peakDayHour(p)
    })
    #----- sql for activity 
    
    
  #============================================= tab:  most Active flows  #============================================= 
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
      p$name = paste0("<a href=https://www.realto.ch/userFlow/",p,">",p$name,"</a>")
      p$id = NULL
      p
    }, escape = F, server = F, caption = "Number of posts per flow, all time, or in the last 7 days")
}


#======================== Run the application
shinyApp(ui = ui, server = server)

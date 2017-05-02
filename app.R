## app.R ##
# devtools::install_github(c("ramnathv/htmlwidgets", "rstudio/dygraphs"))
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
source("./eventcount.R")
source("./password.R")

header <- dashboardHeader(title = "REALTO dashboard")

sidebar <- dashboardSidebar(sidebarMenu(
  menuItem("Most active flows", tabName = "flows", icon = icon("dashboard")),
  menuItem("All activity",tabName = "activity", icon = icon("th")),
  menuItem("Posts and Users/posts", tabName = "posts", icon = icon("th")),
  menuItem("Users", tabName = "uniqueUsers",    icon = icon("th") ),
  menuItem("Most active users", tabName = "mostActive", icon = icon("th")),
  menuItem("REALTO", icon = icon("file-code-o"), href = "https://www.realto.ch")
))

body <- dashboardBody(tabItems(
  # First tab content
  tabItem(
    tabName = "activity",
      dygraphOutput("rollerActivities"),
    flowLayout(
      sliderInput("rollPeriod", "Smoothing:", 1, 100, 1),
      radioButtons(
        "activityProf",
        "Professions",
        c(
          "All" = "all",
          "Clothing designers" = "clothesDesigner",
          "Florists" = "florist",
          "Carpenters" = "carpenter",
          "Multimedia electronicians" = "multimediaElectronician"
        )
      ),
      radioButtons(
        "activityLang",
        "Languages",
        c(
          "All" = "all",
          "German" = "de",
          "French" = "fr",
          "Italian" = "it"
        )
      ),
      radioButtons(
        "userRole",
        "User role",
        c(
          "All"="all",
          "Apprentice"="apprentice",
          "Teacher"="teacher",
          "Supervisor"="supervisor"
        )
      )
    ),
    htmlOutput("activitySql")
  ),
  tabItem(
    tabName = "posts",
    dygraphOutput("rollerPost"),
    plotOutput("postUsers"),
    flowLayout(
      sliderInput("rollPeriodpost", "Smoothing:", 1, 100, 1),
      radioButtons(
        "activityProfpost",
        "Professions",
        c(
          "All" = "all",
          "Clothing designers" = "clothesDesigner",
          "Florists" = "florist",
          "Carpenters" = "carpenter",
          "Multimedia electronicians" = "multimediaElectronician"
        )
      ),
      radioButtons(
        "activityLangpost",
        "Languages",
        c(
          "All" = "all",
          "German" = "de",
          "French" = "fr",
          "Italian" = "it"
        )
      ),
      radioButtons(
        "userRolepost",
        "User role",
        c(
          "All"="all",
          "Apprentice"="apprentice",
          "Teacher"="teacher",
          "Supervisor"="supervisor"
        )
      ),
      radioButtons(
        "postType",
        "Post type",
        c(
          "All"="all",
          "post"="standard",
          "learnDoc"="learnDoc",
          "activity"="activity",
          "activitySubmission"="activitySubmission",
          #"learningJournal"="learningJournal",
          "standardLd"="standardLd"
        )
      )
    ),
    htmlOutput("postsql"),
    textOutput("postsUsersSql")

  ),
  tabItem(
    tabName = "uniqueUsers",
    fluidRow(
    h3('Unique users/timeframe'),
    dygraphOutput("uniqueUsersPlot"),
    h3('Cumulative registered users'),
    plotOutput("cumulUsersPlot")
    ),
    flowLayout(
      sliderInput("uniqueUsersSmoothing", "Smoothing:", 1, 100, 1),
      radioButtons(
        "uniqueUsersGran",
        "Unique users per:",
        c(
          "Day" = "day",
          "Week" = "week",
          "Month" = "month"
        )
      ),
      radioButtons(
        "activityProfUnique",
        "Professions",
        c(
          "All" = "all",
          "Clothing designers" = "clothesDesigner",
          "Florists" = "florist",
          "Carpenters" = "carpenter",
          "Multimedia electronicians" = "multimediaElectronician"
        )
      ),
      radioButtons(
        "activityLangUnique",
        "Languages",
        c(
          "All" = "all",
          "German" = "de",
          "French" = "fr",
          "Italian" = "it"
        )
      ),
      radioButtons(
        "userRoleUnique",
        "User role",
        c(
          "All"="all",
          "Apprentice"="apprentice",
          "Teacher"="teacher",
          "Supervisor"="supervisor"
        )
      )
    ),
    htmlOutput("uniqueSql"),
    htmlOutput("cumulUsers")
  ),

  tabItem(tabName = "mostActive",
          DT::dataTableOutput("mostActiveTable")),
  
  tabItem(tabName = "flows",
          DT::dataTableOutput("flowsTable"),
          textOutput('flowq'))
  
))

ui <- dashboardPage(header, sidebar, body)

drv <- dbDriver("PostgreSQL")
con <-
  dbConnect(
    drv,
    host = "icchilisrv1.epfl.ch",
    user = "shiny",
    dbname = "realto",
    password = password
  )

server <- function(input, output) {
  # all activity
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
  activitySql = reactive({
    paste(users_sub(input$activityLang, input$userRole, input$activityProf), "SELECT a.date::DATE, count(a.*) AS n FROM user_activity_logs_cleaner_m a RIGHT JOIN users_sub u ON a.user_id = u._id GROUP BY date::date")
  })
  output$activitySql = reactive({ activitySql() })
  
  activityData = reactive({
    pall = dbGetQuery(con, activitySql())
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

  # new posts
  postsql = reactive({ 
    typeq = if(input$postType != 'all') paste0(" WHERE p.deleted = FALSE AND (LD.deleted IS NULL OR LD.deleted = TRUE) AND p.post_type LIKE '", input$postType, "'") else ""
    
    paste(users_sub(input$activityLangpost, input$userRolepost, input$activityProfpost),
          "select p.CREATED_AT::date as n, count(p.*) FROM posts p INNER JOIN users_sub u on u._id = p.owner_id LEFT JOIN learning_doc_entries LD on LD._id = p.ld_id ", typeq, " GROUP BY p.created_at::date", sep=' ')
  })
  
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
  
  output$postsql = reactive({ postsql() })
  
  postsUsersData = reactive({
    dbGetQuery(con, postsUsersSql())
  })
  
  postdata = reactive({
    postcgroup = dbGetQuery(con, postsql())
    xts(postcgroup[, -1], order.by = as.Date(postcgroup[, 1]))
})
    
  output$rollerPost = renderDygraph({
    pqxts = postdata()
    dygraph(pqxts) %>% dyRangeSelector %>% dyRoller(rollPeriod = as.numeric(input$rollPeriodpost))
  })
  
  output$postUsers = renderPlot({
    p = postsUsersData()
    ggplot(p, aes(n)) + geom_histogram(binwidth=5,fill="blue",) + 
      scale_x_continuous('Number of posts', breaks=seq(0,1000,5))+labs(x="Age", y='Number of users')
  })
  
  # weekly unique users
  uniqueUsersSql = reactive({
    paste0(
      users_sub(input$activityLangUnique, input$userRoleUnique, input$activityProfUnique),
      ", b AS (SELECT a.user_id, date_trunc('",
      input$uniqueUsersGran,
      "', a.DATE) AS DATE FROM user_activity_logs_cleaner_m a INNER JOIN users_sub u on u._id = a.user_id )
      SELECT DATE, count(distinct(user_id)) FROM b GROUP BY DATE ORDER BY DATE desc", sep=' '
    )
  })
  
  cumulUsers = reactive({
    a = input$uniqueUsersPlot_date_window  
    paste0(
      users_sub(input$activityLangUnique, input$userRoleUnique, input$activityProfUnique),
      "SELECT created_at::DATE as date from users_sub WHERE created_at::DATE > '",a[[1]],"' AND created_at::DATE < '", a[[2]], 
      "' ORDER BY DATE"
    )
  })

  output$cumulUsers = reactive({ cumulUsers()})
  
  output$uniqueSql = reactive({ uniqueUsersSql()})
  
  uniqueUsersData = reactive({
    month = dbGetQuery(con, uniqueUsersSql())
    xts(month[, -1], order.by = as.Date(month[, 1]))
  })
  cumulUsersData = reactive({
    dbGetQuery(con, cumulUsers())
  })

  output$cumulUsersPlot = renderPlot({
    ggplot(cumulUsersData(), aes(date)) + stat_bin(aes(y = cumsum(..count..)), geom="step", col='blue') + scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", labels=date_format("%m/%y"))  
  })
    
  output$uniqueUsersPlot <- renderDygraph({
    mxts = uniqueUsersData()
    dygraph(mxts) %>% dyRangeSelector %>% dyRoller(rollPeriod = as.numeric(input$uniqueUsersSmoothing))
  })
  
  # most active users
  output$mostActiveTable <- DT::renderDataTable({
    mostactiveq = "WITH postcnt AS (SELECT count(*) AS n, owner_id AS uid FROM posts GROUP BY uid),
    ldcnt AS (SELECT count(*) AS n, apprentice_id AS uid FROM learning_doc_entries WHERE deleted = FALSE GROUP BY uid)
    SELECT u._id as personalflow, u.first_name, u.last_name, coalesce(max(p.n),0) as postsn, coalesce(max(l.n),0) as ldocsn FROM users u FULL JOIN postcnt p ON u._id = p.uid FULL JOIN ldcnt l ON u._id = l.uid GROUP BY u._id;"
    mostactive = dbGetQuery(con, mostactiveq)
    mostactive$posts = paste0(
      "<a href=https://www.realto.ch/userFlow/",
      mostactive$personalflow,
      ">",
      as.numeric(mostactive$postsn),
      "</a>"
    )
    mostactive$learndocs = paste0(
      "<a href=https://www.realto.ch/learnDoc?apprentice=",
      mostactive$personalflow,
      ">",
      as.numeric(mostactive$ldocsn),
      "</a>"
    )
    mostactive$personalflow = NULL
    mostactive$postsn = NULL
    mostactive$ldocsn = NULL
    mostactive
  }, escape = F, server = F)
  flowq = "WITH p_n AS (SELECT flow_id, count(*) AS n FROM posts GROUP BY flow_id),
    p_wk AS (SELECT flow_id, count(*) AS n FROM posts WHERE (created_at > CURRENT_DATE - INTERVAL '7 days')
  GROUP BY flow_id),
  ff AS (SELECT f._id, coalesce(max(p_n.n),0) AS alltime, coalesce(max(p_wk.n),0) AS last7 FROM flows f FULL JOIN p_n ON f._id = p_n.flow_id FULL JOIN p_wk ON f._id = p_wk.flow_id WHERE f.type LIKE 'group' OR f.type LIKE 'school' AND f.deleted = FALSE GROUP BY f._id)
  SELECT flows._id AS id, flows.name, alltime, last7 FROM ff LEFT JOIN flows ON ff._id = flows._id;"
  output$flowq = reactive({flowq})
  # most active flows
  output$flowsTable <- DT::renderDataTable({
    
    
    flowd = dbGetQuery(con, flowq)
    flowd$name = paste0("<a href=https://www.realto.ch/userFlow/",
                        flowd$id,
                        ">",
                        flowd$name,
                        "</a>")
    flowd$id = NULL
    flowd
  }, escape = F, server = F, caption = "Number of posts per flow, all time, or in the last 7 days")
}

# Run the application
shinyApp(ui = ui, server = server)

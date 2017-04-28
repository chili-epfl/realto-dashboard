## app.R ##
library(shiny)
library(shinydashboard)
library(magrittr)
library(RPostgreSQL)
library(dygraphs)
library(xts)
library(DT)
library(reshape2)
source("/home/haklev/app/eventcount.R")

header <- dashboardHeader(title = "REALTO dashboard")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Most active flows", tabName = "flows", icon = icon("th")),
    menuItem("All activity", tabName = "activity", icon = icon("dashboard")),
    menuItem("New posts", tabName = "posts", icon = icon("th")),
    menuItem("Unique users by month", tabName = "uniqueUsers", icon = icon("th")),
    menuItem("Most active users", tabName = "mostActive", icon = icon("th")),
    menuItem("REALTO", icon = icon("file-code-o"), 
             href = "https://www.realto.ch")
  )
)

body <- dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "activity", 
            verticalLayout(
                dygraphOutput("rollerActivities"),
                dygraphOutput("rollerActivities1")),
            flowLayout(
              sliderInput("rollPeriod", "Smoothing:", 1, 100, 1),
              radioButtons("activityProf", "Professions",
                           c("All" = "all",
                             "Clothing designers" = "clothesDesigner",
                             "Florists" = "florist",
                             "Carpenters" = "carpenter",
                             "Multimedia electronicians"= "multimediaElectronician")),
              radioButtons("activityLang", "Languages",
                           c("All" = "all",
                             "German" = "de",
                             "French" = "fr",
                             "Italian" = "it"))
              
            )
            
    ),
    tabItem(tabName = "posts",
            dygraphOutput("rollerPost"),
            sliderInput("rollPost", "Smoothing:", 1, 100, 1)
    
    ),
    tabItem(tabName = "uniqueUsers",
            dygraphOutput("uniqueUsersPlot"),
            fillRow(
            sliderInput("uniqueUsersSmoothing", "Smoothing:", 1, 100, 1),
            radioButtons("uniqueUsersGran", "Unique users per:",
                         c("Day" = "day",
                           "Week" = "week",
                           "Month" = "month"))
    )),
    tabItem(tabName = "mostActive",
      DT::dataTableOutput("mostActiveTable")
    ),
    
    tabItem(tabName = "flows",
            DT::dataTableOutput("flowsTable")
    )
    
  )
)

ui <- dashboardPage(header, sidebar, body)

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host="127.0.0.1", user="shiny", dbname="realto", password="d0f98sd0f9832oj42kFDFDF")

server <- function(input, output) {
  
  # all activity
  activityData = reactive({
    professionq = function(x) { paste0("LEFT JOIN professions p ON u.profession_id = p._id WHERE p.name LIKE '",x ,"'") }
    localeq = function(x) { paste0("u.locale LIKE '", x,"'") }
    
    if(input$activityLang != 'all') { 
      langq = localeq(input$activityLang)
    } else { 
      langq = "" 
    }
    
    if(input$activityProf != 'all') { 
      profq = professionq(input$activityProf)
      if (langq != "") { 
        langq = paste0(" AND ", langq) 
      }
    } else {
      profq = "" 
      if (langq != "") { 
        langq = paste0(" WHERE ", langq) 
      }
    }

    query = paste0("SELECT a.date::DATE, u.role_id, count(a.*) AS n FROM user_activity_logs_cleaner a LEFT JOIN users u ON a.user_id = u._id ", profq, langq," GROUP BY date::date, u.role_id")
    queryall = paste0("SELECT a.date::DATE, count(a.*) AS n FROM user_activity_logs_cleaner a LEFT JOIN users u ON a.user_id = u._id ", profq, langq," GROUP BY date::date")
    pcount = dbGetQuery(con, query)
    pall = dbGetQuery(con, queryall)
    if(nrow(pall) == 0) {
      c("empty", "empty")
    } else {
      pwide = dcast(pcount, date ~ role_id)
      pwide$admin = NULL
      pwide$researcher = NULL
      
      qxts <- xts(pwide[,-1], order.by=as.Date(pwide[,1])) 
      qxts1 <- xts(pall[,-1], order.by=as.Date(pall[,1])) 
      list(qxts, qxts1)
    }
  })
    output$rollerActivities <- renderDygraph({
      a = activityData()[[1]]
      if(class(a) == "character" && a == "empty") {
        NA
      } else {
        dygraph(a, main="Groups", group="1") %>% 
          dyRoller(rollPeriod = as.numeric(input$rollPeriod)) %>% 
          dyOptions(fillGraph = TRUE, fillAlpha = 0.2)
      }
    })
    output$rollerActivities1 <- renderDygraph({
      b = activityData()[[2]]
      if(class(b) == "character" && b == "empty") {
        dygraph(x)
      } else {
        dygraph(b, main="All", group="1") %>% 
          dyRangeSelector %>% 
          dyRoller(rollPeriod = as.numeric(input$rollPeriod)) %>%
          dyOptions(fillGraph = TRUE, fillAlpha = 0.4)
      }
    })
      
    # new posts
    postcgroup = dbGetQuery(con, "select CREATED_AT::date as n, count(*) FROM posts GROUP BY created_at::date")
    pqxts <- xts(postcgroup[,-1], order.by=as.Date(postcgroup[,1])) 
    output$rollerPost <- renderDygraph({
      dygraph(pqxts) %>% dyRangeSelector %>% dyRoller(rollPeriod = as.numeric(input$rollPost))
    })
  
    # weekly unique users
    output$uniqueUsersPlot <- renderDygraph({
      monthquery = paste0("WITH a AS (SELECT user_id, date_trunc('", input$uniqueUsersGran, "', DATE) AS DATE FROM user_activity_logs) 
                                     SELECT DATE, count(distinct(user_id)) FROM a GROUP BY DATE ORDER BY DATE desc")
      month = dbGetQuery(con, monthquery)
      mxts <- xts(month[,-1], order.by=as.Date(month[,1])) 
      dygraph(mxts) %>% dyRangeSelector %>% dyRoller(rollPeriod = as.numeric(input$uniqueUsersSmoothing))
    
  })
  
  # most active users
  output$mostActiveTable <- DT::renderDataTable({
      mostactiveq = "WITH postcnt AS (SELECT count(*) AS n, owner_id AS uid FROM posts GROUP BY uid),
ldcnt AS (SELECT count(*) AS n, apprentice_id AS uid FROM learning_doc_entries GROUP BY uid)
      SELECT u._id as personalflow, u.first_name, u.last_name, coalesce(max(p.n),0) as postsn, coalesce(max(l.n),0) as ldocsn FROM users u FULL JOIN postcnt p ON u._id = p.uid FULL JOIN ldcnt l ON u._id = l.uid GROUP BY u._id;"
      mostactive = dbGetQuery(con, mostactiveq)
      mostactive$posts = paste0("<a href=https://www.realto.ch/userFlow/", mostactive$personalflow, ">", as.numeric(mostactive$postsn),"</a>")
      mostactive$learndocs = paste0("<a href=https://www.realto.ch/learnDoc?apprentice=", mostactive$personalflow, ">", as.numeric(mostactive$ldocsn),"</a>")
      mostactive$personalflow = NULL
      mostactive$postsn = NULL
      mostactive$ldocsn = NULL
      mostactive
  }, escape=F, server=F)    
  
  # most active flows
  output$flowsTable <- DT::renderDataTable({
    flowq = "WITH p_n AS (SELECT flow_id, count(*) AS n FROM posts GROUP BY flow_id),
p_wk AS (SELECT flow_id, count(*) AS n FROM posts WHERE (created_at > CURRENT_DATE - INTERVAL '7 days')
    GROUP BY flow_id),
    ff AS (SELECT f._id, coalesce(max(p_n.n),0) AS alltime, coalesce(max(p_wk.n),0) AS last7 FROM flows f FULL JOIN p_n ON f._id = p_n.flow_id FULL JOIN p_wk ON f._id = p_wk.flow_id WHERE f.type LIKE 'group' OR f.type LIKE 'school' AND f.deleted = FALSE GROUP BY f._id)
    SELECT flows._id AS id, flows.name, alltime, last7 FROM ff LEFT JOIN flows ON ff._id = flows._id;"
    
    flowd = dbGetQuery(con, flowq)
    flowd$name = paste0("<a href=https://www.realto.ch/userFlow/", flowd$id, ">", flowd$name,"</a>")
    flowd$id = NULL
    flowd
  }, escape=F, server=F, caption="Number of posts per flow, all time, or in the last 7 days")    
}

# Run the application 
shinyApp(ui = ui, server = server)


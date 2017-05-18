########################################## filters #################################
#-----------filters
professionsList=c(
  "All" = "all",
  "Clothing designers" = "clothesDesigner",
  "Florists" = "florist",
  "Carpenters" = "carpenter",
  "Multimedia electronicians" = "multimediaElectronician",
  "Painter" = 'painter'
  #"Automatiker"="automatiker"
  # "Sanitaire"="sanitaire",
  # "ElectricalFitter"="electricalFitter",
  # "Mecanic Production"="mecanicProduction",
  # "PolyMecanic"="polyMecanic"
)

languageList=c(
  "All" = "all",
  "German" = "de",
  "French" = "fr",
  "Italian" = "it"
)
roleList=c(
  "All"="all",
  "Apprentice"="apprentice",
  "Teacher"="teacher",
  "Supervisor"="supervisor"
)
postTypeList=
  c(
    "All"="all",
    "standard post"="standard",
    "learnDoc"="learnDoc",
    "activity"="activity",
    "activity Submission"="activitySubmission",
    #"learningJournal"="learningJournal",
    "standardLd"="standardLd"
  )
########################################## UI #################################
header <- dashboardHeader(title = "REALTO dashboard")

sidebar <- dashboardSidebar(sidebarMenu(
  menuItem("Most active flows", tabName = "flows", icon = icon("dashboard")),
  menuItem("Most active users", tabName = "mostActiveUsers", icon = icon("th")),
  menuItem("Users", tabName = "uniqueUsers",    icon = icon("th") ),
  menuItem("All activity",tabName = "activity", icon = icon("th")),
  menuItem("Posts and Users/posts", tabName = "posts", icon = icon("th")),
  menuItem("Users clusters", tabName = "Usersclusters", icon = icon("th")),
  menuItem("Social Network", tabName = "SocialNetwork", icon = icon("th")),
  menuItem("REALTO", icon = icon("file-code-o"), href = "https://www.realto.ch")
))


body <- dashboardBody(tabItems(
  #----------------------- tab: activity ----------------------------
  tabItem( tabName = "activity",
           dygraphOutput("rollerActivities"),
           flowLayout(
             sliderInput("rollPeriod", "Smoothing:", 1, 100, 1),
             uiOutput('activityProf_dropdown'),
             # selectInput("activityProf", "Professions", professionsList),
             selectInput( "activityLang",   "Languages",languageList),
             selectInput( "userRole",  "User role",     roleList    ),
             uiOutput('activitySchool_dropdown')
           ),
           htmlOutput("activitySql")
  ),
  
  #----------------------- tab: posts ----------------------------
  tabItem(    tabName = "posts",
              h3('Number of posts over time'),
              dygraphOutput("rollerPost"),
              flowLayout(
                sliderInput("rollPeriodpost", "Smoothing:", 1, 100, 1),
                uiOutput('activityProfpost_dropdown'),
                # selectInput("activityProfpost","Professions",professionsList  ),
                selectInput( "activityLangpost","Languages", languageList),
                selectInput("userRolepost","User role",roleList),
                selectInput( "postType", "Post type", postTypeList),
                uiOutput('activitySchoolpost_dropdown')
              ),
              h3('Sequence of post Types by each individual user'),
              h5('Each rows represents one user, columns represent weeks/month, colors encode type of activity'),
              selectInput(   "posSeq_time_window",     "Time window:", c( "Week" = "week",  "Month" = "month")  ),
              plotOutput("usersPostSequencePlot"),
              h3('Users and posts distribution'),
              plotOutput("postUsers"),
              
              htmlOutput("postsql"),
              textOutput("postsUsersSql"),
              textOutput("usersPostSequenceSql")
  ),
  
  #----------------------- tab:  uniqueUsers ----------------------------
  tabItem(  tabName = "uniqueUsers",
            fluidRow(
              h3('Unique users/timeframe'),
              dygraphOutput("uniqueUsersPlot"),
              flowLayout(
                sliderInput("uniqueUsersSmoothing", "Smoothing:", 1, 100, 1),
                selectInput( "uniqueUsersGran", "Unique users per:",   c("Day" = "day",  "Week" = "week",    "Month" = "month" )  ),
                uiOutput('activityProfUnique_dropdown'),
                # selectInput("activityProfUnique", "Professions",professionsList  ),
                selectInput("activityLangUnique","Languages", languageList ),
                selectInput( "userRoleUnique","User role",roleList),
                uiOutput('activitySchoolUnique_dropdown')
                
              ),
              h3('Cumulative registered users'),
              plotOutput("cumulUsersPlot")
            ),
            
            htmlOutput("uniqueSql"),
            htmlOutput("cumulUsersSql")
  ),
  #----------------------- tab:  most Active users ----------------------------
  tabItem(tabName = "mostActiveUsers",
          DT::dataTableOutput("mostActiveTable"),
          htmlOutput("mostActiveUsersSql")
  ),
  
  #----------------------- tab: platform usage clusters ----------------------------
  tabItem(tabName = "Usersclusters",
          h3("Clusters of users based on their platform usage"),
          h5("Use slider below the chart to change the number of clustes."),
          plotOutput("usageClustersPlot"),
          flowLayout(
            sliderInput("users_clust_cnt:","Nubmer of clusters", 1, 9, 1),
            checkboxInput("NormalizeVals", 'Normalize values', value = FALSE, width = NULL),
            uiOutput('userClustProf_dropdown'),
            # selectInput("userClustProf", "Professions",professionsList  ),
            selectInput("userClustLang","Languages", languageList ),
            selectInput( "userClustRole","User role",roleList),
            uiOutput('userClustSchool_dropdown')
            
          ),
          plotOutput("usageBarPlot"),
          DT::dataTableOutput("usageTable"),
          htmlOutput("UsersclusterSql")
  ),
  
  #----------------------- tab:  most Active flows ----------------------------
  tabItem(tabName = "flows",
          DT::dataTableOutput("flowsTable"),
          htmlOutput("flowSql")
  ),
  #----------------------- tab:  Social network ----------------------------
  tabItem(tabName = "SocialNetwork",
          h3("Social network of users"),
          #-----force net
          forceNetworkOutput("socialNetPlot_force"),
          #---- options
          flowLayout(
            selectInput( "socialLinkType","Link type: ",c("All"="'comment', 'like'" , "Comment" = "'comment'",  "Like" = "'like'" )  ),  
            uiOutput('socialProf_dropdown'),
            # selectInput("socialProf", "Professions",professionsList  ),
            selectInput("socialLang","Languages", languageList ),
            uiOutput('socialSchool_dropdown')
            
          ),
          #-----sankey
          h3("Strongest connections"),
          # h5("Use slider to change the number of visualized connections."),
          sliderInput("social_num_link:","Nubmer of connections", 2, 300, 15),
          sankeyNetworkOutput("socialNetPlot_sankey"),
          # showOutput('socialNetPlot_sankey2', 'd3_sankey'),
          #------- sql query
          htmlOutput("socialNetSql")
  )
))

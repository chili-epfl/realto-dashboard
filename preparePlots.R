# ========================== sequence of post types =====================================
getPostSequencePlot <-  function (p, input){
  dateLimits= input$rollerPost_date_window  
  startT= as.Date(dateLimits[[1]], format = "%Y-%m-%dT%H:%M:%S.")
  endT= as.Date(dateLimits[[2]], format = "%Y-%m-%dT%H:%M:%S.")
 
  postsTypefilter =input$postType
  p$User_name= paste(p$first_name, p$last_name)
  p$time=  as.Date(p$time, format = "%Y-%m-%d %H:%M:%S")                                         
  if (postsTypefilter=='all'){  # to reduce cimbinations, replace standardLd with learnDoc and activitySubmission by activity
    p$post_type=gsub('activitySubmission', 'activity', p$post_type)
    p$post_type=gsub('standardLd', 'learnDoc', p$post_type)
    p$post_type=gsub('standard', 'standard post', p$post_type)
    #--- remove duplicates after the replacement
    p$post_type= vapply(lapply(strsplit(p$post_type, ", "), unique), paste, character(1L), collapse = ", ")
  }  
  ggplot(data=p, aes(time,User_name  ))+ geom_tile(aes(fill = post_type))+
    scale_x_date(breaks = waiver(),labels = date_format("%d %b %y"),limits = c(startT,endT) )+labs( main='')
  
}
#============================ Social network ================================================
# getSocialNetPlot_sankey2 <-  function(p, input){
#   p$from_fullName= paste(p$from_first_name, p$from_last_name)
#   p$to_fullName= paste(p$to_first_name, p$to_last_name)
#   p=p[, c('from_fullName' , 'to_fullName' ,'weight'  )]
#   colnames(p) <- c("source", "target", "value")
#   # remove self loops, as sankey doesn't support that
#   p = filter(p, source != target)
#   #---- number of links to be shown
#   links_count = min (nrow(p) , input$social_num_link)
#   # change target labels, in sankey if source and target overlap, they 
#   p$target=paste0(p$target,' ')
#   # sort : highest weights on top
#   p=p[order(-p$value),]
#   sankeyPlot <- rCharts$new()
#   sankeyPlot$setLib("./d3_sankey")
#   sankeyPlot$set(
#     data = p[1:links_count,],    nodeWidth = 30,    nodePadding = 7,    layout = 30,
#     width = 900,     height =600,
#     labelFormat = "0.01"
#   )
#   (sankeyPlot)
# }
getSocialNetPlot_sankey <-  function(p, input){
    p$from_fullName= paste(p$from_first_name, p$from_last_name)
    p$to_fullName= paste(p$to_first_name, p$to_last_name)
    p=p[, c('from_fullName' , 'to_fullName' ,'weight'  )]
    colnames(p) <- c("source", "target", "value")
  # change target labels, in sankey if source and target overlap, they 
    p$target=paste0(p$target,' ')
  # remove self loops, as sankey doesn't support that
    # p = filter(p, source != target)
    p=p[order(-p$value),] #--- sort links based on weight
    links_count = min (nrow(p) , input$social_num_link) #number of links to be shown
    p=p[1:links_count,]# keep only 
    
  
    ####### first build igraph
    edgeslist=(p[,c("source", "target")])
    edgeslist=  as.vector(as.character(as.matrix(t(edgeslist))))
    g <- graph(edgeslist)
    E(g)$weight <- p$value
    
    ####### then convert it to networkd3 graph
    wc <- cluster_walktrap(g)
    members <- membership(wc)
    # Convert to object suitable for networkD3
    g_d3 <- igraph_to_networkD3(g, group = members)
    #--- plot as sankey net
    sankeyNetwork(Links = g_d3$links, Nodes = g_d3$nodes,
                Source = 'source', Target = 'target', NodeID = 'name', Value='value',
                units = "TWh", fontSize = 12, nodeWidth = 30,nodePadding=15)
  
}   

getSocialNetPlot_force <-  function(p, input){
  p$from_fullName= paste(p$from_first_name, p$from_last_name)
  p$to_fullName= paste(p$to_first_name, p$to_last_name)
  p=p[, c('from_fullName' , 'to_fullName' ,'weight'  )]
  colnames(p) <- c("source", "target", "value")
  # remove self loops, as sankey doesn't support that
  # p = filter(p, source != target)
  ####### first build igraph
  edgeslist=(p[,c("source", "target")])
  edgeslist=  as.vector(as.character(as.matrix(t(edgeslist))))
  g <- graph(edgeslist)
  E(g)$weight <- p$value
  
  ####### then convert it to networkd3 graph
  wc <- cluster_walktrap(g)
  members <- membership(wc)
  # Convert to object suitable for networkD3
  g_d3 <- igraph_to_networkD3(g, group = members)
  #--- plot as force directed net
  forceNetwork(Links = g_d3$links, Nodes = g_d3$nodes,
               Source = 'source', Target = 'target', NodeID = 'name',Value='value',
               Group = 'group',opacity = 1, bounded = TRUE, zoom=T,
               # linkWidth = networkD3::JS("function(d) {  return Math.sqrt(d.value)*2; }"),
               opacityNoHover =10, fontSize = 12 )
}   

# ========================== individuals platform usage =====================================

getUsageBarPlot<-  function(p, input) {
  p$comment=p$comments_n; p$comments_n=NULL
  fs=c( "satndard_posts", "learning_document","activity_submission", "activity","comment" )
  
  mp= melt(p[,], id.vars = c("personalflow","first_name","last_name" )) 
  names(mp)[names(mp)=='variable']='type'
  mp$user_Name= paste(mp$first_name, mp$last_name)
  mp=filter(mp,type %in% fs) 
  #---- barplot 
  ggplot(mp, aes(user_Name,value)) +geom_bar(stat = "identity", aes(fill = type))+theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
}
#============================ clusters of platform usage ================================================
getUsageClusterPlot<-  function(p, input) {
  p$comment=p$comments_n; p$comments_n=NULL
  fs=c( "learning_document","satndard_posts", "activity_submission", "activity","comment" )
  #++++++++++++++++1 cluster users based on behaviours -----------
  pointsToCluster <- p [, fs]
  Nclust=input$users_clust_cnt
  n=nrow(pointsToCluster)
  #------- 'CLARA' ----------
  clMethod='CLARA'
  sample_size=min(n,input$social_num_link);
  set.seed(123456); fitClara=clara(pointsToCluster, k=Nclust,samples=(n/sample_size)*2, sampsize=sample_size, keep.data=F)
  cluster=(fitClara$clustering); table(cluster)
  #-----------2. boxplot split by cluster, scale all measures to 0-1 range
  subset2= pointsToCluster [, c(fs)]
  (cn= as.data.frame(table(cluster)))
  subset2$cluster=cluster;
  subset2=merge(subset2, cn)
  subset2$cluster= paste('Cluster',subset2$cluster, '(N:', subset2$Freq, ')')
  subset2=subset2[c(fs,'cluster' )]
  if (input$NormalizeVals)
    subset2<- data.frame(lapply(subset2, function(y) if(is.numeric(y)) (y)/(max(y, na.rm = T)+0.001) else y)) # 
  melted<-melt(subset2,id=c("cluster")); melted$type=melted$variable; melted$variable=NULL
  
  ggplot(melted, aes(type,value, fill=type ))+geom_boxplot( outlier.shape=1)+facet_wrap(~cluster)+
      theme(text = element_text(30),axis.text.x = element_blank())+
      labs(title="",   x =" ", y = "Normalized Value" )
    
}

#============================Regularity================================================
  #------- BINARY similarity -------------
  binarydSimilarity= function(a, b) {
    ab= a*b
    maxWorkDays= max(length(a[a>0]) , length(b[b>0]))
    sim=length(ab[ab>0])/maxWorkDays
    return (round(sim,2))
  }
# *********************** 1. WSB *************
    getRegularity_WSB= function(events) {
      #------- prepare events file
      events$weekofyear=(events$year - min(events$year))*53+events$weekofyear
      events$weekofyear=events$weekofyear- min(events$weekofyear)
      events$fullname=paste(events$first_name, events$last_name)
      # a=ddply(events, .(user_id,first_name,last_name,role_id,weekofyear), summarize, cnt=length(date))
      # hist(a$weekofyear, breaks=max(a$weekofyear))
      #---------------------- compute weekly profile similarity (excclude inactive weeks from computation ----------------
      user_week_day=ddply(events, .(user_id,fullname,role_id,weekofyear,dyofweek), summarize, Study.time=length(unique(hourofday)))
      users=sort(unique(user_week_day$fullname))
      users_WSB= data.frame(fullname=users, WSB=0,activeWeeks=0)
      weeksCount=max(user_week_day$weekofyear)
      for (u in users){
        #---- 1. build weekly profile
        currentUserActs= filter(user_week_day,fullname==u)
        weeks_profile=matrix(0, nrow = weeksCount, ncol = 7)
        for( w in 0:weeksCount)
          for( d in 1:7 )
          {
            a=filter(currentUserActs,weekofyear==w, dyofweek==d)
            if(nrow(a)>0)
              weeks_profile[w,d] = a$Study.time
          }
        #----- 2. compute pairwise weekly similarity
        BIN_sumSimilarity=NULL; # JSD_sumSimilarity=0;  CHI_sumSimilarity=0;
        activeWeeksCount=0
        for(i in 1:(weeksCount-1))
        {
          if (Norm(weeks_profile[i,], p = 2)!=0) activeWeeksCount=activeWeeksCount+1;
          for (j in (i+1):weeksCount)
          {
            if (Norm(weeks_profile[i,], p = 2)*Norm(weeks_profile[j,], p = 2)!=0) 
            {       
              curSim=binarydSimilarity(weeks_profile[i,],weeks_profile[j,] )
              BIN_sumSimilarity = c(BIN_sumSimilarity ,curSim )
              # print(paste0(i,'    ', j, '   --- ' , round(curSim,2)))
            }
          }
        }
        #---- 3. add average to users
        # print(paste0(u,'    ' ,round(mean(BIN_sumSimilarity),2), '      weeks: ', activeWeeksCount))
        users_WSB[which(users_WSB$fullname==u),2]=round(mean(BIN_sumSimilarity),2)
        users_WSB[which(users_WSB$fullname==u),3]=activeWeeksCount
      }
      #------ multiply computed WSB s by ratio of active weeks
      second_max=sort(users_WSB$activeWeeks,decreasing = T)[2]
      users_WSB$weekRatio=users_WSB$activeWeeks/second_max; 
      users_WSB[which(users_WSB$weekRatio>1), 'weekRatio']=1
      users_WSB$WSB_2=round(users_WSB$WSB*users_WSB$weekRatio,2)
      # ----- 4 merge computed values
      user_week_day=merge(user_week_day,users_WSB)
      
      ######## boxplot of class average
      apprentices=filter(user_week_day,role_id=='apprentice')$fullname
      apprentices_reg=filter(users_WSB,fullname %in% apprentices)
      
      m=round(mean(apprentices_reg$WSB_2),2); sd=round (sd((apprentices_reg$WSB_2)),2)
      boxplot_wsb_flow=ggplot(apprentices_reg, aes(x='WSB', y=WSB_2))+geom_boxplot(fill='springgreen4',alpha=0.7)+theme_bw()+
        scale_y_continuous(name = "Weekly Similarity Binary (WSB)",limits=c(0, 1))+
        labs(x ="", y = "WSB", title= paste0('Flow overview \n m=',m, '   sd=', sd))+
        theme(text = element_text(size=15), axis.text.x = element_text(angle = 0, hjust = 0.5),
              plot.title = element_text(hjust = 0.5),panel.grid.minor = element_blank())
      
      #------- 5 plot weeks profile
      maxVal=max(user_week_day$Study.time)
      cellsCol='red'
      daylabels=c('Mon','Tue', 'Wed','Thu','Fri','Sat','Sun')
      ######## weeks profile
       plot_profiles=ggplot(user_week_day, aes(dyofweek, weekofyear)) +theme_bw()+
        geom_tile(aes(fill = Study.time),colour = "red" )+
        scale_fill_gradient(limits=c(0,maxVal),low = "white",high = cellsCol, na.value = "dimgrey")+
        # scale_y_continuous(limits = c(0.5, 10.5), breaks=seq(1,weeksCount,1))+
        scale_x_continuous(breaks=seq(1,7,1), labels = daylabels)+
        theme(text = element_text(size=15), axis.text.x = element_text(angle = 0, hjust = 0.5), 
              legend.position ="right",strip.background = element_rect( fill='papayawhip'),panel.grid.major =element_blank())+
        labs(x ="Day", y = "Week", title='')+ facet_wrap(~paste(role_id, ': ',fullname )+paste('  WSB:', WSB_2 ), ncol=4)
      print(plot_profiles)
    
      p=grid.arrange(plot_profiles,boxplot_wsb_flow, ncol=2, widths=c(80,20))
      return(p)
    }
  # *********************** 2. PWD: Peak on Week day regularity *************
#------------------ entropy function
    H <- function(v) {
      v <- v[v > 0]
      return(sum(-v * log(v)))
        }
    getRegularity_peakWeekDay= function(events) {
      daylabels=c('Mon','Tue', 'Wed','Thu','Fri','Sat','Sun')
      #------- prepare events file
      events$weekofyear=(events$year - min(events$year))*53+events$weekofyear
      events$weekofyear=events$weekofyear- min(events$weekofyear)
      events$fullname=paste(events$first_name, events$last_name)
      
      #----------- weekly histograms -------------------
      user_weekday_count=ddply(events, .(user_id,fullname,role_id,dyofweek), summarize, countOfWeeks=length(unique(weekofyear)))
      users=sort(unique(user_weekday_count$fullname))
      users_PWD= data.frame(fullname=users, PWD=0)
      for (u in users){
        currentUserHist=filter(user_weekday_count,fullname==u)
        W_d=currentUserHist[order(currentUserHist$dyofweek),'countOfWeeks']
        max_val_in_WeekDay=max(W_d)
        W_d_normalized=W_d / sum(W_d)
        (E_w=H(W_d_normalized))
        PWD= (log(7) - E_w) * max_val_in_WeekDay
        users_PWD[which(users_PWD$fullname==u),2]=round(PWD,2)
      }
      #---merge
      user_weekday_count=merge(user_weekday_count,users_PWD)
      
      ######### boxplot of class average  for PWD
      apprentices=filter(user_weekday_count,role_id=='apprentice')$fullname
      apprentices_reg=filter(users_PWD,fullname %in% apprentices)
      m=round(mean(apprentices_reg$PWD),2); sd=round (sd((apprentices_reg$PWD)),2)
      boxplot_PWD_flow=
        ggplot(apprentices_reg, aes(x='PWD', y=PWD))+geom_boxplot(fill='springgreen4',alpha=0.7)+theme_bw()+
        scale_y_continuous(name = "Peak on Week Day (PWD)")+
        labs(x ="", y = "PWD", title= paste0('Flow overview \n m=',m, '   sd=', sd))+
        theme(text = element_text(size=15), axis.text.x = element_text(angle = 0, hjust = 0.5),
              plot.title = element_text(hjust = 0.5),  panel.grid.minor = element_blank())      
      ######### weekly histograms and PWD for each user
      weeklyHistogramPlots=
        ggplot(data=user_weekday_count, aes(x=dyofweek,y=countOfWeeks))+theme_bw()+
        geom_bar(stat="identity",fill='blue')+
        # scale_y_continuous(limits = c(0, 10), breaks=seq(0,weeksCount,2))+
        scale_x_continuous(breaks=seq(1,7,1), labels = daylabels)+
        labs(title="Weekly histogram of each apprentice and teacher",   x ="Week day", y = "Count of weeks",  title= 'Weekly histograms and PWD regularity') +
        theme(text = element_text(size=13), axis.text.x = element_text(angle = 0, hjust = 0.5),
              strip.background = element_rect( fill='papayawhip'),
              panel.grid.minor = element_blank(),plot.title = element_text(hjust = 0.5))+
        facet_wrap(~paste(role_id, ': ',fullname )+paste('  PWD:', PWD ), ncol=4)
      ######### accumulated weekly histograms and PDH apps vs tacher
      weeklyHistogramAppsandTeacher=
        ggplot(data=user_weekday_count, aes(x=dyofweek,y=countOfWeeks))+theme_bw()+
        geom_bar(stat="identity",fill='blue')+
        # scale_y_continuous(limits = c(0, 10), breaks=seq(0,weeksCount,2))+
        scale_x_continuous(breaks=seq(1,7,1), labels = daylabels)+
        labs(title="Acuumulated weekly histogram of all apprentices and teacher",   x ="Week day", y = "Count of weeks",  title= 'Weekly histograms and PWD regularity') +
        theme(text = element_text(size=15), axis.text.x = element_text(angle = 0, hjust = 0.5),
              strip.background = element_rect( fill='papayawhip'),
              panel.grid.minor = element_blank(),plot.title = element_text(hjust = 0.5))+
        facet_wrap(~paste(role_id), scales = "free",ncol=4)
      
      #------ merge plots --------
      p=grid.arrange(weeklyHistogramPlots,boxplot_PWD_flow, weeklyHistogramAppsandTeacher, 
                     layout_matrix = rbind(c(1,1,1,2),c(1,1,1,2),c(1,1,1,2),c(1,1,1,2),c(1,1,1,2), c(3,3,3,3),c(3,3,3,3)))
      
      return(p)
    }
    
    
# *********************** 3. PDH: peak on week day regularity *************
    getRegularity_peakDayHour= function(events) {
      # daylabels=c('Mon','Tue', 'Wed','Thu','Fri','Sat','Sun')
      #------- prepare events file
      events$weekofyear=(events$year - min(events$year))*53+events$weekofyear
      events$weekofyear=events$weekofyear- min(events$weekofyear)
      events$dayofyear=(events$year - min(events$year))*365+events$dayofyear
      events$fullname=paste(events$first_name, events$last_name)
      
      #----------- daily histograms -------------------
      user_dayhour_count=ddply(events, .(user_id,fullname,role_id,hourofday), summarize, countOfDays=length(unique(dayofyear)))
      users=sort(unique(user_dayhour_count$fullname))
      users_PDH= data.frame(fullname=users, PDH=0)
      for (u in users){
        # u=users[1]
        currentUserHist=filter(user_dayhour_count,fullname==u)
        D_h=rep(0, 24)
        D_h[currentUserHist$hourofday]=currentUserHist$countOfDays
        max_val_in_dayHour=max(D_h)
        D_h_normalized=D_h / sum(D_h)
        (E_d=H(D_h_normalized))
        PDH= (log(24) - E_d) * max_val_in_dayHour
        users_PDH[which(users_PDH$fullname==u),2]=round(PDH,2)
      }
      
      #---merge
      user_dayhour_count=merge(user_dayhour_count,users_PDH)
      
      ######### boxplot of class average  for PWD
      apprentices=filter(user_dayhour_count,role_id=='apprentice')$fullname
      apprentices_reg=filter(users_PDH,fullname %in% apprentices)
      m=round(mean(apprentices_reg$PDH),2); sd=round (sd((apprentices_reg$PDH)),2)
      boxplot_PDH_flow=
        ggplot(apprentices_reg, aes(x='PDH', y=PDH))+geom_boxplot(fill='springgreen4',alpha=0.7)+theme_bw()+
        scale_y_continuous(name = "Peak on Day Hour (PDH)")+
        labs(x ="", y = "PDH", title= paste0('Flow overview \n m=',m, '   sd=', sd))+
        theme(text = element_text(size=12), axis.text.x = element_text(angle = 0, hjust = 0.5),
              plot.title = element_text(hjust = 0.5),  panel.grid.minor = element_blank())      
      ######### daily histograms and PDH for each user
      dailyHistogramPlots=
        ggplot(data=user_dayhour_count, aes(x=hourofday,y=countOfDays))+theme_bw()+
        geom_bar(stat="identity",fill='blue')+
        # scale_y_continuous(limits = c(0, 10), breaks=seq(0,weeksCount,2))+
        scale_x_continuous(breaks=seq(1,24,3), labels = seq(1,24,3))+
        labs(title="Daily histogram of each apprentice and teacher",   x="Day hour", y = "Count of days" ,  title= 'Weekly histograms and PWD regularity') +
        theme(text = element_text(size=13), axis.text.x = element_text(angle = 0, hjust = 0.5),
              strip.background = element_rect( fill='papayawhip'),
              panel.grid.minor = element_blank(),plot.title = element_text(hjust = 0.5))+
        facet_wrap(~paste(role_id, ': ',fullname )+paste('  PDH:', PDH ), ncol=4)
      
      ######### accumulated daily histograms and PDH apps vs tacher
      dailyHistogramAppsandTeacher=
        ggplot(data=user_dayhour_count, aes(x=hourofday,y=countOfDays))+theme_bw()+
        geom_bar(stat="identity",fill='blue')+
        # scale_y_continuous(limits = c(0, 10), breaks=seq(0,weeksCount,2))+
        scale_x_continuous(breaks=seq(1,24,1), labels = c(1:24))+
        labs(title="Acuumulated daily histogram of all apprentices, and teacher",   x="Day hour", y = "Count of days" ,  title= 'Weekly histograms and PWD regularity') +
        theme(text = element_text(size=15), axis.text.x = element_text(angle = 0, hjust = 0.5),
              strip.background = element_rect( fill='papayawhip'),
              panel.grid.minor = element_blank(),plot.title = element_text(hjust = 0.5))+
        facet_wrap(~paste(role_id),scales = "free", ncol=4)
      #------ merge plots --------
      p=grid.arrange(dailyHistogramPlots,boxplot_PDH_flow, dailyHistogramAppsandTeacher, 
                     layout_matrix = rbind(c(1,1,1,2),c(1,1,1,2),c(1,1,1,2),c(1,1,1,2),c(1,1,1,2), c(3,3,3,3),c(3,3,3,3)))
      
    }
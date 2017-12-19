# ========================== sequence of post types =====================================
getPostSequencePlot <-  function (p, input){
  dateLimits= input$rollerPost_date_window  
  startT= as.Date(dateLimits[[1]], format = "%Y-%m-%dT%H:%M:%S.")
  endT= as.Date(dateLimits[[2]], format = "%Y-%m-%dT%H:%M:%S.")
 
  postsTypefilter =input$postType
  # p$User_name= paste(p$first_name, p$last_name)
  p$User_name=paste(substr(p$first_name,1,1), '. ', substr(p$last_name,1,2),'. ')
  p$Action_type=p$post_type
  p$time=  as.Date(p$time, format = "%Y-%m-%d %H:%M:%S")                                         
  if (postsTypefilter=='all'){  # to reduce cimbinations, replace standardLd with learnDoc and activitySubmission by activity
    p$Action_type=gsub('activitySubmission', 'activitySub', p$Action_type)
    p$Action_type=gsub('standardLd', 'learnDoc', p$Action_type)
    p$Action_type=gsub('standard', 'post', p$Action_type)
    #--- remove duplicates after the replacement
    p$Action_type= vapply(lapply(strsplit(p$Action_type, ", "), unique), paste, character(1L), collapse = ", ")
  }  
  pl=ggplot(data=p, aes(time,User_name  ))+ geom_tile(aes(fill = Action_type))+
    scale_x_date(breaks = waiver(),labels = date_format("%d %b %y"),limits = c(startT,endT) )+
    theme_bw()+theme(text = element_text(15))+   labs(title="",   x ="Time", y = "User Name" )
  print(pl)
  # ggsave (paste0('PostSequence_',input$userRolepost,'_', input$activityProfpost, '.png'), width = 30, height = 12, units = 'cm')
  return(pl)
}
# ========================== individuals platform usage =====================================

getUsageBarPlot<-  function(p, input) {
  # p$2comment=p$comments_n; p$comments_n=NULL
  names(p)[names(p)=='comments_n']='comment'
  names(p)[names(p)=='satndard_posts']='post'
   # p$post=p$satndard_posts; p$satndard_posts=NULL
  fs=c( "learning_document","post","activity_submission", "activity","comment" )
  mp= melt(p, id.vars = c("personalflow","first_name","last_name" )) 
  names(mp)[names(mp)=='variable']='type'
  # mp$User_name= paste(mp$first_name, mp$last_name)
  mp$User_name=paste(substr(mp$first_name,1,1), '. ', substr(mp$last_name,1,2),'. ')
  
  mp=filter(mp,type %in% fs) 
  
  #---- barplot 
  pl=ggplot(mp, aes(User_name,value)) +geom_bar(stat = "identity", aes(fill = type))+theme_bw()+
    theme_bw()+ theme(text = element_text(15),axis.text.x = element_blank())+labs(title="",   x =" User Name", y = "Count" )+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  print(pl)
  # ggsave (paste0('Users_usage_', input$userClustRole,'_',input$userClustProf, '.png'), width = 30, height = 12, units = 'cm')
  return(pl)
}
#============================ clusters of platform usage ================================================
getUsageClusterPlot<-  function(p, input) {
  names(p)[names(p)=='comments_n']='comment'
  # p$comment=p$comments_n; p$comments_n=NULL
  names(p)[names(p)=='satndard_posts']='post'
  # p$post=p$satndard_posts; p$satndard_posts=NULL
  fs=c( "learning_document","post","activity_submission", "activity","comment" )
  
  #++++++++++++++++1 cluster users based on behaviours -----------
  pointsToCluster <- p [, fs]
  Nclust=input$users_clust_cnt
  n=nrow(pointsToCluster)
 
  #------- 'HC' ----------
  # model_clust <- hclust(dist(pointsToCluster), method="ward.D2")#"ward.D2")
  # cluster <- cutree(model_clust, k=Nclust) # cut tree into K clusters
   #------- 'CLARA' ----------
  clMethod='CLARA'
  sample_size=min(n,input$social_num_link);
  set.seed(123456); fitClara=clara(pointsToCluster, k=Nclust,samples=(n/sample_size)*2, sampsize=sample_size, keep.data=F)
  cluster=(fitClara$clustering); table(cluster)
  #------- 'PAM' ----------
  # model_clust=pam(pointsToCluster, k=Nclust)
  # cluster=model_clust$clustering; 
  
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
  
  pl=ggplot(melted, aes(type,value, fill=type ))+geom_boxplot( outlier.shape=1)+facet_wrap(~cluster, ncol = 4)+
    theme_bw()+ theme(text = element_text(15),axis.text.x = element_blank())+labs(title="",   x =" ", y = "Count" )
  print(pl)# 
  ## ggsave (paste0('ClusterBoxPlot_',input$users_clust_cnt,'Clusters_',input$userClustRole,'role_',input$userClustProf,'prof_','.png'), width = 10, height = 10, units = 'cm')
  ## if (!input$NormalizeVals)
    #ggsave (paste0('Clusters_',input$userClustRole,'_', input$userClustProf, '.png'), width = 25, height = 8*(ceiling(input$users_clust_cnt/4)), units = 'cm')
  return(pl)
}

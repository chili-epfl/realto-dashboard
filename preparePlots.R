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

#----------------- clusters of platform usage
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

getUsageClusterPlot<-  function(p, input) {
  p$comment=p$comments_n; p$comments_n=NULL
  fs=c( "satndard_posts", "learning_document","activity_submission", "activity","comment" )
  #++++++++++++++++1 cluster users based on behaviours -----------
  pointsToCluster <- p [, fs]
  Nclust=4
  n=nrow(pointsToCluster)
  #------- 'CLARA' ----------
  clMethod='CLARA'
  sample_size=100;
  set.seed(123456); fitClara=clara(pointsToCluster, k=Nclust,samples=(n/sample_size)*2, sampsize=sample_size, keep.data=F)
  cluster=(fitClara$clustering); table(cluster)
  #-----------2. boxplot split by cluster, scale all measures to 0-1 range
  subset2= pointsToCluster [, c(fs)]
  (cn= as.data.frame(table(cluster)))
  subset2$cluster=cluster;
  subset2=merge(subset2, cn)
  subset2$cluster= paste('Cluster',subset2$cluster, '(N:', subset2$Freq, ')')
  subset2=subset2[c(fs,'cluster' )]
  subset2<- data.frame(lapply(subset2, function(y) if(is.numeric(y)) (y-min(y, na.rm = T))/(max(y, na.rm = T)-min(y, na.rm = T)) else y)) # # N(m=0, sigma=1)
  melted<-melt(subset2,id=c("cluster")); melted$type=melted$variable; melted$variable=NULL
  
  ggplot(melted, aes(type,value, fill=type ))+geom_boxplot( outlier.shape=1)+facet_wrap(~cluster)+
      theme(text = element_text(25),axis.text.x = element_blank())+
      labs(title="",   x =" ", y = "Normalized Value" )
    
}
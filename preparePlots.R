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
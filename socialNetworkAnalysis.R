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
  #---- filter
  roles_type=input$socialRoleType
  if(roles_type=='all')
    roles_type= c('apprentice', 'supervisor','teacher')
  p=filter(p, (from_role== 'apprentice' &  to_role %in% roles_type ) | (to_role== 'apprentice' &  from_role %in% roles_type ))
  
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


prepareNetwork <-  function(p, roles_type){
  p$from_fullName= paste(p$from_first_name, p$from_last_name)
  p$to_fullName= paste(p$to_first_name, p$to_last_name)
  #---- filter based on roles
  # roles_type=input$socialRoleType
  if(roles_type=='all')    roles_type= c('apprentice', 'supervisor','teacher')
  p=filter(p, (from_role== 'apprentice' &  to_role %in% roles_type ) | (to_role== 'apprentice' &  from_role %in% roles_type ))
  #---- get users-role mapping
  from_u_role=unique(p[, c('from_fullName', 'from_role')]);   names(from_u_role)= c('name', 'role')
  to_u_role=unique(p[, c('to_fullName', 'to_role')]);  names(to_u_role)= c('name', 'role')
  names_roles=(rbind(from_u_role, to_u_role))
  names_roles=names_roles[!duplicated(names_roles$name),]
  #----  build igraph
  edgeslist=(p[,c("from_fullName", "to_fullName")])
  edgeslist=  as.vector(as.character(as.matrix(t(edgeslist))))
  g <- graph(edgeslist)
  E(g)$weight <- p$weight
  V(g)$role   <-   as.vector(sapply(V(g)$name, function(v) {as.character(names_roles[which(names_roles$name==v), 'role']) }))
  V(g)$totalDegree=as.character(igraph::degree(g))
  V(g)$outDegree=as.character(igraph::degree(g,mode = 'out'))
  V(g)$inDegree=as.character(igraph::degree(g,mode = 'in'))
  # write.graph(g,'nn', 'gml')
  g
}
plotSocialNetPlot_force <-  function(g, input){
  #---- convert igraph to networkd3 graph
  #---color nodes based on detected communities
  # wc <- cluster_walktrap(g)
  # members <- membership(wc)
  # g_d3 <- igraph_to_networkD3(g, group =members)
  #-- color nodes based on role
  g_d3 <- igraph_to_networkD3(g, group = V(g)$role)
  g_d3$nodes$totlDegree=V(g)$totalDegree
  g_d3$nodes$inDegree=V(g)$inDegree
  g_d3$nodes$outDegree=V(g)$outDegree
  #--- plot as force directed net
  forceNetwork(Links = g_d3$links, Nodes = g_d3$nodes,Source = 'source', Target = 'target',
               NodeID = 'name', Nodesize ='outDegree',Value='value',
               zoom=F,Group = 'group',opacity = 10,opacityNoHover =0.2, fontSize = 12 ,legend=T,arrows = T, bounded =T,
               # linkWidth = networkD3::JS("function(d) {  return Math.sqrt(d.value)*2; }"),
               height = 600,width = 600,colourScale = JS("force.velocityDecay([0.5]);d3.scaleOrdinal(d3.schemeCategory10);"))
}

getNetworkAttributes <- function(g, roles_type){
  ug <- as.undirected(g)
  sp <- shortest.paths(g)
  pathesInd <- intersect(which(sp > 0), which(sp < Inf))
  
  componentsDiameters= sapply(decompose(g), function(sg) {
    diameter(sg)
  })
  componentsDiameters=componentsDiameters[order(componentsDiameters,decreasing = T)]
  componentsSize=(components(g)$csize)
  componentsSize=componentsSize[order(componentsSize, decreasing = T)]
  
  res=data.frame( networkType=roles_type,
                  nodes=length(V(g)),
                  edges=length(E(g)),
                  avg_degree=mean(igraph::degree(g,mode = 'all')),
                  density=( 2 * length(E(ug)) / (length(V(ug)) * (length(V(ug)) - 1))), 
                  reciprocity = reciprocity(g),
                  diameter=diameter(g),
                  avg_pathLen=mean(sp[pathesInd]), 
                  clust_coeff =mean(transitivity(g,type="local"), na.rm = T),
                  components_cnt=components(g)$no,
                  comp_meanSize=mean(componentsSize)
  )
  res
  res= data.frame(lapply(res, function(y) if(is.numeric(y)) round(y, 2) else y)) 
}

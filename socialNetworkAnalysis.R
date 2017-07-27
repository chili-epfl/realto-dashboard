#----------------------------------- filterLinkTypes --------------------------------------- 
filterLinkTypes<-  function (p, linkTypes)
{
  print(paste0('linkTypes: ', linkTypes))
#--- filter link type
  linkTypes_set=linkTypes
  if (linkTypes=='ld_all') linkTypes_set =c("ld_create", "ld_feedback", "ld_comment")
  if (linkTypes=='social') linkTypes_set =c("comment", "like", "ld_comment")
  if (linkTypes !='all'){    p= filter(p,link_type %in% linkTypes_set )  }
#----- remove links from / to researcher and admins$
  p= filter(p,from_role %in% c('apprentice' , 'supervisor' ,   'teacher' ) & to_role %in% c('apprentice', 'supervisor', 'teacher' )) 
#------- build full name -------
  p$from_fullName= paste(p$from_first_name, p$from_last_name)
  p$to_fullName= paste(p$to_first_name, p$to_last_name)
  p= filter(p, from_fullName != 'Valentina Caruso' & to_fullName!='Valentina Caruso')
  #------remove self loops
  p= filter(p, from_fullName != to_fullName)
  #----- order so that links in alphabetic order (to get fixed color for apprentice role in network)
  p=p[order(p$from_role, p$to_role),]
  #   if (linkTypes %in% c("ld_feedback", "ld_comment"))      p=p[order(p$to_role, p$from_role),]
  p
}
#----------------------------------- prepareNetwork --------------------------------------- 

  prepareNetwork <-  function(p, roles_type){
    #---------------- compute weight of connections
    p= ddply(p,.(from_fullName,to_fullName, from_role, to_role), summarize, weight=length(from_role) )
    print(paste0('roles_type: ', roles_type))
    #---- filter based on roles
    roles=roles_type
    if(roles_type=='all')  roles= c('apprentice', 'supervisor','teacher')
    # apprentices connected to both teacher and supervisor
    if(roles_type=='teacher_supervisor')       roles= c('supervisor','teacher')
    if (roles_type!='all') p=filter(p, (from_role %in% c('apprentice') &  to_role %in% roles )      | 
                (from_role %in% roles &  to_role %in% c('apprentice') )
                )
    #---- get users-role mapping
    from_u_role=unique(p[, c('from_fullName', 'from_role')]);   names(from_u_role)= c('name', 'role')
    to_u_role=unique(p[, c('to_fullName', 'to_role')]);  names(to_u_role)= c('name', 'role')
    names_roles=(rbind(from_u_role, to_u_role))
    names_roles=names_roles[!duplicated(names_roles$name),]
    #----  build igraph
    p=p[order(p$from_role, p$to_role),]
    
    edgeslist=(p[,c("from_fullName", "to_fullName")])
    edgeslist=  as.vector(as.character(as.matrix(t(edgeslist))))
    g <- graph(edgeslist)
    E(g)$weight <- p$weight
    
    V(g)$role   <-   as.vector(sapply(V(g)$name, function(v) {as.character(names_roles[which(names_roles$name==v), 'role']) }))
    if(roles_type=='teacher_supervisor'){
      V(g)$connectedRoles <- sapply(1:length(V(g)), function(i) { 
                          neighborsRoles=paste(sort(unique(unlist(neighbors(g, V(g)[i],'all')$role))), collapse = '_') } )
      g <- induced.subgraph (g, (grepl('supervisor_teacher',V(g)$connectedRoles   )) | (V(g)$role != 'apprentice'))
      g <- induced.subgraph(g, which(degree(g) > 0))
    }
    V(g)$totalDegree=as.character(igraph::degree(g))
    V(g)$outDegree=as.character(igraph::degree(g,mode = 'out'))
    V(g)$inDegree=as.character(igraph::degree(g,mode = 'in'))
    
    # write.graph(g,'nn', 'gml')
    #---- remove edges with a total weight smaller than 2 in the undirected network and remove isolated nodes
    # g=delete.edges(g, which(E(as.undirected(g))$weight <=1)) # here's my condition.
    # g <- induced.subgraph(g, which(degree(g) > 0))
    g
  }
#----------------------------------- plotSocialNetPlot_force --------------------------------------- 

  plotSocialNetPlot_force <-  function(g){
    #---- convert igraph to networkd3 graph
    #---color nodes based on detected communities
    # wc <- cluster_walktrap(g)
    # members <- membership(wc)
    # g_d3 <- igraph_to_networkD3(g, group =members)
    #-- color nodes based on role
    g_d3 <- igraph_to_networkD3(g, group = V(g)$role)
    g_d3$nodes$totalDegree=V(g)$totalDegree
    g_d3$nodes$inDegree=V(g)$inDegree
    g_d3$nodes$outDegree=V(g)$outDegree
    #--- plot as force directed net
    forceNetwork(Links = g_d3$links, Nodes = g_d3$nodes,Source = 'source', Target = 'target',
                 NodeID = 'name', Nodesize ='totalDegree',Value='value',
                 zoom=F,Group = 'group',opacity = 10,opacityNoHover =0.0, fontSize = 14 ,legend=T,arrows = T, bounded =T,
                 # linkWidth = networkD3::JS("function(d) {  return Math.sqrt(d.value)*2; }"),height = 600,width = 600,
                 colourScale = JS('d3.select("svg").style("background-color", "white");force.velocityDecay([0.65]);d3.scaleOrdinal(d3.schemeCategory10);'))
  }
#----------------------------------- getNetworkAttributes --------------------------------------- 

  getNetworkAttributes <- function(g, roles_type){
    ug <- as.undirected(g)
    sp <- shortest.paths(g)
    pathesInd <- intersect(which(sp > 0), which(sp < Inf))
    componentsDiameters= sapply(decompose(g), function(sg) {diameter(sg)  })
    
    componentsDiameters=componentsDiameters[order(componentsDiameters,decreasing = T)]
    componentsSize=(components(g)$csize)
    componentsSize=componentsSize[order(componentsSize, decreasing = T)]
    componentsDensity= sapply(decompose(g), function(sg) {    ug=as.undirected(sg)
      density=( 2 * length(E(ug)) / (length(V(ug)) * (length(V(ug)) - 1)))
      density
    })
    res=data.frame( #networkType=roles_type,
                    nodes=length(V(g)),
                    edges=length(E(g)),
                    sum_linkWeight= sum(E(g)$weight),
                    avg_linkWeight= mean(E(g)$weight),
                    avg_degree=mean(igraph::degree(g,mode = 'all')),
                    avg_weightedDegree=mean(igraph::strength(g,mode = 'all')),
                    density=( 2 * length(E(ug)) / (length(V(ug)) * (length(V(ug)) - 1))), 
                    reciprocity = reciprocity(g),
                    apprentices=length(V(g)[which(V(g)$role=='apprentice')]),
                    teachers=length(V(g)[which(V(g)$role=='teacher')]),
                    supervisors=length(V(g)[which(V(g)$role=='supervisor')]),
                    apps_inDeg=mean(degree(g, mode='in')[which(V(g)$role=='apprentice')]),
                    apps_outDeg=mean(degree(g, mode='out')[which(V(g)$role=='apprentice')]),
                    apps_totDeg=mean(degree(g)[which(V(g)$role=='apprentice')]),
                    teacher_inDeg=mean(degree(g, mode='in')[which(V(g)$role=='teacher')]),
                    teacher_outDeg=mean(degree(g, mode='out')[which(V(g)$role=='teacher')]),
                    teacher_totDeg=mean(degree(g)[which(V(g)$role=='teacher')]),
                    supervisor_inDeg= mean(degree(g, mode='in')[which(V(g)$role=='supervisor')]),
                    supervisor_outDeg=mean(degree(g, mode='out')[which(V(g)$role=='supervisor')]),
                    supervisor_totDeg=mean(degree(g)[which(V(g)$role=='supervisor')]),
                    apprentices_betweenness=mean(betweenness(g, directed = F, normalized = F)[which(V(g)$role=='apprentice')]),
                    teachers_betweenness=mean(betweenness(g, directed = F, normalized = F)[which(V(g)$role=='teacher')]),
                    supervisors_betweenness=mean(betweenness(g, directed = F, normalized = F)[which(V(g)$role=='supervisor')]),
                    apprentices_weighted_InDeg=mean(igraph::strength(g,mode = 'in')[which(V(g)$role=='apprentice')]),
                    apprentices_weighted_OutDeg=mean(igraph::strength(g,mode = 'out')[which(V(g)$role=='apprentice')]),
                    teacher_weighted_InDeg=mean(igraph::strength(g,mode = 'in')[which(V(g)$role=='teacher')]),
                    teacher_weighted_OutDeg=mean(igraph::strength(g,mode = 'out')[which(V(g)$role=='teacher')]),
                    supervisor_weighted_InDeg=mean(igraph::strength(g,mode = 'in')[which(V(g)$role=='supervisor')]),
                    supervisor_weighted_OutDeg=mean(igraph::strength(g,mode = 'out')[which(V(g)$role=='supervisor')]),
                    diameter=diameter(g),
                    avg_pathLen=mean(sp[pathesInd]), 
                    clust_coeff =transitivity(g,type="global"),#mean(transitivity(g,type="local"), na.rm = T),
                    components_cnt=components(g)$no,
                    comp_meanSize=mean(componentsSize),
                    comp_meanDensits=mean(componentsDensity)
    )
    res= data.frame(lapply(res, function(y) if(is.numeric(y)) round(y, 2) else y)) 
  }
#----------------------------------- getBlockModel --------------------------------------- 
  getBlockModel <- function(g, rolesCount){
    clusters <- getRegularSimilarityClusters(g,rolesCount)
    table(clusters)
    dens <- length(E(g)) / (length(V(g)) * (length(V(g)) - 1))
    bm <- crit.fun(get.adjacency(g, sparse=FALSE), clusters, approach="bin", blocks=c("null","reg"),
                   blockWeights=c(null=1,reg=dens/(1-dens)), norm=TRUE)
    
    adjMat <- matrix(0, nrow=nrow(bm$IM), ncol=ncol(bm$IM))
    adjMat[bm$IM == "reg"] <- 1
    roleGraph <- graph.adjacency(adjMat)
    
    V(roleGraph)$members <- sapply(1:length(V(roleGraph)), function(cl) {
      paste(V(g)$name[bm$clu == cl], collapse=",")})
  
    V(roleGraph)$roles <- sapply(1:length(V(roleGraph)), function(cl) {
      paste(V(g)$role[bm$clu == cl], collapse=",")})

    V(roleGraph)$membersCount <- sapply(1:length(V(roleGraph)), function(cl) {
      length(which(bm$clu == cl))  })
    
    V(roleGraph)$size <- round( (V(roleGraph)$membersCount / length(V(g)))*100 * 1.5 , 0)
    
    V(roleGraph)$apprenticesCount <- sapply(1:length(V(roleGraph)), function(cl) {
      sum(V(g)$role[bm$clu == cl]=='apprentice')})
    
    V(roleGraph)$teachersCount <- sapply(1:length(V(roleGraph)), function(cl) {
      sum(V(g)$role[bm$clu == cl]=='teacher')})
    
    V(roleGraph)$supervisorCount <- sapply(1:length(V(roleGraph)), function(cl) {
      sum(V(g)$role[bm$clu == cl]=='supervisor')})
    
    V(roleGraph)$AvgIndegree <- sapply(1:length(V(roleGraph)), function(cl) {
      round(mean(as.numeric(V(g)$inDegree)[bm$clu == cl]),1)})
    
    V(roleGraph)$AvgOutdegree <- sapply(1:length(V(roleGraph)), function(cl) {
      round(mean(as.numeric(V(g)$outDegree)[bm$clu == cl]),1)})
    
    V(roleGraph)$AvgTotalDegree <- sapply(1:length(V(roleGraph)), function(cl) {
      round(mean(as.numeric(V(g)$totalDegree)[bm$clu == cl]),1)})
    
    #-- give names to roles based on connection patterns
    V(roleGraph)$label <-sapply(1:length(V(roleGraph)), function(node) {
      paste('R', node )
    })
    V(roleGraph)$role <-sapply(1:length(V(roleGraph)), function(node) {
      inDeg=degree(roleGraph, mode='in',loops=F)[node]
      outDeg=degree(roleGraph, mode='out',loops=F)[node]
      totalDeg_noLoop=degree(roleGraph, mode='all',loops=F)[node]
      totalDeg_withLoop=degree(roleGraph, mode='all',loops=T)[node]
      label=NULL
      if (inDeg>0)  label=c(label,'In')  ;    if (outDeg>0) label=c(label,'Out') 
      if (totalDeg_withLoop>totalDeg_noLoop)   label=c(label,'Self') 
      label=paste(label, collapse = '_')
      # if(label=='In_Out_Self') label='Core'; 
      if(label=='Self') {
        label='Self'; 
      }  else if(grepl('Self', label)) {
        label='Core'; 
      } else if(label=='') {
        label='Inactive'
      }
      # label=paste0(label, '-',V(roleGraph)$membersCount[node])
      return(label)
    })
    # write.graph(roleGraph, 'bm', "gml")
    return(roleGraph)
  
  }
  
  
# #----------------------------------- getBlockModelAttributes --------------------------------------- 
  getBlockModelAttributes <- function(roleGraph){
    res=sapply(V(roleGraph), function(b) {
      c( 
        'Label'=V(roleGraph)[b]$label,
        'Structural Role'=V(roleGraph)[b]$role,
        'Size'=V(roleGraph)[b]$membersCount,
        '#apprentices'=V(roleGraph)[b]$apprenticesCount,
        '#teachers'=V(roleGraph)[b]$teachersCount,
        '#supervisors'=V(roleGraph)[b]$supervisorCount,
        'Avg In-degree'=V(roleGraph)[b]$AvgIndegree,
        'Avg out-degree'=V(roleGraph)[b]$AvgOutdegree,
        'Avg total-degree'=V(roleGraph)[b]$AvgTotalDegree
      )
    })
    res
  }
 
#----------------------------------- plotBlockModel --------------------------------------- 
  
  plotBlockModel <- function(roleGraph){
    myColPal=brewer.pal(length(V(roleGraph)),'Accent')
    plot.igraph(roleGraph, 
                edge.width=2,edge.color='black',
                vertex.color=myColPal, #vertex.size=80,
                vertex.label.color="black", vertex.label.cex=1.5, vertex.label.dist=0, vertex.shape="square")
    }
#------------------------------- getRegularSimilarityClusters ------------------------------
  getRegularSimilarityClusters <- function(g,nroles) {
    regeDist <- REGE.for(get.adjacency(g, sparse=FALSE))$E
    # require(fpc)
    # nc <- nselectboot(regeDist, clustermethod = disthclustCBI, method="ward.D", krange = 2:5)$kopt
    nc <- nroles  # number of roles
    dendogram <- hclust(as.dist(1 - regeDist), method = "ward.D2")
    # plot(dendogram)
    cutree(dendogram, nc)
    
  }
  
#---- add a new role for inactive users
#   roleGraph <- add.vertices(roleGraph, 1)
#   V(roleGraph)$members[length(V(roleGraph))] <- paste(inactives, collapse=",")
#   V(roleGraph)$size[length(V(roleGraph))]    <- length(inactives)

#----------------------------------- getSocialNetPlot_sankey --------------------------------------- 

  getSocialNetPlot_sankey <-  function(p, input){
    p= ddply(p,.(from_fullName,to_fullName, from_role, to_role), summarize, weight=length(from_role) )
    
    #---- filter role
    roles_type=input$socialRoleType
    if(roles_type=='all')
      roles_type= c('apprentice', 'supervisor','teacher')
    p=filter(p, (from_role== 'apprentice' &  to_role %in% roles_type ) | (to_role== 'apprentice' &  from_role %in% roles_type ))
    
    p=p[, c('from_fullName' , 'to_fullName' ,'weight'  )]
    colnames(p) <- c("source", "target", "value")
    # change target labels, sankey chart doesn't support self loops
    p$target=paste0(p$target,' ')
    p=p[order(-p$value),] #--- sort links based on weight
    links_count = min (nrow(p) , input$social_num_link) #number of links to be shown
    p=p[1:links_count,]# keep only strongest links
    
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

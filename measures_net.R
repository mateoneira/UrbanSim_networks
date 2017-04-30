#functions for measures
#Author: Mateo Neira
require(igraph)
#put all edge measures in data frame
g.df<-function(g_london){
  tube_edges<- data.frame(
    name=V(g_london)$name,
    long=V(g_london)$x,
    lat=V(g_london)$y,
    deg=degree(g_london),
    cls=closeness(g_london),
    btw=betweenness(g_london, v=V(g_london), directed = F, normalized = FALSE),
    cls_t=closeness(g_london, weights = NA),
    btw_t=betweenness(g_london, v=V(g_london), directed = F, normalized = FALSE, weights = NA)
  )
  
  #normalize measures
  tube_edges$nrm_cls<-(tube_edges$cls-min(tube_edges$cls))/(max(tube_edges$cls)-min(tube_edges$cls))
  tube_edges$norm_btw<-(tube_edges$btw-min(tube_edges$btw))/(max(tube_edges$btw)-min(tube_edges$btw))
  return(tube_edges)
}

#average inverse geodesic distance (average inverse shortest path)
average.inv.path<-function(x){
  #x must be a igraph
  inv.dist.matrix<-1/distances(x)
  diag(inv.dist.matrix)<-0
  res<-(1/(vcount(x)*(vcount(x)-1)))*sum(colSums(inv.dist.matrix/2))
  return(res)
}

topo.integ<-function(x0,x){
  #x is new graph and x0 is original graph
  gclust<-clusters(x0)
  g.comp0<-induced.subgraph(x0, V(x0)[which(gclust$membership == which.max(gclust$csize))])
  
  gclust<-clusters(x)
  g.comp<-induced.subgraph(x, V(x)[which(gclust$membership == which.max(gclust$csize))])
  
  res<-vcount(g.comp)/vcount(g.comp0)
  return(res)
}

random.failures<-function(x, n){
  for(j in c(1:n)){
    print(paste("running random removals: ", j))
    g0<-x
    g<-x
    f<-c() #fraction of removed nodes
    aip<-c() # average inverse path
    ti<-c() #topological integrity
    d<-c() #diameter
    for(i in c(1:vcount(g0))){
      f[i]<-i/vcount(g0)
      aip[i]<-average.inv.path(g)/average.inv.path(g0)
      ti[i]<-topo.integ(g0,g)
      d[i]<-diameter(g)/diameter(g0)
      
      #chose one vertex to remove
      r.vertex<-sample(V(g)$name,1)
      
      #delete chosen vertex
      g<-delete.vertices(g,r.vertex)
    }
    
    #create data frame with values
    if(j==1){
      aip.df<-data.frame(aip)
      ti.df<-data.frame(ti)
      d.df<-data.frame(d)
    }
    aip.df<-cbind(aip.df,data.frame(aip))
    ti.df<-cbind(ti.df, data.frame(aip))
    d.df<-cbind(d.df, data.frame(d))
  }
  
  res<-data.frame(fn=f, invpath= rowMeans(aip.df), tint= rowMeans(ti.df), dia= rowMeans(d.df), type = "random")
  return(res)
}

directed.failures<-function(x, type="betweenness"){
  g0<-x
  g<-x
  f<-c() #fraction of removed nodes
  aip<-c() # average inverse path
  ti<-c() #topological integrity
  d<-c() #diameter
  
  for(i in c(1:vcount(g0))){
    print(i)
    f[i]<-i/vcount(g0)
    aip[i]<-average.inv.path(g)/average.inv.path(g0)
    ti[i]<-topo.integ(g0,g)
    d[i]<-diameter(g)/diameter(g0)
    
    #chose one vertex to remove
    #calculatemeasures
    if(type == "betweenness"){
      measures<-betweenness(g)
      r.vertex<-names(sort(measures, decreasing=T)[1])
    }
    
    if(type == "closeness"){
      gclust<-clusters(g)
      g.comp<-induced.subgraph(g, V(g)[which(gclust$membership == which.max(gclust$csize))])
      measures<-closeness(g.comp)
      r.vertex<-names(sort(measures, decreasing=T)[1])
      if(is.na(measures[1])){
        r.vertex<-names(measures[1])
      }
    }
    
    if(type == "degree"){
      measures<-degree(g)
      r.vertex<-names(sort(measures, decreasing=T)[1])
    }
    print(paste("Removing node: ", r.vertex))
    #delete chosen vertex
    g<-delete.vertices(g,r.vertex)
  }
  
  res<-data.frame(fn=f, invpath=aip, tint=ti, dia=d, type = type)
  return(res)
}


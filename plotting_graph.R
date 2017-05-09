library(reshape2)
library(OpenStreetMap)
library(ggplot2)
library(RColorBrewer)
library(igraph)

#load graph
g_london<-read_graph("london_tube.graphml", format = "graphml")

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

data<-g.df(g_london)

#set parameters for map, including background map
map_layout<-theme(
  axis.text = element_text(size=8, family = "sans", face="plain"),
  panel.background=element_blank(), 
  plot.title=element_text(hjust=0.5, size = 11, family = "sans", face="bold"), 
  plot.subtitle = element_text(hjust=0.5, size = 10, family = "sans", face="plain"),
  legend.position = "right",
  legend.background = element_blank(),
  plot.caption = element_text(hjust=0.5, size=8, family = "sans", face="plain")
)

#bounding box for background map, in WGS84
x1<- -0.621
x2<- 0.261
y1<- 51.3922
y2<- 51.7152

#get map
basemap<-openmap(c(y2,x1),c(y1,x2), zoom=NULL, "stamen-toner")
basemap_BNG<-openproj(basemap, projection="+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs")

#set palette
col<-rev(brewer.pal(11, "Spectral"))
palette<-colorRampPalette(col)
palette<-scale_colour_gradientn(colours = col, "")

autoplot(basemap_BNG)+
  geom_point(data=data, aes(x=long, y=lat, color=norm_btw), size=1.5, alpha=0.8)+
  palette+
  map_layout+ggtitle("London Underground Network", subtitle = "Centrality")+
  labs(
    x="Eastings",
    y="Northings",
    title="London Underground Network",
    subtitle = "Betweenness Centrality",
    caption = "\n basemap: ©OpenStreetMap contributors")



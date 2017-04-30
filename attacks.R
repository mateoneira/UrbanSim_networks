source("measures_net.R")
library(reshape2)

g_london<-read_graph("london_tube.graphml", format = "graphml")

#calculate on random failures
rnd.fl<-random.failures(g_london, 20)

#calculate betweeness centraility failure
btw.fl<-directed.failures(g_london, type="betweenness")
clo.fl<-directed.failures(g_london, type="closeness")
deg.fl<-directed.failures(g_london, type="degree")

failures.df<-rbind(btw.fl, clo.fl, rnd.fl)

#plot results
data<-melt(failures.df, measure.vars=c("invpath", "tint", "dia"))

library(ggplot2)
library(ggthemes)
ggplot(data=data, aes(x=fn, y=value))+
  geom_path(aes(colour= type, group=type))+
  theme_minimal()+palette+
  facet_wrap("variable" , nrow = 1)+
  scale_y_continuous(breaks = seq(from=0, to=1.25, by=0.25))+coord_equal()

ggplot(data=data[which(data$fn<0.1),], aes(x=fn, y=value))+
  geom_path(aes(colour= type, group=type))+
  theme_minimal()+palette+
  facet_wrap("variable" , nrow = 1)+
  scale_y_continuous(breaks = seq(from=0, to=1.25, by=0.25))


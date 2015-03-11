rm(list=ls())
setwd('~/Dropbox/workspace/Projects/BL-strategies/')

library(ProjectTemplate)
load.project()
delta<-1L;tau=1/50;baselines <- c('true','naive','default');confid.id <- c('cons','last','ma');percentile <- 0.05
##rolling rankings: 20 qtrs - no winning; 16 qtrs - win roll.sd (last); 12

### market data: ~ 113 sec (mcapply: ~ 52 sec)
system.time(source('munge/01-data.q.ret.R'))

### data for first paper
system.time(source('src/paper.trading.APS.true.R'))

### State var. data: ~118 sec
#system.time(source('munge/02-state-variables.R'))
### Analysts ranking data: ~ 280 sec
system.time(source('munge/03-analysts.process.R'))
### Predicting ~ 347 sec
#system.time(source('src/predicting.R'))

#pt.accu[,mean(value),by=.(variable)]

#ggplot(pt.accu[,mean(value),by=.(q.id,variable)],aes(x=as.Date(q.id),y=V1,group=variable,color=variable))+geom_line(size=0.5,alpha=0.7)+geom_smooth(method='loess',se=F,size=1L)+theme_bw()

### trading: ~ 322 sec
#system.time(source('src/trading.R'))

#ggplot(res.accu[,mean(value,na.rm=T),by=.(q.id,variable,conf)],aes(x=as.Date(q.id),y=V1,color=variable,group=variable))+geom_line()+facet_grid(conf~.,scale='free_y')+theme_bw()



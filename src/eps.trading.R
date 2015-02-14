rm(list=ls())
setwd('~/Dropbox/workspace/Projects/Black-Litterman/BL-strategies/')

library(ProjectTemplate)
load.project()

delta<-2.5
system.time(source('munge/01-data.pre.process.R'))
system.time(source('munge/02-variables.R'))
###Ranking periods:
###true=1999 Q3 till 2010 Q1, naive: 1999 Q2 till 2009 Q4

###subset of core stocks/broker: min. 3 broker in quarter and min. 12 quarters of coverage
# setkey(market.set,q.num)

core.dt <- q.data[,core.b:=.N>=12,by=list(Stock,Broker)][(core.b)][,rank:=rank(score),by=list(q.id,Stock)][,core.s:=.N>=3,by=list(q.id,Stock)][(core.s)]

#core.dt[,descriptive.f(.SD,basic=F),by=q.id,.SDcols=c('b.view','meanTper','rank')]
#ggplot(melt(core.dt[,descriptive.f(.SD,basic=F),by=q.id,.SDcols=c('b.view','meanTper')],id.vars=c('q.id','V1'))[V1=='mean'],aes(x=as.Date(q.id),y=value,color=variable,fill=variable))+geom_bar(stat='identity')+facet_grid(variable~.)+theme_bw()

##core rankings
pt.new <- acast(core.dt,q.id~Broker~Stock,value.var='rank')
### Ranking algorithm ~ 13 min----
rank.parameters <- c(n=10,diff.lag=1,sd.lag=8)
methods<-c('raw','1diff','random','roll.sd')

#stocks=intersect(dimnames(pt.new)[[3]],dimnames(array.all.vvs)[[2]])
stocks=intersect(dimnames(pt.new)[[3]],market.set[,Stock])
 baseline.rankings <- baseline.rankings.f(pt.new[1:43,,stocks],1)
 stock.vvs <- vvs.combine(stocks,broker.vvs,array.all.vvs,t=dim(pt.new)[1])
#
 system.time(pred.r<- predict.ranking.sript.f(methods,stock.vvs,pt.new[1:43,,stocks],rank.parameters))
# #dimnames(pred.r)[[2]] <- dimnames(pt.new)[[1]][4:43]
 all.rankings <- abind(baseline.rankings[3:42,,,],aperm(pred.r,c(2,1,3,4)),along=4)
cache('all.rankings')
#load('cache/all.rankings.RData')
#------
##load EPS rankings
#load('~/Dropbox/workspace/Projects/EPS/cache/bl.all.rankings.RData')
#all.rankings <- bl.all.rankings
###Ranking: making brokers view (extended TPER for 4 quarters)
brok.tper <- acast(q.data,q.id~Broker~Stock,value.var='b.view')

###an alternaive to measure accuracy: coef. of variation. Coefficient of variation: lower values means less variable,thus, more confidence. The higher the CV, the greater the dispersion in the variable [http://www.ats.ucla.edu/stat/mult_pkg/faq/general/coefficient_of_variation.htm]

#load('cache/all.rankings.RData')
all.s <- findIntersect(all.rankings,brok.tper,3)
all.b <- findIntersect(all.rankings,brok.tper,2)


percentile <- 0.05
### Timing of brokers tper should be exactly as for ranking(pt.new)
trunk.brok.tper <- truncate.f(brok.tper[2:44,all.b,all.s],percentile)
### pt.new: 1999 Q2 - 2009 Q4 (1:43)
### baseline rankings: 1999 Q3- 2009 Q4 (3:44,of the market)
### true 1999 Q2 - 2009 Q3
### naive 1999 Q3 - 2009 Q4
### Market 1999 Q1 - 2009 Q4 (1:44)
### all.rankings 2000 Q1 - 2009 Q4

list.rank <- alply(bl.script.f(all.s,all.rankings[,all.b,all.s,],trunk.brok.tper,t=dim(all.rankings)[1]),3,.dims=T)
rank.conf <- buildOmega(apply(all.rankings[,all.b,all.s,],c(1,3),function(s){apply(s,2,evaluation.simple,s[,'true'],'s','p' ) }))

rank.views <- setnames(data.table(melt(list.rank)),c('q.num','Stock','View','Method'))[,q.id:=dimnames(all.rankings)[[1]],by=list(Stock,Method)][,year:=format(as.yearqtr(q.id),'%Y')]
cache('rank.views')

##BL inputs for non-rank strategy:meanTper and s.coefVar
meanTper <-acast(unique(setkey(q.data,Stock)[all.s],by=c('Stock','q.id')),q.id~Stock,value.var='meanTper')
trunk.meanTper <- truncate.f(meanTper,percentile)
list.non.rank <- alply(abind(true.nr=trunk.meanTper[4:43,],naive.nr=trunk.meanTper[5:44,],default.nr=rollapplyr(trunk.meanTper[5:44,],seq_len(nrow(trunk.meanTper[5:44,])),mean,na.rm=T),along=3),3,.dims=T)

nr.views <- setnames(data.table(melt(list.non.rank)),c('q.id','Stock','View','Method'))[,year:=format(as.yearqtr(q.id),'%Y')]
cache('nr.views')
require(scales)
conf.coef <- acast(unique(setkey(q.data,Stock)[all.s],by=c('Stock','q.id')),q.id~Stock,value.var='s.coefVar')

### Aggregation of non.r.conf
non.r.conf <- buildOmega(aperm(replicate(3,conf.coef[5:44,]),c(3,1,2)))
###True confidence for non.rank
non.r.conf[,1,,][!is.na(non.r.conf[,1,,])] <- 0
dimnames(non.r.conf)[[2]] <- names(list.non.rank)


#ggplot(data.table(melt(non.r.conf,na.rm=T)),aes(x=Var1,y=value,group=Var1))+geom_point()+facet_wrap(Var2~Var4)

#ggplot(data.table(melt(list.non.rank,na.rm=T)),aes(x=Var1,y=value,group=Var1))+geom_boxplot()+facet_grid(.~L1)

#(3:44)[4:42]
bl.period <- 4:40
m.period <-(length(market.list)-length(bl.period)+1) : (length(market.list))

###market timing: market.list[rankings][views]
#(1:44)[2:44][4:43]
###4:43
#m.period <- (5:44)
#m.period <- (4:43)
#m.period <- 3:44
#ggplot(data.table(melt(true.Q,na.rm=T)),aes(x=as.Date(as.yearqtr(Var1)),y=value,group=Var1))+geom_boxplot()+theme_bw()
#ggplot(data.table(melt(norm.conf,na.rm=T)),aes(x=as.Date(as.yearqtr(Var1)),y=value,group=Var1))+geom_point()


### case 1: views={rank,non.rank}; conf={non-rank}

case.1.non.r.conf <- non.r.conf[,,all.s,]
dimnames(case.1.non.r.conf)[[2]] <- names(list.rank)[1:3]
opt.w <- opt.w.f(c('last','ma','w.ma'),list.rank,c(list.rank[1:3],list.non.rank),case.1.non.r.conf,non.r.conf,case='1',0.02,delta)
cache('opt.w')

### case 2: views={rank,non-rank} and conf={rank,non-rank}
#opt.w <- opt.w.f(c('last','ma','w.ma'),list.rank,list.non.rank,rank.conf,non.r.conf,case='2',tau=0.02,delta)


final.bl <- setkey(unique(bl.results.f(opt.w),by=c('Method','Aggregation','q.id')),Method)
# order for plotting c('true','naive','default','true.nr','naive.nr','default.nr','Market')
final.bl$Method <- factor(final.bl$Method,levels=(unique(final.bl$Method)[c(6,4,2,7,5,3,1)]))
cache('final.bl')
colourCount = length(unique(final.bl$Method))
getPalette = colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))

p.cum.ret <- ggplot(final.bl,aes(x=as.Date(Quarters),y=cum.ret,group=Method,color=Method))+geom_line(size=0.5)+geom_hline(yintercept=100)+facet_wrap(~Aggregation,scale='free_x',ncol=3)+ylab('Portfolio wealth (initial=$100)')+xlab('Quarters')+ggtitle('Portfolio performance with $100 initial investment')+theme_bw(base_family='Avenir')+theme(plot.title = element_text(colour = "Blue"),legend.position='top')+scale_color_manual(values=getPalette(colourCount))+guides(color=guide_legend(nrow=3))

#+scale_x_date(breaks='year',labels=date_format('%Y'))


p.ann.ret <- ggplot(unique(melt(final.bl[,list(Method,Aggregation,ann.ret,ann.sd,ann.sr)],id.vars=c('Method','Aggregation'))),aes(x=Method,y=value))+geom_bar(aes(fill=Method),stat='identity',alpha=0.7)+facet_grid(variable~Aggregation,scale='free_y')+theme_bw(base_family='Avenir')+ggtitle('Annualized portfolio measures (return, st.dev, and the Sharpe ratio) \n conditional on confidence aggregation')+theme(plot.title = element_text(colour = "Blue"),legend.position='top',axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+scale_y_continuous(labels=percent)+ylab('Percent')+geom_text(aes(label=round(value,3)*100),angle=90,size=3,hjust=1.2)+scale_fill_manual(values=getPalette(colourCount))+guides(fill=guide_legend(nrow=3))



p.TO <- ggplot(unique(melt(final.bl[,list(Method,Aggregation,meanViews,Ave.TO)],id.vars=c('Method','Aggregation'))),aes(x=Method,y=value,fill=Method))+theme_bw(base_family='Avenir')+geom_bar(stat='identity')+facet_grid(variable~Aggregation,scale='free_y')+ggtitle('Trading statistics conditional confidence aggregation \n (averages of number of views and turnover ratio)')+theme(legend.position='none',axis.title.x=element_blank(),plot.title = element_text(colour = "Blue"),axis.title.y=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+scale_fill_manual(values=getPalette(colourCount))+guides(fill=guide_legend(nrow=3))
#+geom_text(aes(label=round(value,2)),angle=90,size=3,hjust=1.2)

require(gridExtra)
cairo_pdf("graphs/temp.ma.results.pdf", family="Avenir",h = 11.7, w = 8.3)
grid.arrange(p.cum.ret,p.ann.ret,p.TO)
dev.off()

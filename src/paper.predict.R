### tried to implement the ranking with data.table, too complicated. For rankings, see the array based approach (trading.R). Close this development.

rm(list=ls())
setwd('~/Dropbox/workspace/Projects/Black-Litterman/BL-strategies/')

library(ProjectTemplate)
load.project()
#source('lib/BL-functions.APS.R')
source('lib/BL-functions.R')
#source('lib/ranking.scripts.APS.R')
#brok.tper <- acast(unique(q.data,by=c('Broker','q.id','Stock'),fromLast = T),q.id~Broker~Stock,value.var='b.view')

stocks <- sort(unique(unlist(lapply(market.list,function(m){m$stock.names}))))
quarters <- setnames(unique(market.set[,.(Quarters)]),'q.id')[,q.id:=as.yearqtr(q.id)]

core.dt <- setkey(na.omit(setkey(na.omit(q.data),q.id)[setkey(quarters,q.id)])[,core.b:=.N>=12,by=list(Stock,Broker)][(core.b)][,true:=rank(score),by=list(q.id,Stock)][,core.s:=.N>=3,by=list(q.id,Stock)][(core.s)],Stock)[stocks]

ranked.pt.dt <- core.dt[,merge(setkey(quarters,q.id),.SD,all=T),by=list(Broker,Stock),.SDcols=c('q.id','Broker','Stock','true')][,.(q.id,Broker,Stock,true)]
cache('ranked.pt.dt')

percentile <- 0.05

pt.trunk.brok.tper <- setkey(core.dt[,merge(setkey(quarters,q.id),.SD,all=T),by=list(Broker,Stock),.SDcols=c('q.id','Broker','Stock','b.view')][,.(q.id,Broker,Stock,b.view)][,true:=truncate.f(b.view,percentile)],q.id,Stock,Broker)

pt.exp.tmp <- pt.trunk.brok.tper[,naive:=c(NA,head(true,-1)),by=.(Broker,Stock)][,default:=rollapplyr(true,seq_len(length(true)),mean,na.rm=T),by=.(Broker,Stock)]

pt.exp.ret <- setkey(melt(pt.exp.tmp,id.vars = c('q.id','Stock','Broker'),measure.vars = c('true','naive','default'),value.name = 'exp.ret',variable.name = 'Method'),q.id,Stock,Broker)


pt.new.tmp <- ranked.pt.dt[,naive:=c(NA,head(true,-1)),by=.(Broker,Stock)][,mean.rank:=rollapplyr(true,seq_len(length(true)),mean,na.rm=T),by=.(Broker,Stock)][,default:=rank(mean.rank,na.last = 'keep'),by=.(q.id,Stock)]

pt.new <- setkey(melt(pt.new.tmp,id.vars = c('q.id','Stock','Broker'),measure.vars = c('true','naive','default'),value.name = 'rank',variable.name = 'Rank'),q.id,Stock,Broker)

pt.baselines <- setkey(pt.exp.ret[pt.new,allow.cartesian=T],q.id,Stock)

pt.base.exp.ret <- pt.baselines[,rank.exp.ret.f(rank,exp.ret),by=.(q.id,Stock,Method,Rank)][V1!=0,][q.id!='1999 Q2'][Method==Rank][,Method:=NULL]


setkey(all.vvs[,q.num:=1:.N,by=.(Stock,variable)],q.num,Stock,variable)

#state.vvs <- na.omit(all.vvs)[,state.vvs.f(value,lag = 1,lag.sd = 8),by=.(Stock,variable)][,q.num:=1:.N,by=.(Stock,variable)]
#cache('state.vvs')
#setkey(state.vvs,q.num,Stock,variable)

all.vvs <- melt(all.vvs[state.vvs],id.vars = c('q.id','Stock','q.num','variable'),measure.vars = c('raw','1diff','roll.sd','random'),variable.name = 'state')

#setkey(all.vvs,q.id,Stock)
#pred.dt <- all.vvs[pt.baselines,allow.cartesian=T]

vvs <- acast(unique(all.vvs,by=c('q.id','Stock','variable','state')),q.id~Stock~variable~state,value.var = 'value')

rank.m <- acast(unique(pt.baselines[Method==Rank][Rank=='true'],by=c('q.id','Stock','Broker')),q.id~Stock~Broker,value.var = 'rank')

pred.stocks <- intersect(dimnames(rank.m)[[2]],dimnames(vvs)[[2]])

pred.rank <- setnames(data.table(melt(predict.ranking.sript.f(pred.stocks,rank.m,vvs,100)))[,Var2:=as.yearqtr(Var2)],c('Broker','q.id','Stock','Rank','rank'))

setkey(pred.rank,q.id,Stock,Broker)
pred.r.exp.ret <- pred.rank[pt.exp.ret[Method=='true']][,rank.exp.ret.f(rank,exp.ret),by=.(q.id,Stock,Method,Rank)][V1!=0,][q.id!='1999 Q2'][,Method:=NULL]

pt.ret <- rbind(pt.base.exp.ret,pred.r.exp.ret)[,full.set:=.N>3,by=.(q.id,Stock)][(full.set)]


ggplot(pt.ret,aes(x=as.Date(q.id),y=V1,group=as.Date(q.id)))+geom_boxplot()+facet_grid(Rank~.,scales='free_x')+theme_bw()

pt.list.rank <- dlply(unique(pt.ret,by=c('q.id','Stock','Rank')),'Rank',acast,q.id~Stock,value.var='V1')
pt.pred.rank.views <- setnames(pt.ret[,year:=format(as.yearqtr(q.id),'%Y')],'V1','View')
#cache('pt.rank.views')
###add NA before and after
#llply(..,function(i){rbind(rep(NA,ncol(i)),i,rep(NA,ncol(i)) ) })
#names(pt.list.rank) <- dimnames(pt.baseline.rankings)[[4]]

#pt.rank.views <- pt.exp.ret[,year:=format(as.yearqtr(q.id),'%Y')]
cache('pt.pred.rank.views')
###Accuracy
pt.pred <- rbind(pt.new,pred.rank)[,full.set:=.N>3,by=.(q.id,Stock,Broker)][(full.set)]
pt.transp <- dcast.data.table(pt.pred,q.id+Stock+Broker~Rank,value.var='rank')

pt.accu <- melt(pt.transp[,lapply(.SD,function(i){evaluation.simple(.SD[[1]],i) }),by=.(q.id,Stock),.SDcols=c('true','naive','default','raw','1diff','roll.sd','random')],id.vars=c('q.id','Stock'),na.rm=T)

ggplot(pt.accu[,mean(value),by=.(q.id,variable)],aes(x=as.Date(q.id),y=V1,group=variable,color=variable))+geom_line()
+facet_grid(Rank~.,scales='free_x')+theme_bw()

###EPS case
load('~/Dropbox/workspace/Projects/EPS/cache/complete.dt.RData')
eps.dt <- unique(setkey(na.omit(setkey(complete.dt[,':='(true=rank,q.id=calQ)],Stock)[stocks]),q.id)[quarters],by=c('q.id','Broker','Stock'))

ranked.eps.dt <- eps.dt[,merge(setkey(quarters,q.id),.SD,all=T),by=list(Broker,Stock),.SDcols=c('q.id','Broker','Stock','true')][,.(q.id,Broker,Stock,true)]
load('cache/ranked.pt.dt.RData')
ranked.eps.dt <- setkey(ranked.pt.dt,q.id,Broker,Stock)[ranked.eps.dt][,true:=NULL][,true:=i.true][,i.true:=NULL]

#cache('ranked.eps.dt')

eps.new.tmp <- ranked.eps.dt[,naive:=c(NA,head(true,-1)),by=.(Broker,Stock)][,mean.rank:=rollapplyr(true,seq_len(length(true)),mean,na.rm=T),by=.(Broker,Stock)][,default:=rank(mean.rank,na.last = 'keep'),by=.(q.id,Stock)]

eps.new <- setkey(melt(pt.new.tmp,id.vars = c('q.id','Stock','Broker'),measure.vars = c('true','naive','default'),value.name = 'rank',variable.name = 'Rank'),q.id,Stock,Broker)

eps.baselines <- setkey(pt.exp.ret[pt.new,allow.cartesian=T],q.id,Stock)

eps.base.exp.ret <- eps.baselines[,rank.exp.ret.f(rank,exp.ret),by=.(q.id,Stock,Method,Rank)][V1!=0,][q.id!='1999 Q2'][Method==Rank][,Method:=NULL]

eps.rank.m <- acast(unique(eps.baselines[Method==Rank][Rank=='true'],by=c('q.id','Stock','Broker')),q.id~Stock~Broker,value.var = 'rank')

eps.pred.stocks <- intersect(dimnames(eps.rank.m)[[2]],dimnames(vvs)[[2]])

eps.pred.rank <- setnames(data.table(melt(predict.ranking.sript.f(eps.pred.stocks,eps.rank.m,vvs,100)))[,Var2:=as.yearqtr(Var2)],c('Broker','q.id','Stock','Rank','rank'))

setkey(eps.pred.rank,q.id,Stock,Broker)

eps.pred.r.exp.ret <- eps.pred.rank[pt.exp.ret[Method=='true']][,rank.exp.ret.f(rank,exp.ret),by=.(q.id,Stock,Method,Rank)][V1!=0,][q.id!='1999 Q2'][,Method:=NULL]

eps.ret <- rbind(eps.base.exp.ret,eps.pred.r.exp.ret)[,full.set:=.N>3,by=.(q.id,Stock)][(full.set)]


eps.list.rank <- dlply(unique(eps.ret,by=c('q.id','Stock','Rank')),'Rank',acast,q.id~Stock,value.var='V1')
eps.pred.rank.views <- setnames(eps.ret[,year:=format(as.yearqtr(q.id),'%Y')],'V1','View')
cache('eps.list.rank')

meanTper <- na.omit(melt(unique(pt.trunk.brok.tper[,true:=median(true,na.rm=T),by=list(q.id,Stock)],by=c('q.id','Stock'))[,.(q.id,Stock,true)][,':='(naive=c(NA,head(true,-1)),default=rollapplyr(true,seq_len(length(true)),mean,na.rm=T)),by=Stock],id.vars = c('q.id','Stock'),measure.vars = c('true','naive','default'),variable.name = 'Method')[q.id!='1999 Q2',])

nr.views <- setnames(meanTper[,year:=format(as.yearqtr(q.id),'%Y')],'value','View')
cache('nr.views')

cons.list.rank <- dlply(unique(meanTper,by=c('q.id','Stock','Method')),'Method',acast,q.id~Stock,value.var='View')

require(scales)
#conf.coef <- acast(unique(core.dt,by=c('q.id','Stock'),fromLast = T),q.id~Stock,value.var='s.coefVar')

conf.dt.tmp <- unique(core.dt,by=c('q.id','Stock'),fromLast = T)[,.(q.id,Stock,s.coefVar)][,true:=0]

#conf.dt <- melt(conf.dt.tmp[,naive:=rollapplyr(s.coefVar,4,mean,na.rm=T,fill=NA,partial=T),by=Stock][,default:=naive],id.vars = c('q.id','Stock'),measure.vars = c('true','naive','default'))[q.id!='1999 Q2',]

conf.dt <- melt(conf.dt.tmp[,naive:=c(NA,head(s.coefVar,-1)),by=Stock][,default:=rollapplyr(naive,seq_len(length(naive)),mean,na.rm=T,partial=T),by=Stock],id.vars = c('q.id','Stock'),measure.vars = c('true','naive','default'))[q.id!='1999 Q2',]


conf.coef <- acast(conf.dt,q.id~Stock~variable,value.var='value')

eps.stocks <- intersect(colnames(eps.list.rank[[1]]),dimnames(conf.coef)[[2]])
pt.stocks <- intersect(colnames(pt.list.rank[[1]]),dimnames(conf.coef)[[2]])

##BL inputs for non-rank strategy:meanTper and s.coefVar
#cons.rankings <- pt.baseline.rankings[3:42,pt.all.b,pt.all.s,]
#cons.rankings[!is.na(cons.rankings)] <- 1


#nr.views <- na.omit(setnames(data.table(reshape2::melt(cons.list.rank)),c('q.num','Stock','View','Method'))[,q.id:=dimnames(pt.baseline.rankings)[[1]],by=list(Stock,Method)][,year:=format(as.yearqtr(q.id),'%Y')])

bl.period <- 1:40
m.period <-(length(market.list)-length(bl.period)+1) : (length(market.list))
        
delta=1
tau=1/50
#pt.opt.w <- operation.bl(pt.list.rank,conf.coef,'ma')[,Views:='TP']
#cons.opt.w <- operation.bl(cons.list.rank,conf.coef,'ma')[,Views:='CONS']
#eps.opt.w <- operation.bl(eps.list.rank,conf.coef[,eps.stocks],'ma')[,Views:='EPS']

source('lib/BL-functions.R')

pt.opt.w <- opt.w.f(pt.list.rank,conf.coef[,pt.stocks,],tau)[,Views:='TP']
cons.opt.w <- opt.w.f(cons.list.rank,conf.coef,tau)[,Views:='CONS']
eps.opt.w <- opt.w.f(eps.list.rank,conf.coef[,eps.stocks,],tau)[,Views:='EPS']


opt.w <- rbind(pt.opt.w,cons.opt.w,eps.opt.w)
cache('opt.w')


final.bl <- setkey(unique(bl.results.f(opt.w),by=c('Method','q.id','Views')),Method)
final.bl$Method <- factor(final.bl$Method,levels=unique(final.bl$Method)[c(3,2,1)])
final.bl$Views <- factor(final.bl$Views,levels=unique(final.bl$Views)[c(4,1,2,3)])
cache('final.bl')
#require("knitr")
#knit('doc/paper/paper-knitr-APS.Rnw',output = 'doc/paper/')
colourCount = length(unique(final.bl$Views))
getPalette = colorRampPalette(RColorBrewer::brewer.pal(colourCount, "Set1"))

ggplot(final.bl,aes(x=as.Date(Quarters),y=cum.ret,group=Views,color=Views))+geom_line(size=0.5)+facet_wrap(~ Method,scale='free')+ylab('Portfolio wealth (initial=$100)')+xlab('Quarters')+ggtitle('Portfolio performance with $100 initial investment')+theme_bw(base_family='Avenir')+theme(plot.title = element_text(colour = "Blue"),legend.position='top')+scale_color_manual(values=getPalette(colourCount))+guides(color=guide_legend(nrow=1))+geom_hline(yintercept=100)



ggplot(final.bl,aes(x=as.Date(Quarters),y=port.ret,group=Views,color=Views))+geom_line(size=0.5)+facet_wrap(~ Method,scale='free_x')+ylab('Portfolio wealth (initial=$100)')+xlab('Quarters')+ggtitle('Portfolio performance with $100 initial investment')+theme_bw(base_family='Avenir')+theme(plot.title = element_text(colour = "Blue"),legend.position='top')+scale_color_manual(values=getPalette(colourCount))+guides(color=guide_legend(nrow=1))



final.bl[,chart.CumReturns(xts(port.ret,order.by=as.Date(Quarters))),by=.(Method,Aggregation,Views)]

test <- acast(final.bl,Quarters~Method~Views,value.var = 'port.ret')

require(Quandl)
requre(quantmod)
Quandl.auth("X5ws5JEoYdP6VFefbPmQ")
prices = getSymbols("^GSPC", from = "1999-01-01", to = "2009-12-31")
tbil <- unique(data.table(Quandl('FRED/DTB3',start_date = '1999-01-01',end_date = '2009-12-31',sort='asc'))[,Quarters:=as.yearqtr(Date)][,rf:=mean(Value)/100/4,by=Quarters],by='Quarters')[3:44,.(Quarters,rf)]
sp.ind <- (quarterlyReturn(get(prices)[,6]))[3:44,]-tbil[,rf]


test.xts <- cbind(xts(test[,'true',],order.by=as.Date(as.yearqtr(dimnames(test)[[1]]))),coredata(sp.ind))

chart.CumReturns(test.xts,wealth.index = T,legend.loc = "topleft")
table.AnnualizedReturns(test.xts)
InformationRatio(test.xts[,c(1:3)],test.xts[,3])
table.CAPM(test.xts[,c(1:3)],test.xts[,4])
chart.RollingPerformance(test.xts,width = 12,legend.loc = "topleft")
chart.RiskReturnScatter(test.xts,add.sharpe = NA, scale = 4)
chart.Drawdown(test.xts, main = "Drawdowns")
#+geom_hline(yintercept=100)
###Plot of num. views
### Check the CONS for number of stocks. APS: number of stocks shoud be the same across all information sets. Fixed number of stocks  in each quarter for each view (CONS, TP, EPS)

ggplot(unique(opt.w,by=c('n.views','Quarters','Method','Views')),aes(x=as.Date(as.yearqtr(Quarters)),y=n.views,color=Method,group=Method))+geom_line()+facet_grid(Views~.,scale='free_x')+theme_bw()+geom_point()

#+scale_x_date(breaks='year',labels=date_format('%Y'))


ggplot(unique(melt(final.bl[,list(Views,Method,ann.ret,ann.sd,ann.sr)],id.vars=c('Method','Views'))),aes(x=Views,y=value))+geom_bar(aes(fill=Views),stat='identity',alpha=0.7)+facet_grid(variable~Method,scale='free_y')+theme_bw(base_family='Avenir')+ggtitle('Annualized portfolio measures (return, st.dev, and the Sharpe ratio) \n conditional on confidence aggregation')+theme(plot.title = element_text(colour = "Blue"),legend.position='top',axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+scale_y_continuous(labels=percent)+ylab('Percent')+geom_text(aes(label=round(value,3)*100),angle=90,size=3,hjust=1.2)+scale_fill_manual(values=getPalette(colourCount))#+guides(fill=guide_legend(nrow=1))


periods.id <- c(paste(gsub('[[:space:]]','',rollapplyr(as.character(quarters[m.period,q.id]),20,first,by=4)),gsub('[[:space:]]','',rollapplyr(as.character(quarters[m.period,q.id]),20,last,by=4)),sep='/'),'All period')

periods <- rbind(final.bl[,data.table(rollapplyr(port.ret,20,roll.ret.f,by=4,partial=F)),by=.(Views,Method)][,strat:='yes'],final.bl[,data.table(rollapplyr(ns.port.ret,20,roll.ret.f,by=4,partial=F)),by=.(Views,Method)][,strat:='no'],final.bl[,data.table(roll.ret.f(port.ret)),by=.(Views,Method)][,strat:='yes'],final.bl[,data.table(roll.ret.f(ns.port.ret)),by=.(Views,Method)][,strat:='no'])[,period:=periods.id,by=.(Views,Method,strat)][,sr:=ann.ret/ann.sd]


periods.array <- acast(melt(periods,id.vars = c('Views','Method','period','strat')),period~Views~Method~strat~variable,value.var = 'value')

ggplot(periods,aes(x=Views,y=ann.ret,fill=Views))+geom_bar(stat='identity',position='dodge') +facet_grid(Method~strat~period,scale='free_y')+theme_bw(base_family='Avenir')+theme(plot.title = element_text(colour = "Blue"),legend.position='top')+scale_color_manual(values=getPalette(colourCount))+guides(color=guide_legend(nrow=1))+geom_text(aes(label=round(ann.ret,2)),angle=90,size=3,hjust=1.2)
       
ggplot(periods,aes(x=period,y=ann.ret,color=Views,group=Views))+geom_line() +facet_wrap(~ Method,scale='free_x')+theme_bw(base_family='Avenir')+theme(plot.title = element_text(colour = "Blue"),legend.position='top')+scale_color_manual(values=getPalette(colourCount))+guides(color=guide_legend(nrow=1))



ggplot(unique(melt(final.bl[,list(Views,Method,meanViews,Ave.TO)],id.vars=c('Method','Views'))),aes(x=Views,y=value,fill=Views))+theme_bw(base_family='Avenir')+geom_bar(stat='identity')+facet_grid(variable~Method,scale='free_y')+ggtitle('Trading statistics conditional confidence aggregation \n (averages of number of views and turnover ratio)')+theme(legend.position='none',axis.title.x=element_blank(),plot.title = element_text(colour = "Blue"),axis.title.y=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+scale_fill_manual(values=getPalette(colourCount))+guides(fill=guide_legend(nrow=1))
#+geom_text(aes(label=round(value,2)),angle=90,size=3,hjust=1.2)

#require(gridExtra)
#cairo_pdf("graphs/bl.results.pdf", family="Avenir",h = 11.7, w = 8.3)
#grid.arrange(p.cum.ret,p.ann.ret,p.TO)
#dev.off()

##Looks like these are the dates for SP500 from=1998-10-01 to 2009-09-30; shoud be from 1999-01-01 to 2009-12-31
#prices = getSymbols("^GSPC", from = "1999-01-01", to = "2009-12-31")

#prices = getSymbols("SPY", from = "1999-01-01", to = "2009-12-31")

#rf = getSymbols("DTB3",src='FRED')


#market.spy <- na.omit(setnames(setkey(data.table(Quarters=index(quarterlyReturn(get(prices)[,6])),coredata(quarterlyReturn(get(prices)[,6])))[,Quarters:=as.yearqtr(Quarters)],Quarters)[risk.free],2,'sp.ret')[,port.ret:=sp.ret-rf][,Views:='SPY'])

market.spy<- setnames(data.table(Quarters=as.yearqtr(index(quarterlyReturn(get(prices)[,6]))),coredata(quarterlyReturn(get(prices)[,6]))),2,'ind.ret')[,Views:='Market'][,n.views:=500][,q.id:=.I][,.(Quarters,Views,ind.ret,n.views,q.id)]

market.sp <- unique(market.set[,sp.ret:=sum(mw*ex.ret),by=Quarters][,Quarters:=as.yearqtr(Quarters)][,list(Quarters,sp.ret)])[,n.views:=500]

m.all <- melt(setkey(market.spy,Quarters)[market.sp],id.vars = c('Quarters'),measure.vars = c('ind.ret','sp.ret'))
ggplot(m.all,aes(x=as.Date(Quarters),y=value,color=variable))+geom_line()+theme_bw()


chart.CumReturns(cbind(sp.ind,market.port.ret[,port.ret],unique(final.bl[Method=='true'&Views=='TP'&Aggregation=='ma'][,port.ret]),unique(final.bl[Method=='true'&Views=='CONS'&Aggregation=='ma'][,port.ret]),unique(final.bl[Method=='true'&Views=='EPS'&Aggregation=='ma'][,port.ret])))

StdDev.annualized(cbind(sp.ind,market.port.ret[,port.ret],unique(final.bl[Method=='true'&Views=='TP'&Aggregation=='ma'][,port.ret]),unique(final.bl[Method=='true'&Views=='CONS'&Aggregation=='ma'][,port.ret]),unique(final.bl[Method=='true'&Views=='EPS'&Aggregation=='ma'][,port.ret])))
StdDev.annualized(cbind(sp.ind,market.port.ret[,port.ret],unique(final.bl[Method=='naive'&Views=='TP'&Aggregation=='ma'][,port.ret]),unique(final.bl[Method=='naive'&Views=='CONS'&Aggregation=='ma'][,port.ret]),unique(final.bl[Method=='naive'&Views=='EPS'&Aggregation=='ma'][,port.ret])))
StdDev.annualized(cbind(sp.ind,market.port.ret[,port.ret],unique(final.bl[Method=='default'&Views=='TP'&Aggregation=='ma'][,port.ret]),unique(final.bl[Method=='default'&Views=='CONS'&Aggregation=='ma'][,port.ret]),unique(final.bl[Method=='default'&Views=='EPS'&Aggregation=='ma'][,port.ret])))



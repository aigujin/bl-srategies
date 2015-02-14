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

core.dt <- na.omit(setkey(setkey(q.data,q.id)[setkey(quarters,q.id)][,core.b:=.N>=12,by=list(Stock,Broker)][(core.b)][,true:=rank(score),by=list(q.id,Stock)][,core.s:=.N>=3,by=list(q.id,Stock)][(core.s)],Stock)[stocks])

#quarters <- data.table(q.id=core.dt[order(q.id)][,unique(q.id)])


ranked.pt.dt <- core.dt[,merge(setkey(quarters,q.id),.SD,all=T),by=list(Broker,Stock),.SDcols=c('q.id','Broker','Stock','true')][,.(q.id,Broker,Stock,true)]

percentile <- 0.05

pt.trunk.brok.tper <- setkey(core.dt[,merge(setkey(quarters,q.id),.SD,all=T),by=list(Broker,Stock),.SDcols=c('q.id','Broker','Stock','b.view')][,.(q.id,Broker,Stock,b.view)][,true:=truncate.f(b.view,percentile)],q.id,Stock,Broker)

pt.exp.tmp <- pt.trunk.brok.tper[,naive:=c(NA,head(true,-1)),by=.(Broker,Stock)][,default:=rollapplyr(naive,seq_len(length(naive)),mean,na.rm=T),by=.(Broker,Stock)]

pt.exp.ret <- setkey(melt(pt.exp.tmp,id.vars = c('q.id','Stock','Broker'),measure.vars = c('true','naive','default'),value.name = 'exp.ret',variable.name = 'Method'),q.id,Stock,Broker,Method)

cache('ranked.pt.dt')


pt.new.tmp <- core.dt[,naive:=c(NA,head(true,-1)),by=.(Broker,Stock)][,mean.rank:=rollapplyr(naive,seq_len(length(naive)),mean,na.rm=T),by=.(Broker,Stock)][,default:=rank(mean.rank,na.last = 'keep'),by=.(q.id,Stock)]

pt.new <- setkey(melt(pt.new.tmp,id.vars = c('q.id','Stock','Broker'),measure.vars = c('true','naive','default'),value.name = 'rank',variable.name = 'Method'),q.id,Stock,Broker,Method)

pt.set <- pt.new[pt.exp.ret]

pt.ret <- pt.set[,rank.exp.ret.f(rank,exp.ret),by=.(q.id,Stock,Method)][V1!=0,][q.id!='1999 Q2',]

#ggplot(pt.ret,aes(x=as.Date(q.id),y=View,group=as.Date(q.id)))+geom_boxplot()+facet_grid(Method~.,scales='free_x')+theme_bw()

pt.list.rank <- dlply(unique(pt.ret,by=c('q.id','Stock','Method')),'Method',acast,q.id~Stock,value.var='V1')
pt.rank.views <- setnames(pt.ret[,year:=format(as.yearqtr(q.id),'%Y')],'V1','View')
#cache('pt.rank.views')
###add NA before and after
#llply(..,function(i){rbind(rep(NA,ncol(i)),i,rep(NA,ncol(i)) ) })
#names(pt.list.rank) <- dimnames(pt.baseline.rankings)[[4]]

#pt.rank.views <- pt.exp.ret[,year:=format(as.yearqtr(q.id),'%Y')]
cache('pt.rank.views')

###EPS case
load('~/Dropbox/workspace/Projects/EPS/cache/complete.dt.RData')
eps.dt <- unique(setkey(na.omit(setkey(complete.dt[,':='(true=rank,q.id=calQ)],Stock)[stocks]),q.id)[quarters],by=c('q.id','Broker','Stock'))

ranked.eps.dt <- eps.dt[,merge(setkey(quarters,q.id),.SD,all=T),by=list(Broker,Stock),.SDcols=c('q.id','Broker','Stock','true')][,.(q.id,Broker,Stock,true)]

cache('ranked.eps.dt')

eps.new.tmp <- ranked.eps.dt[,naive:=c(NA,head(true,-1)),by=.(Broker,Stock)][,mean.rank:=rollapplyr(naive,seq_len(length(naive)),mean,na.rm=T),by=.(Broker,Stock)][,default:=rank(mean.rank,na.last = 'keep'),by=.(q.id,Stock)]

eps.new <- setkey(melt(eps.new.tmp,id.vars = c('q.id','Stock','Broker'),measure.vars = c('true','naive','default'),value.name = 'rank',variable.name = 'Method'),q.id,Stock,Broker,Method)

eps.set <- eps.new[pt.exp.ret]

eps.exp.ret <- eps.set[,rank.exp.ret.f(rank,exp.ret),by=.(q.id,Stock,Method)][V1!=0,][q.id!='1999 Q2',]

eps.rank.views <- setnames(eps.exp.ret[,year:=format(as.yearqtr(q.id),'%Y')],'V1','View')
cache('eps.rank.views')

eps.list.rank <- dlply(unique(eps.exp.ret,by=c('q.id','Stock','Method')),'Method',acast,q.id~Stock,value.var='View')


meanTper <- na.omit(melt(unique(pt.trunk.brok.tper[,true:=median(true,na.rm=T),by=list(q.id,Stock)],by=c('q.id','Stock'))[,.(q.id,Stock,true)][,':='(naive=c(NA,head(true,-1)),default=rollapplyr(true,seq_len(length(true)),mean,na.rm=T)),by=Stock],id.vars = c('q.id','Stock'),measure.vars = c('true','naive','default'),variable.name = 'Method')[q.id!='1999 Q2',])

nr.views <- setnames(meanTper[,year:=format(as.yearqtr(q.id),'%Y')],'value','View')
cache('nr.views')

cons.list.rank <- dlply(unique(meanTper,by=c('q.id','Stock','Method')),'Method',acast,q.id~Stock,value.var='View')

require(scales)
conf.coef <- acast(unique(core.dt,by=c('q.id','Stock'),fromLast = T),q.id~Stock,value.var='s.coefVar')

eps.stocks <- intersect(colnames(eps.list.rank[[1]]),colnames(conf.coef))


##BL inputs for non-rank strategy:meanTper and s.coefVar
#cons.rankings <- pt.baseline.rankings[3:42,pt.all.b,pt.all.s,]
#cons.rankings[!is.na(cons.rankings)] <- 1


#nr.views <- na.omit(setnames(data.table(reshape2::melt(cons.list.rank)),c('q.num','Stock','View','Method'))[,q.id:=dimnames(pt.baseline.rankings)[[1]],by=list(Stock,Method)][,year:=format(as.yearqtr(q.id),'%Y')])

bl.period <- 1:41
m.period <-(length(market.list)-length(bl.period)+1) : (length(market.list))
        
delta=2.5
pt.opt.w <- operation.bl(pt.list.rank,conf.coef,'ma')[,Views:='TP']
cons.opt.w <- operation.bl(cons.list.rank,conf.coef,'ma')[,Views:='CONS']
eps.opt.w <- operation.bl(eps.list.rank,conf.coef[,eps.stocks],'ma')[,Views:='EPS']

opt.w <- rbind(pt.opt.w,cons.opt.w,eps.opt.w)
cache('opt.w')

source('lib/BL-functions.R')
final.bl <- setkey(unique(bl.results.f(opt.w),by=c('Method','Aggregation','q.id','Views')),Method)
final.bl$Method <- factor(final.bl$Method,levels=unique(final.bl$Method)[c(3,2,1)])
final.bl$Views <- factor(final.bl$Views,levels=unique(final.bl$Views)[c(4,1,2,3)])
cache('final.bl')
colourCount = length(unique(final.bl$Views))
getPalette = colorRampPalette(RColorBrewer::brewer.pal(colourCount, "Set1"))

ggplot(final.bl[Aggregation=='ma'],aes(x=as.Date(Quarters),y=cum.ret,group=Views,color=Views))+geom_line(size=0.5)+facet_wrap(~ Method,scale='free_x')+ylab('Portfolio wealth (initial=$100)')+xlab('Quarters')+ggtitle('Portfolio performance with $100 initial investment')+theme_bw(base_family='Avenir')+theme(plot.title = element_text(colour = "Blue"),legend.position='top')+scale_color_manual(values=getPalette(colourCount))+guides(color=guide_legend(nrow=1))

#+geom_hline(yintercept=100)
###Plot of num. views
### Check the CONS for number of stocks. APS: number of stocks shoud be the same across all information sets. Fixed number of stocks  in each quarter for each view (CONS, TP, EPS)

ggplot(unique(opt.w,by=c('n.views','Quarters','Method','Aggregation','Views'))[Aggregation=='ma'],aes(x=as.Date(as.yearqtr(Quarters)),y=n.views,color=Method,group=Method))+geom_line()+facet_grid(Views~.,scale='free_x')+theme_bw()+geom_point()

#+scale_x_date(breaks='year',labels=date_format('%Y'))


ggplot(unique(melt(final.bl[Aggregation=='ma',list(Views,Method,ann.ret,ann.sd,ann.sr)],id.vars=c('Method','Views'))),aes(x=Views,y=value))+geom_bar(aes(fill=Views),stat='identity',alpha=0.7)+facet_grid(variable~Method,scale='free_y')+theme_bw(base_family='Avenir')+ggtitle('Annualized portfolio measures (return, st.dev, and the Sharpe ratio) \n conditional on confidence aggregation')+theme(plot.title = element_text(colour = "Blue"),legend.position='top',axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+scale_y_continuous(labels=percent)+ylab('Percent')+geom_text(aes(label=round(value,3)*100),angle=90,size=3,hjust=1.2)+scale_fill_manual(values=getPalette(colourCount))#+guides(fill=guide_legend(nrow=1))



p.TO <- ggplot(unique(melt(final.bl[Aggregation=='ma',list(Views,Method,meanViews,Ave.TO)],id.vars=c('Method','Views'))),aes(x=Views,y=value,fill=Views))+theme_bw(base_family='Avenir')+geom_bar(stat='identity')+facet_grid(variable~Method,scale='free_y')+ggtitle('Trading statistics conditional confidence aggregation \n (averages of number of views and turnover ratio)')+theme(legend.position='none',axis.title.x=element_blank(),plot.title = element_text(colour = "Blue"),axis.title.y=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+scale_fill_manual(values=getPalette(colourCount))+guides(fill=guide_legend(nrow=1))
#+geom_text(aes(label=round(value,2)),angle=90,size=3,hjust=1.2)

#require(gridExtra)
#cairo_pdf("graphs/bl.results.pdf", family="Avenir",h = 11.7, w = 8.3)
#grid.arrange(p.cum.ret,p.ann.ret,p.TO)
#dev.off()

risk.free <- unique(market.set[,.(Quarters,rf)])
##Looks like these are the dates for SP500 from=1998-10-01 to 2009-09-30; shoud be from 1999-01-01 to 2009-12-31
require(quantmod)
#prices = getSymbols("^GSPC", from = "1999-01-01", to = "2009-12-31")
require(Quandl)
Quandl.auth("X5ws5JEoYdP6VFefbPmQ")
s.date='1999-01-01'
e.date='2009-31-12'
tbil <- data.table(Quandl('FRED/DTB3',start_date = '1999-01-01',end_date = '2009-12-31',sort='asc'))
sp500 <- setnames(data.table(Quandl("YAHOO/INDEX_SPY", trim_start='1999-01-01', trim_end='2009-12-31',column=6,sort='asc')),2,'Price')
sp500.ret<- setnames(na.omit(setkey(sp500[,sp.ret:=c(NA,diff(log(Price)))][,list(Date,sp.ret)],Date)[setkey(tbil[,rf:=Value/100/252][,list(Date,rf)],Date)])[,exs.ret:=sp.ret-rf][,Quarters:=as.yearqtr(Date)][,Return.cumulative(exs.ret),by=Quarters],2,'port.ret')
                                

#prices = getSymbols("SPY", from = "1999-01-01", to = "2009-12-31")
prices = getSymbols("^GSPC", from = "1998-10-01", to = "2009-09-30")
#rf = getSymbols("DTB3",src='FRED')


#market.spy <- na.omit(setnames(setkey(data.table(Quarters=index(quarterlyReturn(get(prices)[,6])),coredata(quarterlyReturn(get(prices)[,6])))[,Quarters:=as.yearqtr(Quarters)],Quarters)[risk.free],2,'sp.ret')[,port.ret:=sp.ret-rf][,Views:='SPY'])


market.ind<- setnames(cbind(risk.free,port.ret=coredata(quarterlyReturn(get(prices)[,6]))),3,'sp.ret')[,ind.ret:=sp.ret-rf][,Views:='Market'][,n.views:=500][,q.id:=.I][,.(Quarters,Views,ind.ret,n.views,q.id)]

market.sp <- unique(market.set[,sp.ret:=sum(mw*ex.ret),by=Quarters][,Quarters:=as.yearqtr(Quarters)][,list(Quarters,sp.ret)])[,n.views:=500]

m.all <- melt(setkey(market.ind,Quarters)[market.sp],id.vars = c('Quarters'),measure.vars = c('ind.ret','sp.ret'))
ggplot(m.all,aes(x=as.Date(Quarters),y=value,color=variable))+geom_line()+theme_bw()

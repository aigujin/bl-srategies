

stocks <- sort(unique(unlist(lapply(market.list,function(m){m$stock.names}))))
quarters <- setnames(unique(market.set[,.(Quarters)]),'q.id')[,q.id:=as.yearqtr(q.id)]

core.dt <- setkey(q.data[,core.b:=.N>=12,by=list(Stock,Broker)][(core.b)][,true:=rank(score),by=list(q.id,Stock)][,core.s:=.N>=3,by=list(q.id,Stock)][(core.s)][,core.q:=length(unique(q.id))>=8,by=.(Stock)][(core.q)],Stock)[stocks]


pt.exp.ret <- setkey(melt(core.dt[,merge(setkey(quarters,q.id),.SD,all=T),by=list(Broker,Stock),.SDcols=c('q.id','Broker','Stock','b.view')][,.(q.id,Broker,Stock,b.view)][,true:=truncate.f(b.view,percentile)][,naive:=c(NA,head(true,-1)),by=.(Broker,Stock)][,default:=rollapplyr(naive,seq_len(length(naive)),mean,na.rm=T),by=.(Broker,Stock)],id.vars = c('q.id','Stock','Broker'),measure.vars = c('true','naive','default'),value.name = 'exp.ret',variable.name = 'Method'),q.id,Stock,Broker,Method)

### Need ranked dt for contigency analysis
ranked.pt.dt <- core.dt[,merge(setkey(quarters,q.id),.SD,all=T),by=list(Broker,Stock),.SDcols=c('q.id','Broker','Stock','true')][,.(q.id,Broker,Stock,true)]

cache('ranked.pt.dt')

pt.new <- setkey(melt(ranked.pt.dt[,naive:=c(NA,head(true,-1)),by=.(Broker,Stock)][,mean.rank:=rollapplyr(naive,seq_len(length(naive)),mean,na.rm=T),by=.(Broker,Stock)][,default:=rank(mean.rank,na.last = 'keep'),by=.(q.id,Stock)],id.vars = c('q.id','Stock','Broker'),measure.vars = c('true','naive','default'),value.name = 'rank',variable.name = 'Method'),q.id,Stock,Broker,Method)



pt.set <- pt.new[pt.exp.ret]

pt.ret <- pt.set[,rank.exp.ret.f(rank,exp.ret),by=.(q.id,Stock,Method)][V1!=0,][q.id!='1999 Q2',]

#ggplot(pt.ret,aes(x=as.Date(q.id),y=V1,group=as.Date(q.id)))+geom_boxplot()+facet_grid(Method~.,scales='free_x')+theme_bw()

pt.list.rank <- acast(unique(pt.ret,by=c('q.id','Stock','Method')),q.id~Stock~Method,value.var='V1')

pt.rank.views <- setnames(pt.ret[,year:=format(as.yearqtr(q.id),'%Y')],'V1','View')
#cache('pt.rank.views')
###add NA before and after
#llply(..,function(i){rbind(rep(NA,ncol(i)),i,rep(NA,ncol(i)) ) })
#names(pt.list.rank) <- dimnames(pt.baseline.rankings)[[4]]

#pt.rank.views <- pt.exp.ret[,year:=format(as.yearqtr(q.id),'%Y')]
cache('pt.rank.views')

###EPS case
load('~/Dropbox/workspace/Projects/EPS/cache/complete.dt.RData')

ranked.eps.dt <- unique(setkey(na.omit(setkey(complete.dt[,':='(true=rank,q.id=fis.q)],Stock)[stocks]),q.id)[quarters],by=c('q.id','Broker','Stock'))[,merge(setkey(quarters,q.id),.SD,all=T),by=list(Broker,Stock),.SDcols=c('q.id','Broker','Stock','true')][,.(q.id,Broker,Stock,true)]

cache('ranked.eps.dt')

eps.dt <- setkey(melt(ranked.eps.dt[,naive:=c(NA,head(true,-1)),by=.(Broker,Stock)][,mean.rank:=rollapplyr(naive,seq_len(length(naive)),mean,na.rm=T),by=.(Broker,Stock)][,default:=rank(mean.rank,na.last = 'keep'),by=.(q.id,Stock)],id.vars = c('q.id','Stock','Broker'),measure.vars = c('true','naive','default'),value.name = 'rank',variable.name = 'Method'),q.id,Stock,Broker,Method)

#load('cache/ranked.pt.dt.RData')
#ranked.eps.dt <- setkey(ranked.pt.dt,q.id,Broker,Stock)[ranked.eps.dt][,true:=NULL][,true:=i.true][,i.true:=NULL]

#cache('ranked.eps.dt')


eps.set <- eps.dt[pt.exp.ret]


eps.exp.ret <- eps.set[,rank.exp.ret.f(rank,exp.ret),by=.(q.id,Stock,Method)][V1!=0,][q.id!='1999 Q2',]

eps.list.rank <- acast(unique(eps.exp.ret,by=c('q.id','Stock','Method')),q.id~Stock~Method,value.var='V1')


eps.rank.views <- setnames(eps.exp.ret[,year:=format(as.yearqtr(q.id),'%Y')],'V1','View')

cache('eps.rank.views')



meanTper <- na.omit(melt(unique(core.dt[,merge(setkey(quarters,q.id),.SD,all=T),by=list(Broker,Stock),.SDcols=c('q.id','Broker','Stock','b.view')][,.(q.id,Broker,Stock,b.view)][,true:=truncate.f(b.view,percentile)][,true:=median(true,na.rm=T),by=list(q.id,Stock)],by=c('q.id','Stock'))[,.(q.id,Stock,true)][,naive:=c(NA,head(true,-1)),by=Stock][,default:=rollapplyr(naive,seq_len(length(naive)),mean,na.rm=T),by=Stock],id.vars = c('q.id','Stock'),measure.vars = c('true','naive','default'),variable.name = 'Method')[q.id!='1999 Q2',])

#meanTper <- pt.exp.ret[,median(exp.ret,na.rm=T),by=.(q.id,Stock,Method)]


nr.views <- setnames(meanTper[,year:=format(as.yearqtr(q.id),'%Y')],'value','View')
cache('nr.views')

cons.list.rank <- acast(unique(meanTper,by=c('q.id','Stock','Method')),q.id~Stock~Method,value.var='View')

require(scales)


conf.dt <- melt(unique(core.dt,by=c('q.id','Stock'),fromLast = T)[,.(q.id,Stock,s.coefVar)][,true:=0][,naive:=c(NA,head(s.coefVar,-1)),by=Stock][,default:=rollapplyr(naive,seq_len(length(naive)),mean,na.rm=T,partial=T),by=Stock],id.vars = c('q.id','Stock'),measure.vars = c('true','naive','default'))[q.id!='1999 Q2',]

#conf.dt <- melt(conf.dt[,naive:=c(NA,head(s.coefVar,-1)),by=Stock][,default:=rollapplyr(naive,seq_len(length(naive)),mean,na.rm=T,partial=T),by=Stock],id.vars = c('q.id','Stock'),measure.vars = c('true','naive','default'))[q.id!='1999 Q2',]


conf.coef <- acast(conf.dt,q.id~Stock~variable,value.var='value')

eps.stocks <- intersect(dimnames(eps.list.rank)[[2]],dimnames(conf.coef)[[2]])
#pt.stocks <- intersect(dimnames(pt.list.rank)[[2]],dimnames(conf.coef)[[2]])

##BL inputs for non-rank strategy:meanTper and s.coefVar
#cons.rankings <- pt.baseline.rankings[3:42,pt.all.b,pt.all.s,]
#cons.rankings[!is.na(cons.rankings)] <- 1


#nr.views <- na.omit(setnames(data.table(reshape2::melt(cons.list.rank)),c('q.num','Stock','View','Method'))[,q.id:=dimnames(pt.baseline.rankings)[[1]],by=list(Stock,Method)][,year:=format(as.yearqtr(q.id),'%Y')])

bl.period <- 3:42
m.period <-(length(market.list)-length(bl.period)+1) : (length(market.list))
        
delta=1
tau=1/50
#pt.opt.w <- operation.bl(pt.list.rank,conf.coef,'ma')[,Views:='TP']
#cons.opt.w <- operation.bl(cons.list.rank,conf.coef,'ma')[,Views:='CONS']
#eps.opt.w <- operation.bl(eps.list.rank,conf.coef[,eps.stocks],'ma')[,Views:='EPS']

#source('lib/BL-functions.R')

pt.opt.w <- opt.w.f(pt.list.rank,conf.coef,tau)[,Views:='TP']
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

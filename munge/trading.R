
source('lib/BL-functions.R')
stocks <- sort(unique(unlist(lapply(market.list,function(m){m$stock.names}))))
quarters <- setnames(unique(market.set[,.(Quarters)]),'q.id')[,q.id:=as.yearqtr(q.id)]

core.dt <- na.omit(setkey(na.omit(q.data),q.id)[setkey(quarters,q.id)])[,core.b:=.N>=12,by=list(Stock,Broker)][(core.b)][,true:=rank(score),by=list(q.id,Stock)][,core.s:=.N>=3,by=list(q.id,Stock)][(core.s)][,core.q:=length(unique(q.id))>=8,by=.(Stock)][(core.q)]



pt.trunk.brok.tper <- core.dt[,merge(setkey(quarters,q.id),.SD,all=T),by=list(Broker,Stock),.SDcols=c('q.id','Broker','Stock','b.view')][,.(q.id,Broker,Stock,b.view)][,true:=truncate.f(b.view,percentile)][,naive:=c(NA,head(true,-1))][,':='(default=rollapplyr(naive,seq_len(length(naive)),mean,na.rm=T,partial=T),raw=naive,'1diff'=naive,roll.sd=naive,random=naive),by=.(Broker,Stock)]

pt.exp.ret <- setkey(melt(pt.trunk.brok.tper,id.vars = c('q.id','Stock','Broker'),measure.vars = c(baselines,methods),value.name = 'exp.ret',variable.name = 'Method'),q.id,Stock,Broker,Method)




pt.ret <-pt.exp.ret[ranked.pt.dt][,rank.exp.ret.f(rank,exp.ret),by=.(q.id,Stock,Method)][V1!=0,]

pt.list.rank <- acast(pt.ret,q.id~Stock~Method,value.var='V1')

#pt.list.rank <- dlply(pt.ret,'Method',acast,q.id~Stock,value.var='V1')

#ggplot(pt.ret,aes(x=as.Date(q.id),y=V1,group=as.Date(q.id)))+geom_boxplot()+facet_grid(Method~.,scales='free_x')+theme_bw()
#pt.pred.rank.views <- setnames(pt.ret[,year:=format(as.yearqtr(q.id),'%Y')],'V1','View')


require(scales)


conf.dt.tmp <- unique(core.dt,by=c('q.id','Stock'),fromLast = T)[,.(q.id,Stock,s.coefVar)][,true:=0]


conf.dt <- setkey(melt(conf.dt.tmp[,':='(naive=c(NA,head(s.coefVar,-1)),default=rollapplyr(s.coefVar,seq_len(length(s.coefVar)),mean,na.rm=T,partial=T),raw=NA_real_,'1diff'=NA_real_,roll.sd=NA_real_,random=NA_real_),by=Stock],id.vars = c('q.id','Stock'),measure.vars=c(baselines,methods),value.name='cons'),q.id,Stock,variable)


#conf.coef <- acast(conf.dt,q.id~Stock~variable,value.var='cons')

res.accu <- melt(conf.dt[setkey(pt.accu[,':='(last=(1-rescale(value))/rescale(value),ma={tmp <- rollapplyr(value,4,mean,na.rm=T,partial=T);(1-rescale(tmp))/rescale(tmp)}),by=Stock],q.id,Stock,variable)],id.vars=c('q.id','Stock','variable'),measure.vars=confid.id,variable.name='conf')[variable=='true',value:=0]

set(res.accu,i=which(is.infinite(res.accu[[5L]])),5L,value=9e+15 )

conf.coef <- acast(res.accu,q.id~Stock~variable~conf,value.var='value')

pt.stocks <- intersect(dimnames(pt.list.rank)[[2]],dimnames(conf.coef)[[2]])


bl.period <- 1:dim(pt.list.rank)[[1]]
m.period <-(length(market.list)-length(bl.period)+1) : (length(market.list))

opt.w<- rbindlist(lapply(confid.id, function(i){
        opt.w.f(pt.list.rank,conf.coef[,pt.stocks,,i],tau)[,confAgg:=i]}))[,Views:='TP']


final.bl <- setkey(unique(pred.bl.results.f(opt.w),by=c('Method','q.id','Views','confAgg')),Method)
final.bl$Method <- factor(final.bl$Method,levels=unique(final.bl$Method)[c(8,4,3,6,1,5,7,2)])
final.bl$Views <- factor(final.bl$Views,levels=unique(final.bl$Views)[c(2,1)])
colourCount = length(unique(final.bl$Method))
getPalette = colorRampPalette(RColorBrewer::brewer.pal(colourCount, "Set1"))
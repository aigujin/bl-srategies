source('/Users/aiguzhinov/Documents/Dropbox/workspace/MetaRanks/my.paths.R')
prediction <- function(sel.stocks,forecasts,actuals,rank.m,rank.parameters){
  ###forecast dim(q,b,s)
  
    start.q <- switch(rank.m,'pt'=43,'eps'=43,'pt.new'=43)
    forecast.p <- (dim(forecasts)[1]-start.q+1):dim(forecasts)[1]
  if(rank.m=='pt.new'){tr <- pt.new[,,sel.stocks];broker.vvs <- broker.vvs}else{
    tr<- target.ranking(sel.stocks,forecasts[forecast.p,,sel.stocks],actuals[forecast.p,sel.stocks],coverage=12,min.brokers=3)
  broker.vvs <- broker.vvs.f(forecasts=forecasts[forecast.p,dimnames(tr)[[2]],dimnames(tr)[[3]]],actual=actuals[forecast.p,dimnames(tr)[[3]]])
  }
baseline.rankings <- replicate(1,baseline.rankings.f(tr,1))
sectors<-c('Consumer Discretionary',
  'Consumer Staples',
  'Energy',
  'Financials',
  'Health Care',
  'Industrials',
  'IT',
  'Materials',
  'Telecom Services',
  'Utilities')
source(project.dir('/NBr/generic-nbr.R'))
methods<-c('raw','1diff','random','roll.sd')
load(analysts.dir('/Sectors/All Sectors/all.stocks.list.RData'))
names(all.stocks.list) <- sectors
colnames(sector.ret) <- sectors

stock.vvs <- vvs.combine(stocks=dimnames(tr)[[3]],sectors.stock=all.stocks.list,broker=broker.vvs,ret.vol=ret.vol.sp.pt,sector.ret=sector.ret,t=dim(tr)[[1]])
pred.r<- predirank.f(methods,stock.vvs,macro.vvs,tr,rank.parameters)
pred.r <- replicate(1,pred.r)
all.rankings <- abind(baseline.rankings,aperm(pred.r,c(2,1,3,4,5)),along=4,new.names=list(NULL,NULL,NULL,NULL,rank.m))
accuracy <- apply(all.rankings,c(1,3,5),function(s){apply(s,2,evaluation.simple,s[,'true'],'s','p' ) })
  assign(paste(rank.m,'all.rankings',sep='.'),all.rankings,pos=.GlobalEnv)
  assign(paste(rank.m,'accuracy',sep='.'),accuracy,pos=.GlobalEnv)
}


lapply(list('abind','xts','date','parallel'), FUN = function(X) {
    do.call("require", list(X)) })


load(analysts.dir('Sectors/All Sectors/ret.vol.sp.RData'))
load(data.stock.dir('Market/macro.sector.vvs.RData'))
rank.parameters <- c(n=10,diff.lag=1,sd.lag=8)

load(analysts.dir('Sectors/All Sectors/xts.sp.d.prices.RData'))
load(project.dir('RankPriceTarget/all.pt.ext.RData'))
#load(project.dir('Price Targets/new.exp.ret.RData'))
end.q <- endpoints(index(xts.sp.d.prices),on='quarters')
act.pr.end.q <- xts.sp.d.prices[end.q[(length(end.q)-dim(all.pt.ext)[1]+1):length(end.q)],]

#sel.stocks <- unique(intersect(dimnames(all.pt.ext)[[3]],rownames(ret.vol.sp.pt)))

#prediction(sel.stocks,forecast=all.pt.ext[1:43,,],actuals=act.pr.end.q,'pt',rank.parameters)
#save(pt.all.rankings,pt.accuracy,file=project.dir('/Predict Analysts/pt.RData'))


#load(project.dir('EPS/eps.fis.q.RData'))
#load(project.dir('EPS/brokers.estimates.RData'))
#sel.stocks <- unique(intersect(intersect(dimnames(estimates)[[2]],rownames(ret.vol.sp.pt)),colnames(eps.fis.q)))

#prediction(sel.stocks,aperm(estimates,c(1,3,2)),eps.fis.q[2:85,],'eps',rank.parameters)

#save(eps.all.rankings,eps.accuracy,file=project.dir('/Predict Analysts/eps.RData'))

#load(project.dir('/Price Targets/tr.data.table.RData'))
sel.stocks <- unique(intersect(dimnames(pt.new)[[3]],rownames(ret.vol.sp.pt)))
pt.new <- pt.new[1:43,,sel.stocks]
pt.new.b <- intersect(dimnames(pt.new)[[2]],dimnames(all.pt.ext)[[2]])
#pt.new.s <- intersect(dimnames(pt.new)[[3]],dimnames(all.pt.ext)[[3]])
#sel.s <- intersect(sel.stocks,pt.new.s)
broker.vvs <- broker.vvs.f(all.pt.ext[,pt.new.b,sel.stocks],act.pr.end.q[,sel.stocks])
prediction(sel.stocks,all.pt.ext[,pt.new.b,sel.stocks],act.pr.end.q[,sel.stocks],'pt.new',rank.parameters)

save(pt.new.all.rankings,pt.new.accuracy,file=project.dir('/Predict Analysts/pt.new.RData'))
save(pt.new.all.rankings,pt.new.accuracy,file='cache/pt.new.RData')
    
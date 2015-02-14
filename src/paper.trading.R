rm(list=ls())
setwd('~/Dropbox/workspace/Projects/Black-Litterman/BL-strategies/')

library(ProjectTemplate)
load.project()

#brok.tper <- acast(unique(q.data,by=c('Broker','q.id','Stock'),fromLast = T),q.id~Broker~Stock,value.var='b.view')

core.dt <- q.data[,core.b:=.N>=12,by=list(Stock,Broker)][(core.b)][,rank:=rank(score),by=list(q.id,Stock)][,core.s:=.N>=3,by=list(q.id,Stock)][(core.s)]

brok.tper <- acast(unique(core.dt,by=c('Broker','q.id','Stock'),fromLast = T),q.id~Broker~Stock,value.var='b.view')

#pt.stocks=intersect(dimnames(pt.new)[[3]],market.set[,Stock])
#pt.trunk.brok.tper <- truncate.f(brok.tper[,pt.all.b,pt.all.s],percentile)


##core rankings
pt.new <- acast(unique(core.dt,by=c('Broker','q.id','Stock'),fromLast = T),q.id~Broker~Stock,value.var='rank')
pt.stocks=intersect(dimnames(pt.new)[[3]],market.set[,Stock])
pt.baseline.rankings <- baseline.rankings.f(pt.new[1:43,,pt.stocks],1)
pt.all.s <- findIntersect(pt.baseline.rankings,brok.tper,3)
pt.all.b <- findIntersect(pt.baseline.rankings,brok.tper,2)
percentile <- 0.05
pt.trunk.brok.tper <- truncate.f(brok.tper[,pt.all.b,pt.all.s],percentile)

pt.list.rank <- alply(bl.script.f(pt.all.s,pt.baseline.rankings[3:42,pt.all.b,pt.all.s,],pt.trunk.brok.tper,t=dim(pt.baseline.rankings[3:42,,,])[1]),3,.dims=T)

pt.rank.views <- setnames(data.table(reshape2::melt(pt.list.rank)),c('q.num','Stock','View','Method'))[,q.id:=dimnames(pt.baseline.rankings)[[1]][3:42],by=list(Stock,Method)][,year:=format(as.yearqtr(q.id),'%Y')]
cache('pt.rank.views')

load('~/Dropbox/workspace/Projects/EPS/cache/complete.dt.RData')

eps.new <- acast(unique(complete.dt,by=c('Broker','q.id','Stock'),fromLast = T),q.id~Broker~Stock,value.var='rank')
eps.stocks=intersect(dimnames(eps.new[42:84,,])[[3]],market.set[,Stock])
eps.baseline.rankings <- baseline.rankings.f(eps.new[42:84,,eps.stocks],1)
eps.all.s <- findIntersect(eps.baseline.rankings,pt.trunk.brok.tper,3)
eps.all.b <- findIntersect(eps.baseline.rankings,pt.trunk.brok.tper,2)

eps.list.rank <- alply(bl.script.f(eps.all.s,eps.baseline.rankings[3:42,eps.all.b,eps.all.s,],pt.trunk.brok.tper[,eps.all.b,eps.all.s],t=dim(eps.baseline.rankings[3:42,,,])[1]),3,.dims=T)

eps.rank.views <- setnames(data.table(reshape2::melt(eps.list.rank,na.rm=F)),c('q.num','Stock','View','Method'))[,q.id:=dimnames(pt.baseline.rankings)[[1]][3:42],by=list(Stock,Method)][,year:=format(as.yearqtr(q.id),'%Y')]
cache('eps.rank.views')

bl.period <- 4:40
m.period <-(length(market.list)-length(bl.period)+1) : (length(market.list))




require(scales)
conf.coef <- acast(unique(core.dt,by=c('q.id','Stock'),fromLast = T),q.id~Stock,value.var='s.coefVar')



##BL inputs for non-rank strategy:meanTper and s.coefVar
cons.rankings <- pt.baseline.rankings[3:42,pt.all.b,pt.all.s,]
cons.rankings[!is.na(cons.rankings)] <- 1
cons.list.rank <- alply(bl.script.f(pt.all.s,cons.rankings,pt.trunk.brok.tper,t=dim(pt.baseline.rankings[3:42,,,])[1]),3,.dims=T)


#meanTper <-acast(unique(setkey(core.dt[,meanTper:=mean(b.view),by=list(q.id,Stock)],Stock)[pt.all.s],by=c('Stock','q.id')),q.id~Stock,value.var='meanTper')
#trunk.meanTper <- truncate.f(meanTper,percentile)
#list.non.rank <- alply(abind(true=trunk.meanTper[3:42,],naive=trunk.meanTper[4:43,],default=rollapplyr(trunk.meanTper[4:43,],seq_len(nrow(trunk.meanTper[4:43,])),mean,na.rm=T),along=3),3,.dims=T)
nr.views <- na.omit(setnames(data.table(reshape2::melt(cons.list.rank)),c('q.num','Stock','View','Method'))[,q.id:=dimnames(pt.baseline.rankings)[[1]][3:42],by=list(Stock,Method)][,year:=format(as.yearqtr(q.id),'%Y')])
cache('nr.views')

pt.opt.w <- operation.bl(pt.list.rank,conf.coef[4:43,],pt.all.s,'ma')[,Views:='TP']
cons.opt.w <- operation.bl(cons.list.rank,conf.coef[4:43,],pt.all.s,'ma')[,Views:='CONS']
eps.opt.w <- operation.bl(eps.list.rank,conf.coef[4:43,],eps.all.s,'ma')[,Views:='EPS']

opt.w <- rbind(pt.opt.w,cons.opt.w,eps.opt.w)
cache('opt.w')

final.bl <- setkey(unique(bl.results.f(opt.w),by=c('Method','Aggregation','q.id','Views')),Method)
final.bl$Method <- factor(final.bl$Method,levels=unique(final.bl$Method)[c(3,2,1)])
final.bl$Views <- factor(final.bl$Views,levels=unique(final.bl$Views)[c(4,1,2,3)])
ProjectTemplate::cache('final.bl')
colourCount = length(unique(final.bl$Views))
getPalette = colorRampPalette(RColorBrewer::brewer.pal(colourCount, "Set1"))

p.cum.ret <- ggplot(final.bl[Aggregation=='ma'],aes(x=as.Date(Quarters),y=cum.ret,group=Views,color=Views))+geom_line(size=0.5)+geom_hline(yintercept=100)+facet_grid(~Method,scale='free_x')+ylab('Portfolio wealth (initial=$100)')+xlab('Quarters')+ggtitle('Portfolio performance with $100 initial investment')+theme_bw(base_family='Avenir')+theme(plot.title = element_text(colour = "Blue"),legend.position='top')+scale_color_manual(values=getPalette(colourCount))+guides(color=guide_legend(nrow=1))

###Plot of num. views
### Check the CONS for number of stocks. APS: number of stocks shoud be the same across all information sets. Fixed number of stocks  in each quarter for each view (CONS, TP, EPS)
setdiff(names(show.no.na(eps.list.rank[[2]][22,])),names(show.no.na(eps.list.rank[[3]][22,])))

test <- sapply(1:37,function(i)
{
        m.stocks <- market.list[[m.period[i]]]$stock.names
        sapply(3,function(j){
                b.stocks.cons <- names(show.no.na(eps.list.rank[[j-1]][bl.period[i],]))
                b.stocks <- names(show.no.na(eps.list.rank[[j-2]][bl.period[i],]))
                c(cons=length(intersect(m.stocks,b.stocks.cons)),tp=length(intersect(m.stocks,b.stocks)))
                  })
})

ggplot(unique(opt.w,by=c('n.views','Quarters','Method','Aggregation','Views'))[Aggregation=='ma'],aes(x=as.Date(as.yearqtr(Quarters)),y=n.views,color=Method,group=Method))+geom_line()+facet_grid(Views~.,scale='free_x')+theme_bw()+geom_jitter()

#+scale_x_date(breaks='year',labels=date_format('%Y'))


p.ann.ret <- ggplot(unique(melt(final.bl[Aggregation=='ma',list(Views,Method,ann.ret,ann.sd,ann.sr)],id.vars=c('Method','Views'))),aes(x=Views,y=value))+geom_bar(aes(fill=Views),stat='identity',alpha=0.7)+facet_grid(variable~Method,scale='free_y')+theme_bw(base_family='Avenir')+ggtitle('Annualized portfolio measures (return, st.dev, and the Sharpe ratio) \n conditional on confidence aggregation')+theme(plot.title = element_text(colour = "Blue"),legend.position='top',axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+scale_y_continuous(labels=percent)+ylab('Percent')+geom_text(aes(label=round(value,3)*100),angle=90,size=3,hjust=1.2)+scale_fill_manual(values=getPalette(colourCount))#+guides(fill=guide_legend(nrow=1))



p.TO <- ggplot(unique(melt(final.bl[Aggregation=='ma',list(Views,Method,meanViews,Ave.TO)],id.vars=c('Method','Views'))),aes(x=Views,y=value,fill=Views))+theme_bw(base_family='Avenir')+geom_bar(stat='identity')+facet_grid(variable~Method,scale='free_y')+ggtitle('Trading statistics conditional confidence aggregation \n (averages of number of views and turnover ratio)')+theme(legend.position='none',axis.title.x=element_blank(),plot.title = element_text(colour = "Blue"),axis.title.y=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+scale_fill_manual(values=getPalette(colourCount))+guides(fill=guide_legend(nrow=1))
#+geom_text(aes(label=round(value,2)),angle=90,size=3,hjust=1.2)

require(gridExtra)
#cairo_pdf("graphs/bl.results.pdf", family="Avenir",h = 11.7, w = 8.3)
grid.arrange(p.cum.ret,p.ann.ret,p.TO)
#dev.off()


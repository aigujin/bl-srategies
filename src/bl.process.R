opt.w<- rbindlist(lapply(c('last','ma','w.ma'),function(agg){
  #list.conf <- c(alply(rank.conf[,,,agg],2,.dims=T),alply(non.r.conf[,,,agg],2,.dims=T))
  list.conf <- c(alply(case.2.non.r.conf[,,,agg],2,.dims=T),alply(non.r.conf[,,,agg],2,.dims=T))
  #list.conf <- alply(non.r.conf[,,,agg],2,.dims=T)
  rbindlist(lapply(names(list.views),function(type){
    rbindlist(non.rank.script.weights.env.f(type,market.list[m.period],list.views[[type]][bl.period,],list.conf[[type]][bl.period,],0.02,2.5))[,Aggregation:=agg]
  }))
}))



opt.w.f <- function(agg.names,rank.views,nr.views,rank.conf,nr.conf,case='1',tau,delta){
  
  rbindlist(lapply(agg.names,function(agg){
    list.conf <- c(alply(rank.conf[,,,agg],2,.dims=T),alply(nr.conf[,,,agg],2,.dims=T))
    switch(case,
      '1'=rbindlist(lapply(names(nr.views),function(type){
        rbindlist(non.rank.script.weights.env.f(type,market.list[m.period],nr.views[[type]][bl.period,],list.conf[[type]][bl.period,],tau,delta))[,Aggregation:=agg]
      })),
      
      '2'=rbind(rbindlist(lapply(names(rank.views),function(type){
        rbindlist(script.weights.env.f(type,market.list[m.period],rank.views[[type]][bl.period,],list.conf[[type]][bl.period,],tau,delta))[,Aggregation:=agg]})),
        rbindlist(lapply(names(nr.views),function(type){
          rbindlist(non.rank.script.weights.env.f(type,market.list[m.period],nr.views[[type]][bl.period,],list.conf[[type]][bl.period,],tau,delta))[,Aggregation:=agg]})))
    )}))
}


# ####run script from here----
# require(scales)
# portf.perform <- abind(lapply(dimnames(all.Qs)[[1]],function(rank){ abind(lapply(baselines,function (method){
#   sapply(dimnames(all.conf)[[5]],function(meth){
#     opt.w<- script.weights.env.f(method,market.inputs[m.period],all.Qs[rank,bl.period,,],all.conf[rank,bl.period,,,meth],tau,delta)
#     period.ret <- sapply(seq_along(opt.w),function(i){
#       portfolio.return.f(opt.w[[i]]$opt.w,xts.sp.q.ret[(m.period)[i],rownames(opt.w[[i]]$opt.w)],opt.w[[i]]$rf)})
#     assign(paste(rank,'q.ret',method,meth,sep='.'),period.ret,pos=.GlobalEnv)
#     as.matrix(rbind(table.AnnualizedReturns(xts(period.ret,index(xts.sp.q.ret[m.period,]))),mean(sapply(seq_along(opt.w),function(i){opt.w[[i]]$n.views})),mean(turnover(opt.w=opt.w))))
#   })
# }),along=3,new.names=list(c('Ann.Ret','Ann.std','Ann.SR','Ave.stocks','Ave.turnover'),NULL,baselines))
# }),along=4,new.names=list(NULL,NULL,NULL,dimnames(all.Qs)[[1]])
# )
# ###market.return
# market.ret <- sapply(seq_along(mw[m.period]),function(i){
#   stocks <- market.inputs[m.period][[i]]$stock.names
#       portfolio.return.f(as.matrix(mw[m.period][[i]][stocks]),xts.sp.q.ret[(m.period)[i],stocks],market.inputs[m.period][[i]]$rf)
#   })
# m.ret.dt <- setnames(data.table(melt(market.ret)),c('Return'))
# m.ret.dt[,':='(Quarters=1:.N,Method='Market')]
# ####market perform table
# market.perform <- rbind(table.AnnualizedReturns(xts(market.ret,index(xts.sp.q.ret[m.period,]))),mean(sapply(seq_along(market.ret),function(i){length(market.inputs[m.period][[i]]$stock.names)})),mean(market.turnover(mw[m.period])))
# market.dt <- setnames(data.table(melt(data.frame(Measure=dimnames(portf.perform)[[1]],Market=market.perform),id.vars='Measure')),c('Measure','Method','value'))
# 
# ret.q <- abind(lapply(dimnames(all.Qs)[[1]],function(rank){
#   abind(lapply(baselines,function (method){
#     sapply(dimnames(views.omegas)[[5]],function(meth){
#       get(paste(rank,'q.ret',method,meth,sep='.'))
#     })
#   }),along=3,new.names=list(NULL,NULL,baselines))
# }),along=4,new.names=list(NULL,NULL,NULL,dimnames(all.Qs)[[1]]))
# 
# 
# ret.q.dt <- setnames(data.table(melt(ret.q[,,,ranking,drop=F])),c('Quarters','Accuracy','Method','Ranking','Return'))
# 
# non.rank.dt <- setnames(data.table(melt(ret.q[,,1,2])),c('Quarters','Accuracy','Return'))[,Method:='non.rank']
# 
# all.q.ret <- rbind(ret.q.dt,ret.q.dt[,as.list(non.rank.dt),by=Ranking],ret.q.dt[,as.list(m.ret.dt),by=list(Accuracy,Ranking)],use.names=T)
# 
# all.q.ret <- strategyResults(all.q.ret)
# 
# #save(portf.perform,all.q.ret,file=project.dir('Black-Litterman/results.RData'))
# #load(project.dir('Black-Litterman/results.RData'))
# 
# all.q.ret[,base:=ifelse(Method=='true'|Method=='naive'|Method=='default'|Method=='Market'|Method=='non.rank','baselines','nbr')]
# 
# #cache('all.q.ret')
# #cache('portf.perform')
# 
# cairo_pdf(paste('graphs/',ranking,'.all.portfolio.performance.pdf',sep=''), family="Avenir",width = 11.7, heigh = 8.3)
# ggplot(all.q.ret,aes(x=Quarters,y=cum.ret,group=Method,color=Method))+geom_line(size=0.5)+geom_hline(yintercept=100)+facet_grid(Accuracy~Ranking,scale='free_x')+ylab('Portfolio wealth (initial=$100)')+ggtitle('Portfolio performance with $100 initial investment')+theme_bw(base_family='Avenir')+theme(plot.title = element_text(colour = "Blue"),legend.position='top')+scale_color_brewer(palette='Set1')
# dev.off()
# 
# cairo_pdf(paste('graphs/',ranking,'.portfolio.performance.pdf',sep=''), family="Avenir",width = 11.7, heigh = 8.3)
# ggplot(all.q.ret[base=='baselines'],aes(x=Quarters,y=cum.ret,group=Method,color=Method))+geom_line(size=0.5)+geom_hline(yintercept=100)+facet_grid(Accuracy~Ranking,scale='free_x')+ylab('Portfolio wealth (initial=$100)')+ggtitle('Portfolio performance with $100 initial investment')+theme_bw(base_family='Avenir')+theme(plot.title = element_text(colour = "Blue"),legend.position='top')+scale_color_brewer(palette='Set1')
# dev.off()
# 
# bl.dt <- melt(all.q.ret,id.vars=c('Accuracy','Method','Ranking','base'),measure.vars=c('ann.ret','ann.sd','ann.sr'))
# 
# ###Plot ann ret., st dev, sharpe
# cairo_pdf(paste('graphs/',ranking,'.ann.ret.pdf',sep=''), family="Avenir",width = 11.7, heigh = 8.3)
# ggplot(unique(bl.dt),aes(x=Method,y=value))+geom_bar(aes(fill=Method),stat='identity',alpha=0.7)+facet_grid(variable~Ranking+Accuracy,scale='free_y')+theme_bw(base_family='Avenir')+ggtitle('Annualized portfolio measures (return, st.dev, and the Sharpe ratio) \n conditional on rankings and ranking accuracy')+theme(plot.title = element_text(colour = "Blue"),legend.position='top',axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+scale_y_continuous(labels=percent)+ylab('Percent')+scale_color_brewer(palette='Set1')+geom_text(aes(label=round(value,3)*100),angle=90,size=4,hjust=1.2)
# 
# #ggplot(unique(all.q.ret,by=c('Ranking','Accuracy','Method')),aes(x=ann.sd,y=ann.ret,group=Method,color=Method))+geom_point(size=3)+facet_grid(Accuracy~Ranking,scale='fixed')+ggtitle('Risk vs. Return')+theme_bw(base_family='Avenir')+theme_bw(base_family='Avenir')+theme(plot.title = element_text(colour = "Blue"),legend.position='top')+scale_color_brewer(palette='Set1')+geom_smooth(aes(group=1),method='lm',position='identity',se=F)+scale_y_continuous(labels=percent)+scale_x_continuous(labels=percent)
#  
# 
# dev.off()
# 
# ###Quarterly returns---
# cairo_pdf(paste('graphs/',ranking,'.quarterly.ret.pdf',sep=''), family="Avenir",width = 11.7, heigh = 8.3)
# ggplot(all.q.ret,aes(x=Quarters,y=Return,group=Ranking,fill=Ranking))+geom_bar(stat='identity',position='dodge')+facet_grid(Method~Ranking+Accuracy,scale='free_x')+theme_bw(base_family='Avenir')+ylab('Quarterly return')+ggtitle('Quarterly returns overtime')+theme(plot.title = element_text(colour = "Blue"),legend.position='top')+scale_y_continuous(labels=percent)+ylab('Percent')
# dev.off()
# 
# perform.df <- setnames(data.table(melt(portf.perform[,,,1,drop=F],na.rm=T)),c('Measure','Accuracy','Method','Ranking','value'))
# non.rank.per <- setnames(data.table(melt(portf.perform[,,1,2])),c('Measure','Accuracy','value'))
# non.rank.per[,Method:='non.rank']
# all.perform <- rbind(perform.df,perform.df[,as.list(non.rank.per),by=list(Ranking)],perform.df[,as.list(market.dt),by=list(Accuracy,Ranking)],use.names=T)
# #cache('all.perform')
# cairo_pdf(paste('graphs/',ranking,'.portfolio.stat.pdf',sep=''), family="Avenir",w = 11.7, h = 8.3)
# ggplot(all.perform[Measure=='Ave.stocks'|Measure=='Ave.turnover'],aes(x=Method,y=value,fill=Method))+theme_bw(base_family='Avenir')+geom_bar(stat='identity')+facet_grid(Measure~Ranking+Accuracy,scale='free_y')+ggtitle('Trading statistics conditional on rankings and confidence')+theme(legend.position='top',axis.title.x=element_blank(),axis.text.x=element_blank(),plot.title = element_text(colour = "Blue"),axis.title.y=element_blank())
# dev.off()
# 

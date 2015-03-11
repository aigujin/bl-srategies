rm(list=ls())
setwd('~/Dropbox/workspace/Projects/BL-strategies/')

library(ProjectTemplate)
load.project()
delta<-1L;tau=1/50;baselines <- c('true','naive','default');confid.id <- c('cons','last','ma');percentile <- 0.05; cores=4L
##rolling rankings: 20 qtrs - no winning; 16 qtrs - win roll.sd (last); 12

### market data: ~ 113 sec (mcapply: ~ 52 sec)
system.time(source('munge/01-data.q.ret.R'))

### data for first paper
system.time(source('src/paper.trading.APS.true.R'))

ggplot(final.bl,aes(x=as.Date(Quarters),y=cum.ret,group=Views,color=Views))+geom_line(size=0.5)+ylab('Portfolio wealth (initial=$100)')+xlab('Quarters')+ggtitle('Portfolio performance with $100 initial investment')+theme_bw(base_family='Avenir')+theme(plot.title = element_text(colour = "Blue"),legend.position='top')+scale_color_manual(values=getPalette(colourCount))+guides(color=guide_legend(nrow=1L))+geom_hline(yintercept=100L)+facet_grid(~Method,scale='free_x')

ggplot(unique(melt(final.bl[,list(Views,Method,ann.ret,ann.sd,ann.sr)],id.vars=c('Method','Views'))),aes(x=Views,y=value))+geom_bar(aes(fill=Views),stat='identity',alpha=0.7)+facet_grid(variable~Method,scale='free_y')+theme_bw(base_family='Avenir')+ggtitle('Annualized portfolio measures (return, st.dev, and the Sharpe ratio) \n conditional on confidence aggregation')+theme(plot.title = element_text(colour = "Blue"),legend.position='top',axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+scale_y_continuous(labels=percent)+ylab('Percent')+geom_text(aes(label=round(value,3L)*100L),angle=90L,size=3L,hjust=1.2)+scale_fill_manual(values=getPalette(colourCount))

require("knitr")
setwd('~/Dropbox/workspace/Projects/BL-strategies/doc/paper/')
knit2pdf('paper-knitr-APS.Rnw',quiet=T)
setwd('~/Dropbox/workspace/Projects/BL-strategies/')


rm(list=ls())
library('ProjectTemplate')
load.project()
load('data/adj.prices.v5.RData')
yahoo.p <- setkey(melt(data.table(Date=index(p),coredata(p)),id.vars = 'Date',variable.name = 'Stock',value.name = 'yahoo',na.rm=T)[,Stock:=as.character(Stock)],Date,Stock)

load('data/yahoo.close.RData')
yahoo.close.p <- setkey(melt(data.table(Date=index(p),coredata(p)),id.vars = 'Date',variable.name = 'Stock',value.name = 'close.yahoo')[,Stock:=as.character(Stock)],Date,Stock)

yahoo.set <- yahoo.p[yahoo.close.p]


load('data/ref.matrix.RData')
#make DT of it
ref.dt <- setnames(setkey(data.table(Stock=ref.matrix[,1],s.id=rownames(ref.matrix)),s.id),1:2,c('Stock','s.id'))
sp.id <- fread('data/sp.ids.new.csv',header=F)
miss.tkt <- c('AMP','ANV','ATCH','ACS','ASND','BT','HPH','MEYR','MWI','MII','RN','UCC')
miss.dt <- data.table(Stock=miss.tkt,s.id=ref.dt[sp.id][which(is.na(Stock))][,s.id])
ref.dt <- setkey(rbind(ref.dt,miss.dt)[,Stock:=as.character(Stock)][which(duplicated(rbind(ref.dt,miss.dt)[,Stock])),Stock:=paste(Stock,'1',sep='.')],s.id)
s.id <- ref.dt[sp.id,Stock]

ds.adj.sp <- setkey(melt(setnames(data.table(read.csv2('data/prices02.csv',skip=4,sep=';'))[,Code:=as.Date(Code,format('%d-%m-%Y'))],c('Date',s.id)),id.vars = 'Date',variable.name = 'Stock',value.name = 'dsAdj')[,Stock:=as.character(Stock)],Date,Stock)[Stock=='AAPL',':='(dsAdj=dsAdj*7)]


ds.full.p.adj <- setkey(melt(setnames(data.table(read.csv2('~/Dropbox/Datastream/SP500/Prices/full.period.prices.csv',skip=4,sep=';'))[,Code:=as.Date(Code,format('%d-%m-%Y'))],c('Date',s.id)),id.vars = 'Date',variable.name = 'Stock',value.name = 'dsAdj')[,Stock:=as.character(Stock)],Date,Stock)[Stock=='AAPL',':='(dsAdj=dsAdj*7)]

ds.unadj <- setkey(melt(setnames(data.table(read.csv2('data/unadj.price.csv',skip=4,sep=';'))[,Code:=as.Date(Code,format('%d-%m-%Y'))],c('Date',s.id)),id.vars = 'Date',variable.name = 'Stock',value.name = 'unAdj')[,Stock:=as.character(Stock)],Date,Stock)


#prices <- setkey(na.omit(setkey(all.prices,Date,Stock)[setkey(ds.up,Date,Stock),allow.cartesian=T]),Date,Stock)

#pt.prices <- na.omit(unique(pt.dt)[ds.adj[ds.unadj[yahoo.set]]])

#pt.prices <- na.omit(unique(setkey(pt.dt,Date,Stock)[setkey(ds.adj,Date,Stock),allow.cartesian=T],,by=c('Date','Broker','Stock'))[,p:=as.numeric(p)])

load(file='eda/prices.RData')
pt.prices <- na.omit(unique(setkey(pt.dt,Date,Stock)[setkey(prices,Date,Stock),allow.cartesian=T],,by=c('Date','Broker','DSCD')))[,':='(priceAdj=ifelse(priceAdj==0.02,NA,priceAdj),priceClose=ifelse(priceClose==0.02,NA,priceClose),pricePT=ifelse(pricePT==0.02,NA,pricePT))][,':='(tper=PT/pricePT-1),by=list(Date,Stock,Broker)][,':='(trunc.tper=truncate.f(tper,0.01),trunc.tper5=truncate.f(tper,0.05))]
#save(pt.prices,file='eda/pt.prices.RData')
#load('eda/pt.prices.RData')

pt.yahoo <- yahoo.set[pt.prices][,':='(priceAdj=ifelse(priceAdj==0.02,NA,priceAdj),priceClose=ifelse(priceClose==0.02,NA,priceClose))][,':='(tper.ds.adj=PT/priceAdj-1,tper.y.adj=PT/yahoo-1,tper.ds.close=PT/priceClose-1,tper.y.close=PT/close.yahoo-1,ds.y.adj=priceAdj/yahoo,ds.y.close=priceClose/close.yahoo)]




unique(pt.prices[priceAdj==0.02][,sort(Stock)])

### the following stocks have price of 2 cents,yahoo reports different values
unique(ds.adj[p==0.02][,sort(Stock)])
unique(ds.unadj[unAdj==0.02][,sort(Stock)])
#write.table(unique(ds.unadj[unAdj==0.02][,sort(Stock)]),file='/Users/aiguzhinov/Documents/Dropbox/Datastream/AllStocks/002.Stocks.csv',sep=';',row.names=F,col.names=F)


#save(pt.prices,file='doc/StockPrices/pt.prices.RData')
load('doc/StockPrices/pt.prices.RData')
pt.prices[,':='(priceAdj=ifelse(priceAdj==0.02,NA,priceAdj),priceClose=ifelse(priceClose==0.02,NA,priceClose),pricePT=ifelse(pricePT==0.02,NA,pricePT))]

pt.prices[,year:=format(q.id,'%Y')]
require(scales)
ggplot(melt(pt.prices,id.vars = 'q.id',measure.vars = c('tper','trunc.tper5')),aes(x=as.Date(q.id),y=value,group=as.Date(q.id)))+geom_boxplot() + facet_wrap(~variable,scale='free_y',ncol=3)+scale_y_continuous(labels = percent)+ylab('Expected return, in percent')+theme_bw()+xlab('Quarters')

ggplot(melt(pt.yahoo,id.vars = 'q.id',measure.vars = c('tper.ds.adj','tper.y.adj','tper.ds.close','tper.y.close')),aes(x=as.Date(q.id),y=value,group=as.Date(q.id)))+geom_boxplot() + facet_wrap(~variable,scale='free_y',ncol=2)+scale_y_continuous(labels = percent)+ylab('Expected return, in percent')+theme_bw()+xlab('Quarters')+ggtitle('TPER by different databases (DataStream vs. Yahoo) \n and type of stock prices (adjusted vs. unadjusted)')
#ggsave('graphs/different.tper.pdf',w = 11.7, h = 8.3)

#ggplot(melt(pt.prices,id.vars = 'q.id',measure.vars = c('tper','trunc.tper')),aes(value))+geom_bar(binwidth=0.1)+ facet_wrap(~variable,scale='free_x')+theme_bw()+ylab('Frequency')+xlab('Expected return')


ggplot(melt(pt.prices[,descriptive.f(.SD),by=q.id,.SDcols=c('tper','trunc.tper')][V1=='mean'],id.vars = c('q.id','V1')),aes(x=as.Date(q.id),y=value))+geom_bar(stat='identity')+facet_wrap(~variable,scale='free_y',ncol=3)+scale_y_continuous(labels = percent)+ylab('Average expected return, in percent')+theme_bw()+ggtitle('Average TPER per quarter')


require(pastecs)
#pt.prices[,describe(.SD),.SDcols=c('tperDS','tperYH','tperFix'),by=q.id]
pt.yahoo[,descriptive.f(.SD),.SDcols=c('ds.adj','y.adj','ds.close','y.close')]

ggplot(melt(pt.prices[,descriptive.f(.SD),by=q.id,.SDcols=c('tper','trunc.tper')][V1=='mean'],id.vars = c('year','stat')),aes(x=year,y=value,group=stat))+geom_line()+facet_wrap(stat~variable,scale='free_y')+theme_bw()

### Getting meanPT data to compare with pt.prices

pt.data.collect<- function(data.id,n)
{
  rbindlist(lapply(list.files('~/Dropbox/Datastream/AllStocks/it3/pt/',pattern=paste('^',data.id,'.*.csv',sep='')),function(i){
    id=strsplit(i,split = '\\.');
    names.tmp <- sapply(read.csv2(paste('~/Dropbox/Datastream/AllStocks/it3/pt/',i,sep=''),nrow=1,sep=',',na.strings='#ERROR',header=F,skip=1)
      ,function(i){strsplit(as.character(i),split = '[[:punct:]]')[[1]][n]})
    melt(setnames(data.table(read.csv2(paste('~/Dropbox/Datastream/AllStocks/it3/pt/',i,sep=''),sep=',',na.strings='',skip=1))[,Code:=as.yearqtr(paste(str_sub(Code,4,7),str_sub(Code,1,2),sep=' '))],c('Date',names.tmp[2:length(names.tmp)])),id.vars = 'Date',variable.name = 'DSCD',value.name = id[[1]][1],na.rm=T)}))
}


ds.mn.pt <- unique(na.omit(setnames(pt.data.collect('mn',1),c('Date','mn'),c('q.id', 'mn.PT.DS'))),by=c('q.id','DSCD'))
ds.md.pt <- unique(na.omit(setnames(pt.data.collect('md',1),c('Date','md'),c('q.id', 'md.PT.DS'))),by=c('q.id','DSCD'))
ds.num.pt <- unique(na.omit(setnames(pt.data.collect('ne',1),c('Date','ne'),c('q.id', 'num.PT.DS'))),by=c('q.id','DSCD'))
ds.pt <- setkey(setkey(ds.mn.pt,q.id,DSCD)[setkey(ds.md.pt,q.id,DSCD)][ds.num.pt],DSCD)
save(ds.pt,file='eda/ds.pt.RData')
pt.ratio <- na.omit(setkey(unique(q.data[,':='(consPT=mean(PT),mdPT=median(PT),numPT=.N),by=list(q.id,Stock)][,list(q.id,DSCD,Stock,consPT,mdPT,numPT)],by=c('q.id','Stock')),q.id,DSCD)[setkey(ds.pt,q.id,DSCD),allow.cartesian=T])[,':='(md.ratio=round(as.numeric(md.PT.DS)/mdPT,3),mn.ratio=consPT/as.numeric(mn.PT.DS),num.ratio=numPT/as.numeric(num.PT.DS))]

pt.ratio[,descriptive.f(md.ratio)]
ggplot(pt.ratio,aes(x=md.ratio))+geom_histogram()+geom_vline(xintercept=1,color='red')+theme_bw()

ggplot(pt.ratio,aes(y=md.ratio,x=index(md.ratio)))+geom_point()+geom_hline(yintercept=1,color='red')+theme_bw()
#+geom_text(aes(label=Stock,hjust=-0.2,vjust=-0.2))
#Stock==VMED,ds pt is high, looks like error of DS

pt.ratio[,descriptive.f(md.ratio)]
pt.ratio[md.ratio==1][,descriptive.f(md.ratio)]
pt.ratio[md.ratio>(1+0.1)][,descriptive.f(md.ratio)]
pt.ratio[md.ratio<(1-0.1)][,descriptive.f(md.ratio)]

### analysis of ds meadian PT data
load('eda/prices.RData')
na.omit(setkey(unique(prices,by=c('q.id','DSCD'),fromLast = T),q.id,DSCD)[ds.pt])[,descriptive.f(truncate.f(as.numeric(md.PT.DS)/priceAdj-1,0.05))]
ggplot(na.omit(setkey(unique(prices,by=c('q.id','DSCD'),fromLast = T),q.id,DSCD)[ds.pt][,list(q.id,truncate.f(as.numeric(md.PT.DS)/pricePT-1,0.05))]),aes(x=as.Date(q.id),y=V2,group=as.Date(q.id)))+geom_boxplot()+theme_bw()


q.data[,year:=format(s.Date,'%Y')]
q.data[,descriptive.f(truncate.f(b.view,0.05))]
q.data[,trunk.view:=truncate.f(b.view,0.05)]

q.data[!is.na(trunk.view),.N,by=year] #number of PT per year
q.data[!is.na(trunk.view),.N,by=list(year,Broker)][,.N,by=year]#numb of brokers
q.data[!is.na(trunk.view),.N,by=list(year,Stock)][,.N,by=year]# numb of Stocks
q.data[!is.na(trunk.view),mean(truncate.f(b.view,0.05),na.rm=T),by=list(year)]
q.data[,median(b.view),by=list(q.id)]

ggplot(q.data[,median(b.view),by=list(year)],aes(x=year,y=V1))+geom_bar(stat='identity')+theme_bw()

ggplot(melt(pt.ratio,id.vars = 'q.id',measure.vars = c('md.ratio','mn.ratio')),aes(x=as.Date(q.id),y=value,group=as.Date(q.id)))+geom_boxplot()+theme_bw()+ facet_wrap(~variable,scale='free_x',ncol=2)

ggplot(melt(mean.pt.dt,id.vars = 'q.id',measure.vars = c('tper.pt.ds.adj','tper.pt.ds.y.adj','tper.pt.ds.close','tper.pt.ds.y.close')),aes(x=as.Date(q.id),y=value,group=as.Date(q.id)))+geom_boxplot() + facet_wrap(~variable,scale='free_y',ncol=2)+scale_y_continuous(labels = percent)+ylab('Expected return, in percent')+theme_bw()+xlab('Quarters')+ggtitle('TPER by different databases (DataStream vs. Yahoo) \n and type of stock prices (adjusted vs. unadjusted)')

mean.pt.dt[,pt.ratio][,descriptive.f(round(pt.ratio,3))]
mean.pt.dt[pt.ratio==1][,descriptive.f(round(pt.ratio),3)]
mean.pt.dt[pt.ratio<1][,descriptive.f(pt.ratio)]

mean.pt.dt[,descriptive.f(pt.ratio)]
mean.pt.dt[pt.ratio>3][order(pt.ratio,decreasing=T)][,list(q.id,PT.DS,Stock,old.PT,meanPT,priceAdj,pt.ratio,tper,tper.pt.ds.adj)]
mean.pt.dt[order(pt.ratio,decreasing=T)]

# ### TODO:----
# ###missing stocks in the PT datasets, check them in DS again
# write.csv(stock.ids[sort(unique(pt.prices[is.na(p)][,Stock]))],file='/Users/aiguzhinov/Documents/Dropbox/Datastream/AllStocks/miss.in.PT.csv')
# ### stocks that don'have Type (numerical DS id)
# write.csv(sort(unique(stock.ids[Type==""][,Stock])),file='/Users/aiguzhinov/Documents/Dropbox/Datastream/AllStocks/miss.type.csv')
# write.csv(unique(ds.adj[p==0.02][,sort(Stock)]),file='/Users/aiguzhinov/Documents/Dropbox/Datastream/AllStocks/002.Stocks.csv')
#
# ###SP500 stocks have some errors in s.id
# setkey(stock.ids,Stock)[setkey(ref.dt,Stock)]
# setkey(stock.ids,Type)[setkey(sp.id,V1)]
#
# sp.ids <- melt(setnames(fread('/Users/aiguzhinov/Documents/Dropbox/Datastream/SP500/sp.monthly.ds.ids.csv',select =c(1:135),header=F,na.strings = ''),as.character(as.yearmon(seq.Date(from=as.Date('1998-12-01'),to=as.Date('2010-02-01'),by='month')))),measure.vars = as.character(as.yearmon(seq.Date(from=as.Date('1998-12-01'),to=as.Date('2010-02-01'),by='month'))),variable.name='Month',value.name = 'Type')
#
#
# sp.id <- setkey(setnames(fread('data/sp.ids.new.csv',header=F),'Type'))
# setkey(stock.ids,Type)[setkey(unique(sp.ids,by='Type'),Type)]
#
# write.csv(na.omit(setkey(stock.ids,Type)[setkey(unique(sp.ids,by='Type'),Type)][is.na(Stock),Type]),file='/Users/aiguzhinov/Documents/Dropbox/Datastream/SP500/miss.ds.codes.csv',row.names=F,col.names=F)
#
#
#
# #yahoo.tickers <- unique(yahoo.p[,Stock])
# #p <- get.stocks(start = '1989-01-01',end = '2009-12-31',tckk = yahoo.tickers,type = 'Close')
# #stocks <- get.stocks.quandl(start = '1989-01-01',end = '2009-12-31',tckk = yahoo.tickers[1:3])
#
# ### DS full stock set
# stock.ref <- unique(rbindlist(lapply(list.files('/Users/aiguzhinov/Documents/Dropbox/Datastream/AllStocks/names/',pattern='name.ds.*.csv'),function(i){fread(paste('/Users/aiguzhinov/Documents/Dropbox/Datastream/AllStocks/names/',i,sep=''),header=T,na.strings = '')})),by='DSCD')
#
# # ids
# #load('/Users/aiguzhinov/Documents/Dropbox/Datastream/AllStocks/ds.stock.names.RData')
# #tickers
# #load('/Users/aiguzhinov/Documents/Dropbox/Datastream/AllStocks/names.ds.ticker.RData')
# #merge together
# #all.ds.stocks.id <- data.table(Stock=names.ds.ticker,ds.code=names(ds.stock.names))
#
#
# sp.stocks <- cbind(setkey(melt(setnames(data.table(read.csv('data/sp.500.complete.csv',header=T,sep=',',dec='.',skip=1,na.string=''))[,.SD,.SDcols=seq(1,176,4)],as.character(1:44)),measure.vars = as.character(1:44),value.name = 'Type',variable.name = 'q.id'),q.id),setkey(melt(setnames(data.table(read.csv('data/sp.500.complete.csv',header=T,sep=',',dec='.',skip=1,na.string=''))[,.SD,.SDcols=seq(2,176,4)],as.character(1:44)),measure.vars = as.character(1:44),value.name = 'LongName',variable.name = 'q.id'),q.id),setkey(melt(setnames(data.table(read.csv('data/sp.500.complete.csv',header=T,sep=',',dec='.',skip=1,na.string=''))[,.SD,.SDcols=seq(3,176,4)],as.character(1:44)),measure.vars = as.character(1:44),value.name = 'Mnemonic',variable.name = 'q.id'),q.id))
#
# setkey(stock.ref,DSCD)[unique(setkey(sp.stocks,Type))]
#
# ### There are 3767 stocks in DS database, I have to split into smaller .csv; choose to have 500 stocks per batch
# #
# # cut.points <- sort(c(seq(0,nrow(all.ds.stocks.id),500),seq(1,nrow(all.ds.stocks.id),500),nrow(all.ds.stocks.id)))
# #
# #
# # sapply(2:length(cut.points),function(batch)
# #   {
# #       ids <- (cut.points)[batch-1]:cut.points[batch]
# #
# # write.table(all.ds.stocks.id[ids,ds.code],file=paste('/Users/aiguzhinov/Documents/Dropbox/Datastream/AllStocks/ds.codes.',batch-1,'.csv',sep=''),sep=';',row.names=F,col.names=F)
# #   })
#
# #ggplot(melt(pt.prices,id.vars = 'q.id',measure.vars = c('tperYH')),aes(x=as.Date(q.id),y=value,group=as.Date(q.id)))+geom_line() + facet_wrap(~variable)
#
# ### SP500 data form DS
#
# data.collect<- function(data.id)
# {
#   stock.ref <- unique(rbindlist(lapply(list.files('/Users/aiguzhinov/Documents/Dropbox/Datastream/AllStocks/names/',pattern='name.ds.*.csv'),function(i){fread(paste('/Users/aiguzhinov/Documents/Dropbox/Datastream/AllStocks/names/',i,sep=''),header=T,na.strings = '')})),by='DSCD')
#
# all <- rbindlist(lapply(list.files('/Users/aiguzhinov/Documents/Dropbox/Datastream/AllStocks/it2/',pattern=paste('^',data.id,'.*.csv',sep='')),function(i){id=strsplit(i,split = '\\.');melt(setnames(fread(paste('/Users/aiguzhinov/Documents/Dropbox/Datastream/AllStocks/it2/',i,sep=''),skip = 4,header=F,sep=',',na.strings='')[,V1:=as.Date(V1,format = "%m/%d/%Y")],c('Date',fread(paste('/Users/aiguzhinov/Documents/Dropbox/Datastream/AllStocks/names/name.ds.codes',id[[1]][2],'csv',sep='.'),header=T,na.strings = '')[,DSCD])),id.vars = 'Date',variable.name = 'Stock',value.name = id[[1]][1],na.rm=T)}))
#
# setnames(setkey(all,Stock)[setkey(stock.ref,DSCD),allow.cartesian=T][,list(Date,Stock,NAME,WC05601,get(data.id))],c('Stock','WC05601','V5'),c('Type','Stock',data.id))
# }
#
# ds.adj <- na.omit(unique(data.collect('p'),by=c('Date','Type')))
# ds.up <- na.omit(unique(data.collect('up'),by=c('Date','Type')))
# ds.ret <- na.omit(unique(data.collect('ret'),by=c('Date','Type')))
# ds.af <- unique(data.collect('af'),by=c('Date','Type'))[,':='(year=format(Date,'%Y'),af=as.numeric(af),Time=Date)][,Date:=NULL]
#
# #ds.adj <- setkey(rbind(ds.p.2.6,ds.p)[,Stock:=as.character(Stock)],Date,Stock)
# save(ds.adj,file='/Users/aiguzhinov/Documents/Dropbox/Datastream/AllStocks/Prices/ds.adj.RData')
#

#dscd <- setnames(rbindlist(lapply(list.files('/Users/aiguzhinov/Documents/Dropbox/Datastream/AllStocks/stock.values',pattern='dscd.*.csv'),function(i){fread(paste('/Users/aiguzhinov/Documents/Dropbox/Datastream/AllStocks/stock.values/',i,sep=''),skip = 1,header=T)})),c('ds.code','Type'))

#stock.ids <- setkey(all.ds.stocks.id,ds.code)[setkey(dscd,ds.code)]
#save(stock.ids,file='/Users/aiguzhinov/Documents/Dropbox/Datastream/AllStocks/stock.ids.RData')

#ds.up.2.4 <- rbindlist(lapply(list.files('/Users/aiguzhinov/Documents/Dropbox/Datastream/AllStocks/stock.values',pattern='^up.*.csv')[5:6],function(i){id=strsplit(i,split = '\\.');melt(setnames(data.table(read.csv2(paste('/Users/aiguzhinov/Documents/Dropbox/Datastream/AllStocks/stock.values/',i,sep=''),skip = 6,header=F))[,V1:=as.Date(V1,format = "%d.%m.%y")],c('Date',setkey(all.ds.stocks.id,ds.code)[fread(paste('/Users/aiguzhinov/Documents/Dropbox/Datastream/AllStocks/ds.codes',id[[1]][2],'csv',sep='.'),header=F)][,Stock])),id.vars = 'Date',variable.name = 'Stock',value.name = id[[1]][1],na.rm=T)}))

#ds.up.rest<- rbindlist(lapply(list.files('/Users/aiguzhinov/Documents/Dropbox/Datastream/AllStocks/stock.values',pattern='^up.*.csv')[-c(5:6)],function(i){id=strsplit(i,split = '\\.');melt(setnames(data.table(read.csv2(paste('/Users/aiguzhinov/Documents/Dropbox/Datastream/AllStocks/stock.values/',i,sep=''),skip = 6,header=F,sep=','))[,V1:=as.Date(V1,format = "%m/%d/%Y")],c('Date',setkey(all.ds.stocks.id,ds.code)[fread(paste('/Users/aiguzhinov/Documents/Dropbox/Datastream/AllStocks/ds.codes',id[[1]][2],'csv',sep='.'),header=F)][,Stock])),id.vars = 'Date',variable.name = 'Stock',value.name = id[[1]][1],na.rm=T)}))

#ds.up <- setkey(rbind(ds.up.2.4,ds.up.rest)[,Stock:=as.character(Stock)],Date,Stock)

#save(ds.up,file='/Users/aiguzhinov/Documents/Dropbox/Datastream/AllStocks/Prices/ds.unadj.RData')

#load('/Users/aiguzhinov/Documents/Dropbox/Datastream/AllStocks/Prices/ds.unadj.RData')

#ds.ret.2.4 <- rbindlist(lapply(list.files('/Users/aiguzhinov/Documents/Dropbox/Datastream/AllStocks/stock.values',pattern='^ret.*.csv')[5:6],function(i){id=strsplit(i,split = '\\.');melt(setnames(data.table(read.csv2(paste('/Users/aiguzhinov/Documents/Dropbox/Datastream/AllStocks/stock.values/',i,sep=''),skip = 6,header=F))[,V1:=as.Date(V1,format = "%d.%m.%y")],c('Date',setkey(all.ds.stocks.id,ds.code)[fread(paste('/Users/aiguzhinov/Documents/Dropbox/Datastream/AllStocks/ds.codes',id[[1]][2],'csv',sep='.'),header=F)][,Stock])),id.vars = 'Date',variable.name = 'Stock',value.name = id[[1]][1],na.rm=T)}))

#ds.ret.rest<- rbindlist(lapply(list.files('/Users/aiguzhinov/Documents/Dropbox/Datastream/AllStocks/stock.values',pattern='^ret.*.csv')[-c(5:6)],function(i){id=strsplit(i,split = '\\.');melt(setnames(data.table(read.csv2(paste('/Users/aiguzhinov/Documents/Dropbox/Datastream/AllStocks/stock.values/',i,sep=''),skip = 6,header=F,sep=','))[,V1:=as.Date(V1,format = "%m/%d/%Y")],c('Date',setkey(all.ds.stocks.id,ds.code)[fread(paste('/Users/aiguzhinov/Documents/Dropbox/Datastream/AllStocks/ds.codes',id[[1]][2],'csv',sep='.'),header=F)][,Stock])),id.vars = 'Date',variable.name = 'Stock',value.name = id[[1]][1],na.rm=T)}))

#ds.ret <- setkey(rbind(ds.ret.2.4,ds.ret.rest)[,Stock:=as.character(Stock)],Date,Stock)

#save(ds.ret,file='/Users/aiguzhinov/Documents/Dropbox/Datastream/AllStocks/Returns/ds.d.ret.RData')


#ds.af.2.4 <- rbindlist(lapply(list.files('/Users/aiguzhinov/Documents/Dropbox/Datastream/AllStocks/stock.values',pattern='^af.*.csv')[5:6],function(i){id=strsplit(i,split = '\\.');melt(setnames(data.table(read.csv2(paste('/Users/aiguzhinov/Documents/Dropbox/Datastream/AllStocks/stock.values/',i,sep=''),skip = 6,header=F))[,V1:=as.Date(V1,format = "%d.%m.%y")],c('Time',setkey(all.ds.stocks.id,ds.code)[fread(paste('/Users/aiguzhinov/Documents/Dropbox/Datastream/AllStocks/ds.codes',id[[1]][2],'csv',sep='.'),header=F)][,Stock])),id.vars = 'Time',variable.name = 'Stock',value.name = id[[1]][1],na.rm=T)}))

#ds.af.rest<- rbindlist(lapply(list.files('/Users/aiguzhinov/Documents/Dropbox/Datastream/AllStocks/stock.values',pattern='^af.*.csv')[-c(5:6)],function(i){id=strsplit(i,split = '\\.');melt(setnames(data.table(read.csv2(paste('/Users/aiguzhinov/Documents/Dropbox/Datastream/AllStocks/stock.values/',i,sep=''),skip = 6,header=F,sep=','))[,V1:=as.Date(V1,format = "%m/%d/%Y")],c('Time',setkey(all.ds.stocks.id,ds.code)[fread(paste('/Users/aiguzhinov/Documents/Dropbox/Datastream/AllStocks/ds.codes',id[[1]][2],'csv',sep='.'),header=F)][,Stock])),id.vars = 'Time',variable.name = 'Stock',value.name = id[[1]][1],na.rm=T)}))

#ds.af <- setkey(rbind(ds.af.2.4,ds.af.rest)[,':='(Stock=as.character(Stock),af=as.numeric(af))],Stock)[,year:=format(Time,'%Y')]

#save(ds.af,file='/Users/aiguzhinov/Documents/Dropbox/Datastream/AllStocks/Prices/ds.af.RData')




# s.ids <- list.files('~/Documents/Dropbox/Datastream/ids')
#
# dt <- rbindlist(lapply(seq_along(tmp),function(i){
#   s.id <- as.character(ref.dt[fread(paste('~/Documents/Dropbox/Datastream/ids/',s.ids[i],sep=''),header=F)][,Stock])
#   price.dt <- setnames(fread(paste('~/Documents/Dropbox/Datastream/Data/csv/',tmp[i],sep=''),skip=3,sep = ';')[,V1:=as.Date(V1,format('%d.%m.%y'))][,.SD,.SDcols=c(1,seq(2,ncol(fread(paste('~/Documents/Dropbox/Datastream/Data/csv/',tmp[i],sep=''),skip=3,sep = ';')),3))],c('Date',s.id))
#   for (i in seq_along(price.dt)) set(price.dt, j=which(is.na(colnames(price.dt))), value=NA)
#   melt(price.dt,na.rm=T,id.vars='Date',variable.name = 'Stock',value.name = 'Price')
# }))
# setkey(dt[,newDS:=Price[c(rep(NA,3),1:(.N-3))],by=Stock][,Stock:=as.character(Stock)],Date,Stock)
#
# setkey(ref.dt,s.id)
# s.id <- as.character(ref.dt[fread('~/Documents/Dropbox/Datastream/Full Set/sp.ids.new.csv',header=F),Stock])
# new.p <- na.omit(setkey(melt(setnames(fread('~/Documents/Dropbox/Datastream/Data/Prices/prices02.csv',skip=4,sep=';')[,Code:=as.Date(Code,format('%d-%m-%Y'))],c('Date',s.id)),id.vars = 'Date',variable.name = 'Stock',value.name = 'Price')[,new.dsPT:=Price[c(rep(NA,3),1:(.N-3))],by=Stock][,Stock:=as.character(Stock)],Date,Stock))
#
# new.p[Stock=='AAPL',':='(Price=Price*7,new.dsPT=new.dsPT*7)]


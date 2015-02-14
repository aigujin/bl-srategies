###This script for data.table excercise
# getStockData <- function(index,tmp,skip,ref.m,val.name,date.format){
#   data <- tmp[-skip,.SD,.SDcols=c(1,index)]
# setnames(data,c('Date',ref.m))
# data[,Date:=as.Date(Date,format=date.format)]
# melt(data,id.vars='Date',variable.name='Stock',value.name=val.name)
# }
data.collect<- function(data.id,n)
{
  rbindlist(lapply(list.files('~/Dropbox/Datastream/AllStocks/it3/',pattern=paste('^',data.id,'.*.csv',sep='')),function(i){
    id=strsplit(i,split = '\\.');
    names.tmp <- sapply(read.csv2(paste('~/Dropbox/Datastream/AllStocks/it3/',i,sep=''),nrow=1,sep=',',na.strings='#ERROR',header=F,skip=1)
      ,function(i){strsplit(as.character(i),split = '[[:punct:]]')[[1]][n]})
    melt(setnames(fread(paste('~/Dropbox/Datastream/AllStocks/it3/',i,sep=''),sep=',',na.strings='')[,V1:=as.Date(V1,format = "%m/%d/%Y")],c('Date',names.tmp[2:length(names.tmp)])),id.vars = 'Date',variable.name = 'DSCD',value.name = id[[1]][1],na.rm=T)}))
}

af.data.collect<- function(data.id,n)
{
  rbindlist(lapply(list.files('~/Dropbox/Datastream/AllStocks/it3/',pattern=paste('^',data.id,'.*.csv',sep='')),function(i){
    id=strsplit(i,split = '\\.');
    names.tmp <- sapply(read.csv2(paste('~/Dropbox/Datastream/AllStocks/it3/',i,sep=''),nrow=1,sep=',',na.strings='#ERROR',header=F,skip=1)
      ,function(i){strsplit(as.character(i),split = '[[:punct:]]')[[1]][n]})
    melt(setnames(fread(paste('~/Dropbox/Datastream/AllStocks/it3/',i,sep=''),sep=',',na.strings='',skip=2)[,V1:=as.Date(V1,format = "%m/%d/%Y")],c('Date',names.tmp[2:length(names.tmp)])),id.vars = 'Date',variable.name = 'DSCD',value.name = id[[1]][1],na.rm=T)}))
}
setwd('~/Dropbox/workspace/Projects/Black-Litterman/BL-strategies')
source("lib/my.paths.R")


lapply(list('xts','reshape2','data.table','plyr','stringr'),function(X) {do.call("require", list(X)) })


stock.ref <- setkey(unique(rbindlist(lapply(list.files('~/Dropbox/Datastream/AllStocks/names/',pattern='name.ds.*.csv'),function(i){fread(paste('~/Dropbox/Datastream/AllStocks/names/',i,sep=''),header=T,na.strings = '')})),by='DSCD')[,Stock:=WC05601],DSCD,Stock)

load('data/ref.matrix.RData')
#make DT of it
ref.dt <- setkey(data.table(Stock=ref.matrix[,1],DSCD=rownames(ref.matrix)),DSCD)
sp.id <- setkey(setnames(fread('data/sp.ids.new.csv',header=F),'DSCD'),DSCD)
miss.tkt <- c('AMP','ANV','ATCH','ACS','ASND','BT','HPH','MEYR','MWI','MII','RN','UCC')
miss.dt <- data.table(Stock=miss.tkt,DSCD=ref.dt[sp.id][which(is.na(Stock))][,DSCD])
ref.dt <- setkey(rbind(ref.dt,miss.dt)[,Stock:=as.character(Stock)][which(duplicated(rbind(ref.dt,miss.dt)[,Stock])),Stock:=paste(Stock,'1',sep='.')],DSCD,Stock)

full.stock.ref <- setkey(unique(merge(stock.ref,ref.dt,all=T),by=c('DSCD','Stock')),DSCD,Stock)
s.id <- setkey(full.stock.ref[ref.dt][,list(DSCD,NAME,Stock)],DSCD)

###all stocks to used for variables
all.stock.prices <- na.omit(setkey(setnames(unique(data.collect('p',1),by=c('Date','DSCD')),'p','priceAdj')[,pricePT:=c(rep(NA,3),head(priceAdj,-3)),by=DSCD],DSCD)[setkey(full.stock.ref,DSCD),allow.cartesian=T])[,list(Date,Stock,DSCD,NAME,priceAdj,pricePT)]

### SP stock daily prices (used for BL model)
all.prices <- setkey(setnames(unique(data.collect('p',1),by=c('Date','DSCD')),'p','priceAdj'),DSCD)[s.id][,q.id:=as.yearqtr(Date)]

#q.ret.sp <- all.prices[,quarterlyReturn(xts(priceAdj,order.by=Date)),by=.(Stock,Quarters)]
q.ret.sp<-all.prices[,q.ret:=(log(last(priceAdj))-log(first(priceAdj))),by=.(Stock,q.id)][,.(q.id,DSCD,Stock,q.ret)]
d.ret.sp <- all.prices[,d.ret:=c(NA,diff(log(priceAdj))),by=Stock][,.(Date,DSCD,Stock,d.ret)]

### All stock UNADUSTED prices (used for EDA analysis)
#ds.up <- na.omit(setkey(setnames(unique(data.collect('up',1),by=c('Date','DSCD')),'up','priceClose'),DSCD)[setkey(full.stock.ref,DSCD),allow.cartesian=T])[,list(Date,Stock,DSCD,NAME,priceClose)]


### SP stock daily returns 
#sp.ret <- setkey(unique(data.collect('ret',3),by=c('Date','DSCD')),DSCD)[s.id]

#set(sp.ret, which(sp.ret[[3]]==0), 3L, value=NA)


### SP stock adjustment factor (used for analsyts rankings)
#ds.af <- setkey(unique(af.data.collect('af',1),by=c('Date','DSCD'))[,':='(year=format(Date,'%Y'),af=as.numeric(af),Time=Date)][,Date:=NULL],DSCD)[s.id]
### Adustment factor (AF) for all stock
ds.af.all.stock <- full.stock.ref[setkey(unique(af.data.collect('af',1),by=c('Date','DSCD'))[,':='(year=format(Date,'%Y'),af=as.numeric(af),Time=Date)][,Date:=NULL],DSCD),allow.cartesian=T][,list(Stock,DSCD,af,year)]

##For AP report, create  DS of all prices
#prices <- na.omit(setkey(all.stock.prices,Date,DSCD,Stock,NAME)[setkey(ds.up,Date,DSCD,Stock,NAME)])
#prices.sp <- setkey(prices,DSCD)[s.id]
#save(prices,file='eda/prices.RData')



### Reproducible part: stock prices from Datastream to DT#
# matrix of cross referenc of stocks ids in DataStream with tickers
#load('data/ref.matrix.RData')
#make DT of it
#ref.dt <- setnames(setkey(data.table(Stock=ref.matrix[,1],s.id=rownames(ref.matrix)),s.id),1:2,c('Stock','s.id'))
#sp.id <- setkey(setnames(fread('data/sp.ids.new.csv',header=F),'Type'))


#miss.tkt <- c('AMP','ANV','ATCH','ACS','ASND','BT','HPH','MEYR','MWI','MII','RN','UCC')
#miss.dt <- data.table(Stock=miss.tkt,s.id=ref.dt[sp.id][which(is.na(Stock))][,s.id])
#ref.dt <- setkey(rbind(ref.dt,miss.dt)[,Stock:=as.character(Stock)][which(duplicated(rbind(ref.dt,miss.dt)[,Stock])),Stock:=paste(Stock,'1',sep='.')],s.id)


### loading raw data: SP- q.ret,
###File sp.500.daily.p.ret.csv, has both daily prices and daily returns. For each stock, it has 2 columns
#s.prices.tmp <- fread('data/sp.500.daily.p.ret.csv')
#q.ret.tmp <- data.table(read.csv('data/sp.500.quarterly.p.ret.csv',skip=4,header=T))[,Code:=as.yearqtr(paste(str_sub(Code,4,7),str_sub(Code,1,2),sep=' '))]

#rf.tmp<-setkey(setnames(fread(data.stock.dir('Market/t.bill.daily.csv'))[,':='(V1=as.Date(V1,format='%m.%d.%Y'),V2=V2/100/252)],c('Date','rf')),Date)

#risk.free<-fread(data.stock.dir('Market/risk-free.csv'))
#sp.index <- data.table(read.csv('data/sp.500.complete.csv',header=T,sep=',',dec='.',skip=1))

#price.ind <- seq(2,ncol(fread('data/sp.500.daily.p.ret.csv')),3)
#ret.ind <- seq(4,ncol(fread('data/sp.500.daily.p.ret.csv')),3)
#q.ret.ind <- seq(4,ncol(read.csv('data/sp.500.quarterly.p.ret.csv',skip=4,header=T)),3)

###quarterly data
###quarterly data
#quarters <- seq(as.Date('1999-01-01'),as.Date('2009-10-01'),by='3 month')-1
#quarters <- seq(as.Date('1999-01-01'),as.Date('2010-01-01'),by='3 month')-1
# require(Quandl)
# Quandl.auth("X5ws5JEoYdP6VFefbPmQ")
# tbil.data <- data.table(Quandl('FRED/DTB3',start_date = '1999-01-01',end_date = '2009-12-31',sort='asc'))
# 
# q.rf <- unique(tbil.data[,Quarters:=as.yearqtr(Date)][,rf:=mean(Value/100)/4,by=Quarters],by=c('Quarters'))[,.(Quarters,rf)]
# 
#q.ret.rf.tmp <- data.table(read.csv2('data/q.ret02.csv',skip=4,sep=';',na.strings=''))
# names.tmp <- sapply(colnames(q.ret.rf.tmp),function(i){strsplit(as.character(i),split = '[[:punct:]]')[[1]][3]})
# 
# q.ret.rf <- s.id[setkey(setkey(melt(setnames(setnames(q.ret.rf.tmp,c('Code',names.tmp[2:length(names.tmp)]))[,Code:=as.yearqtr(paste(str_sub(Code,4,7),str_sub(Code,1,2),sep=' '))],'Code', 'q.id'),id.vars='q.id',variable.name='DSCD',value.name='q.ret'),q.id)[,q.num:=.GRP,by=q.id][setkey(q.rf,Quarters)] ,DSCD)]
# 
# set(q.ret.rf, which(q.ret.rf[[5]]==0), 5L, value=NA)
# 
# q.ret.rf <- q.ret.rf[,ex.ret:=q.ret-rf]
# set(q.ret.rf, which(is.na(q.ret.rf[[8]])), 8L, value=0)
require(Quandl)
Quandl.auth("X5ws5JEoYdP6VFefbPmQ")
#tbil.data <- data.table(Quandl('FRED/DTB3',start_date = '1998-12-31',end_date = '2009-12-31',sort='asc'))

tbil.data <- data.table(Quandl('FRED/DTB3',start_date = '1999-01-01',end_date = '2009-12-31',sort='asc'))

q.rf <- unique(tbil.data[,Quarters:=as.yearqtr(Date)][,rf:=mean(Value/100)/4,by=Quarters],by=c('Quarters'))[,.(Quarters,rf)]

q.ret.rf <- setkey(unique(q.ret.sp),q.id)[q.rf][,ex.ret:=q.ret-rf]
set(q.ret.rf, which(is.na(q.ret.rf[[6]])), 6L, value=0)


#q.ret.rf.tmp <- data.table(read.csv2('data/q.ret02.csv',skip=4,sep=';',na.strings=''))
#names.tmp <- sapply(colnames(q.ret.rf.tmp),function(i){strsplit(as.character(i),split = '[[:punct:]]')[[1]][3]})
#check.q.ret <- s.id[setkey(melt(setnames(setnames(q.ret.rf.tmp,c('Code',names.tmp[2:length(names.tmp)]))[,Code:=as.yearqtr(paste(str_sub(Code,4,7),str_sub(Code,1,2),sep=' '))],'Code', 'q.id'),id.vars='q.id',variable.name='DSCD',value.name='q.ret'),DSCD)]
#aapl.p <- xts(all.stock.prices[Stock=='AAPL'][,priceAdj],order.by = all.stock.prices[Stock=='AAPL'][,Date])


#q.ret.rf <- s.id[setkey(setkey(melt(setnames(setnames(q.ret.rf.tmp,c('Code',names.tmp[2:length(names.tmp)]))[,Code:=as.yearqtr(paste(str_sub(Code,4,7),str_sub(Code,1,2),sep=' '))],'Code', 'q.id'),id.vars='q.id',variable.name='DSCD',value.name='q.ret'),q.id)[,q.num:=.GRP,by=q.id][setkey(q.rf,Quarters)] ,DSCD)]


#q.ret.rf <- s.id[setkey(setkey(melt(setnames(setnames(q.ret.rf.tmp,c('Code',names.tmp[2:length(names.tmp)]))[,Code:=as.yearqtr(quarters)],'Code', 'q.id'),id.vars='q.id',variable.name='DSCD',value.name='q.ret'),q.id)[,q.num:=.GRP,by=q.id][setkey(q.rf,Quarters)] ,DSCD)]

#set(q.ret.rf, which(q.ret.rf[[5]]==0), 5L, value=NA)

#q.ret.rf <- q.ret.rf[,ex.ret:=q.ret-rf]
#set(q.ret.rf, which(is.na(q.ret.rf[[8]])), 8L, value=0)





#s.ex.ret <- setkey(setkey(sp.ret,Date)[setkey(tbil.data[,rf:=Value/100/252][,.(Date,rf)],Date)][,':='(excess=ret-rf,q.id=as.yearqtr(Date))],q.id)[,q.num:=.GRP,by=q.id]

s.ex.ret <- setkey(setkey(d.ret.sp,Date)[setkey(tbil.data[,rf:=Value/100/252][,.(Date,rf)],Date)][,':='(excess=d.ret-rf,q.id=as.yearqtr(Date))],q.id)[,q.num:=.GRP,by=q.id]


#set(s.ex.ret, i=which(is.na(s.ex.ret[[7]])), 7L, value=0)

set(s.ex.ret, i=which(is.na(s.ex.ret[[6]])), 6L, value=0)
###quarterly SP500 index, value and market weights
quarters <- seq(as.Date('1999-04-01'),as.Date('2010-01-01'),by='3 month')-1
sp.set.tkt <- setkey(na.omit(s.id[,list(DSCD,Stock)][setkey(data.table(setkey(melt(setnames(data.table(read.csv('data/sp.500.complete.csv',header=T,sep=',',dec='.',skip=1,na.strings=''))[,.SD,.SDcols=seq(1,176,4)],as.character(as.yearqtr(quarters))),measure.vars = as.character(as.yearqtr(quarters)),variable.name='Quarters',value.name='DSCD'),Quarters),setkey(melt(setnames(data.table(read.csv('data/sp.500.complete.csv',header=T,sep=',',dec='.',skip=1,na.strings=''))[,.SD,.SDcols=seq(4,176,4)],as.character(as.yearqtr(quarters))),measure.vars = as.character(as.yearqtr(quarters)),variable.name='Quarters',value.name='value'),Quarters))[,q.num:=.GRP,by=Quarters][,Quarters:=NULL],DSCD)][,list(DSCD,Stock,Quarters,mv=value)][,q.id:=as.yearqtr(Quarters)]),q.id)[,q.num:=.GRP,by=q.id][,mw:=mv/sum(mv,na.rm=T),by=q.num]



require('BurStFin')
market.list<-lapply(1:44,function(i)
{
#Select SP stocks for given quarter
        sp.in.i <- unique(setkey(sp.set.tkt[q.num==i],q.id,Stock,DSCD))
#Merge SP with daily returns for sigma
        s.daily <- na.omit(setkey(s.ex.ret,q.id,Stock,DSCD)[sp.in.i])
#Merge SP and daily returns with quarterly returns 
#to create a data.table with all necessary information for a given quarter
        all.set.i <- setkey(q.ret.rf,q.id,Stock,DSCD)[setkey(s.daily,q.id,Stock,DSCD),allow.cartesian=T]
        
        sigma<-var.shrink.eqcor(acast(all.set.i,Date~Stock,value.var='excess',fill=0))
        
        diag(sigma) <- diag(sigma)*252
        #w.mkt.i<- as.matrix(round(sp.in.i[dimnames(sigma)[[1]],mw],4))
        w.mkt.i <- as.matrix(unique(all.set.i,by='Stock')[,mw])
        rf <- unique(all.set.i[,rf])
        #var.market <- crossprod(w.mkt.i,sigma)%*%w.mkt.i
        #m.ret<-unique(setkey(q.ret.rf,Stock)[q.num==i][dimnames(sigma)[[1]],])[,q.ret]
        #delta <- as.numeric((crossprod(w.mkt.i,m.ret)-rf)/var.market)
        impl.sp.ret<-round(delta*crossprod(sigma,w.mkt.i),4)
        list(sigma=sigma,impl.ret=impl.sp.ret,rf=rf,stock.names=dimnames(sigma)[[1]],ex.ret=unique(all.set.i,by=c('Stock'))[,ex.ret],Quarters=unique(all.set.i[,Quarters]))
})


setkey(sp.set.tkt,q.id,DSCD,Stock)
setkey(q.ret.rf,q.id,DSCD,Stock)
market.set <- unique(na.omit(sp.set.tkt[q.ret.rf]),by=c('q.id','Stock'))

cache('market.list')
cache('market.set')

#s.prices.close <- setkey(melt(setnames(data.table(read.csv2('data/unadj.price.csv',skip=4,sep=';'))[,Code:=as.Date(Code,format('%d-%m-%Y'))],c('Date',s.id)),id.vars = 'Date',variable.name = 'Stock',value.name = 'priceClose')[,Stock:=as.character(Stock)],Date,Stock)


#all.prices <- setkey(s.prices[s.prices.close],Stock)
#load('data/adj.prices.v5.RData')
#yahoo.p <- setkey(melt(data.table(Date=index(p),coredata(p)),id.vars = 'Date',variable.name = 'Stock',value.name = 'yahoo')[,yahooPT:=yahoo[c(rep(NA,3),1:(.N-3))],by=Stock][,Stock:=as.character(Stock)],Date,Stock)[Stock=='AAPL',':='(yahoo=yahoo*7,yahooPT=yahooPT*7)]

### DS prices
#all.prices <- setkey(melt(setnames(data.table(read.csv2('data/prices02.csv',skip=4,sep=';'))[,Code:=as.Date(Code,format('%d-%m-%Y'))],c('Date',s.id)),id.vars = 'Date',variable.name = 'Stock',value.name = 'priceAdj')[,pricePT:=priceAdj[c(rep(NA,3),1:(.N-3))],by=Stock][,Stock:=as.character(Stock)],Date,Stock)[Stock=='AAPL',':='(priceAdj=priceAdj*7,pricePT=pricePT*7)]

### Subs 0.02 DS prices with yahoo
#all.prices <- na.omit(setkey(melt(setnames(data.table(read.csv2('data/prices02.csv',skip=4,sep=';'))[,Code:=as.Date(Code,format('%d-%m-%Y'))],c('Date',s.id)),id.vars = 'Date',variable.name = 'Stock',value.name = 'priceAdj')[,pricePT:=priceAdj[c(rep(NA,3),1:(.N-3))],by=Stock][,Stock:=as.character(Stock)],Date,Stock)[Stock=='AAPL',':='(priceAdj=priceAdj*7,pricePT=pricePT*7)][setkey(melt(data.table(Date=index(p),coredata(p)),id.vars = 'Date',variable.name = 'Stock',value.name = 'yahoo')[,yahooPT:=yahoo[c(rep(NA,3),1:(.N-3))],by=Stock][,Stock:=as.character(Stock)],Date,Stock)[Stock=='AAPL',':='(yahoo=yahoo*7,yahooPT=yahooPT*7)]][,priceAdj:=ifelse(priceAdj==0.02,yahoo,priceAdj)][,pricePT:=ifelse(pricePT==0.02,yahooPT,pricePT)])

### Yahoo prices
#all.prices <- setkey(melt(data.table(Date=index(p),coredata(p)),id.vars = 'Date',variable.name = 'Stock',value.name = 'priceAdj')[,pricePT:=priceAdj[c(rep(NA,3),1:(.N-3))],by=Stock][,Stock:=as.character(Stock)],Date,Stock)[Stock=='AAPL',':='(priceAdj=priceAdj*7,pricePT=pricePT*7)]

#pt.prices[,fix.p:=ifelse(priceAdj==0.02,yahoo,priceAdj)]
#
# setkey(pt.dt,Date,Stock)
# setkey(all.prices[,Stock:=as.character(Stock)],Date,Stock)
# pt.prices <- na.omit(all.prices[yahoo.p])

load(file='data/missing.brokers.RData')

temp.pt.dt <- setnames(unique(na.omit(na.omit(setnames(rbindlist(lapply(c('Consumer Discretionary','Consumer Staples','Energy','Financials','Health Care','Industrials','IT','Materials','Telecom Services','Utilities'),function(sec){stocks <- list.files(paste(data.stock.dir(sec),'Price Targets',sep='/'));dt <- lapply(stocks,function(i){stock.dt <- data.table(read.csv(paste(data.stock.dir(sec),'Price Targets',i,sep='/')));stock.dt[,Stock:=strsplit(i,'.csv')]});sec.dt <- rbindlist(dt);sec.dt[,Sector:=sec]}))[,Date:=as.Date(Date,format('%m/%d/%Y'))][,Broker:=as.character(Broker)],'Recommendation','PT'))[,Broker.new:=ifelse(is.na(miss.brok[pmatch(Broker,miss.brok[,1],duplicates.ok=T),2]),Broker,miss.brok[pmatch(Broker,miss.brok[,1],duplicates.ok=T),2])][,Broker.clean:=ifelse(length(strsplit(Broker.new,split='[[:space:] \\| [:punct:]]')[[1]])>1L,NA_character_,Broker.new ),by=Broker.new]))[,':='(Broker=NULL,Broker.new=NULL,q.id=as.yearqtr(Date))],'Broker.clean','Broker')

#test.pt <- na.omit(rbindlist(lapply(c('Consumer Discretionary','Consumer Staples','Energy','Financials','Health Care','Industrials','IT','Materials','Telecom Services','Utilities'),function(sec){stocks <- list.files(paste(data.stock.dir(sec),'Price Targets',sep='/'));dt <- lapply(stocks,function(i){stock.dt <- data.table(read.csv(paste(data.stock.dir(sec),'Price Targets',i,sep='/')));stock.dt[,Stock:=strsplit(i,'.csv')]});sec.dt <- rbindlist(dt);sec.dt[,Sector:=sec]}))[,Date:=as.Date(Date,format('%m/%d/%Y'))][,Broker:=as.character(Broker)])[,year:=format(Date,'%Y')]

### Price target ajustments for stocks that had a stock split since 2010
adj.factor <- setkey(na.omit(ds.af.all.stock[year>2010&af!=1][,prod(unique(af)),by=list(Stock)]),Stock)

pt.dt <- unique(na.omit(setnames(merge(setkey(temp.pt.dt,Stock),adj.factor,allow.cartesian=T,all=T)[,new.PT:=PT*V1][,new.PT:=ifelse(is.na(new.PT),PT,new.PT)][,c(colnames(temp.pt.dt),'new.PT'),with=F],c('PT','new.PT'),c('old.PT','PT'))),by=c('Date','Broker','Stock'))[,b.id:=.I][Stock=='SAPE'&Broker=='HCWAIN',PT:=PT*prod(unique(ds.af.all.stock[Stock=='SAPE'],by='af')[,af])][Stock=='TIBB',PT:=PT*unique(ds.af.all.stock[Stock=='TIBB'],by='af')[8,af]][Stock=='PCBC',PT:=PT*unique(ds.af.all.stock[Stock=='PCBC'],by='af')[5,af]][Stock=='DSCO',PT:=PT*unique(ds.af.all.stock[Stock=='DSCO'],by='af')[2,af]]

#setkey(pt.dt,q.id,Stock)
#pt.dt[,b.id:=.I]
### Get Stock prices from IBES (quarterly prices)
# price.tmp <- lapply(sectors,function(sec)
# {
#     p.data <- data.table(read.csv2(paste(data.stock.dir(sec),'Acc.Variables/stock.prices.csv',sep='/'),sep=';'))
#   p.dt <- melt(p.data[,q.id:=.I],id.var='q.id',variable.name='Stock',value.name='Price')
#   p.dt[,Sector:=sec]
# })
# price.q <- na.omit(rbindlist(price.tmp))
#
# ### Add quarter id as yearqtr
# quarters <- seq(as.Date('1989-04-01'),as.Date('2010-04-01'),by='3 month')-1
# price.q[,qtr:=as.yearqtr(quarters)[q.id]]
# setkey(price.q,qtr,Stock)
#
# #ggplot(price.q,aes(x=q.id,y=Price,group=q.id))+geom_boxplot()
#
# pt.with.p <- na.omit(price.q[pt.dt[,list(q.id,Stock,Broker,PT,Date)]])
# pt.with.p[,tper:=PT/Price-1]
#ggplot(pt.with.p,aes(x=Date,y=tper,group=Date))+geom_boxplot()
#ggplot(pt.dt,aes(x=Date,y=PT,group=Date))+geom_boxplot()

###Data.table of daily stock prices (stocks from SP500)----
#save(prices.pt,file=project.dir('Price Targets/stock.pt.dt.RData'))

###Calculating the daily FEs of brokers----

###Assuming PT is valid for one year,
setkey(pt.dt,b.id)
setkey(all.stock.prices,Date,Stock)
#source("http://bioconductor.org/biocLite.R")
#biocLite("IRanges")
require(IRanges)
qtree <- IntervalTree(IRanges(start=as.numeric(pt.dt$Date),width=365L))
stree <- IntervalTree(IRanges(as.numeric(unique(all.stock.prices$Date)),width=1L))
overlaps=findOverlaps(qtree,stree)
#ext.days <- data.table(b.id=queryHits(findOverlaps(qtree,stree)),s.Date=as.Date(unique(all.prices$Date)[subjectHits(findOverlaps(qtree,stree))],origin='1970-01-01'),key='b.id')

###daily stats (fe, pmafe, rank)
#abs(PT-mean(pricesAdj))
### meanQprice - average stock price for the quarter
### b.meanPrice - average stock price SINCE the PT report
ext.d <- data.table(b.id=queryHits(overlaps),s.Date=unique(all.stock.prices$Date)[subjectHits(overlaps)],key='b.id')
#test.s <- unique(na.omit(setkey(pt.dt[ext.d],s.Date,Stock)[all.prices]),by=c('Broker','s.Date','priceAdj'),fromLast = T)


#test.full <- test.s[,q.id:=as.yearqtr(s.Date)][,meanQprice:=unique(.SD,by=c('s.Date'))[,mean(priceAdj)],by=list(q.id,Stock)][,b.meanPrice:=mean(priceAdj),by=list(q.id,Stock,Broker)][,':='(meanQprice.fe=abs(PT-meanQprice),b.meanPrice.fe=abs(PT-b.meanPrice),d.fe=abs(PT-priceAdj))][,b.coefVar:=sd(d.fe)/mean(d.fe), by=list(q.id,Stock,Broker)][,':='(pmafe=d.fe/mean(d.fe),s.coefVar=sd(d.fe)/mean(d.fe)),by=list(q.id,Stock)][,score:=mean(pmafe),by=list(q.id,Stock,Broker)][,b.view:=first(PT/pricePT-1),by=list(Date,Stock,Broker)][,meanTper:=mean(b.view),by=list(q.id,Stock)]

full.set <- unique(na.omit(setkey(pt.dt[ext.d],s.Date,Stock)[all.stock.prices,allow.cartesian=T]),by=c('s.Date','Broker','priceAdj'),fromLast = T)[,q.id:=as.yearqtr(s.Date)][,meanQprice:=unique(.SD,by=c('s.Date'))[,mean(priceAdj)],by=list(q.id,Stock)][,b.meanPrice:=mean(priceAdj),by=list(q.id,Stock,Broker)][,':='(meanQprice.fe=abs(PT-meanQprice),b.meanPrice.fe=abs(PT-b.meanPrice),d.fe=abs(PT-priceAdj))][,b.coefVar:=sd(d.fe)/mean(d.fe), by=list(q.id,Stock,Broker)][,':='(pmafe=d.fe/mean(d.fe),s.coefVar=sd(d.fe)/mean(d.fe)),by=list(q.id,Stock)][,score:=mean(pmafe),by=list(q.id,Stock,Broker)][,b.view:=first(PT/pricePT-1),by=list(Date,Stock,Broker)][,meanTper:=mean(b.view),by=list(q.id,Stock)]



q.data <- unique(full.set,by=c('q.id','Stock','Broker'),fromLast = T)

q.days <- full.set[,s.Date[endpoints(s.Date,on='quarters')],by=Stock]
setkey(q.days,Stock,V1)
setkey(full.set,Stock,s.Date)
end.q.data <- full.set[q.days]

#ext.pt <- unique(q.data,by=c('b.id'))[,cbind(.SD,as.yearqtr(q.id+seq(0.25,1,0.25))),by=list(Sector,Broker,q.id,Stock),.SDcols='b.view']

#end.q.ext.pt <- unique(end.q.data,by=c('b.id'))[,cbind(.SD,as.yearqtr(q.id+seq(0.25,1,0.25))),by=list(Sector,Broker,q.id,Stock),.SDcols='b.view']


#cache('ext.pt')
#cache('end.q.ext.pt')
cache('q.data')
cache('end.q.data')
example.dt <- full.set[Stock=='AMZN'&q.id=='1999 Q2'][order(score)][,list(s.Date,PT,Broker,priceAdj)]
cache('example.dt')
#example.dt$Broker <- factor(example.dt$Broker,levels=unique(example.dt$Broker))
#ggplot(example.dt,aes(x=s.Date,y=PT,group=Broker,color=Broker))+geom_line(size=1)+theme_bw()+ylab('Dollars')+xlab('1999Q2')+ggtitle('Example of daily stock price (AMZN),compared to broker\'s price target (ranked by fe)')+geom_line(aes(y=priceAdj),color='black')

#ggsave('graphs/accurateBroke.pdf',w=11,h=8)
###subset of core stocks/broker: min. 3 broker in quarter and min. 12 quarters of coverage
# setkey(market.set,q.num)
# q.data[,core.b:=.N>11,by=list(Stock,Broker)]
# core.dt <- q.data[(core.b)][,rank:=rank(score),by=list(q.id,Stock)]
# core.dt[,clean.s:=.N>2,by=list(q.id,Stock)]
# ###core rankings
# pt.new <- acast(core.dt[(clean.s)],q.id~Broker~Stock,value.var='rank')
# ##baselines
# baseline.rankings <- baseline.rankings.f(pt.new,1)

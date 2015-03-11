###This script for data.table excercise
# getStockData <- function(index,tmp,skip,ref.m,val.name,date.format){
#   data <- tmp[-skip,.SD,.SDcols=c(1,index)]
# setnames(data,c('Date',ref.m))
# data[,Date:=as.Date(Date,format=date.format)]
# melt(data,id.vars='Date',variable.name='Stock',value.name=val.name)
# }

#setwd('~/Dropbox/workspace/Projects/Black-Litterman/BL-strategies')
#source("lib/my.paths.R")


#lapply(list('xts','reshape2','data.table','plyr','stringr'),function(X) {do.call("require", list(X)) })


stock.ref <- setkey(unique(rbindlist(lapply(list.files('~/Dropbox/Datastream/AllStocks/names/',pattern='name.ds.*.csv'),function(i){fread(paste('~/Dropbox/Datastream/AllStocks/names/',i,sep=''),header=T,na.strings = '')})),by='DSCD')[,Stock:=WC05601],DSCD,Stock)

load('~/Dropbox/workspace/Projects/BL-strategies/data/ref.matrix.RData')
#make DT of it
ref.dt <- setkey(data.table(Stock=ref.matrix[,1],DSCD=rownames(ref.matrix)),DSCD)
sp.id <- setkey(setnames(fread('~/Dropbox/workspace/Projects/BL-strategies/data/sp.ids.new.csv',header=F),'DSCD'),DSCD)
miss.tkt <- c('AMP','ANV','ATCH','ACS','ASND','BT','HPH','MEYR','MWI','MII','RN','UCC')
miss.dt <- data.table(Stock=miss.tkt,DSCD=ref.dt[sp.id][which(is.na(Stock))][,DSCD])
ref.dt <- setkey(rbind(ref.dt,miss.dt)[,Stock:=as.character(Stock)][which(duplicated(rbind(ref.dt,miss.dt)[,Stock])),Stock:=paste(Stock,'1',sep='.')],DSCD,Stock)

full.stock.ref <- setkey(unique(merge(stock.ref,ref.dt,all=T),by=c('DSCD','Stock')),DSCD,Stock)
s.id <- setkey(full.stock.ref[ref.dt][,list(DSCD,NAME,Stock)],DSCD)

###all stocks to used for variables
#all.stock.prices <- na.omit(setkey(setnames(unique(data.collect('p',1),by=c('Date','DSCD')),'p','priceAdj')[,pricePT:=c(rep(NA,3),head(priceAdj,-3)),by=DSCD],DSCD)[setkey(full.stock.ref,DSCD),allow.cartesian=T])[,list(Date,Stock,DSCD,NAME,priceAdj,pricePT)]

### SP stock daily prices (used for BL model)
all.prices <- setkey(setnames(unique(data.collect('p',1),by=c('Date','DSCD')),'p','priceAdj'),DSCD)[s.id][,q.id:=as.yearqtr(Date)]

#q.ret.sp <- all.prices[,quarterlyReturn(xts(priceAdj,order.by=Date)),by=.(Stock,Quarters)]
q.ret.sp<-all.prices[,q.ret:=(log(last(priceAdj))-log(first(priceAdj))),by=.(Stock,q.id)][,.(q.id,DSCD,Stock,q.ret)]
d.ret.sp <- all.prices[,d.ret:=c(NA,diff(log(priceAdj))),by=Stock][,.(Date,DSCD,Stock,d.ret)]


### SP stock daily returns 
#sp.ret <- setkey(unique(data.collect('ret',3),by=c('Date','DSCD')),DSCD)[s.id]

#set(sp.ret, which(sp.ret[[3]]==0), 3L, value=NA)




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

sp.set.tkt <- setkey(na.omit(s.id[,list(DSCD,Stock)][setkey(data.table(setkey(melt(setnames(data.table(read.csv('~/Dropbox/workspace/Projects/BL-strategies/data/sp.500.complete.csv',header=T,sep=',',dec='.',skip=1,na.strings=''))[,.SD,.SDcols=seq(1,176,4)],as.character(as.yearqtr(quarters))),measure.vars = as.character(as.yearqtr(quarters)),variable.name='Quarters',value.name='DSCD'),Quarters),setkey(melt(setnames(data.table(read.csv('~/Dropbox/workspace/Projects/BL-strategies/data/sp.500.complete.csv',header=T,sep=',',dec='.',skip=1,na.strings=''))[,.SD,.SDcols=seq(4,176,4)],as.character(as.yearqtr(quarters))),measure.vars = as.character(as.yearqtr(quarters)),variable.name='Quarters',value.name='value'),Quarters))[,q.num:=.GRP,by=Quarters][,Quarters:=NULL],DSCD)][,list(DSCD,Stock,Quarters,mv=value)][,q.id:=as.yearqtr(Quarters)]),q.id)[,q.num:=.GRP,by=q.id][,mw:=mv/sum(mv,na.rm=T),by=q.num]



require('BurStFin')
market.list<-mclapply(1:44,function(i)
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
},mc.cores=cores)


setkey(sp.set.tkt,q.id,DSCD,Stock)
setkey(q.ret.rf,q.id,DSCD,Stock)
market.set <- unique(na.omit(sp.set.tkt[q.ret.rf]),by=c('q.id','Stock'))

cache('market.list')
cache('market.set')
rm('stock.ref','ref.matrix','sp.id','miss.tkt','miss.dt','ref.dt','full.stock.ref','s.id','all.prices','q.ret.sp','d.ret.sp','tbil.data','q.rf','q.ret.rf','s.ex.ret','quarters','sp.set.tkt')
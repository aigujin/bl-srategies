###This script for data.table excercise

setwd('~/Dropbox/workspace/Projects/BL-strategies')
source("lib/my.paths.R")

lapply(list('xts','reshape2','data.table','plyr','stringr'),function(X) {do.call("require", list(X)) })


stock.ref <- setkey(unique(rbindlist(lapply(list.files('~/Dropbox/Datastream/AllStocks/names/',pattern='name.ds.*.csv'),function(i){fread(paste('~/Dropbox/Datastream/AllStocks/names/',i,sep=''),header=T,na.strings = '')})),by='DSCD')[,Stock:=WC05601],DSCD,Stock)

load('data/ref.matrix.RData')
load(file='data/missing.brokers.RData')

#make DT of it
ref.dt <- setkey(data.table(Stock=ref.matrix[,1],DSCD=rownames(ref.matrix)),DSCD)
sp.id <- setkey(setnames(fread('data/sp.ids.new.csv',header=F),'DSCD'),DSCD)
miss.tkt <- c('AMP','ANV','ATCH','ACS','ASND','BT','HPH','MEYR','MWI','MII','RN','UCC')
miss.dt <- data.table(Stock=miss.tkt,DSCD=ref.dt[sp.id][which(is.na(Stock))][,DSCD])
ref.dt <- setkey(rbind(ref.dt,miss.dt)[,Stock:=as.character(Stock)][which(duplicated(rbind(ref.dt,miss.dt)[,Stock])),Stock:=paste(Stock,'1',sep='.')],DSCD,Stock)

full.stock.ref <- setkey(unique(merge(stock.ref,ref.dt,all=T),by=c('DSCD','Stock')),DSCD,Stock)
s.id <- setkey(full.stock.ref[ref.dt][,list(DSCD,NAME,Stock)],DSCD)

###all stocks to used for variables and PT
all.stock.prices <- na.omit(setkey(setnames(unique(data.collect('p',1),by=c('Date','DSCD')),'p','priceAdj')[,pricePT:=c(rep(NA,3),head(priceAdj,-3)),by=DSCD],DSCD)[setkey(full.stock.ref,DSCD),allow.cartesian=T])[,list(Date,Stock,DSCD,NAME,priceAdj,pricePT)]

### Adustment factor (AF) for all stock
ds.af.all.stock <- full.stock.ref[setkey(unique(af.data.collect('af',1),by=c('Date','DSCD'))[,':='(year=format(Date,'%Y'),af=as.numeric(af),Time=Date)][,Date:=NULL],DSCD),allow.cartesian=T][,list(Stock,DSCD,af,year)]


temp.pt.dt <- setnames(unique(na.omit(na.omit(setnames(rbindlist(lapply(c('Consumer Discretionary','Consumer Staples','Energy','Financials','Health Care','Industrials','IT','Materials','Telecom Services','Utilities'),function(sec){stocks <- list.files(paste(data.stock.dir(sec),'Price Targets',sep='/'));dt <- lapply(stocks,function(i){stock.dt <- data.table(read.csv(paste(data.stock.dir(sec),'Price Targets',i,sep='/')));stock.dt[,Stock:=strsplit(i,'.csv')]});sec.dt <- rbindlist(dt);sec.dt[,Sector:=sec]}))[,Date:=as.Date(Date,format('%m/%d/%Y'))][,Broker:=as.character(Broker)],'Recommendation','PT'))[,Broker.new:=ifelse(is.na(miss.brok[pmatch(Broker,miss.brok[,1],duplicates.ok=T),2]),Broker,miss.brok[pmatch(Broker,miss.brok[,1],duplicates.ok=T),2])][,Broker.clean:=ifelse(length(strsplit(Broker.new,split='[[:space:] \\| [:punct:]]')[[1]])>1L,NA_character_,Broker.new ),by=Broker.new]))[,':='(Broker=NULL,Broker.new=NULL,q.id=as.yearqtr(Date))],'Broker.clean','Broker')


### Price target ajustments for stocks that had a stock split since 2010
adj.factor <- setkey(na.omit(ds.af.all.stock[year>2010&af!=1][,prod(unique(af)),by=list(Stock)]),Stock)

pt.dt <- unique(na.omit(setnames(merge(setkey(temp.pt.dt,Stock),adj.factor,allow.cartesian=T,all=T)[,new.PT:=PT*V1][,new.PT:=ifelse(is.na(new.PT),PT,new.PT)][,c(colnames(temp.pt.dt),'new.PT'),with=F],c('PT','new.PT'),c('old.PT','PT'))),by=c('Date','Broker','Stock'))[,b.id:=.I][Stock=='SAPE'&Broker=='HCWAIN',PT:=PT*prod(unique(ds.af.all.stock[Stock=='SAPE'],by='af')[,af])][Stock=='TIBB',PT:=PT*unique(ds.af.all.stock[Stock=='TIBB'],by='af')[8,af]][Stock=='PCBC',PT:=PT*unique(ds.af.all.stock[Stock=='PCBC'],by='af')[5,af]][Stock=='DSCO',PT:=PT*unique(ds.af.all.stock[Stock=='DSCO'],by='af')[2,af]]

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

###daily stats (fe, pmafe, rank)
#abs(PT-mean(pricesAdj))
### meanQprice - average stock price for the quarter
### b.meanPrice - average stock price SINCE the PT report
ext.d <- data.table(b.id=queryHits(overlaps),s.Date=unique(all.stock.prices$Date)[subjectHits(overlaps)],key='b.id')

full.set <- unique(na.omit(setkey(pt.dt[ext.d],s.Date,Stock)[all.stock.prices,allow.cartesian=T]),by=c('s.Date','Broker','priceAdj'),fromLast = T)[,q.id:=as.yearqtr(s.Date)][,meanQprice:=unique(.SD,by=c('s.Date'))[,mean(priceAdj)],by=list(q.id,Stock)][,b.meanPrice:=mean(priceAdj),by=list(q.id,Stock,Broker)][,':='(meanQprice.fe=abs(PT-meanQprice),b.meanPrice.fe=abs(PT-b.meanPrice),d.fe=abs(PT-priceAdj))][,b.coefVar:=sd(d.fe)/mean(d.fe), by=list(q.id,Stock,Broker)][,':='(pmafe=d.fe/mean(d.fe),s.coefVar=sd(d.fe)/mean(d.fe)),by=list(q.id,Stock)][,score:=mean(pmafe),by=list(q.id,Stock,Broker)][,b.view:=first(PT/pricePT-1),by=list(Date,Stock,Broker)][,meanTper:=mean(b.view),by=list(q.id,Stock)]



q.data <- unique(full.set,by=c('q.id','Stock','Broker'),fromLast = T)

q.days <- full.set[,s.Date[endpoints(s.Date,on='quarters')],by=Stock]
setkey(q.days,Stock,V1)
setkey(full.set,Stock,s.Date)
#end.q.data <- full.set[q.days]

#cache('q.data')
#cache('end.q.data')
example.dt <- full.set[Stock=='AMZN'&q.id=='1999 Q2'][order(score)][,list(s.Date,PT,Broker,priceAdj)]
#cache('example.dt')

rm('stock.ref','sp.id','miss.tkt','miss.dt','ref.dt','full.stock.ref','s.id','all.stock.prices','ds.af.all.stock','temp.pt.dt','adj.factor','pt.dt','qtree','stree','overlaps','ext.d','full.set','miss.brok','q.days','ref.matrix','long.name.sec')

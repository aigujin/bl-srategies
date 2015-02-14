
views.Qs <- aaply(all.rankings,5,bl.script.f,ret=data,baselines=baselines,t=dim(all.rankings)[1],.drop=F)
###Adding Qs from tper-based method
both.s <- intersect(findIntersect(meanTper,norm.conf,2),dimnames(views.Qs)[[3]])
all.Qs <- abind(views.Qs[1,drop=F,,both.s,],non.rank=aperm(replicate(1,replicate(7,meanTper[1:dim(views.Qs)[2],both.s,drop=F])),c(4,1,2,3)),along=1)
views.omegas <- aaply(accuracy,4,buildOmega,.drop=F)
tper.all.conf <- buildOmega(aperm(replicate(1,norm.conf),c(3,1,2)))

all.conf <- abind(views.omegas[,,,both.s,],aperm(replicate(7,tper.all.conf[1:dim(views.Qs)[2],,drop=F,both.s,]),c(2,1,5,3,4)),along=1)
dimnames(all.conf)[[1]] <- dimnames(all.Qs)[[1]]

bl.period <- 4:dim(all.Qs)[2]
m.period <-(length(market.inputs)-length(bl.period)+1) : length(market.inputs)

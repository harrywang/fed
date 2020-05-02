
d.combo.public <- read.csv(file=paste(path,"d.combo.public.csv",sep="") )
d <- d.combo.public 

# Mhobs := number of impressions per placement
# mhobs := number of impressions per placement-creative pair (532 uniques)
# yhobs := number of conversions per placement-creative pair (532 uniques)

## Recall: 'h' indicates incremental counts per period and 't' is cumulative counts
# e.g., m mhobs.combo 

# For batching at weekly level
Mhobs.combo <- read.csv(file=paste(path,"capMhobs.combo.csv",sep="") )
mhobs.combo <- read.csv(file=paste(path,"mhobs.combo.csv",sep="") )
yhobs.combo <- read.csv(file=paste(path,"yhobs.combo.csv",sep="") )

# # For batching at daily level
# Mhobs.combo <- read.csv( file=paste(path,"capMhobs.combo.daily.csv",sep="") )
# mhobs.combo <- read.csv( file=paste(path,"mhobs.combo.daily.csv",sep="") )
# yhobs.combo <- read.csv( file=paste(path,"yhobs.combo.daily.csv",sep="") )



hend <- ncol(Mhobs.combo) # number of time periods

K <- 4 # number of unique ad creatives per placement

allplacements <- d$placement
n.placements <- length(unique(allplacements))

J <- n.placements # number of placements: 133


mtotalvec <- mtobs.combo[,hend]
muobsagg <- sum(ytobs.combo[,hend])/sum(mtobs.combo[,hend])

muobsvec <- ytobs.combo[,hend]/mtobs.combo[,hend] 
muobsJbyK <- matrix(muobsvec, ncol=4, byrow=T)

yobs_byplace <- tapply(ytobs.combo[,hend],IND=d$placeid,FUN=sum)
mobs_byplace <- tapply(mtobs.combo[,hend],IND=d$placeid,FUN=sum)
muobs_byplace <- yobs_byplace/mobs_byplace


# set truth for data-generating process
mutruevec <- muobsvec
mutruemat <- matrix(muobsvec, nrow=numobs, ncol=hend, byrow=F)



## ==== SUMMARIZE RAW DATA ======

# cumulative conversions
#ytobs have cumulative conversions
#mtobs have cumulative impressions

yaggovertimecum.combo <- apply(ytobs.combo,2,sum) 
maggovertimecum.combo <- apply(mtobs.combo, 2, sum) 
paggovertimecum.combo <- yaggovertimecum.combo/maggovertimecum.combo



## Allocation over time
## Impressions by concept over time
impvol_concept_by_time.combo <- cbind(
  apply( mhobs.combo[d$concept==levels(d$concept)[1],], 2, sum),
  apply( mhobs.combo[d$concept==levels(d$concept)[2],], 2, sum),
  apply( mhobs.combo[d$concept==levels(d$concept)[4],], 2, sum),
  apply( mhobs.combo[d$concept==levels(d$concept)[4],], 2, sum)
)
maggovertime.combo <- apply(mhobs.combo, 2, sum)
impalloc_concept_by_time.combo <- cbind(
  maggovertime.combo,
  impvol_concept_by_time/maggovertime.combo
  )



windows()
ylimcur <- 1e6*range(paggovertimecum.test,paggovertimecum.control)
plot( 1:hend, 1e6*paggovertimecum.control, type="n",
      ylim=ylimcur, frame.plot=F,axes=F,ylab="",xlab="",main="")
axis(1,1:hend)
axis(2)
lines(1:hend, 1e6*paggovertimecum.control, lty=2, lwd=1)
lines(1:hend, 1e6*paggovertimecum.test, lty=1, lwd=2)
points(1:hend, 1e6*paggovertimecum.control, pch=15)
points(1:hend, 1e6*paggovertimecum.test, pch=16)
title(xlab="time periods", ylab="conversion rate (per million)")

      
# change relative to starting points
paggovertimecumrel.control <- 100*(paggovertimecum.control-paggovertimecum.control[1])/paggovertimecum.control[1]
paggovertimecumrel.test <- 100*(paggovertimecum.test-paggovertimecum.test[1])/paggovertimecum.test[1]


windows()
ylimcur <- range(paggovertimecumrel.test,paggovertimecumrel.control)
plot( 1:hend, paggovertimecumrel.control, type="n",
      ylim=ylimcur, frame.plot=F,axes=F,ylab="",xlab="",main="")
axis(1,1:hend)
axis(2)
lines(1:hend, paggovertimecumrel.control, lty=2, lwd=1)
lines(1:hend, paggovertimecumrel.test, lty=1, lwd=2)
points(1:hend, paggovertimecumrel.control, pch=15)
points(1:hend, paggovertimecumrel.test, pch=16)
title(xlab="time periods", ylab="relative change (as % over starting conversion rate)")


# impressions over time and absolute conversion rates
windows()
xscale <- cumsum(maggovertime.combo)/1e6
ylimcur <- 1e6*range(paggovertimecum.test,paggovertimecum.control)
plot( xscale, 1e6*paggovertimecum.control, type="l",col="white",
      ylim=ylimcur, frame.plot=F,axes=F,ylab="",xlab="",main="")
axis(1,xscale, at=seq(0,900,by=100), labels=seq(0,900,by=100) )
axis(2)
lines(xscale, 1e6*paggovertimecum.control, lty=2, lwd=1)
lines(xscale, 1e6*paggovertimecum.test, lty=1, lwd=2)
points(xscale, 1e6*paggovertimecum.control, pch=15)
points(xscale, 1e6*paggovertimecum.test, pch=16)
title(xlab="impressions over time (millions)", ylab="conversion rate (per million)")

# impressions over time and relative conversion rates
windows()
xscale <- cumsum(maggovertime.combo)/1e6
ylimcur <- range(paggovertimecumrel.test,paggovertimecumrel.control)
plot( xscale, paggovertimecumrel.control, type="l",col="white",
      ylim=ylimcur, frame.plot=F,axes=F,ylab="",xlab="",main="")
axis(1,xscale, at=seq(0,900,by=100), labels=seq(0,900,by=100) )
axis(2)
lines(xscale, paggovertimecumrel.control, lty=2, lwd=1)
lines(xscale, paggovertimecumrel.test, lty=1, lwd=2)
points(xscale, paggovertimecumrel.control, pch=15)
points(xscale, paggovertimecumrel.test, pch=16)
title(xlab="impressions over time (millions)", ylab="relative change (as % over starting conversion rate)")

     
# change relative to starting points
paggovertimeincrel.control <- 100*(paggovertime.control-paggovertime.control[1])/paggovertime.control[1]
paggovertimeincrel.test <- 100*(paggovertime.test-paggovertime.test[1])/paggovertime.test[1]

# NOT CUMULATIVE impressions over time and relative conversion rates
windows()
xscale <- cumsum(maggovertime.combo)/1e6
ylimcur <- range(paggovertimeincrel.test,paggovertimeincrel.control)
plot( xscale, paggovertimeincrel.control, type="l",col="white",
      ylim=ylimcur, frame.plot=F,axes=F,ylab="",xlab="",main="")
axis(1,xscale, at=seq(0,900,by=100), labels=seq(0,900,by=100) )
axis(2)
lines(xscale, paggovertimeincrel.control, lty=2, lwd=1)
lines(xscale, paggovertimeincrel.test, lty=1, lwd=2)
points(xscale, paggovertimeincrel.control, pch=15)
points(xscale, paggovertimeincrel.test, pch=16)
title(xlab="impressions over time (millions)", ylab="relative change (as % over starting conversion rate)")



#====== plot distribution of conversion rates ====

summary(muobsvec)

numobs <- nrow(d)
sum(muobsvec == 0)
sum(muobsvec < 1e-6)
sum(muobsvec < 1e-5)
sum(muobsvec < 1e-4)
sum(muobsvec == 0)/numobs
sum(muobsvec < 1e-6)/numobs
sum(muobsvec < 1e-5)/numobs
sum(muobsvec < 1e-4)/numobs

cvr1 <- muobsvec[muobsvec<1e-5]
cvr2 <- muobsvec[muobsvec >= 1e-5]

hist(muobsvec[muobsvec<1e-5 & muobsvec>0])

### --- FIX THIS AND RESAVE IT AS 10e-5 correctly as 1/100,000
pdf(file=paste(path,"cvrhist.pdf",sep=""),w=7,h=4)
  par(mfrow=c(1,2))
  hist(cvr1, main="Rates less than 1/100,000",xlab="Obs. conversion rate")
  hist(cvr2, main="Rates greater than 1/100,000",xlab="Obs. conversion rate",ylab="")
dev.off()


pdf(file=paste(path,"cvrbyimp.pdf",sep=""))
  plot(mtotalvec,muobsvec,log="xy",ylim=c(2e-7,max(muobsvec)), axes=F,ylab="",xlab="")
  title(xlab="Total impressions (powers of 10)", ylab="Obs. conversion rate (powers of 10)")
  axis(1,at=10^(1:8),labels=1:8)
  axis(2,at=10^(-7:-3),labels=-7:-3)
  abline(h=muobsagg,col="darkgray",lty=2)
  points(mtotalvec,muobsvec)
  points(mtotalvec[muobsvec==0], rep(2e-7,sum(muobsvec==0)) , pch=2)
  abline(h=3e-7)
dev.off()

pdf(file=paste(path,"cvrbyimp_byplace.pdf",sep=""))
  plot(mobs_byplace,muobs_byplace,log="xy",ylim=c(2e-7,max(muobs_byplace)), axes=F,ylab="",xlab="")
  title(xlab="Total impressions (powers of 10)", ylab="Obs. conversion rate (powers of 10)")
  axis(1,at=10^(1:8),labels=1:8)
  axis(2,at=10^(-7:-3),labels=-7:-3)
  abline(h=muobsagg,col="darkgray",lty=2)
  points(mobs_byplace,muobs_byplace)
  points(mobs_byplace[muobs_byplace==0], rep(2e-7,sum(muobs_byplace==0)) , pch=2)
  abline(h=3e-7)
dev.off()


## clicks #######
cor.test(dall$conversions/dall$impressions, dall$clicks/dall$impressions)

conversions.combo <- (dall$conversions[indTest] + dall$conversions[indControl])
impressions.combo <- (dall$impressions[indTest] + dall$impressions[indControl])
clicks.combo <- (dall$clicks[indTest] + dall$clicks[indControl])
cvr.combo <- conversions.combo/impressions.combo
ctr.combo <- clicks.combo/impressions.combo

cor.test(cvr.combo,ctr.combo)


pdf(file=paste(path,"ctrhist.pdf",sep=""),w=7,h=4)
  par(mfrow=c(1,2))
  ctr1 <- ctr.combo[ctr.combo<1e-3]
  ctr2 <- ctr.combo[ctr.combo >= 1e-3] 
  hist(ctr1, main="Rates less than 1/1,000",xlab="Obs. click through rate")
  hist(ctr2, main="Rates greater than 1/1,000",xlab="Obs. click through rate",ylab="")
dev.off()

pdf(file=paste(path,"ctrbyimp.pdf",sep=""))
 plot(mtotalvec,ctr.combo,log="xy",ylim=c(1e-5,max(ctr.combo)), axes=F,ylab="",xlab="")
  title(xlab="Total impressions (powers of 10)", ylab="Obs. click through rate (powers of 10)")
  axis(1,at=10^(1:8),labels=1:8)
  axis(2,at=10^(-7:-1),labels=-7:-1)
  abline(h=muobsagg,col="darkgray",lty=2)
  points(mtotalvec,ctr.combo)
dev.off()



pdf(file=paste(path,"cvrbyctr.pdf",sep=""))
  plot( cvr.combo, ctr.combo,
    xlim=range(1e-7,5e-4), ylim=c(1e-5,5e-3),log="xy", axes=F, ylab="",xlab="")
  title(xlab="Obs. conversion rate (powers of 10)", ylab="Obs. click through rate (powers of 10)")
  axis(1,at=10^(-7:-1),labels=-7:-1)
  axis(2,at=10^(-7:-1),labels=-7:-1)
  points( rep(1e-7,sum(cvr.combo==0)), ctr.combo[cvr.combo==0], pch=2)
dev.off()




##=============================
##
## show uncertainty around actual outcomes
# path of cumulative conversions obtained over time
# save each "policy" result as a matrix of G worlds by T time


# data = Test, Control, Combined

obspropvec.control <- ytobs[indControl,hend]/mtobs[indControl,hend] 
obspropvec.test <- ytobs[indTest,hend]/mtobs[indTest,hend]
obspropvec.combo <- ytobs.combo[,hend]/mtobs.combo[,hend]

cbind(obspropvec.combo, obspropvec.test)
plot(obspropvec.combo, obspropvec.test)
cor.test(  sort(obspropvec.test, method = "sh", index.return=TRUE)$ix,
           sort(obspropvec.control, method = "sh", index.return=TRUE)$ix )
cor.test(  sort(obspropvec.test, method = "sh", index.return=TRUE)$ix,
           sort(obspropvec.combo, method = "sh", index.return=TRUE)$ix )
#recall
# J <- 133
# K <- 4
nworlds <- 100

obspropmat.combo <- matrix(obspropvec.combo, J*K, hend) #columns are identical, stationarity
obspropmat.test <- matrix(obspropvec.test, J*K, hend)
obspropmat.control <- matrix(obspropvec.control, J*K, hend)

write.csv(obspropvec.combo, paste(path,"mutruevec_obspropvec_combo.csv",sep="") )

##===============


################################################

mutruemat <- obspropmat.combo
mutruevec <- obspropvec.combo
Mhobs.cur <- Mhobs.combo

################################################

wmat.balanced <- matrix(1/K,J*K,hend)
mhstar.cur <- round(Mhobs.cur*wmat.balanced)

ystarovertime <- matrix(0,nworlds,hend)
yhstar.all <- array(0, dim=c(J*K,hend,nworlds))
for (i in 1:nworlds){
  yhstarcur <- matrix(rbinom(J*K*hend, mhstar.cur, mutruemat) , J*K, hend, byrow=F)
  ystarovertime[i,] <- apply(yhstarcur,2,sum)
  yhstar.all[,,i] <- yhstarcur  
}

ytotaldist.balanced <- apply(yhstar.all,3,sum)
ptotaldist.balanced <- apply(yhstar.all,3,sum)/sum(mhstar.cur)

 #**** be sure to convert vector to matrix correctly **** #
maggovertime <- apply(mhstar.cur, 2, sum)
maggovertime.mat <- matrix(maggovertime,nworlds,hend, byrow=T)


yaggovertime.combo <- apply(yhobs.combo,2,sum)
paggovertime.combo <- yaggovertime.combo/maggovertime

ystarovertime.min <- apply(ystarovertime, 2, min)
ystarovertime.max <- apply(ystarovertime, 2, max)


pstarovertime <- ystarovertime/maggovertime.mat
pstarovertime.min <- apply(pstarovertime, 2, min)
pstarovertime.max <- apply(pstarovertime, 2, max)
pstarovertime.mean <- apply(pstarovertime, 2, mean)
pstarovertime.95top <- apply(pstarovertime, 2, quantile, .975)
pstarovertime.95bot <- apply(pstarovertime, 2, quantile, .025)

ylimcur <- 1e6*range(c(pstarovertime,paggovertime.combo))
plot( 1:hend, 1e6*paggovertime.combo, type="l",
      ylim=ylimcur, frame.plot=F,axes=F,ylab="",xlab="",main="")
axis(1,1:hend)
axis(2)
lines(1:hend, 1e6*pstarovertime.95bot, lty=2)
lines(1:hend, 1e6*pstarovertime.95top, lty=2)
lines(1:hend, 1e6*pstarovertime.mean, lty=2, lwd=2)
lines(1:hend, 1e6*paggovertime.combo, lty=1, lwd=2)
title(xlab="time periods", ylab="conversion rate (per million)")


# cumulative conversions

 #**** be sure to convert vector to matrix correctly **** #
maggovertimecum <- cumsum( apply(mhstar.cur, 2, sum) )
maggovertimecum.mat <- matrix(maggovertimecum,nworlds,hend, byrow=T)
               
yaggovertimecum.combo <- cumsum( apply(yhobs.combo,2,sum) )
paggovertimecum.combo <- yaggovertimecum.combo/maggovertimecum

ystarovertimecum <- t( apply(ystarovertime, 1, cumsum) )

ystarovertimecum.min <- apply(ystarovertimecum, 2, min)
ystarovertimecum.max <- apply(ystarovertimecum, 2, max)

rbind(ystarovertimecum.min, 
      yaggovertimecum.combo,
      ystarovertimecum.max)


pstarovertimecum <- ystarovertimecum/maggovertimecum.mat
pstarovertimecum.min <- apply(pstarovertimecum, 2, min)
pstarovertimecum.max <- apply(pstarovertimecum, 2, max)
pstarovertimecum.mean <- apply(pstarovertimecum, 2, mean)
pstarovertimecum.95top <- apply(pstarovertimecum, 2, quantile, .975)
pstarovertimecum.95bot <- apply(pstarovertimecum, 2, quantile, .025)

windows()
ylimcur <- 1e6*range(c(pstarovertimecum,paggovertimecum.combo,paggovertimecum.control,paggovertimecum.test))
plot( 1:hend, 1e6*paggovertimecum.combo, type="n",
      ylim=ylimcur, frame.plot=F,axes=F,ylab="",xlab="",main="")
axis(1,1:hend)
axis(2)
lines(1:hend, 1e6*pstarovertimecum.95bot, lty=2)
lines(1:hend, 1e6*pstarovertimecum.95top, lty=2)
lines(1:hend, 1e6*pstarovertimecum.mean, lty=2, lwd=1)
lines(1:hend, 1e6*paggovertimecum.control, lty=2, lwd=2)
lines(1:hend, 1e6*paggovertimecum.test, lty=1, lwd=2)
title(xlab="time periods", ylab="cumulative conversion rate (per million)")


windows()
ylimcur <- 1e6*range(c(pstarovertimecum,paggovertimecum.combo,paggovertimecum.control,paggovertimecum.test))
xscale <- cumsum(maggovertime.combo)/1e6
plot( xscale, 1e6*paggovertimecum.combo, type="n",
      ylim=ylimcur, frame.plot=F,axes=F,ylab="",xlab="",main="")
axis(1,xscale, at=seq(0,900,by=100), labels=seq(0,900,by=100) )
axis(2)
lines(xscale, 1e6*pstarovertimecum.95bot, lty=2)
lines(xscale, 1e6*pstarovertimecum.95top, lty=2)
lines(xscale, 1e6*pstarovertimecum.mean, lty=2, lwd=1)
lines(xscale, 1e6*paggovertimecum.control, lty=2, lwd=2)
lines(xscale, 1e6*paggovertimecum.test, lty=1, lwd=2)
title(xlab="impressions through time (millions)", ylab="cumulative conversion rate (per million)")


# change relative to starting points
#****  for denominator, want all columns to be identical to first period **** 
pstarovertimecumrel <- 100*(pstarovertimecum-pstarovertimecum[,1]) / matrix(pstarovertimecum[,1],nworlds,hend,byrow=F)
pstarovertimecumrel.min <- apply(pstarovertimecumrel, 2, min)
pstarovertimecumrel.max <- apply(pstarovertimecumrel, 2, max)
pstarovertimecumrel.mean <- apply(pstarovertimecumrel, 2, mean)
pstarovertimecumrel.95top <- apply(pstarovertimecumrel, 2, quantile, .975)
pstarovertimecumrel.95bot <- apply(pstarovertimecumrel, 2, quantile, .025)



pdf( file=paste(path,"fieldwithbounds.pdf",sep="") )
par(mar=c(5,5,4,2)+0.1)  #c(bottom, left, top, right), The default is c(5, 4, 4, 2) + 0.1.
ylimcur <- range(c(pstarovertimecumrel,paggovertimecumrel.control,paggovertimecumrel.test))
xscale <- cumsum(maggovertime.combo)/1e6
plot( xscale, paggovertimecumrel.test, type="n",
      ylim=ylimcur, frame.plot=F,axes=F,ylab="",xlab="",main="")
xscale <- cumsum(maggovertime.combo)/1e6
axis(1,xscale, at=seq(0,900,by=100), labels=seq(0,900,by=100) )
axis(2)
lines(xscale, pstarovertimecumrel.95bot, lty=2)
lines(xscale, pstarovertimecumrel.95top, lty=2)
lines(xscale, pstarovertimecumrel.mean, lty=2, lwd=1)
lines(xscale, paggovertimecumrel.control, lty=2, lwd=2)
lines(xscale, paggovertimecumrel.test, lty=1, lwd=2)
points(xscale, paggovertimecumrel.test, pch=1)
title(xlab="cumulative impressions through time (millions)", 
      ylab="change in cumulative conversion rate \n (% of conversion rate in initial period)")
dev.off()


pdf( file=paste(path,"fieldrelative.pdf",sep="") )
par(mar=c(5,5,4,2)+0.1)  #c(bottom, left, top, right), The default is c(5, 4, 4, 2) + 0.1.
ylimcur <- range(c(pstarovertimecumrel,paggovertimecumrel.control,paggovertimecumrel.test))
xscale <- cumsum(maggovertime.combo)/1e6
plot( xscale, paggovertimecumrel.test, type="n",
      ylim=ylimcur, frame.plot=F,axes=F,ylab="",xlab="",main="")
xscale <- cumsum(maggovertime.combo)/1e6
axis(1,xscale, at=seq(0,900,by=100), labels=seq(0,900,by=100) )
axis(2)
lines(xscale, paggovertimecumrel.control, lty=2, lwd=2)
lines(xscale, paggovertimecumrel.test, lty=1, lwd=2)
points(xscale, paggovertimecumrel.test, pch=1)
title(xlab="cumulative impressions through time (millions)", 
      ylab="change in cumulative conversion rate \n (% of conversion rate in initial period)")
dev.off()
      

pdf( file=paste(path,"fieldrelative_inc.pdf",sep="") )
paggovertimeincrel.control <- 100*(paggovertime.control-paggovertime.control[1])/paggovertime.control[1]
paggovertimeincrel.test <- 100*(paggovertime.test-paggovertime.test[1])/paggovertime.test[1]
par(mar=c(5,5,4,2)+0.1)  #c(bottom, left, top, right), The default is c(5, 4, 4, 2) + 0.1.
ylimcur <- range(paggovertimeincrel.test,paggovertimeincrel.control)
xscale <- cumsum(maggovertime.combo)/1e6
plot( xscale, paggovertimeincrel.control, type="l",col="white",
      ylim=ylimcur, frame.plot=F,axes=F,ylab="",xlab="",main="")
axis(1,xscale, at=seq(0,900,by=100), labels=seq(0,900,by=100) )
axis(2)
lines(xscale, paggovertimeincrel.control, lty=2, lwd=2)
lines(xscale, paggovertimeincrel.test, lty=1, lwd=2)
points(xscale, paggovertimeincrel.test, pch=1)
title(xlab="cumulative impressions through time (millions)", 
      ylab="change in incremental conversion rate \n (% of conversion rate in initial period)")
dev.off()
      




#=====================
#=== begin to run policies 

d <- d.combo.public
Mhobs.cur <- Mhobs.combo

nworlds <- 100
hinitial <- 1
K <- 4
J <- nrow(d)/K
nparams <- 12
#nparams <- 12 + nrow(d) #heterogeneous model

nworlds <- 5


# Establishing truth for data-generating process
linpred.true <- log(mutruevec/(1-mutruevec))
mutruemat <- matrix( mutruevec , nrow=nrow(d), ncol=hend) 
mutruevec <- mutruemat[,1]

## --- PERFECT INFORMATION POLICY , best (oracle policy) 
mutrueJbyK <- matrix(mutruevec, ncol=K, byrow=T)
winnertruename <- apply(mutrueJbyK, 1, which.max)
winnertrueindmat <- matrix(0,J,K)
winnertrueindmat[ cbind(1:J , apply(mutrueJbyK, 1, which.max)) ] <- 1                             
winnertrueindvec <- c(t(winnertrueindmat))
mutrueJbyKrange <- t( apply(mutrueJbyK, 1, range) )
mutrueJbyKsorted <- t( apply(mutrueJbyK, 1, sort) )
mutrueJbyKperchng <- mutrueJbyKsorted/mutrueJbyKsorted[,1]

ytotaldist.best <- rep(0,nworlds)
for (i in 1:nworlds){
  ytotaldist.best[i] <- sum( rbinom(J*hend, Mhobs.cur[winnertrueindvec==1,], mutruevec[winnertrueindvec==1]) )
}
ptotaldist.best <- ytotaldist.best/sum(Mhobs.cur[winnertrueindvec==1,])




##### --- BALANCED POLICY ------

wmatbalanced <- matrix(1/K, numobs, hend)
ytotaldist.balanced <- rep(0,nworlds)
ptotaldist.balanced <- rep(0,nworlds)
for (i in 1:nworlds){ 
  ytotaldist.balanced[i] <- sum( rbinom(numobs*hend, round(Mhobs.cur*wmatbalanced), mutruemat) )
  ptotaldist.balanced[i] <- ytotaldist.balanced[i]/sum( round(Mhobs.cur*wmatbalanced) )
}

reslist.balanced = list(NULL);
reslist.balanced$ytotaldist <- ytotaldist.balanced
reslist.balanced$ptotaldist <- ptotaldist.balanced
reslist.balanced$wstardist <- array(wmatbalanced, dim=c(numobs,hend,nworlds) )







###### ---- EXPLORE THAN EXPLOIT POLICIES ------

### Use policy.exp.then.exp functions
source(file=paste(path,"Greedy_variant_policies_201505.r",sep=""))

MODEL=F 
nsamp <- 100
n.iters <- 100
niters <- 100
n.worlds <- 50 #n.worlds <- 5 
nworlds <- 50 #nworlds <- 5




res.ETE.g04.t1 <- policy.exp.then.exp(hinit=1,policy.greedy.pooled4,Mhobs.cur)
res.ETE.g04.t2 <- policy.exp.then.exp(hinit=2,policy.greedy.pooled4,Mhobs.cur)
res.ETE.g04.t3 <- policy.exp.then.exp(hinit=3,policy.greedy.pooled4,Mhobs.cur)
res.ETE.g04.t4 <- policy.exp.then.exp(hinit=4,policy.greedy.pooled4,Mhobs.cur)
res.ETE.g04.t5 <- policy.exp.then.exp(hinit=5,policy.greedy.pooled4,Mhobs.cur)
res.ETE.g04.t6 <- policy.exp.then.exp(hinit=6,policy.greedy.pooled4,Mhobs.cur)

res.ETE.g12.t1 <- policy.exp.then.exp(hinit=1,policy.greedy.pooled12,Mhobs.cur)
res.ETE.g12.t2 <- policy.exp.then.exp(hinit=2,policy.greedy.pooled12,Mhobs.cur)
res.ETE.g12.t3 <- policy.exp.then.exp(hinit=3,policy.greedy.pooled12,Mhobs.cur)
res.ETE.g12.t4 <- policy.exp.then.exp(hinit=4,policy.greedy.pooled12,Mhobs.cur)
res.ETE.g12.t5 <- policy.exp.then.exp(hinit=5,policy.greedy.pooled12,Mhobs.cur)
res.ETE.g12.t6 <- policy.exp.then.exp(hinit=6,policy.greedy.pooled12,Mhobs.cur)

res.ETE.unpooled.t1 <- policy.exp.then.exp(hinit=1,policy.greedy.unpooled,Mhobs.cur)
res.ETE.unpooled.t2 <- policy.exp.then.exp(hinit=2,policy.greedy.unpooled,Mhobs.cur)
res.ETE.unpooled.t3 <- policy.exp.then.exp(hinit=3,policy.greedy.unpooled,Mhobs.cur)
res.ETE.unpooled.t4 <- policy.exp.then.exp(hinit=4,policy.greedy.unpooled,Mhobs.cur)
res.ETE.unpooled.t5 <- policy.exp.then.exp(hinit=5,policy.greedy.unpooled,Mhobs.cur)
res.ETE.unpooled.t6 <- policy.exp.then.exp(hinit=6,policy.greedy.unpooled,Mhobs.cur)

ptotaldist.ETE.g04.t1 <- res.ETE.g04.t1$ptotal
ptotaldist.ETE.g04.t2 <- res.ETE.g04.t2$ptotal
ptotaldist.ETE.g04.t3 <- res.ETE.g04.t3$ptotal
ptotaldist.ETE.g04.t4 <- res.ETE.g04.t4$ptotal
ptotaldist.ETE.g04.t5 <- res.ETE.g04.t5$ptotal
ptotaldist.ETE.g04.t6 <- res.ETE.g04.t6$ptotal

ptotaldist.ETE.g12.t1 <- res.ETE.g12.t1$ptotal
ptotaldist.ETE.g12.t2 <- res.ETE.g12.t2$ptotal
ptotaldist.ETE.g12.t3 <- res.ETE.g12.t3$ptotal
ptotaldist.ETE.g12.t4 <- res.ETE.g12.t4$ptotal
ptotaldist.ETE.g12.t5 <- res.ETE.g12.t5$ptotal
ptotaldist.ETE.g12.t6 <- res.ETE.g12.t6$ptotal

ptotaldist.ETE.unpooled.t1 <- res.ETE.unpooled.t1$ptotal
ptotaldist.ETE.unpooled.t2 <- res.ETE.unpooled.t2$ptotal
ptotaldist.ETE.unpooled.t3 <- res.ETE.unpooled.t3$ptotal
ptotaldist.ETE.unpooled.t4 <- res.ETE.unpooled.t4$ptotal
ptotaldist.ETE.unpooled.t5 <- res.ETE.unpooled.t5$ptotal
ptotaldist.ETE.unpooled.t6 <- res.ETE.unpooled.t6$ptotal


res.ETE.totals <- data.frame(
  explore= rep(rep(1:6,each=nworlds),2),
  greedy= rep( c("g04","g12"),each=6*nworlds ),
  ptotal=c(
res.ETE.g04.t1$ptotal,
res.ETE.g04.t2$ptotal,
res.ETE.g04.t3$ptotal,
res.ETE.g04.t4$ptotal,
res.ETE.g04.t5$ptotal,
res.ETE.g04.t6$ptotal,
res.ETE.g12.t1$ptotal,
res.ETE.g12.t2$ptotal,
res.ETE.g12.t3$ptotal,
res.ETE.g12.t4$ptotal,
res.ETE.g12.t5$ptotal,
res.ETE.g12.t6$ptotal)
)

rbind(
c(summary(res.ETE.g04.t1$ptotal),sd(res.ETE.g04.t1$ptotal)),
c(summary(res.ETE.g04.t2$ptotal),sd(res.ETE.g04.t2$ptotal)),
c(summary(res.ETE.g04.t3$ptotal),sd(res.ETE.g04.t3$ptotal)),
c(summary(res.ETE.g04.t4$ptotal),sd(res.ETE.g04.t4$ptotal)),
c(summary(res.ETE.g04.t5$ptotal),sd(res.ETE.g04.t5$ptotal)),
c(summary(res.ETE.g04.t6$ptotal),sd(res.ETE.g04.t6$ptotal)),
c(summary(res.ETE.g12.t1$ptotal),sd(res.ETE.g12.t1$ptotal)),
c(summary(res.ETE.g12.t2$ptotal),sd(res.ETE.g12.t2$ptotal)),
c(summary(res.ETE.g12.t3$ptotal),sd(res.ETE.g12.t3$ptotal)),
c(summary(res.ETE.g12.t4$ptotal),sd(res.ETE.g12.t4$ptotal)),
c(summary(res.ETE.g12.t5$ptotal),sd(res.ETE.g12.t5$ptotal)),
c(summary(res.ETE.g12.t6$ptotal),sd(res.ETE.g12.t1$ptotal))
)




#densities
windows()
xlimcur <- range(res.ETE.totals$ptotal)
plot( density(res.ETE.g04.t1$ptotal), type="n",
     xlim=xlimcur, frame.plot=F,axes=F,ylab="",xlab="",main="")
axis(1)
lines(density(res.ETE.g04.t1$ptotal),col=1)     
lines(density(res.ETE.g04.t2$ptotal),col=2)     
lines(density(res.ETE.g04.t3$ptotal),col=3)     
lines(density(res.ETE.g04.t4$ptotal),col=4)     
lines(density(res.ETE.g04.t5$ptotal),col=5)     
lines(density(res.ETE.g04.t6$ptotal),col=6)     

lines(density(res.ETE.g12.t1$ptotal),col=1)     
lines(density(res.ETE.g12.t2$ptotal),col=2)     
lines(density(res.ETE.g12.t3$ptotal),col=3)     
lines(density(res.ETE.g12.t4$ptotal),col=4)     
lines(density(res.ETE.g12.t5$ptotal),col=5)     
lines(density(res.ETE.g12.t6$ptotal),col=6)     
 
lines(density(ptotaldist.balanced),col=1,lty=2)

#time series
res.ETE.overtime <- data.frame(
  explore= rep(rep(1:6,each=nworlds*hend),2),
  greedy = rep( c("g04","g12"),each=6*nworlds*hend ),
  period = rep(1:hend,6*2*nworlds),
rate = c(
res.ETE.g04.t1$pstarovertimecum,
res.ETE.g04.t2$pstarovertimecum,
res.ETE.g04.t3$pstarovertimecum,
res.ETE.g04.t4$pstarovertimecum,
res.ETE.g04.t5$pstarovertimecum,
res.ETE.g04.t6$pstarovertimecum,
res.ETE.g12.t1$pstarovertimecum,
res.ETE.g12.t2$pstarovertimecum,
res.ETE.g12.t3$pstarovertimecum,
res.ETE.g12.t4$pstarovertimecum,
res.ETE.g12.t5$pstarovertimecum,
res.ETE.g12.t6$pstarovertimecum)
)

windows()
xscale <- cumsum(maggovertime.combo)/1e6
ylimcur <-  1e6*quantile(res.ETE.overtime$rate,c(0,1)) 
plot(xscale, 1e6*apply(res.ETE.g04.t1$pstarovertimecum,2,mean), type="n", 
      ylim=ylimcur, frame.plot=F,axes=F,ylab="",xlab="",main="")
axis(1,xscale, at=seq(0,900,by=100), labels=seq(0,900,by=100) )
axis(2)
lines(xscale, 1e6*apply(res.ETE.g04.t1$pstarovertimecum,2,mean), col=1)
lines(xscale, 1e6*apply(res.ETE.g04.t2$pstarovertimecum,2,mean), col=2)
lines(xscale, 1e6*apply(res.ETE.g04.t3$pstarovertimecum,2,mean), col=3)
lines(xscale, 1e6*apply(res.ETE.g04.t4$pstarovertimecum,2,mean), col=4)
lines(xscale, 1e6*apply(res.ETE.g04.t5$pstarovertimecum,2,mean), col=5)
lines(xscale, 1e6*apply(res.ETE.g04.t6$pstarovertimecum,2,mean), col=6)

lines(xscale, 1e6*apply(res.ETE.g12.t1$pstarovertimecum,2,mean), col=1,lty=2)
lines(xscale, 1e6*apply(res.ETE.g12.t2$pstarovertimecum,2,mean), col=2,lty=2)
lines(xscale, 1e6*apply(res.ETE.g12.t3$pstarovertimecum,2,mean), col=3,lty=2)
lines(xscale, 1e6*apply(res.ETE.g12.t4$pstarovertimecum,2,mean), col=4,lty=2)
lines(xscale, 1e6*apply(res.ETE.g12.t5$pstarovertimecum,2,mean), col=5,lty=2)
lines(xscale, 1e6*apply(res.ETE.g12.t6$pstarovertimecum,2,mean), col=6,lty=2)

lines(xscale, 1e6*apply(res.ETE.g12.t1$pstarovertimecum,2,quantile,.05), col=1,lty=2)
lines(xscale, 1e6*apply(res.ETE.g12.t2$pstarovertimecum,2,quantile,.05), col=2,lty=2)
lines(xscale, 1e6*apply(res.ETE.g12.t3$pstarovertimecum,2,quantile,.05), col=3,lty=2)
lines(xscale, 1e6*apply(res.ETE.g12.t4$pstarovertimecum,2,quantile,.05), col=4,lty=2)
lines(xscale, 1e6*apply(res.ETE.g12.t5$pstarovertimecum,2,quantile,.05), col=5,lty=2)
lines(xscale, 1e6*apply(res.ETE.g12.t6$pstarovertimecum,2,quantile,.05), col=6,lty=2)

lines(xscale, 1e6*apply(res.ETE.g12.t1$pstarovertimecum,2,quantile,.95), col=1,lty=2)
lines(xscale, 1e6*apply(res.ETE.g12.t2$pstarovertimecum,2,quantile,.95), col=2,lty=2)
lines(xscale, 1e6*apply(res.ETE.g12.t3$pstarovertimecum,2,quantile,.95), col=3,lty=2)
lines(xscale, 1e6*apply(res.ETE.g12.t4$pstarovertimecum,2,quantile,.95), col=4,lty=2)
lines(xscale, 1e6*apply(res.ETE.g12.t5$pstarovertimecum,2,quantile,.95), col=5,lty=2)
lines(xscale, 1e6*apply(res.ETE.g12.t6$pstarovertimecum,2,quantile,.95), col=6,lty=2)

lines(xscale, 1e6*pstarovertimecum.mean, lty=2, lwd=2)    #fully balanced design

title(xlab="impressions through time", ylab="cumulative conversion rate (per million)")





### ------ GREEDY VARIANTS AND THOMPSON SAMPLING POLICIES -----

 
source(file=paste(path,"policy2res.r",sep=""))
source(file=paste(path,"policy2res_fast.r",sep=""))


source(file=paste(path,"Greedy_variant_policies_201505.r",sep=""))
source(file=paste(path,"RPM_BB_20130221.r",sep=""))


# depends on mutruemat

MODEL=F  # MODEL = TRUE if the policy2res function calls a GLM-based policy

#small , fast
nsamp <- 100
n.iters <- 100
niters <- 100
n.worlds <- 50
nworlds <- 50

#regular
nsamp <- 1000
n.iters <- 1000
niters <- 1000
n.worlds <- 100
nworlds <- 100


# run!  from policies to results
reslist.greedy.pooled4 <- policy2res(policy.greedy.pooled4)
reslist.greedy.pooled12 <- policy2res(policy.greedy.pooled12)
reslist.greedy.unpooled <- policy2res(policy.greedy.unpooled)
reslist.softmax.unpooled <- policy2res(policy.softmax.unpooled)

reslist.gittins.pooled12 <- policy2res(policy.gittins.pooled12)
reslist.gittins.unpooled <- policy2res(policy.gittins.unpooled)

reslist.UCB1.unpooled <- policy2res(policy.UCB1.unpooled)
reslist.UCBtuned.unpooled <- policy2res(policy.UCBtuned.unpooled)

reslist.epsgreedy10.pooled12 <- policy2res(policy.epsgreedy.pooled12,eps=.10)
reslist.epsgreedy20.pooled12 <- policy2res(policy.epsgreedy.pooled12,eps=.20)
reslist.epsgreedy10.unpooled <- policy2res(policy.epsgreedy.unpooled,eps=.10)
reslist.epsgreedy20.unpooled <- policy2res(policy.epsgreedy.unpooled,eps=.20)

reslist.RPM.BB4 <- policy2res(policy.RPM.BB4)
reslist.RPM.BB12 <- policy2res(policy.RPM.BB12)
reslist.RPM.BB4.unpooled <- policy2res(policy.RPM.BB4.unpooled)



# save ptotaldist
ptotaldist.softmax.unpooled <- reslist.softmax.unpooled$ptotaldist    
ptotaldist.greedy.unpooled <- reslist.greedy.unpooled$ptotaldist
ptotaldist.greedy.pooled12 <- reslist.greedy.pooled12$ptotaldist
ptotaldist.greedy.pooled4 <- reslist.greedy.pooled4$ptotaldist

ptotaldist.gittins.pooled12 <- reslist.gittins.pooled12$ptotaldist
ptotaldist.gittins.unpooled <- reslist.gittins.unpooled$ptotaldist

ptotaldist.UCB1.unpooled <- reslist.UCB1.unpooled$ptotaldist
ptotaldist.UCBtuned.unpooled <- reslist.UCBtuned.unpooled$ptotaldist

ptotaldist.epsgreedy10.unpooled <- reslist.epsgreedy10.unpooled$ptotaldist
ptotaldist.epsgreedy20.unpooled <- reslist.epsgreedy20.unpooled$ptotaldist
ptotaldist.epsgreedy10.pooled12 <- reslist.epsgreedy10.pooled12$ptotaldist
ptotaldist.epsgreedy20.pooled12 <- reslist.epsgreedy20.pooled12$ptotaldist

ptotaldist.RPM.BB4 <- reslist.RPM.BB4$ptotaldist
ptotaldist.RPM.BB12 <- reslist.RPM.BB12$ptotaldist
ptotaldist.RPM.BB4.unpooled <- reslist.RPM.BB4.unpooled$ptotaldist



source(file=paste(path,"RPM_Logit_Homog_20130218.r",sep=""))
source(file=paste(path,"RPM_Logit_LC2_flexmix_20130219.r",sep=""))
source(file=paste(path,"RPM_Logit_Heter_LMER_20130218.r",sep=""))

## TS-GLM
startruntime <- Sys.time()
#reslist.RPM.Logit.Homog <- policy2res( policy.RPM.Logit.Homog, MODEL=T, nparams=12)
reslist.RPM.Logit.Homog <- policy2res_fast( policy.RPM.Logit.Homog, MODEL=F, nparams=12, save_curworld=T)
endruntime <- Sys.time()
endruntime - startruntime

# TS-LCGLM
startruntime <- Sys.time()
reslist.RPM.Logit.LC2 <- policy2res_fast( policy.RPM.Logit.LC2, save_curworld=T, MODEL=F, K=12) #watchout for K=12 ?!
endruntime <- Sys.time()
endruntime - startruntime
ptotaldist.RPM.Logit.LC2 <- reslist.RPM.Logit.LC2$ptotaldist
##

# TS-HGLM
nworlds <- 50
startruntime <- Sys.time()
reslist.RPM.Logit.Heter <- policy2res_fast( policy.RPM.Logit.Heter, MODEL=F, save_curworld=T )
endruntime <- Sys.time()
endruntime - startruntime
# ptotaldist.RPM.Logit.Heter <- read.csv(file=paste(path,"pstartotal_append_TSHGLM_20150529.csv",sep="") ,header=F)[,-1]

# run once
restemphomog <- policy.RPM.Logit.Homog(m=mtstar[,hinitial],y=ytstar[,hinitial],nparams=12,MODEL=T)
restempheter <- policy.RPM.Logit.Heter(m=mtstar[,hinitial],y=ytstar[,hinitial])
restempLC2 <- policy.RPM.Logit.LC2(m=mtstar[,hinitial],y=ytstar[,hinitial],K=12) #watchout for K=12 ?!

# save ptotaldist
ptotaldist.RPM.Logit.Homog <- reslist.RPM.Logit.Homog$ptotaldist
ptotaldist.RPM.Logit.LC2 <- reslist.RPM.Logit.LC2$ptotaldist
ptotaldist.RPM.Logit.Heter <- reslist.RPM.Logit.Heter$ptotaldist



#----------------
summaryfn <- function(x)  c( mean=mean(x), sd=sd(x), quantile(x,c(.025,.975)) )
# TABLE summary of ptotal
ptotalsummaryfn <- function(x)  c( mean=mean(x), sd=sd(x), quantile(x,c(.025,.975)) )




ptotaldist.all <- rbind(
ptotaldist.balanced,
ptotaldist.ETE.g04.t1,
ptotaldist.ETE.g04.t2,
ptotaldist.ETE.g04.t3,
ptotaldist.ETE.g04.t4,
ptotaldist.ETE.g04.t5,
ptotaldist.ETE.g04.t6,
ptotaldist.ETE.g12.t1,
ptotaldist.ETE.g12.t2,
ptotaldist.ETE.g12.t3,
ptotaldist.ETE.g12.t4,
ptotaldist.ETE.g12.t5,
ptotaldist.ETE.g12.t6,
ptotaldist.ETE.unpooled.t1,
ptotaldist.ETE.unpooled.t2,
ptotaldist.ETE.unpooled.t3,
ptotaldist.ETE.unpooled.t4,
ptotaldist.ETE.unpooled.t5,
ptotaldist.ETE.unpooled.t6,
ptotaldist.epsgreedy10.pooled12,
ptotaldist.epsgreedy20.pooled12,
ptotaldist.epsgreedy10.unpooled,
ptotaldist.epsgreedy20.unpooled,
ptotaldist.greedy.pooled4,
ptotaldist.greedy.pooled12,            
ptotaldist.softmax.unpooled,
ptotaldist.greedy.unpooled,
ptotaldist.gittins.pooled12,
ptotaldist.gittins.unpooled,
ptotaldist.UCB1.unpooled,
ptotaldist.UCBtuned.unpooled,
ptotaldist.RPM.BB4, 
ptotaldist.RPM.BB12,
ptotaldist.RPM.BB4.unpooled,

ptotaldist.RPM.Logit.Homog,
# ptotaldist.RPM.Logit.LC2,
# ptotaldist.RPM.Logit.Heter,

ptotaldist.best)



ptotaldist.summary <- t( apply(ptotaldist.all,1,ptotalsummaryfn) )




##========= plots and figures for paper  ==========##



policynames <- c("ETE-g04-t1","ETE-g04-t2","ETE-g04-t3",
                 "ETE-g04-t4","ETE-g04-t5","ETE-g04-t6",
                 "ETE-g12-t1","ETE-g12-t2","ETE-g12-t3",
                 "ETE-g12-t4","ETE-g12-t5","ETE-g12-t6",
                 "balanced","greedy-pooled4","greedy-pooled12",
                 "softmax-unpooled","greedy-unpooled",
                 "epsgreedy10-pooled12","epsgreedy20-pooled12",
                 "epsgreedy10-unpooled","epsgreedy20-unpooled",
                 "RPM-binomial4","RPM-binomial12","RPM-binomial4-unpooled",
                 "RPM-logit-homog-MLE","RPM-logit-LC2-EM","RPM-logit-heter-REML",
                 "best")  
rownames(ptotaldist.summary) <- policynames

1e6*ptotaldist.summary
#----------------


ptotaldf <- data.frame( 
  policy = as.factor( rep(c("balanced","greedy-pooled4","greedy-pooled12",
                  "softmax-unpooled","greedy-unpooled",
                  "epsgreedy10-pooled12","epsgreedy20-pooled12",
                  "epsgreedy10-unpooled","epsgreedy20-unpooled",
                  "RPM-binomial4","RPM-binomial12","RPM-binomial4-unpooled",
                  "RPM-logit-homog-MLE","RPM-logit-LC2-EM","RPM-logit-heter-REML"
                  ),each=nworlds) ),
  rate = 1e6*c(ptotaldist.balanced,
            ptotaldist.greedy.pooled4,
            ptotaldist.greedy.pooled12,            
            ptotaldist.softmax.unpooled,
            ptotaldist.greedy.unpooled,
            ptotaldist.epsgreedy10.pooled12,
            ptotaldist.epsgreedy20.pooled12,
            ptotaldist.epsgreedy10.unpooled,
            ptotaldist.epsgreedy20.unpooled,
            ptotaldist.RPM.BB4, 
            ptotaldist.RPM.BB12,
            ptotaldist.RPM.BB4.unpooled,
            ptotaldist.RPM.Logit.Homog,
            ptotaldist.RPM.Logit.LC2,
            ptotaldist.RPM.Logit.Heter
          )
)


ptotaldf$RPM <- rep(F,nrow(ptotaldf))
ptotaldf$RPM[ptotaldf$policy %in%
             c("balanced","RPM-logit-homog-MLE","RPM-logit-LC2-EM","RPM-logit-heter-REML")
             ] <- T


####====== REVISION ON 2015-05-29 ============##
####====== REVISION ON 2013-11-27 ============##
####  - Separate tables and figures for TS only, Heuristics only, TestRollout Only


ptdf.policynames.TSonly <- c(
    "Balanced",
    "TS-BB-pooled",
    "TS-GLM-pooled",
    "TS-BB-unpooled",
    "TS-LCGLM",
    "TS-HGLM",
    "Perfect Info"
    )             
nworlds <- 100
ptdf.TSonly <- data.frame(
  policy = factor( rep(ptdf.policynames.TSonly, each=nworlds), 
    levels=ptdf.policynames.TSonly ),
  rate =1e6*c(
    ptotaldist.balanced,
    ptotaldist.RPM.BB12,
    ptotaldist.RPM.Logit.Homog,
    ptotaldist.RPM.BB4.unpooled,
    ptotaldist.RPM.Logit.LC2,
    ptotaldist.RPM.Logit.Heter,
    ptotaldist.best
    )
)            
pdf( file=paste(path,"ptotal_boxplot_TSonly.pdf",sep="") )
gpt <- ggplot(ptdf.TSonly,aes(y=rate, x=policy))
gpt + geom_boxplot() + 
  labs(y="\n conversion rate (per million)",x="policy") +
  theme_bw()+
  coord_flip() 
dev.off()

#===========

#==========================================
ptdf.policynames.MABlit <- c(
    "Balanced",
    "Gittins-pooled",
    "Gittins-unpooled",
    "UCB1-unpooled",
    "UCBtuned-unpooled",
    "TS-BB-unpooled",
    "TS-HGLM",
    "Perfect Info"
    )             
nworlds <- 100
ptdf.MABlit <- data.frame(
  policy = factor( rep(ptdf.policynames.MABlit, each=nworlds), 
    levels=ptdf.policynames.MABlit ),
  rate =1e6*c(
    ptotaldist.balanced,
    ptotaldist.gittins.pooled12,
    ptotaldist.gittins.unpooled,
    ptotaldist.UCB1.unpooled,
    ptotaldist.UCBtuned.unpooled,
    ptotaldist.RPM.BB4.unpooled,
    ptotaldist.RPM.Logit.Heter,
    ptotaldist.best
    )
)   
pdf( file=paste(path,"ptotal_boxplot_MABlit.pdf",sep="") )
gpt <- ggplot(ptdf.MABlit,aes(y=rate, x=policy))
gpt + geom_boxplot() + 
  labs(y="\n conversion rate (per million)",x="policy") +
  theme_bw()+
  coord_flip() 
dev.off()


#==========================================

ptdf.policynames.HeurOnly <- c(
    "Balanced",
    "epsgreedy10-pooled",
    "epsgreedy20-pooled",
    "epsgreedy10-unpooled",
    "epsgreedy20-unpooled",
    "greedy-pooled",
    "greedy-unpooled",
    "TS-HGLM",
    "Perfect Info"
    )             
nworlds <- 100
ptdf.HeurOnly <- data.frame(
  policy = factor( rep(ptdf.policynames.HeurOnly, each=nworlds), 
    levels=ptdf.policynames.HeurOnly ),
  rate =1e6*c(
    ptotaldist.balanced,
    ptotaldist.epsgreedy10.pooled12,
    ptotaldist.epsgreedy20.pooled12,
    ptotaldist.epsgreedy10.unpooled,
    ptotaldist.epsgreedy20.unpooled,
    ptotaldist.greedy.pooled12,            
    ptotaldist.greedy.unpooled,
    ptotaldist.RPM.Logit.Heter,
    ptotaldist.best
    )
)            
pdf( file=paste(path,"ptotal_boxplot_HeurOnly.pdf",sep="") )
gpt <- ggplot(ptdf.HeurOnly,aes(y=rate, x=policy))
gpt + geom_boxplot() + 
  labs(y="\n conversion rate (per million)",x="policy") +
  theme_bw()+
  coord_flip() 
dev.off()


#============================

ptdf.policynames.TRonly <- c(
    "Balanced",
    "testrollout-t1",
    "testrollout-t2",
    "testrollout-t3",
    "testrollout-t4",
    "testrollout-t5",
    "testrollout-t6",
    "testrollout-unpooled-t1",
    "testrollout-unpooled-t2",
    "testrollout-unpooled-t3",
    "testrollout-unpooled-t4",
    "testrollout-unpooled-t5",
    "testrollout-unpooled-t6",    
    "Perfect Info"
    )             
nworlds <- 100
ptdf.TRonly <- data.frame(
  policy = factor( rep(ptdf.policynames.TRonly, each=nworlds), 
    levels=ptdf.policynames.TRonly ),
  rate =1e6*c(
    ptotaldist.balanced,
    ptotaldist.ETE.g12.t1,
    ptotaldist.ETE.g12.t2,
    ptotaldist.ETE.g12.t3,
    ptotaldist.ETE.g12.t4,
    ptotaldist.ETE.g12.t5,
    ptotaldist.ETE.g12.t6,
    ptotaldist.ETE.unpooled.t1,
    ptotaldist.ETE.unpooled.t2,
    ptotaldist.ETE.unpooled.t3,
    ptotaldist.ETE.unpooled.t4,
    ptotaldist.ETE.unpooled.t5,
    ptotaldist.ETE.unpooled.t6,    
    ptotaldist.best
    )
)            
pdf( file=paste(path,"ptotal_boxplot_TRonly.pdf",sep="") )
gpt <- ggplot(ptdf.TRonly,aes(y=rate, x=policy))
gpt + geom_boxplot() + 
  labs(y="\n conversion rate (per million)",x="policy") +
  theme_bw()+
  coord_flip() 
dev.off()


#=============================


ptdf.policynames.Unpooled <- c(
    "Balanced",
    "testrollout-unpooled-t2",
    "epsgreedy10-unpooled",
    "epsgreedy20-unpooled",
    "greedy-unpooled",
    "Gittins-unpooled",
    "UCBtuned-unpooled",
    "TS-BB-unpooled",
    "Perfect Info"
    )             
nworlds <- 100
ptdf.Unpooled <- data.frame(
  policy = factor( rep(ptdf.policynames.Unpooled, each=nworlds), 
    levels=ptdf.policynames.Unpooled ),
  rate =1e6*c(
    ptotaldist.balanced,
    ptotaldist.ETE.unpooled.t2,    
    ptotaldist.epsgreedy10.unpooled,
    ptotaldist.epsgreedy20.unpooled,
    ptotaldist.greedy.unpooled,
    ptotaldist.gittins.unpooled,
    ptotaldist.UCBtuned.unpooled,
    ptotaldist.RPM.BB4.unpooled,    
    ptotaldist.best
    )
)            
pdf( file=paste(path,"ptotal_boxplot_Unpooled.pdf",sep="") )
gpt <- ggplot(ptdf.Unpooled,aes(y=rate, x=policy))
gpt + geom_boxplot() + 
  labs(y="\n conversion rate (per million)",x="policy") +
  theme_bw()+
  coord_flip() 
dev.off()


ptprobwinfn <- function(x) {
  pnames <- levels(x$policy)
  npolicies <- length(pnames)
  nworlds <- length(x$policy)/npolicies
  prwin <- matrix(NA,npolicies,npolicies)
  rownames(prwin) <- pnames
    for (i in 1:npolicies){
      for (j in 1:npolicies){
        if(i>j){
        idxpi <- which(x$policy==pnames[i])
        idxpj <- which(x$policy==pnames[j])
        prwin[i,j] <- sum(x$rate[idxpi] >= x$rate[idxpj])/nworlds
        }
      }
    }
  return(prwin)
}

ptprobwinfn(ptdf.TSonly)
ptprobwinfn(ptdf.MABlit)
ptprobwinfn(ptdf.Unpooled)
ptprobwinfn(ptdf.HeurOnly)
ptprobwinfn(ptdf.TRonly)




# ========= AFTER OPTIMIZING FOR CLICKS, LOOK AT CONVERSIONS ======

reslist_ctropt_cvrout <- function(reslist_ctropt){ 
  # function takes results optimized for CTR
  #    and looks at the implied CVR results
  # input
  #   reslist_ctropt (based on optimizing clicks)
  #   Mhobs.cur 
  #   mutruemat (conversion rates)

  # grab wstardist from optimizing click through
  wstardist <- reslist_ctropt$wstardist
  numobs <- dim(wstardist)[1]
  hend <- dim(wstardist)[2]
  nworlds <- dim(wstardist)[3]

  ytotaldist <- rep(0,nworlds)
  ptotaldist <- rep(0,nworlds)
  for (i in 1:nworlds){ 
    wstar <- wstardist[,,i]
    ytotaldist[i] <- sum( rbinom(numobs*hend, round(Mhobs.cur*wstar), mutruemat) )
    ptotaldist[i] <- ytotaldist[i]/sum( round(Mhobs.cur*wstar) )
  }

  reslist_cvrout <- list(ptotaldist=ptotaldist)
  return(reslist_cvrout)
} #end function

# ex
ptotaldist.balanced.cvrout <- reslist_ctropt_cvrout(reslist.balanced)$ptotaldist
# ptotaldist.ETE.g12.t1.cvrout <- reslist_ctropt_cvrout(res.ETE.g12.t1)$ptotaldist
# ptotaldist.ETE.g12.t2.cvrout <- reslist_ctropt_cvrout(res.ETE.g12.t2)$ptotaldist
# ptotaldist.ETE.g12.t3.cvrout <- reslist_ctropt_cvrout(res.ETE.g12.t3)$ptotaldist
# ptotaldist.ETE.g12.t4.cvrout <- reslist_ctropt_cvrout(res.ETE.g12.t4)$ptotaldist
# ptotaldist.ETE.g12.t5.cvrout <- reslist_ctropt_cvrout(res.ETE.g12.t5)$ptotaldist
# ptotaldist.ETE.g12.t6.cvrout <- reslist_ctropt_cvrout(res.ETE.g12.t6)$ptotaldist
# ptotaldist.ETE.unpooled.t1.cvrout <- reslist_ctropt_cvrout(res.ETE.unpooled.t1)$ptotaldist
# ptotaldist.ETE.unpooled.t2.cvrout <- reslist_ctropt_cvrout(res.ETE.unpooled.t2)$ptotaldist
# ptotaldist.ETE.unpooled.t3.cvrout <- reslist_ctropt_cvrout(res.ETE.unpooled.t3)$ptotaldist
# ptotaldist.ETE.unpooled.t4.cvrout <- reslist_ctropt_cvrout(res.ETE.unpooled.t4)$ptotaldist
# ptotaldist.ETE.unpooled.t5.cvrout <- reslist_ctropt_cvrout(res.ETE.unpooled.t5)$ptotaldist
# ptotaldist.ETE.unpooled.t6.cvrout <- reslist_ctropt_cvrout(res.ETE.unpooled.t6)$ptotaldist
ptotaldist.epsgreedy10.pooled12.cvrout <- reslist_ctropt_cvrout(reslist.epsgreedy10.pooled12)$ptotaldist
ptotaldist.epsgreedy20.pooled12.cvrout <- reslist_ctropt_cvrout(reslist.epsgreedy20.pooled12)$ptotaldist
ptotaldist.epsgreedy10.unpooled.cvrout <- reslist_ctropt_cvrout(reslist.epsgreedy10.unpooled)$ptotaldist
ptotaldist.epsgreedy20.unpooled.cvrout <- reslist_ctropt_cvrout(reslist.epsgreedy20.unpooled)$ptotaldist
ptotaldist.greedy.pooled4.cvrout <- reslist_ctropt_cvrout(reslist.greedy.pooled4)$ptotaldist
ptotaldist.greedy.pooled12.cvrout <- reslist_ctropt_cvrout(reslist.greedy.pooled12)$ptotaldist
ptotaldist.softmax.unpooled.cvrout <- reslist_ctropt_cvrout(reslist.softmax.unpooled)$ptotaldist
ptotaldist.greedy.unpooled.cvrout <- reslist_ctropt_cvrout(reslist.greedy.unpooled)$ptotaldist
ptotaldist.gittins.pooled12.cvrout <- reslist_ctropt_cvrout(reslist.gittins.pooled12)$ptotaldist
ptotaldist.gittins.unpooled.cvrout <- reslist_ctropt_cvrout(reslist.gittins.unpooled)$ptotaldist
ptotaldist.UCB1.unpooled.cvrout <- reslist_ctropt_cvrout(reslist.UCB1.unpooled)$ptotaldist
ptotaldist.UCBtuned.unpooled.cvrout <- reslist_ctropt_cvrout(reslist.UCBtuned.unpooled)$ptotaldist
ptotaldist.RPM.BB4.cvrout <- reslist_ctropt_cvrout(reslist.RPM.BB4)$ptotaldist
ptotaldist.RPM.BB12.cvrout <- reslist_ctropt_cvrout(reslist.RPM.BB12)$ptotaldist
ptotaldist.RPM.BB4.unpooled.cvrout <- reslist_ctropt_cvrout(reslist.RPM.BB4.unpooled)$ptotaldist
ptotaldist.RPM.Logit.Homog.cvrout <- reslist_ctropt_cvrout(reslist.RPM.Logit.Homog)$ptotaldist
# ptotaldist.RPM.Logit.LC2,
# ptotaldist.RPM.Logit.Heter,


ptotaldist.cvrout <- rbind(
ptotaldist.balanced.cvrout,
# ptotaldist.ETE.g12.t1.cvrout,
# ptotaldist.ETE.g12.t2.cvrout,
# ptotaldist.ETE.g12.t3.cvrout,
# ptotaldist.ETE.g12.t4.cvrout,
# ptotaldist.ETE.g12.t5.cvrout,
# ptotaldist.ETE.g12.t6.cvrout,
# ptotaldist.ETE.unpooled.t1.cvrout,
# ptotaldist.ETE.unpooled.t2.cvrout,
# ptotaldist.ETE.unpooled.t3.cvrout,
# ptotaldist.ETE.unpooled.t4.cvrout,
# ptotaldist.ETE.unpooled.t5.cvrout,
# ptotaldist.ETE.unpooled.t6.cvrout,
ptotaldist.epsgreedy10.pooled12.cvrout,
ptotaldist.epsgreedy20.pooled12.cvrout,
ptotaldist.epsgreedy10.unpooled.cvrout,
ptotaldist.epsgreedy20.unpooled.cvrout,
ptotaldist.greedy.pooled4.cvrout,
ptotaldist.greedy.pooled12.cvrout,            
ptotaldist.softmax.unpooled.cvrout,
ptotaldist.greedy.unpooled.cvrout,
ptotaldist.gittins.pooled12.cvrout,
ptotaldist.gittins.unpooled.cvrout,
ptotaldist.UCB1.unpooled.cvrout,
ptotaldist.UCBtuned.unpooled.cvrout,
ptotaldist.RPM.BB4.cvrout, 
ptotaldist.RPM.BB12.cvrout,
ptotaldist.RPM.BB4.unpooled.cvrout, 
ptotaldist.RPM.Logit.Homog.cvrout)
# ptotaldist.RPM.Logit.LC2.cvrout,
# ptotaldist.RPM.Logit.Heter.cvrout,



ptotaldist.cvrout.summary <- t( apply(ptotaldist.cvrout,1,ptotalsummaryfn) )


####

### end of code ### 


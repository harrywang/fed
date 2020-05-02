policy2res_fast <- function(policycur,hinitial=1,Mhobs.cur=Mhobs.combo,MODEL=F,save_curworld=F,nparams=NULL,eps=.1,...){
                                    
#depends on: mutruemat 

#ytotaldist <- rep(0,nworlds)
ptotaldist <- rep(0,nworlds)

wstardist <- array(0, dim=c(nrow(d),hend,nworlds) )  
#pstardist <- array(0, dim=c(nrow(d),hend,nworlds) )

#save CI for each parameter at each time
#paramstardist <- NULL
#if (MODEL) paramstardist <- array(0, dim=c(2*nparams,hend,nworlds) ) 

start0 <- Sys.time()
for ( i in 1:nworlds){
#starti <- Sys.time()
#print(starti)

wstar <- matrix(0, nrow(d), hend)
  wstar[,1:hinitial] <- 1/K
mhstar <- matrix(0, nrow(d), hend) 
yhstar <- matrix(0, nrow(d), hend) 
mtstar <- matrix(0, nrow(d), hend) 
ytstar <- matrix(0, nrow(d), hend) 
#if (MODEL) paramstar <- matrix(0, 2*nparams, hend) #save CI for each parameter at each time

for (hh in 1:hinitial){          
  mhstar[,hh] <- round(Mhobs.cur[ ,hh]*wstar[,hh])
  yhstar[,hh] <- rbinom( nrow(d), mhstar[,hh], mutruemat[,hh])
}
  mtstar[,1:hinitial] <- t( apply( as.matrix(mhstar[,1:hinitial]), 1, cumsum) )
  ytstar[,1:hinitial] <- t( apply( as.matrix(yhstar[,1:hinitial]), 1, cumsum) )
  outcur <- policycur(m=mtstar[,hinitial],y=ytstar[,hinitial],eps=eps)
  #save( outcur, file=paste(path,"RPM.Logit.Heter.Res.OneWorld.t",hh,".RData",sep="") )
  wstar[,hinitial+1] <- outcur$w
  #if (MODEL) paramstar[,hinitial+1] <- c( apply(outcur$paramvec,2,quantile,c(.025,.975))  ) #save CI for each parameter at each time
  #
for (hh in (hinitial+1):(hend-1)){
  #print(paste("start period",hh))
  mhstar[,hh] <- round(Mhobs.cur[ ,hh]*wstar[,hh])
  yhstar[,hh] <- rbinom( nrow(d), mhstar[,hh], mutruemat[,hh])
  mtstar[,hh] <- apply(mhstar[,1:hh],1,sum)
  ytstar[,hh] <- apply(yhstar[,1:hh],1,sum)
  outcur <- policycur(m=mtstar[,hh],y=ytstar[,hh],eps=eps)  ###***** TIME CONSUMING ****
  #save( outcur, file=paste(path,"RPM.Logit.Heter.Res.OneWorld.t",hh,".RData",sep="") )
  wstar[,hh+1] <- outcur$w
  #if (MODEL) paramstar[,hh+1] <- c( apply(outcur$paramvec,2,quantile,c(.025,.975))  ) #save CI for each parameter at each time
}
hh <- hend
  mhstar[,hh] <- round(Mhobs.cur[ ,hh]*wstar[,hh])
  yhstar[,hh] <- rbinom( nrow(d), mhstar[,hh], mutruemat[,hh])
  mtstar[,hh] <- apply(mhstar[,1:hh],1,sum)
  ytstar[,hh] <- apply(yhstar[,1:hh],1,sum)


mstartotal <- sum(mhstar) 
ystartotal <- sum(yhstar) 
pstar <- apply(ytstar,1,cumsum)/apply(mtstar,1,cumsum)
pstartotal <- sum(yhstar)/sum(mhstar)  
  
#ytotaldist[i] <- ystartotal
ptotaldist[i] <- pstartotal
wstardist[,,i] <- wstar
 
#if (MODEL) paramstardist[,,i] <- paramstar

if (save_curworld){
  reslist_curworld <- 
    list( 
      #ytotaldist=ytotaldist[i],
      #pstardist=pstardist[,,i],
      #paramstardist=paramstardist[,,i],
      wstardist=wstardist[,,i],
      ptotaldist=ptotaldist[i]
    )
  write.table(pstartotal, file=paste(path,"pstartotal_append_curpolicy.csv",sep=""),  
    sep = ",", col.names = FALSE, append=TRUE)  
  save(reslist_curworld , file=paste(path,"Reslist_cur.RPM.Logit.Heter.World.",1000+i,".RData",sep="") )
} # save_curworld

endi_time <- Sys.time()
peri_time <- (endi_time - start0)/i
remain_time <- peri_time*( nworlds - i )

if ((i%%10)==0) print(paste("Finished", i, "out of", nworlds, "at", endi_time ))

if (i==1) print(paste("First iter started at", start0, "finished at", endi_time))
#endi <- Sys.time()
#print(endi - starti)

}

reslist <- 
  list(
    #ytotaldist=ytotaldist,
    #pstardist=pstardist,
    #paramstardist=paramstardist,
    wstardist=wstardist,
    ptotaldist=ptotaldist
    )
return(reslist)
} ## END FUNCTION policy2res

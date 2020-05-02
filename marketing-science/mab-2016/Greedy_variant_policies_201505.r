
#===========================================================
#==  Algorithms ====
#===========================================================

## Greedy variants ##

policy.greedy.pooled4 <- function(m,y,...){
  J <- nrow(d)/K
  mbyK <- matrix(m, ncol=K, byrow=T) 
  ybyK <- matrix(y, ncol=K, byrow=T)    
  muobs <- apply(ybyK, 2, sum)/apply(mbyK, 2, sum)
  whichcol <- rep( as.numeric( which.is.max(muobs) ), J)
  wJbyK <- matrix(0, nrow=J, ncol=K)
  wJbyK[ cbind( 1:J, whichcol ) ] <- 1
  w <- as.vector( t(wJbyK) )
  return(list(w=w))
}

policy.greedy.pooled12 <- function(m,y,...){
# d$sizeandconcept  (12 unique actions)
# d$unitid  (59 unique units)
# d$placeid (133 unique places)
# d$adsize  (3 unique sizes)
  sz <- d$adsize
  J <- nrow(d)/K

  s1 <- (sz==levels(sz)[1])
    mbyK <- matrix(m[s1], ncol=K, byrow=T) 
    ybyK <- matrix(y[s1], ncol=K, byrow=T)    
    muobs <- apply(ybyK, 2, sum)/apply(mbyK, 2, sum)
  s1winner <- as.numeric( which.is.max(muobs) )
  s2 <- (sz==levels(sz)[2])
    mbyK <- matrix(m[s2], ncol=K, byrow=T) 
    ybyK <- matrix(y[s2], ncol=K, byrow=T)    
    muobs <- apply(ybyK, 2, sum)/apply(mbyK, 2, sum)
  s2winner <- as.numeric( which.is.max(muobs) )
  s3 <- (sz==levels(sz)[3])
    mbyK <- matrix(m[s3], ncol=K, byrow=T) 
    ybyK <- matrix(y[s3], ncol=K, byrow=T)    
    muobs <- apply(ybyK, 2, sum)/apply(mbyK, 2, sum)
  s3winner <- as.numeric( which.is.max(muobs) )    

  klabels <- (1:(J*K)) %% K
  klabels[klabels==0] <- K
  w <- rep(0, J*K)
  w[s1] <- as.numeric(klabels[s1] == s1winner)
  w[s2] <- as.numeric(klabels[s2] == s2winner)
  w[s3] <- as.numeric(klabels[s3] == s3winner)
    
  return(list(w=w))
}

policy.epsgreedy.pooled12 <- function(m,y,eps,...){
  wraw <- policy.greedy.pooled12(m,y)$w
  w <- eps*1/K + (1-eps)*wraw    
  return(list(w=w))
}


policy.greedy.always <- function(m,y,always,...){
  #always play action numbered "always"=1,2,...,or K
  J <- nrow(d)/K
  mbyK <- matrix(m, ncol=K, byrow=T) 
  ybyK <- matrix(y, ncol=K, byrow=T)    
  whichcol <- rep(always, J)
  wJbyK <- matrix(0, nrow=J, ncol=K)
  wJbyK[ cbind(1:J, whichcol) ] <- 1
  w <- as.vector( t(wJbyK) )
  return(list(w=w))
}

policy.greedy.unpooled <- function(m,y,...){
  J <- nrow(d)/K
  mbyK <- matrix(m, ncol=K, byrow=T) 
  ybyK <- matrix(y, ncol=K, byrow=T)    
  mgt0 <- which(apply(mbyK,1,sum) > 0)
  muobsmat <- ybyK[mgt0,]/mbyK[mgt0,]
  whichcol <- as.numeric( apply(muobsmat,1,which.is.max) )
  wJbyK <- matrix(0, nrow=J, ncol=K)
  wJbyK[ cbind(mgt0, whichcol) ] <- 1
  wJbyK[ which(apply(mbyK,1,sum)==0), ] <- 1/K
  w <- as.vector( t(wJbyK) )
  return(list(w=w))
}

policy.epsgreedy.unpooled <- function(m,y,eps,...){
  wraw <- policy.greedy.unpooled(m,y)$w
  w <- eps*1/K + (1-eps)*wraw
  return(list(w=w))
}


policy.softmax.unpooled <- function(m,y,...){
  J <- nrow(d)/K
  mbyK <- matrix(m, ncol=K, byrow=T) 
  ybyK <- matrix(y, ncol=K, byrow=T)    
  mgt0 <- which(apply(mbyK,1,sum) > 0)
  meq0 <- which(apply(mbyK,1,sum)==0)
  Vmat <- ybyK[mgt0,]/mbyK[mgt0,]
  tune <- mean(Vmat)
  expVmat <- exp(Vmat/tune)
  softmaxmat <- expVmat/apply(expVmat,1,sum)
  wJbyK <- matrix(0, nrow=J, ncol=K)
  wJbyK[ mgt0, ] <- softmaxmat
  wJbyK[ meq0, ] <- 1/K
  w <- as.vector( t(wJbyK) )
  return(list(w=w))
}

gittins.brezzi.lai <- function( post.mean, post.var, varY, disc) {
  # post.mean = E[theta|Y]
  # post.var  = Var[theta|Y]
  # varY = Var[ Y | theta=post.mean], Variance of likelihood of Y as a function of post.mean
  # disc = e^{-c}  <-->  c = -ln(disc), disc:= discount rate approaches 1.0
  # s = post.var / (c * varY)
  # Psi.func(s):= piecwise non-linear function of s
  # Gittins Index approximation =  post.mean + sqrt(post.var)*psi.func(s)

  #======== Example: Bernoulli Bandit ========
  #a <- 2.439
  #b <- 238.411
  #post.mean.beta <- a/(a+b)
  #post.var.beta <-  a*b/( (1+a+b)*(a+b)^2 )
  #varY.binom <- post.mean.beta*(1-post.mean.beta)
  #gittins.brezzi.lai( post.mean=post.mean.beta, post.var=post.var.beta, varY=varY.binom, disc=.99)
  #[1] 0.01217715
  #==========================

  psi.func <- function(sv){
    psi <- rep(NA,length(sv))
    for (i in 1:length(sv)) {
      s <- sv[i]
      if (s <= 0.2) psi[i]<- sqrt(s/2)
      else if (s <= 1) psi[i]<- 0.49 - 0.11*s^{-.5}
      else if (s <= 5) psi[i]<- 0.63 - 0.26*s^{-.5}
      else if (s <=15) psi[i]<- 0.77 - 0.58*s^{-.5}
      else  (  psi[i]<- 2*log(s) -log(log(s)) -log(16*pi)   )^{.5}
    }
    return(psi)
  }

  c <- -log( disc )
  s <- post.var/(varY*c)
  return( post.mean + sqrt(post.var)*psi.func(s) )

}


policy.gittins.pooled12 <- function(m,y,...){
# d$sizeandconcept  (12 unique actions)
# d$unitid  (59 unique units)
# d$placeid (133 unique places)
# d$adsize  (3 unique sizes)
  sz <- d$adsize
  J <- nrow(d)/K

  s1 <- (sz==levels(sz)[1])
    mbyK <- matrix(m[s1], ncol=K, byrow=T) 
    ybyK <- matrix(y[s1], ncol=K, byrow=T)    
    muobs <- apply(ybyK, 2, sum)/apply(mbyK, 2, sum)
    ## Gittins
      # Bayes updates for Posterior of arm's propensity # Beta prior,posterior
      a <- 1 ; b <- 1 ;
      yyy <- apply(ybyK, 2, sum)
      nnn <- apply(mbyK, 2, sum)
      a.update <- a + yyy
      b.update <- b + nnn - yyy
      beta.mean <- a.update/(a.update + b.update)
      beta.var  <- a.update*b.update/( (a.update + b.update)^2 * (a.update + b.update + 1) )
      gittins_vec <- gittins.brezzi.lai(post.mean=beta.mean, post.var=beta.var, varY=beta.mean*(1-beta.mean) , disc=.99)
  s1winner <- as.numeric( which.is.max(gittins_vec) )
  
  s2 <- (sz==levels(sz)[2])
    mbyK <- matrix(m[s2], ncol=K, byrow=T) 
    ybyK <- matrix(y[s2], ncol=K, byrow=T)    
    muobs <- apply(ybyK, 2, sum)/apply(mbyK, 2, sum)
    ## Gittins
      # Bayes updates for Posterior of arm's propensity # Beta prior,posterior
      a <- 1 ; b <- 1 ;
      yyy <- apply(ybyK, 2, sum)
      nnn <- apply(mbyK, 2, sum)
      a.update <- a + yyy
      b.update <- b + nnn - yyy
      beta.mean <- a.update/(a.update + b.update)
      beta.var  <- a.update*b.update/( (a.update + b.update)^2 * (a.update + b.update + 1) )
      gittins_vec <- gittins.brezzi.lai(post.mean=beta.mean, post.var=beta.var, varY=beta.mean*(1-beta.mean) , disc=.99)
  s2winner <- as.numeric( which.is.max(gittins_vec) )
  
  s3 <- (sz==levels(sz)[3])
    mbyK <- matrix(m[s3], ncol=K, byrow=T) 
    ybyK <- matrix(y[s3], ncol=K, byrow=T)    
    muobs <- apply(ybyK, 2, sum)/apply(mbyK, 2, sum)
    ## Gittins
      # Bayes updates for Posterior of arm's propensity # Beta prior,posterior
      a <- 1 ; b <- 1 ;
      yyy <- apply(ybyK, 2, sum)
      nnn <- apply(mbyK, 2, sum)
      a.update <- a + yyy
      b.update <- b + nnn - yyy
      beta.mean <- a.update/(a.update + b.update)
      beta.var  <- a.update*b.update/( (a.update + b.update)^2 * (a.update + b.update + 1) )
      gittins_vec <- gittins.brezzi.lai(post.mean=beta.mean, post.var=beta.var, varY=beta.mean*(1-beta.mean) , disc=.99)
  s3winner <- as.numeric( which.is.max(gittins_vec) )    

  klabels <- (1:(J*K)) %% K
  klabels[klabels==0] <- K
  w <- rep(0, J*K)
  w[s1] <- as.numeric(klabels[s1] == s1winner)
  w[s2] <- as.numeric(klabels[s2] == s2winner)
  w[s3] <- as.numeric(klabels[s3] == s3winner)
    
  return(list(w=w))
}


policy.gittins.unpooled <- function(m,y,...){
  J <- nrow(d)/K
  mbyK <- matrix(m, ncol=K, byrow=T) 
  ybyK <- matrix(y, ncol=K, byrow=T)    
  mgt0 <- which(apply(mbyK,1,sum) > 0)
  meq0 <- which(apply(mbyK,1,sum)==0)
    ## Gittins
      # Bayes updates for Posterior of arm's propensity # Beta prior,posterior
      a <- 1 ; b <- 1 ;
      yyy <- ybyK[mgt0,]
      nnn <- mbyK[mgt0,]
      a.update <- a + yyy
      b.update <- b + nnn - yyy
      beta.mean <- a.update/(a.update + b.update)
      beta.var  <- a.update*b.update/( (a.update + b.update)^2 * (a.update + b.update + 1) )
      gittins.mat <- gittins.brezzi.lai(post.mean=beta.mean, post.var=beta.var, varY=beta.mean*(1-beta.mean) , disc=.99)
  whichcol <- as.numeric( apply(gittins.mat,1,which.is.max) )
  wJbyK <- matrix(0, nrow=J, ncol=K)
  wJbyK[ cbind(mgt0, whichcol) ] <- 1
  wJbyK[ meq0, ] <- 1/K
  w <- as.vector( t(wJbyK) )
  return(list(w=w))
}


policy.UCB1.unpooled <- function(m,y,...){
  J <- nrow(d)/K
  mbyK <- matrix(m, ncol=K, byrow=T) 
  ybyK <- matrix(y, ncol=K, byrow=T)    
  mgt0 <- which(apply(mbyK,1,sum) > 0)
  meq0 <- which(apply(mbyK,1,sum)==0)
    ## UCB1
      yyy <- ybyK[mgt0,]
      nnn <- mbyK[mgt0,]
      rowsum_m <- apply(nnn,1,sum)
      ov <- sqrt(2*log(rowsum_m)/nnn)   #option value
      ucb.mat <- yyy/nnn + ov
  whichcol <- as.numeric( apply(ucb.mat,1,which.is.max) )
  wJbyK <- matrix(0, nrow=J, ncol=K)
  wJbyK[ cbind(mgt0, whichcol) ] <- 1
  wJbyK[ meq0, ] <- 1/K
  w <- as.vector( t(wJbyK) )
  return(list(w=w))
}

policy.UCBtuned.unpooled <- function(m,y,...){
  J <- nrow(d)/K
  mbyK <- matrix(m, ncol=K, byrow=T) 
  ybyK <- matrix(y, ncol=K, byrow=T)    
  mgt0 <- which(apply(mbyK,1,sum) > 0)
  meq0 <- which(apply(mbyK,1,sum)==0)
    ## UCBtuned
      yyy <- ybyK[mgt0,]
      nnn <- mbyK[mgt0,]
      rowsum_m <- apply(nnn,1,sum)
        Var_upbd_mat <- nnn*(yyy/nnn)*(1-yyy/nnn) + sqrt(2*log(rowsum_m)/nnn)
        Var_upbd_or_25_vec <- apply( cbind( c(Var_upbd_mat), c(nnn)*1/4 ), 1, min)
        Var_upbd_or_25_mat <- matrix(Var_upbd_or_25_vec, ncol=K, byrow=F)
      ov <- sqrt( Var_upbd_or_25_mat * log(rowsum_m)/nnn )   #option value
      ucb.mat <- yyy/nnn + ov
  whichcol <- as.numeric( apply(ucb.mat,1,which.is.max) )
  wJbyK <- matrix(0, nrow=J, ncol=K)
  wJbyK[ cbind(mgt0, whichcol) ] <- 1
  wJbyK[ meq0, ] <- 1/K
  w <- as.vector( t(wJbyK) )
  return(list(w=w))
}

 
policy.exp.then.exp <- function(hinit,policytouse,...) {
# hinit
#
#Mhobs.cur <- Mhobs.combo
#mutruemat
#d
#J 
#K 
#hend  
K <- 4
yhstar.all <- array(0, dim=c(J*K,hend,nworlds))
mhstar.all <- array(0, dim=c(J*K,hend,nworlds))
wstarend <- matrix(0, J*K, nworlds)
for (i in 1:nworlds){
  yhstar <- matrix(0,J*K,hend)
  mhstar <- matrix(0,J*K,hend)
  wstar <- matrix(0,J*K,hend)    
 # explore for hinit periods
  wmat.explore <- matrix(1/K, nrow=J*K, ncol=hinit)
  wstar[,1:hinit] <- wmat.explore
  mhstar.explore <- round(Mhobs.cur[,1:hinit]*wmat.explore)
  mhstar[,1:hinit] <- mhstar.explore 
  yhstar[,1:hinit] <- rbinom( J*K*hinit, mhstar.explore, mutruemat[,1:hinit] )
  if (hinit==1) {mtstar.hinit <- mhstar[,1]; ytstar.hinit <- yhstar[,1]; 
  } else { 
  mtstar.hinit <- apply(mhstar[,1:hinit],1,sum)
  ytstar.hinit <- apply(yhstar[,1:hinit],1,sum)  
  }
 # after explore, identify winner
  res <- policytouse(m=mtstar.hinit,y=ytstar.hinit)
  w <- res$w
 # then exploit for hremain periods
  hremain = length( (hinit+1):hend )
  wmat.exploit <- matrix(w, nrow=J*K, ncol=hremain, byrow=F) 
  wstar[,(hinit+1):hend] <- wmat.exploit
  mhstar.exploit <- round(Mhobs.cur[,(hinit+1):hend]*wmat.exploit)
  mhstar[,(hinit+1):hend ] <- mhstar.exploit
  yhstar[,(hinit+1):hend ] <- rbinom( J*K*hremain, mhstar.exploit, mutruemat[,(hinit+1):hend] )
  
 #store
  mhstar.all[,,i] <- mhstar
  yhstar.all[,,i] <- yhstar   
  wstarend[,i] <- wstar[,hend]
}
 
ystarovertime <- apply(yhstar.all,3:2,sum) 
mstarovertime <- apply(mhstar.all,3:2,sum)
ystarovertimecum <- t( apply(ystarovertime,1,cumsum) )
mstarovertimecum <- t( apply(mstarovertime,1,cumsum) )
pstarovertimecum <- ystarovertimecum/mstarovertimecum

ytotal <- apply(yhstar.all,3,sum)
mtotal <- apply(mhstar.all,3,sum)
ptotal <- ytotal/mtotal



return( list(ptotal=ptotal, 
             pstarovertimecum=pstarovertimecum,
             wstarend=wstarend ) )
} # END  function policy.exp.then.exp 


BBym2w <- function(ym,...){
  # length(ym) = 2*K
  y <- ym[1:K]
  m <- ym[(K+1):(2*K)]
  a0 <- 1
  b0 <- 1
  psamp <- matrix( rbeta(nsamp*K, a0+y, b0+m-y) , ncol=K, byrow=T)
  kstar <- factor( apply(psamp,1,which.max), levels=1:K )
  w <- as.numeric( table(kstar)/nsamp )
}  

policy.RPM.BB4 <- function(m,y,...){
  J <- nrow(d)/K
  mbyK <- matrix(m, ncol=K, byrow=T) 
  ybyK <- matrix(y, ncol=K, byrow=T)   
  ysum <-  apply(ybyK, 2, sum)
  msum <- apply(mbyK, 2, sum)
  # update beliefs
  a0 <- 1
  b0 <- 1
  psamp <- matrix( rbeta(nsamp*K, a0+ysum, b0+msum-ysum) , ncol=K, byrow=T)
  # prob win  
  kstar <- factor( apply(psamp,1,which.max), levels=1:K )
  w <- c( as.numeric( table(kstar)/nsamp ) )
  return(list(w=w))
}

policy.RPM.BB12 <- function(m,y,...){
# d$adsize  (3 unique sizes)
  sz <- d$adsize
  J <- nrow(d)/K
  w <- rep(0,nrow(d))
  s1 <- (sz==levels(sz)[1])
  w[s1] <- policy.RPM.BB4(m=m[s1],y=y[s1],...)$w
  s2 <- (sz==levels(sz)[2])
  w[s2] <- policy.RPM.BB4(m=m[s2],y=y[s2],...)$w
  s3 <- (sz==levels(sz)[3])
  w[s3] <- policy.RPM.BB4(m=m[s3],y=y[s3],...)$w
  return(list(w=w))
}

policy.RPM.BB4.unpooled <- function(m,y,...){
  #m,y are ordered by j=1:J and within each j ordered by k=1:K
  J <- nrow(d)/K
  w <- rep(0,J*K)
  mJbyK <- matrix(m, nrow=J, ncol=K, byrow=T) 
  yJbyK <- matrix(y, nrow=J, ncol=K, byrow=T)    
  wKbyJ <- apply(cbind(yJbyK,mJbyK), 1, BBym2w)  # value is K by J matrix
  w <- c(wKbyJ) # converts matrix to vector by stacking columns j=1,...,J of length K
  return(list(w=w))
}



#
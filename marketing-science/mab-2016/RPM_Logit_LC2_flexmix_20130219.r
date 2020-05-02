
library(MASS)
library(flexmix)
n.iters <- 1000

policy.RPM.Logit.LC2 <- function(m,y,...){

  d$impressions <- m
  d$conversions <- y

  d$arm <- d$sizeandconcept
  arm.names <- levels(d$arm)

  y.perarm <- tapply(d$conversions, list(d$arm), sum)
  m.perarm <- tapply(d$impressions, list(d$arm), sum)
  mu.obs <- y.perarm/m.perarm

  concept <- d$concept
  size <- d$size
  concept_each <- factor(rep(levels(concept),3))
  size_each <- factor(rep(levels(size),each=4))
  creative.names <- paste(size_each, concept_each, sep="_")
  designmatrix.interactions <- model.matrix( ~ size_each*concept_each)[,-1]
  rownames(designmatrix.interactions) <- creative.names

model2seg.interactions <- stepFlexmix(cbind(conversions,impressions-conversions) ~ 1+ size*concept | unitid,
            model = FLXMRglmfix(family = "binomial",fixed = ~1 ),
            data = d, k = 2, nrep = 5,
            control = list(iter=20, verbose=0, classify="weighted")
            )
model2seg.interactions.refit <- refit(model2seg.interactions)

mi <- model2seg.interactions
miref <- model2seg.interactions.refit
ncoef <- length(miref@coef)-1

#miref@coef
#miref@vcov
#miref@coef[(ncoef+1)]
#miref@coef[-(ncoef+1)]
#miref@vcov[-(ncoef+1),-(ncoef+1)]
#posterior(mi)[,2]
#summary(miref)
#miref@coef[(ncoef+1)]
#sqrt( miref@vcov[(ncoef+1),(ncoef+1)] )
#miref@vcov[(ncoef+1),(ncoef+1)]

mu.wgtavg <- apply( fitted(mi)*posterior(mi) , 1, sum)
mu.obs <- d$conversions/d$impressions

latent.class.assignment <- clusters(mi)
latent.class.postprob <- posterior(mi)

# for each segment
idx.1 <- 1:(ncoef/2)
idx.2 <- (ncoef/2+1):ncoef

beta.glm.mean.1 <- miref@coef[idx.1]
beta.glm.se.1 <- sqrt(diag(miref@vcov[idx.1,idx.1]))
beta.glm.varcov.1 <- miref@vcov[idx.1,idx.1]

beta.glm.mean.2 <- miref@coef[idx.2]
beta.glm.se.2 <- sqrt(diag(miref@vcov[idx.2,idx.2]))
beta.glm.varcov.2 <- miref@vcov[idx.2,idx.2]


### ------  estimates2allocprobs( ) ----- ###
### ---- FUNCTION STAR ----- ###
estimates2allocprobs <- function(beta.glm.mean, beta.glm.varcov, desmat=designmatrix.interactions, ...){ # begin
  beta.glm.samp <- mvrnorm(n.iters, beta.glm.mean, beta.glm.varcov)
  zstar.samp <- beta.glm.samp %*% t(cbind(rep(1,nrow(desmat)),desmat))
  zstar.which.max <- factor( apply(zstar.samp, 1, which.max), levels=1:ncol(zstar.samp) )
  alloc.probs <- as.numeric( table( zstar.which.max )/dim(zstar.samp)[1] )

  zstar.which.max.size1 <- factor( apply(zstar.samp[,1:4],1,which.max), levels=1:4 )
  zstar.which.max.size2 <- factor( apply(zstar.samp[,5:8],1,which.max), levels=1:4 )
  zstar.which.max.size3 <- factor( apply(zstar.samp[,9:12],1,which.max), levels=1:4 )
  alloc.probs.size1 <- as.numeric( table( zstar.which.max.size1 )/dim(zstar.samp)[1] )
  alloc.probs.size2 <- as.numeric( table( zstar.which.max.size2 )/dim(zstar.samp)[1] )
  alloc.probs.size3 <- as.numeric( table( zstar.which.max.size3 )/dim(zstar.samp)[1] )

  zstar.byarm.post.mean <- apply( zstar.samp, 2, mean )
  p.samp <- 1/( 1 + exp(-zstar.samp) )
  z.samp <- zstar.samp
  prob.byarm.post.mean <- 1/( 1 + exp(-zstar.byarm.post.mean) )
  alloc.probs.byconcept <- c(sum( alloc.probs[ c(1,5,9) ] ),
                            sum( alloc.probs[ c(2,6,10) ] ),
                            sum( alloc.probs[ c(3,7,11) ] ),
                            sum( alloc.probs[ c(4,8,12) ] ) )
  alloc.probs.withinsize <- c(alloc.probs.size1, alloc.probs.size2, alloc.probs.size3 )
  return(list(p.samp=p.samp,
              z.samp=z.samp,
              alloc.probs=alloc.probs,
              alloc.probs.byconcept=alloc.probs.byconcept,
              alloc.probs.withinsize=alloc.probs.withinsize)
         )
} #
### ---- FUNCTION END ----- ###


psamplist.seg1 <- estimates2allocprobs(beta.glm.mean.1, beta.glm.varcov.1, desmat=designmatrix.interactions)
psamplist.seg2 <- estimates2allocprobs(beta.glm.mean.2, beta.glm.varcov.2, desmat=designmatrix.interactions)

p.samp.seg1 <- psamplist.seg1$p.samp
z.samp.seg1 <- psamplist.seg1$z.samp
alloc.probs.seg1 <- psamplist.seg1$alloc.probs
alloc.probs.byconcept.seg1 <- psamplist.seg1$alloc.probs.byconcept
alloc.probs.withinsize.seg1 <- psamplist.seg1$alloc.probs.withinsize

p.samp.seg2 <- psamplist.seg2$p.samp
z.samp.seg2 <- psamplist.seg2$z.samp
alloc.probs.seg2 <- psamplist.seg2$alloc.probs
alloc.probs.byconcept.seg2 <- psamplist.seg2$alloc.probs.byconcept
alloc.probs.withinsize.seg2 <- psamplist.seg2$alloc.probs.withinsize

# create space for resampling within each j
d.long <- d[ rep(1:nrow(d), each=n.iters), ]
d.long$iter <- rep(1:n.iters, nrow(d))
d.long$p.samp <- rep(0, nrow(d)*n.iters)
d.long$z.samp <- rep(0, nrow(d)*n.iters)

# draw latent class assignments using seg2 probabilities
d.long$latent.class.postprob.seg2 <- latent.class.postprob[ rep(1:nrow(d), each=n.iters), 2]
d.long$segnum <- rbinom(nrow(d.long), d.long$latent.class.postprob.seg2, size=1) + 1
d.long$segment <- paste("segment",d.long$segnum,sep="")

# combine p sampled from each segment in proportion to membership prob to each segment
d.long$adsize <- d.long$size
K <- length(arm.names)
for (k in 1:K){
    curobs.long.1 <- d.long$arm==arm.names[k] & d.long$segment=="segment1"
    curobs.long.2 <- d.long$arm==arm.names[k] & d.long$segment=="segment2"
    d.long$z.samp[curobs.long.1] <- sample(z.samp.seg1[,k],size=sum(curobs.long.1),replace=T)
    d.long$z.samp[curobs.long.2] <- sample(z.samp.seg2[,k],size=sum(curobs.long.2),replace=T)
}


# obtain combined w, looping over j
w <- rep(0,nrow(d))
place <- d$placeid
for ( curplace in levels(place) ){
  curobs.long <- which(d.long$placeid == curplace)
  zsampperj <- matrix( d.long$z.samp[curobs.long], nrow=n.iters, ncol=4, byrow=F )
  zsampperj.whichmax <- factor( apply(zsampperj,1,which.max) , levels=1:4 )
  wcur <- as.numeric( table( zsampperj.whichmax ) )/n.iters
  curobs <- which(d$placeid == curplace)
  w[ curobs ] <- wcur
}

return(list(w=w))

} ### END FUNCTION  policy.RPM.Logit.LC2

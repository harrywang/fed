
# logistic regression (no heterogeneity)
policy.RPM.Logit.Homog <- function(m,y,...){

  d$impressions <- m
  d$conversions <- y

  me <- glm(cbind(conversions,impressions-conversions) ~ size + concept + size:concept, 
           family=binomial(link="logit"), data=d)
  
  
  d.long <- d[ rep(1:nrow(d), each=n.iters), ]
  d.long$iternum <- rep( 1:n.iters, nrow(d) )
   
concept.names <- levels(d$concept)
concept_each <- factor(rep(levels(d$concept),3))
size_each <- factor(rep(levels(d$size),each=4))
creative.names <- paste(size_each, concept_each, sep="_")
designmatrix <- cbind(model.matrix( ~ size_each )[,-1],
                      model.matrix( ~ concept_each)[,-1]
                      )

designmatrix.interactions <- model.matrix( ~ size_each*concept_each)[,-1]
rownames(designmatrix.interactions) <- creative.names
desmat <- designmatrix.interactions


fixef.est.coef <- coef(me)
fixef.est.varcov <- vcov(me)
medf.fixef <- data.frame( fixef.est.coef )
names(medf.fixef) <- "est.mean"
medf.fixef$est.se <- sqrt(diag( fixef.est.varcov ))

fixef.coef.samp <- mvrnorm(n.iters, fixef.est.coef, fixef.est.varcov)
fixef.linpred.samp.wide <- fixef.coef.samp  %*% t(cbind(rep(1,nrow(desmat)),desmat))

fixef.linpred.samp <- rep(0, nrow(d)*n.iters)
for (place in levels(d$placeid) ){
  for (conc in levels(d$concept) ){
    curobslong <- which(d.long$placeid==place & d.long$concept==conc)
    sandc <- unique(d.long$sizeandconcept[curobslong])
    fixef.linpred.samp[ curobslong ] <- fixef.linpred.samp.wide[,sandc]
  }
}

d.long$fixef.linpred.samp <- fixef.linpred.samp
d.long$linpred.samp <- d.long$fixef.linpred.samp 

d.long$iter <- paste("iter", rep(n.iters*10+ 1:n.iters , nrow(d) ) ,sep="")
d.long$placeid_iter <- paste(d.long$placeid, d.long$iter, sep="_")

zstar.samp <- d.long$linpred.samp
zstar.which.max.perplaceid <- tapply(d.long$linpred.samp, INDEX=d.long$placeid_iter, FUN=which.max)
zstar.which.max.perplaceid.table <- table( rep(levels(d$placeid),each=n.iters), factor(zstar.which.max.perplaceid, levels=c(1,2,3,4)) )

alloc.probs.table <- zstar.which.max.perplaceid.table/n.iters
            
alloc.probs.mat <- matrix( as.numeric(alloc.probs.table), ncol=4 , byrow=F)
outallocprob <- c( t(alloc.probs.mat) )

w <- outallocprob

#fixef.coef.samp
out <- list(w=w, paramvec=fixef.coef.samp)            
return(out)
}

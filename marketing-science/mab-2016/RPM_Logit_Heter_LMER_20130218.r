
## RPM - Hierarchical GLM  ##
# logistic regression heterogeneity on coefficients
policy.RPM.Logit.Heter <- function(m,y,...){

  d$impressions <- m
  d$conversions <- y
  
  me <- glmer( cbind(conversions,impressions-conversions) ~ size + concept + size:concept + (0+concept|placeid),
              family=binomial(link="logit"), data=d,  glmerControl(optCtrl = list(maxfun=1000)) )

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

est.fitted <- fitted(me)

vc <- VarCorr(me)
ranef.se <- sqrt( diag(vc$placeid) )
names(ranef.se) <- paste("ranef_se_", names(diag(vc$placeid)),sep="")

ranef.mean.mat <- ranef(me)$placeid
ranef.se.mat <- se.ranef(me)$placeid
ranef.est.mean <- c(t(ranef.mean.mat))
ranef.est.var <- c(t(ranef.se.mat^2))
ranef.coef.samp.wide <- mvrnorm(n.iters, ranef.est.mean, diag(ranef.est.var) ) #mvrnorm(n, mu, Sigma)
ranef.coef.samp <- c(ranef.coef.samp.wide) # append column over column
d.long$ranef.coef.samp <- ranef.coef.samp


fixef.est.coef <- fixef(me)
fixef.est.varcov <- vcov(me)
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


# sample linear predictors for whole dataset from whole model
linpred.samp <- fixef.linpred.samp + ranef.coef.samp
cvr.samp <- exp(linpred.samp)/( 1 + exp(linpred.samp) )

d.long$linpred.samp <- linpred.samp
d.long$cvr.samp <- cvr.samp

d.long$iter <- paste("iter", rep(n.iters*10+ 1:n.iters , nrow(d) ) ,sep="")
d.long$placeid_iter <- paste(d.long$placeid, d.long$iter, sep="_")

zstar.samp <- linpred.samp
zstar.which.max.perplaceid <- tapply(zstar.samp, INDEX=d.long$placeid_iter, FUN=which.max)
zstar.which.max.perplaceid.table <- table( zstar.which.max.perplaceid, rep(levels(d$placeid),each=n.iters) )

alloc.probs.table <- zstar.which.max.perplaceid.table/n.iters

outallocprob <- as.numeric( zstar.which.max.perplaceid.table /n.iters)

w <- outallocprob
paramvec <- data.frame( coef=fixef.coef.samp , linpred=matrix(linpred.samp,nrow=n.iters) )

return(list(w=w,paramvec=paramvec))

}

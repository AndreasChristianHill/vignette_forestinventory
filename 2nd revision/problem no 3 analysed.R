
# Reviewer's problem No. 3:

crit.height<- 320

set.seed(0)
d <- data.frame(
  elv = (e<-runif(500, 300, 550)),     # explanatory variable (observed in s1)
  vol1 = rnorm(e, mean=600-e, sd=100), # response variables (to be observed in s2 only)
  #vol2 = rnorm(e, mean=300, sd=100)*(e<310),
  vol2 = rnorm(e, mean=300, sd=100)*(e<crit.height),
  resp3 = runif(e) < 0.03)


# repeat manually to see what happens to data and model R2:
d$phase <- 1
d$phase[sample(length(d$phase), 80)] <- 2
plot(vol2 ~ elv, data = d[d$phase==2,])
print("mod1 (red)", quote=F);summary(mod1<- lm( vol2 ~ elv, data = d[d$phase==2, ]))$r.squared # fit without masking zeros (thesis: higher R2 than mod2)
print("mod2 (green)", quote=F);summary(mod2<- lm( vol2 ~ elv, data = d[d$phase==2 & d$elv< crit.height, ]))$r.squared  # fit after masking zeros
abline(mod1, col="red")
abline(mod2, col="green")






































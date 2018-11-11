# Editor: The editor does not understand why the authors were not able to reproduce the code examples given by the reviewer. Is there a bug, or just a misunderstanding between authors on one side and reviewer + editor on the other? Please check the code and explain.
# 
# Reviewer A:
#   In the revised version of the manuscript (ID 3280), the authors include a new discussion on coverage rates of confidence intervals (p. 25), which, however, appears to only partially embrace the methodological concerns I raised in the earlier review. In particular, the statement that "the nominal coverage rate of 95% can [be] expected to be met for large sample sizes (e.g., n2=50)" is in conflict with examples 2 and 3 included from my review (n2=80, coverage rate ≤ 85%). I am not convinced by the authors' claim that they could not reproduce the examples.
# 
# On the other hand, the new discussion and in particular Figure 5 are very helpful in illustrating the issue of coverage rates, and I believe this will for most readers be sufficient to make them aware the related problems.
# 
# Based on my rather short reading of the manuscript and given that a formal review is not required at this stage (after previous recommendation "Revisions Required"), I would therefore agree that the manuscript can now be accepted.
# 
# #####
# 
# 
# Response:
# The reviewers code runs without bugs but the simulation was incorrectly formulated.  First of all, the three examples were posed in the finite population context rather than the infinite population context.  The package was written using estimators derived specifically in the infinite population approach.  Let's look at the first simulation example to see the fundamental problem with doing this:
  
  ###
  # Reviewer's artificial data:
  ###
  set.seed(0)
d <- data.frame(
  elv = (e<-runif(500, 300, 550)),     # explanatory variable (observed in s1)
  vol1 = rnorm(e, mean=600-e, sd=100), # response variables (to be observed in s2 only)
  vol2 = rnorm(e, mean=300, sd=100)*(e<310),
  resp3 = runif(e) < 0.03
)


# COMMENT: Right away we have a problem because in this example N=500 not infinity.  We have a similar problem with the auxiliary variable "elv" which seems to indicate that n1=500 despite the fact that the first phase is not being resampled between simulation replicates. This mistake in the formulation is present in all three examples provided by the reviewer. Let's take a look at the first example to see how:

library(forestinventory)

# PROBLEM 1: Small sample size (n2=5)
true_value <- mean(d$vol1) 
cover <- replicate(10000, {
d$phase <- 1
d$phase[sample(length(d$phase), 5)] <- 2
ci <- confint( twophase( vol1 ~ elv, data = d,
phase_id = list(phase.col="phase", terrgrid.id=2) ), level=0.95 )
ci$ci$ci_lower_g <= true_value & true_value <= ci$ci$ci_upper_g
} )
mean(cover)

# COMMENT: The true value should be an integral (i.e. spatial mean over infinite points) but by using the finite mean, the reviewer has effectively acknowledged that actually N = 500 instead of infinity.  Even if we ignore this important detail, we then see that every population unit is assigned to the first phase indicating the intention to apply an exhaustive two-phase estimator.  Notice that in the twophase() function the exhaustive parameter is not used making this a non-exhaustive function call.  This means that by simulation design s1 does not get resampled with every replicate so the variability coming from resampling the first phase is ignored.  All 3 examples given by the reviewer made these same errors so the results clearly can't be considered conclusive on their own merits.  This is why we attempted to reformulate the simulation correctly in the infinite population context and mimic the examples intended effects.  
# 
# The following is some sample code (similar code was added to the Appendix in the last submission) to demonstrate simulation in the infinite population context.  This technique has already applied in Mandallaz (2013a), Mandallaz et al. (2013) and Massey et al. (2015).  

############
## Reviewer Problem #1:
############

library(forestinventory)
library(rmutil)  # used to calculate the integral in the spatial mean

set.seed(1)

# --- function to create density surface: --- #
target.surface <- function(x0, y0){
  local.density <- 30 + 13*x0 - 6*y0 - 4*x0^2 + 3*x0*y0 + 2*y0^2 + 6*cos(pi*x0)*sin(pi*y0)
  local.density
}

# --- True spatial mean for Global Surface Area: --- #
true_value <- int2(target.surface, a = c(0,0), b=c(3,2))/6 # 39.16667  #true mean

# --- sample generator for simple two-phase sampling: --- #
sample.generator<- function(n1, n2){
  
  realization <- matrix(NA,n1,6)
  for(i in 1:n1){
    x0=2*runif(1) # NOTE: The randomness comes from the uniform independent selection of points
    y0=3*runif(1)
    realization[i,1] <- target.surface(x0, y0)
    realization[i,2] <- x0
    realization[i,3] <- y0
    realization[i,4] <- x0*x0
    realization[i,5] <- x0*y0
    realization[i,6] <- y0*y0
  }
  realization <- as.data.frame(realization)
  names(realization) <- c("response","x","y","xx","xy","yy")
  realization$phase <- 1
  realization$phase[sample(nrow(realization), n2)] <- 2
  realization$response[realization$phase == 1] <- NA
  realization
}

# --- Run 10000 simulations with n1 = 500 and n2 = 5 --- #
n1 <- 500
n2 <- 5
num_sims <- 10000
ci.g.logical <- rep(NA, num_sims)   # two-phase coverage indicators for g-weight formula
ci.ext.logical <- rep(NA, num_sims) # two-phase coverage indicators for external formula
ci.op.logical <- rep(NA, num_sims)  # one-phase coverage indicators

for(j in 1: num_sims){
  realization <- sample.generator(n1 = n1, n2 = n2)
  est <- twophase(formula = response ~ y + x + xx, data=realization, phase_id = list(phase.col = "phase", terrgrid.id = 2))
  est_onephase <- onephase(formula = response ~ 1, data=realization, phase_id = list(phase.col = "phase", terrgrid.id = 2))
  ci <- confint(est)
  ci_op <- confint(est_onephase)
  ci.g.logical[j] <- ci$ci$ci_lower_g <= true_value & true_value <= ci$ci$ci_upper_g
  ci.ext.logical[j] <- ci$ci$ci_lower_ext <= true_value & true_value <= ci$ci$ci_upper_ext
  ci.op.logical[j] <- ci_op$ci$ci_lower_op <= true_value & true_value <= ci_op$ci$ci_upper_op
}
mean(ci.g.logical)   # 0.6738
mean(ci.ext.logical) # 0.5632
mean(ci.op.logical)  # 0.9457

# COMMENT:  The one-phase estimator has the correct coverage rate even with sample size 5 (it would also have it for n2 = 2) but it will probably have a very large margin of error.  This also means that a true external model with the two-phase estimator would also have the correct coverage rate but nobody uses true external models so it is not supported by the package. The g-weight variance was a little better than external approach with internally fit models, which is normal.  They have lower than nominal coverage rates because the contribution of the variance by calculating the beta vector is based on approximations (i.e. a first order taylor approximation for the g-weight and ignoring it entirely for the external formula) which are not adequate for this sample size.  The paper has already included a graphic with sample code that addresses the sample size issue and the reviewer has already accepted it so we can leave this issue alone for the time being.


############
## Reviewer Problems #2 and 3:
############
# 
# The 2nd reviewer example involves creating a response variable, "vol2", that is 0 for 98.2% of the values and non-zero where the auxiliary information yields a low value.  Presumably this would occur in situations where there is a treeline dependent on elevation and the vast majority of the "forest" is at elevations where no trees can grow.  This this is problematic for the g-weight and external estimators because all plots in s2 where forest actually exists are high leverage outliers.  Problem 3 is actually the exact same example except that the response variable is 1 at the non-zero plots and the non-zero plots are uniformly distributed among the auxiliary variables.  This may occur if say the inventorist wanted to count how many trees are in the desert where there are just a handful of uniformly distributed oases.  Even in this case, the nonzero plots will be highly influential leverage points in the presence of so many zeros.  This issue is easily seen if you look at the residuals of your linear model, which should always be done.  In any case, the R squared would be very close to zero which asks the question why the model is included in the first case.  
# 
# These two examples are also pathological for the one-phase estimator because a significant proportion of the samples drawn will have zeros for ALL of s2 making the estimate = 0 and the confidence interval just a point with no chance of covering the true value.  If this occurred in practice the sample would immediately be considered suspect.  It is hard to imagine a situation where this would accidentally happen.  Furthermore, trying to measure forest volume in areas where 98% of the land has no trees is non-sensical and would be difficult to accidentally do.  An analogous infinite population example of problem 2 is as follows (it would be the same as problem 3):
  
  set.seed(0)
# --- function to create density surface where 98% are zeros: --- #
target.surface <- function(x0, y0){
  local.density <- 30 + 13*x0 - 6*y0 - 4*x0^2 + 3*x0*y0 + 2*y0^2 + 6*cos(pi*x0)*sin(pi*y0)
  if(x0 > 1 | y0 > 0.12){local.density <- 0}  # 1 * 0.12 = 0.12 which is 2% of 6
  local.density
}

true_value <- int2(target.surface, a = c(0,0), b=c(3,2))/6 # 0.6962766  #true mean

# --- Run 10000 simulations with n1 = 500 and n2 = 80 --- #
n1 <- 500
n2 <- 80
num_sims <- 10000
ci.g.logical <- rep(NA, num_sims)
ci.ext.logical <- rep(NA, num_sims)
ci.op.logical <- rep(NA, num_sims)
all.zero.samples <- 0  # initialize counter for all zero s2 samples

for(j in 1: num_sims){
  realization <- sample.generator(n1 = n1, n2 = n2)
  if(sum(realization$response, na.rm = TRUE) == 0){
    all.zero.samples <- all.zero.samples + 1
  }
  est <- twophase(formula = response ~ y + x + xx, data=realization, phase_id = list(phase.col = "phase", terrgrid.id = 2))
  est_onephase <- onephase(formula = response ~ 1, data=realization, phase_id = list(phase.col = "phase", terrgrid.id = 2))
  ci <- confint(est)
  ci_op <- confint(est_onephase)
  ci.g.logical[j] <- ci$ci$ci_lower_g <= true_value & true_value <= ci$ci$ci_upper_g
  ci.ext.logical[j] <- ci$ci$ci_lower_ext <= true_value & true_value <= ci$ci$ci_upper_ext
  ci.op.logical[j] <- ci_op$ci$ci_lower_op <= true_value & true_value <= ci_op$ci$ci_upper_op
}
mean(ci.g.logical)         # 0.791
mean(ci.ext.logical)       # 0.7918
mean(ci.op.logical)        # 0.7973
all.zero.samples/num_sims  # 0.2017

# The two-phase was outperformed by the one-phase estimator indicating an issue with approximating the variance arising from estimating the beta coefficient.  Regardless 20.17% of the samples consisted entirely of zeros which is pathological.

#----------------------------------------------------------------------------------- #
#  Journal of Statistical Software                                                   #
#  Design-Based Global and Small Area Estimations for Multi-phase Forest Inventories #
#  Andreas Hill & Alexander Massey                                                   #
#                                                                                    #
#  Code for running all examples                                                     #
#                                                                                    #
#----------------------------------------------------------------------------------- #

library("forestinventory")

# **************************************************************************** #
# ------ SECTION 3: TWO-PHASE ESTIMATORS -------------------------------------
# **************************************************************************** #


# ----- 3.1 Global Estimations --------


## load grisons data set:
data("grisons", package = "forestinventory")

## see structure of grisons:
str(grisons)

## see first 12 rows of grisons:
head(grisons, n = 12)


# **********************************************************
## non-exhaustive (NEX) estimator with boundary weight adjustment
reg2p_nex <- twophase(formula = tvol ~ mean + stddev + max + q75,
                      data = grisons,
                      phase_id = list(phase.col = "phase_id_2p", terrgrid.id = 2),
                      boundary_weights = "boundary_weights")

## get summary of estimation results:
summary(reg2p_nex)


# *****************************************************
## exhaustive (EX) estimator with boundary weight adjustment
## first, retrieve order of columnames that the lm()-function processes a formula object:
#  --> actually the order they appear in the design-matrix ...
colnames(lm(formula = tvol ~ mean + stddev + max + q75, data = grisons, x = TRUE)$x)

## store true means of explanatory variables:
true.means.Z <- c(1, 11.39, 8.84, 32.68, 18.03)

## apply estimator:
reg2p_ex <- twophase(formula = tvol ~ mean + stddev + max + q75,
                     data = grisons,
                     phase_id = list(phase.col = "phase_id_2p", terrgrid.id = 2),
                     exhaustive = true.means.Z)

## get summary of estimation results:
summary(reg2p_ex)

## get data frame with estimation results
reg2p_ex$estimation



# ----- 3.2 Small Area Estimations --------


# ***************************************
## pseudo small area estimator (PSMALL) 
#  with boundary weight adjustment

## apply estimator:
psmall_2p <- twophase(formula = tvol ~ mean + stddev + max + q75, data = grisons,
                      phase_id = list(phase.col = "phase_id_2p", terrgrid.id = 2),
                      boundary_weights = "boundary_weights",
                      small_area = list(sa.col = "smallarea", areas = c("A", "B"),
                                        unbiased = TRUE),
                      psmall = TRUE)

## get summary of estimation results:
summary(psmall_2p)


# *************************************************
## extended pseudo synthetic estimator (EXTPSYNTH)
#  with boundary weight adjustment

## apply estimator:
extpsynth_2p <- twophase(formula = tvol ~ mean + stddev + max + q75, data = grisons,
                         phase_id = list(phase.col = "phase_id_2p", terrgrid.id = 2),
                         boundary_weights = "boundary_weights",
                         small_area = list(sa.col = "smallarea", areas = c("A", "B"),
                                           unbiased = TRUE))

## get data frame with estimation results:
extpsynth_2p$estimation


# **************************************
## pseudo synthetic estimator (PSYNTH)
#  with boundary weight adjustment

## apply estimator:
psynth_2p <- twophase(formula = tvol ~ mean + stddev + max + q75, data = grisons,
                      phase_id = list(phase.col = "phase_id_2p", terrgrid.id = 2),
                      boundary_weights = "boundary_weights",
                      small_area = list(sa.col = "smallarea", areas = c("A", "B"),
                                        unbiased = FALSE))

## get data frame with estimation results:
psynth_2p$estimation


# ****************************************
## extended synthetic estimator (EXTSYNTH)
# boundary adjustment was already applied when retrieving the exact means

## retrieve order of columnames that the lm()-function processes a formula object:
#  --> actually the order they appear in the design-matrix ...
colnames(lm(formula = tvol ~ mean + stddev + max + q75, data = grisons, x = TRUE)$x)

## store true means of explanatory variables in a data frame:
true.means.Z.G <- data.frame(Intercept = rep(1, 4),
                             mean = c(12.85, 12.21, 9.33, 10.45),
                             stddev = c(9.31, 9.47, 7.90, 8.36),
                             max = c(34.92, 35.36, 28.81, 30.22),
                             q75 = c(19.77, 19.16, 15.40, 16.91))
rownames(true.means.Z.G) <- c("A", "B", "C", "D")

## see how it is supposed to look like:
true.means.Z.G

## apply estimator:
extsynth_2p <- twophase(formula = tvol ~ mean + stddev + max + q75, data = grisons,
                        phase_id = list(phase.col = "phase_id_2p", terrgrid.id = 2),
                        small_area = list(sa.col ="smallarea", areas = c("A", "B"),
                                          unbiased = TRUE),
                        exhaustive = true.means.Z.G)

## get data frame with estimation results:
extsynth_2p$estimation


# **************************************************************************** #
# ------ SECTION 4: THREE-PHASE ESTIMATORS -------------------------------------
# **************************************************************************** #


library("forestinventory")


# ----- 4.1 Global Estimations --------


## load grisons data set:
data("grisons", package = "forestinventory")

## see structure of grisons:
str(grisons)

## see first 12 rows of grisons:
head(grisons, n = 12)

## define reduced and full regression model formulas:
formula.rm <- tvol ~ mean # reduced model applied to s0 phase
formula.fm <- tvol ~ mean + stddev + max + q75 # full model applied to s1 phase

## apply non-exhaustive (NEX) estimator:
reg3p_nex <- threephase(formula.s0 = formula.rm, 
                       formula.s1 = formula.fm, data = grisons,
                       phase_id = list(phase.col="phase_id_3p", s1.id = 1, 
                                       terrgrid.id = 2),
                       boundary_weights = "boundary_weights")

## get summary of estimation results:
summary(reg3p_nex)


# ----- 4.2 Small Area Estimations --------


# ***************************************
## extended synthetic estimator (EXTSYNTH)
#  with boundary weight adjustment

## store true means of explanatory variables of reduced-model in a data frame:
truemeans.G <- data.frame(Intercept = rep(1, 4),
                          mean = c(12.85, 12.21, 9.33, 10.45))
rownames(truemeans.G) <- c("A", "B", "C", "D")

## apply estimator:
extsynth_3p <- threephase(formula.rm, formula.fm, data = grisons,
                          phase_id = list(phase.col = "phase_id_3p", 
                                          s1.id = 1, terrgrid.id = 2),
                          small_area = list(sa.col = "smallarea", areas = c("A", "B"),
                                            unbiased = TRUE),
                          exhaustive = truemeans.G, 
                          boundary_weights = "boundary_weights")

## get summary of estimation results:
summary(extsynth_3p)



# ****************************
## synthetic estimator (SYNTH)
#  with boundary weight adjustment

synth_3p <- threephase(formula.rm, formula.fm, data = grisons,
                       phase_id = list(phase.col="phase_id_3p", s1.id = 1, terrgrid.id = 2),
                       small_area = list(sa.col = "smallarea", areas = c("A", "B"),
                                         unbiased = FALSE),
                       exhaustive = truemeans.G,
                       boundary_weights = "boundary_weights")

## get summary of estimation results:
summary(synth_3p)



# ***********************************************
## extended pseudo synthetic estimator (EXTPSYNTH)
#  with boundary weight adjustment

extpsynth_3p <- threephase(formula.rm, formula.fm, data = grisons,
                           phase_id = list(phase.col = "phase_id_3p", 
                                           s1.id = 1, terrgrid.id = 2),
                           small_area = list(sa.col = "smallarea", areas = c("A", "B"),
                                             unbiased = TRUE),
                           boundary_weights = "boundary_weights")


## get summary of estimation results:
summary(extpsynth_3p)


# **************************************************************************** #
# ------ SECTION 5: CONFIDENCE INTERVALLS -------------------------------------
# **************************************************************************** #


library("forestinventory")

# ----- 5.1 CI for One-Phase Estimation --------

## load grisons dataset:
data(grisons)

## calculate onephase-estimation for entire dataset:
op <- onephase(formula = tvol~1 ,data = grisons,
               phase_id = list(phase.col = "phase_id_2p",terrgrid.id = 2))

## Confidence Intervals:
confint(op)

## calculate one-phase-estimation for given domains (areas) in dataset:
op.a <- onephase(formula = tvol~1,
                 data = grisons,
                 phase_id = list(phase.col = "phase_id_2p", terrgrid.id = 2),
                 area = list(sa.col = "smallarea", areas = c("A", "B")))

## Confidence Intervals:
confint(op.a)


# ----- 5.2 CI for Two-Phase Estimation --------

## calculate two-phase-estimation for entire dataset:
extpsynth_2p <- twophase(formula = tvol ~ mean + stddev + max + q75, data = grisons,
                        phase_id = list(phase.col = "phase_id_2p", terrgrid.id = 2),
                        small_area = list(sa.col = "smallarea", areas = c("A", "B","C", "D"),
                                          unbiased = TRUE))
## Confidence Intervals:
confint(extpsynth_2p)

## change confidence level:
confint(extpsynth_2p, level = 0.9)

## CIs with bonferroni correction to better meet overall coverage probability
#  (multiple testing problem if a large number of CIs are compared to each other)
confint(extpsynth_2p, adjust.method = "bonferroni")



# **************************************************************************** #
# ------ SECTION 6: SPECIAL CASES AND SCENARIOS ------------------------------
# **************************************************************************** #


library("forestinventory")


# ----- 6.1 Post-stratification  --------

## save grisons as new dataset:
grisons.n <- grisons

## create artificial development stages as categorical (factor) variables
#  to be used for post-stratification:
grisons.n$stage <- as.factor(kmeans(grisons.n$mean, centers = 3)$cluster)

## apply 'double sampling for (post)-stratification':
summary(
  twophase(formula = tvol ~ stage,
           data = grisons.n,
           phase_id = list(phase.col = "phase_id_2p", terrgrid.id = 2),
           boundary_weights = "boundary_weights")
)


## apply 'double sampling for regression':
summary(
  twophase(formula = tvol ~ mean + stddev + max + q75 + stage,
           data = grisons.n,
           phase_id = list(phase.col = "phase_id_2p", terrgrid.id = 2),
           boundary_weights = "boundary_weights")
)

## apply 'double sampling for regression within (post)strata'":
summary(
  twophase(formula = tvol ~ mean + stddev + max + q75 + stage,
           data = grisons.n,
           phase_id = list(phase.col = "phase_id_2p", terrgrid.id = 2),
           boundary_weights = "boundary_weights")
)



# ----- 6.2 Small Area Estimation under Cluster Sampling  --------


## apply extended pseudo synthetic estimator under cluster sampling:
extpsynth.clust <- twophase(formula = basal ~ stade + couver + melange, data=zberg,
                           phase_id = list(phase.col = "phase_id_2p", terrgrid.id = 2),
                           cluster = "cluster",
                           small_area = list(sa.col = "ismallold", areas = c("1"),
                                             unbiased = TRUE))
# --> creates warning message: At least one cluster not entirely included within small area

## check mean of residuals in small area:
extpsynth.clust$mean_Rc_x_hat_G


## alternatively apply pseudo small area estimator:
extpsmall.clust <- twophase(formula = basal ~ stade + couver + melange, data=zberg,
                           phase_id = list(phase.col = "phase_id_2p", terrgrid.id = 2),
                           cluster = "cluster",
                           small_area = list(sa.col = "ismallold", areas = c("1"),
                                             unbiased = TRUE),
                           psmall = TRUE)


## compare estimation results:
extpsynth.clust$estimation
extpsmall.clust$estimation

# --> very similar, only minor differences in the variances




# ----- 6.3 Demonstrate error handling in package (examples)  --------


# Checking the nesting of the sample-design:
# 1) --> each s2-point muss have the complete set of s1-auxvars (s1-info) available
# 2) --> each s2-point muss have the complete set of s0-auxvars (s0-info) available
# 3) --> each s1-point muss have the complete set of s0-auxvars (s0-info) available


# 1) & 2) *********************

## save grisons as new dataset:
grisons.n <- grisons

## delete "mean" value from an s2-(i.e. s1- and s0-) sample point:
grisons.n[which(grisons.n$phase_id_3p == 2)[1], "mean"] <- NA

tp <- threephase(formula.s0 = tvol ~ mean,
                formula.s1 = tvol ~  mean + stddev + max + q75,
                data = grisons.n,
                phase_id = list(phase.col="phase_id_3p", s1.id = 1, terrgrid.id = 2),
                boundary_weights = "boundary_weights")

# Violation:
# s2 point misses expl.variable used in s0

# warning message:
# s2 point misses expl.variable used in s1
# since s1 nested in s0, this expl.variables is also missing in s0

# error handling:
# plot deleted


# 1) & 2) *********************

## save grisons as new dataset:
grisons.n <- grisons


## delete "mean" value from an s2-sample point:
grisons.n[which(grisons.n$phase_id_3p == 2)[1], "q75"] <- NA


tp <- threephase(formula.s0 = tvol ~ mean,
                formula.s1 = tvol ~  mean + stddev + max + q75,
                data = grisons.n,
                phase_id = list(phase.col = "phase_id_3p", s1.id = 1, terrgrid.id = 2),
                boundary_weights = "boundary_weights")

# Violation:
# s2 point misses expl.variable used in reduced model at s1 sample points

# warning message:
# s2 point misses expl.variable used in s1
# since s1 nested in s0, this expl.variables is also missing in s0

# error handling:
# s2 point with missing s1-variable recoded to s0-point

# NOTE: This could violate the "randomness" assumption.
#       Be sure that we you have a "s1 expl.variable completely missing at random" case!!!



# 3) **************************

## save grisons as new dataset:
grisons.n <- grisons

## delete "mean"-value from an s0-sample point:
grisons.n[which(grisons.n$phase_id_3p == 0)[1], "mean"] <- NA

tp <- threephase(formula.s0 = tvol ~ mean,
                formula.s1 = tvol ~ mean + stddev + max + q75,
                data = grisons.n,
                phase_id = list(phase.col = "phase_id_3p", s1.id = 1, terrgrid.id = 2),
                boundary_weights = "boundary_weights")

# Violation:
#  an s0 point misses at least one of the explanatory variables used in the reduced model at the s1-sample points

# error handling:
# plot deleted


# 4) **************************

## save grisons as new dataset:
grisons.n <- grisons

## delete "q75"-value from an s1-aux.set:
grisons.n[which(grisons.n$phase_id_3p == 2)[1], "q75"] <- NA

tp <- threephase(formula.s0 = tvol ~ mean,
                formula.s1 = tvol ~  mean + stddev + max + q75,
                data = grisons.n,
                phase_id = list(phase.col="phase_id_3p", s1.id = 1, terrgrid.id = 2),
                boundary_weights = "boundary_weights")

# Violation:
# each s2-point muss have the complete set of s1-auxvars (s1-info) available

# error message:
# missing expl.variable in s1-aux.set at 1 s2-plot.

# error handling:
# since there is still the complete set of s0 available, the phase-id has been changed
# to s0 (by this, we could keep the plot); but we thereby can no longer use the available
# terrestrial info from this plot --> could hamper sae estimation)

# NOTE: This could violate the "randomness" assumption.
#       Be sure that we you have a "completely missing at random" case!!!


#***********************************
# Missing factor level in s2-sample:
zberg.n <- zberg

levels(zberg.n$stade)

table(zberg$phase_id_2p, zberg$stade)

## delete s2-points with "stade"-level '300'
zberg.n<- zberg[-which(zberg.n$phase_id_2p == 2 & zberg.n$stade == "300"), ]
table(zberg.n$phase_id_2p, zberg.n$stade)

# --> We have 300 s1-plot with 'stade'-level '300', but this factor level is
#     missing in the s2-sample (derivation of model not possible)
try(tpc <- twophase(formula = basal ~ stade + couver + melange,
               data = zberg.n,
               phase_id = list(phase.col = "phase_id_2p", terrgrid.id = 2),
               cluster = "cluster"))

# Violation:
# --> We have 300 s1-plot with 'stade'-level '300', but this factor level is
#     missing in the s2-sample (derivation of model not possible)
#     == Nesting violation

# error message:
# factor level ... only available at s1-points, but not at s2-points

# error handling:
# Execution stopped


# **************************************************************************** #
# ------ SECTION 7: ANALYSIS AND VISUALIZATION ------------------------------
# **************************************************************************** #


library("forestinventory")

## calculate some estimations (one-phase, two-phase, three-phase)

op <- onephase(formula = tvol~1, data = grisons,
               phase_id = list(phase.col = "phase_id_2p", terrgrid.id = 2),
               area = list(sa.col = "smallarea", areas = c("A", "B", "C", "D")))

extpsynth_2p <- twophase(formula = tvol ~ mean + stddev + max + q75, data = grisons,
                        phase_id = list(phase.col = "phase_id_2p", terrgrid.id = 2),
                        small_area = list(sa.col = "smallarea", 
                                          areas = c("A", "B","C", "D"),
                                          unbiased = TRUE),
                        boundary_weights = "boundary_weights")

psynth_2p <- twophase(formula = tvol ~ mean + stddev + max + q75, data = grisons,
                      phase_id = list(phase.col = "phase_id_2p", terrgrid.id = 2),
                      small_area = list(sa.col = "smallarea", 
                                        areas = c("A", "B", "C", "D"),
                                        unbiased = FALSE),
                      boundary_weights = "boundary_weights")

extpsynth_3p <- threephase(formula.s0 = tvol ~ mean,
                          formula.s1 = tvol ~ mean + stddev + max + q75, data = grisons,
                          phase_id = list(phase.col = "phase_id_3p", s1.id = 1, terrgrid.id = 2),
                          small_area = list(sa.col = "smallarea", areas = c("A", "B", "C", "D"),
                                          unbiased = TRUE),
                          boundary_weights = "boundary_weights")

psynth_3p <- threephase(formula.s0 = tvol ~ mean,
                       formula.s1 = tvol ~ mean + stddev + max + q75, data = grisons,
                       phase_id = list(phase.col = "phase_id_3p", s1.id = 1, terrgrid.id = 2),
                       small_area = list(sa.col = "smallarea", 
                                         areas = c("A", "B", "C", "D"),
                                         unbiased = FALSE),
                       boundary_weights = "boundary_weights")



# ----- 7.1 estTable()  --------


## aggregate all estimation results using the estTable()-function:
grisons.sae.table <- estTable(est.list = list(op, extpsynth_2p, psynth_2p,
                                             extpsynth_3p, psynth_3p),
                             sae = TRUE, 
                             vartypes = c("variance", "g_variance",  "ext_variance"))
## get class:
class(grisons.sae.table)

## print:
grisons.sae.table

## see structure:
str(grisons.sae.table)

## turn into data.frame:
grisons.sae.df <- data.frame(grisons.sae.table)
class(grisons.sae.df)


# ----- 7.2 mpahse.gain()  --------

## analyse gain of best multi-phase estimation to one-phase estimation:
mphase.gain(grisons.sae.table, pref.vartype = "g_variance")



# ----- 7.3 plot()-method --------

## plot estimation errors:
plot(grisons.sae.table, ncol = 2) 

## plot point estimates and confidence intervals:
library("ggplot2")
plot(grisons.sae.table, ncol = 2, yvar = "estimate") +  
  ylab("Timber Volume [m3/ha]")




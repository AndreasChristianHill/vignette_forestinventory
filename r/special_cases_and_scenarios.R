#---------------------------------------------------------------------------------- #
#  Journal of Statistical Software                                                  #
#  Design-Based Global and Small Area Estimations for Multiphase Forest Inventories #
#  Andreas Hill & Alexander Massey                                                  #
#                                                                                   #
#  Code for running all examples given in                                           #
#  Section 6  "Special Cases and Scenarios"                                         #
#                                                                                   #
#---------------------------------------------------------------------------------- #

library(forestinventory)

#####################################################
#          Poststratification                       #
#####################################################

## save grisons as new dataset:
grisons.n<- grisons

## create artificial development stages as categorical (factor) variables
#  used for poststratification:
grisons.n$stage<- as.factor(kmeans(grisons.n$mean, centers = 3)$cluster)

## plot:
library(ggplot2)
ggplot(data=grisons.n, aes(x = mean, y = tvol)) +
  geom_point(aes(colour = factor(stage)))


## apply 'double sampling for (post)stratification':
summary(
  twophase(formula=tvol ~ stage,
           data=grisons.n,
           phase_id=list(phase.col = "phase_id_2p", terrgrid.id = 2),
           boundary_weights = "boundary_weights")
)


## apply 'double sampling for regression':
summary(
  twophase(formula=tvol ~ mean + stddev + max + q75 + stage,
           data=grisons.n,
           phase_id=list(phase.col = "phase_id_2p", terrgrid.id = 2),
           boundary_weights = "boundary_weights")
)

## apply 'double sampling for regression within (post)strata'":
summary(
  twophase(formula=tvol ~ mean + stddev + max + q75 + stage,
           data=grisons.n,
           phase_id=list(phase.col = "phase_id_2p", terrgrid.id = 2),
           boundary_weights = "boundary_weights")
)


#####################################################
#  SMALL AREA ESTIMATION UNDER CLUSTER SAMPLING     #
#####################################################


## apply extended pseudo synthetic estimator under cluster sampling:
extpsynth.clust<- twophase(formula = basal ~ stade + couver + melange, data=zberg,
                           phase_id = list(phase.col = "phase_id_2p", terrgrid.id = 2),
                           cluster = "cluster",
                           small_area = list(sa.col = "ismallold", areas = c("1"),
                                             unbiased = TRUE))

# --> creates warning message: At least one cluster not entirely included within small area

## check mean of residuals in small area:
extpsynth.clust$mean_Rc_x_hat_G


## alternatively apply pseudo small area estimator:
extpsmall.clust<- twophase(formula = basal ~ stade + couver + melange, data=zberg,
                           phase_id = list(phase.col = "phase_id_2p", terrgrid.id = 2),
                           cluster = "cluster",
                           small_area = list(sa.col = "ismallold", areas = c("1"),
                                             unbiased = TRUE),
                           psmall = TRUE)


## compare estimation results:
extpsynth.clust$estimation
extpsmall.clust$estimation

# --> very similar, only minor differences in the variances



#####################################################
# Demonstrate error handling in package (examples)  #
#####################################################


# --------------------------------------------------------------------------- #
# TWO-PHASE
# --------------------------------------------------------------------------- #

## save grisons as new dataset:
grisons.n<- grisons

## delete explanatory variables from an s2-(i.e. s1-) sample point:
grisons.n[which(grisons.n$phase_id_2p==2)[1],c(4,5,6,7)]<- NA

tp<- twophase(formula=tvol ~ mean + stddev + max + q75,
                 data=grisons.n,
                 phase_id=list(phase.col = "phase_id_2p", terrgrid.id = 2),
                 boundary_weights = "boundary_weights")

# Violation:
# s2-point MUST have all explanatory variables available

# --> error message due to nesting violation: s2 not nested

# error handling:
# s2 point with missing auxiliary information is deleted from the dataset

# --------------------------------------------------------------------------- #

## save grisons as new dataset:
grisons.n<- grisons

## delete explanatory variables from an s1-sample point:
grisons.n[which(grisons.n$phase_id_2p==1)[1],"mean"]<- NA

tp<- twophase(formula=tvol ~ mean + stddev + max + q75,
              data=grisons.n,
              phase_id=list(phase.col = "phase_id_2p", terrgrid.id = 2),
              boundary_weights = "boundary_weights")

# Violation:
# all s1 point must have all aux.variables available

# warning message:
# --> error message due to missing explanatory variable(s) in s1

# error handling:
# s1 point with missing auxiliary information is deleted from the dataset


# --------------------------------------------------------------------------- #

## save grisons as new dataset:
grisons.n<- grisons

## delete response value from an s2-sample point:
grisons.n[which(grisons.n$phase_id_2p==2)[1],"tvol"]<- NA

tp<- twophase(formula=tvol ~ mean + stddev + max + q75,
              data=grisons.n,
              phase_id=list(phase.col = "phase_id_2p", terrgrid.id = 2),
              boundary_weights = "boundary_weights")

# Violation:
# every s2 point must provide a response value

# error handling:
# s2 point with missing response value recoded to s1-point

# NOTE: This could violate the "randomness" assumption.
#       Be sure that we you have a "s2 response completely missing at random" case!!!




# --------------------------------------------------------------------------- #
# THREE-PHASE
# --------------------------------------------------------------------------- #

# Checking the nesting of the sample-design:
# 1) --> each s2-point muss have the complete set of s1-auxvars (s1-info) available
# 2) --> each s2-point muss have the complete set of s0-auxvars (s0-info) available
# 3) --> each s1-point muss have the complete set of s0-auxvars (s0-info) available


# 1) & 2) ---------------------------------

## save grisons as new dataset:
grisons.n<- grisons

## delete "mean" value from an s2-(i.e. s1- and s0-) sample point:
grisons.n[which(grisons.n$phase_id_3p==2)[1],"mean"]<- NA

tp<- threephase(formula.s0 = tvol ~ mean,
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


# 1) & 2) ---------------------------------

## save grisons as new dataset:
grisons.n<- grisons


## delete "mean" value from an s2-sample point:
grisons.n[which(grisons.n$phase_id_3p==2)[1],"q75"]<- NA


tp<- threephase(formula.s0 = tvol ~ mean,
                formula.s1 = tvol ~  mean + stddev + max + q75,
                data = grisons.n,
                phase_id = list(phase.col="phase_id_3p", s1.id = 1, terrgrid.id = 2),
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



# 3) ---------------------------------

## save grisons as new dataset:
grisons.n<- grisons

## delete "mean"-value from an s0-sample point:
grisons.n[which(grisons.n$phase_id_3p==0)[1],"mean"]<- NA

tp<- threephase(formula.s0 = tvol ~ mean,
                formula.s1 = tvol ~ mean + stddev + max + q75,
                data = grisons.n,
                phase_id = list(phase.col="phase_id_3p", s1.id = 1, terrgrid.id = 2),
                boundary_weights = "boundary_weights")

# Violation:
#  an s0 point misses at least one of the explanatory variables used in the reduced model at the s1-sample points

# error handling:
# plot deleted


# ---------------------------------

## save grisons as new dataset:
grisons.n<- grisons

## delete "q75"-value from an s1-aux.set:
grisons.n[which(grisons.n$phase_id_3p==2)[1],"q75"]<- NA

tp<- threephase(formula.s0 = tvol ~ mean,
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


#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

# Missing factor level in s2-sample:
zberg.n<- zberg

levels(zberg.n$stade)

table(zberg$phase_id_2p, zberg$stade)

## delete s2-points with "stade"-level '300'
zberg.n<- zberg[-which(zberg.n$phase_id_2p == 2 & zberg.n$stade=="300"), ]
table(zberg.n$phase_id_2p, zberg.n$stade)

# --> We have 300 s1-plot with 'stade'-level '300', but this factor level is
#     missing in the s2-sample (derivation of model not possible)

tpc<- twophase(formula = basal ~ stade + couver + melange,
                 data = zberg.n,
                 phase_id = list(phase.col = "phase_id_2p", terrgrid.id = 2),
                 cluster = "cluster")

# Violation:
# --> We have 300 s1-plot with 'stade'-level '300', but this factor level is
#     missing in the s2-sample (derivation of model not possible)
#     == Nesting violation

# error message:
# factor level ... only available at s1-points, but not at s2-points

# error handling:
# Execution stopped


#########
## End ##
#########

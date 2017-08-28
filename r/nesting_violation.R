
## Demonstrate error handling in package (examples)


# --------------------------------------------------------------------------- #
# TWO-PHASE
# --------------------------------------------------------------------------- #

## save grisons as new dataset:
grisons.n<- grisons


## delete explanatory variables from an s2-sample point:
grisons.n[which(grisons.n$phase_id_2p==2)[1],c(4,5,6,7)]<- NA

tp<- twophase(formula=tvol ~ mean + stddev + max + q75,
                 data=grisons.n,
                 phase_id=list(phase.col = "phase_id_2p", terrgrid.id = 2),
                 boundary_weights = "boundary_weights")

# Violation:
# s2-point MUST have all explanatory variables available

# --> error message due to nesting violation: s2 not nested in s1

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

# warning message:
# --> 

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


## delete "mean" value from an s2-sample point:
grisons.n[which(grisons.n$phase_id_3p==2)[1],"mean"]<- NA


tp<- threephase(formula.s0 = tvol ~ mean, 
                formula.s1 = tvol ~  mean + stddev + max + q75, 
                data = grisons.n,
                phase_id = list(phase.col="phase_id_3p", s1.id = 1, terrgrid.id = 2),
                boundary_weights = "boundary_weights")

# Violation:
# s2 point misses expl.variable used in s1 and s0

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
# s2 point misses expl.variable used in s1

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

## delete "mean"-value from an s1-aux.set:
grisons.n[which(grisons.n$phase_id_3p==1)[1],"mean"]<- NA

tp<- threephase(formula.s0 = tvol ~ mean, 
                formula.s1 = tvol ~  mean + stddev + max + q75, 
                data = grisons.n,
                phase_id = list(phase.col="phase_id_3p", s1.id = 1, terrgrid.id = 2),
                boundary_weights = "boundary_weights")

# Violation:
# for s1-plot, one expl.variable also used in s0 is missing

# warning message: 
# for s1-plot, one expl.variable also used in s0 is missing

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



## End ##


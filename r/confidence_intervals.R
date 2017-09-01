#---------------------------------------------------------------------------------- #
#  Journal of Statistical Software                                                  #
#  Design-Based Global and Small Area Estimations for Multiphase Forest Inventories #
#  Andreas Hill & Alexander Massey                                                  #
#                                                                                   #
#  Code for running all examples given in                                           #
#  Section 5  "Calculation of Confidence Intervals"                                 #
#                                                                                   #
#---------------------------------------------------------------------------------- #

library(forestinventory)


#####################################################
# ----- CI for One-Phase Estimation --------------- #
#####################################################

## load grisons dataset:
data(grisons)

## calculate onephase-estimation for entire dataset:
op <- onephase(formula = tvol~1 ,data = grisons,
               phase_id =list(phase.col = "phase_id_2p",terrgrid.id = 2))

## Confidence Intervals:
confint(op)

## 2) calculate one-phase-estimation for given domains (areas) in dataset:
op.a <- onephase(formula = tvol~1,
                 data = grisons,
                 phase_id = list(phase.col = "phase_id_2p", terrgrid.id = 2),
                 area = list(sa.col = "smallarea", areas = c("A", "B")))

## Confidence Intervals:
confint(op.a)


#####################################################
# ----- CI for Two-Phase Estimation --------------- #
#####################################################

## calculate two-phase-estimation for entire dataset:
extpsynth_2p<- twophase(formula = tvol ~ mean + stddev + max + q75, data = grisons,
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


#######
# END #
#######

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

## 1) calculate onephase-estimation for entire dataset:
op <- onephase(formula = tvol~1 ,data = grisons,
               phase_id =list(phase.col = "phase_id_2p",terrgrid.id = 2))
summary(op)
confint(op)

## 2) calculate onephase-estimation for given domains (areas) in dataset:
op.a <- onephase(formula = tvol~1,
                 data = grisons,
                 phase_id = list(phase.col = "phase_id_2p", terrgrid.id = 2),
                 area = list(sa.col = "smallarea", areas = c("A", "B")))
summary(op.a)
confint(op.a)


#####################################################
# ----- CI for Two-Phase Estimation --------------- #
#####################################################


extpsynth_2p<- twophase(formula = tvol ~ mean + stddev + max + q75, data = grisons,
                        phase_id = list(phase.col = "phase_id_2p", terrgrid.id = 2),
                        small_area = list(sa.col = "smallarea", areas = c("A", "B","C", "D"),
                                          unbiased = TRUE))

confint(extpsynth_2p)

## CIs with bonferroni correction
confint(extpsynth_2p, adjust.method = "bonferroni")






































#######
# END #
#######



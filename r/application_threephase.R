#---------------------------------------------------------------------------------- #
#  Journal of Statistical Software                                                  #
#  Design-Based Global and Small Area Estimations for Multiphase Forest Inventories #
#  Andreas Hill & Alexander Massey                                                  #
#                                                                                   #
#  Code for running all examples given in                                           #
#  Section 4.1  "Application of three-phase global and small area estimators"       #
#                                                                                   #
#---------------------------------------------------------------------------------- #

library(forestinventory)


#####################################
# GLOBAL THREE-PHASE ESTIMATIONS    #
#####################################

## get sample sizes of three-phase secenario:
table(grisons$phase_id_3p)

## define reduced and full regression model:
formula.s0 <- tvol ~ mean # reduced model:
formula.s1 <- tvol ~ mean + stddev + max + q75 # full model

## calculate one-phase estimation:
op <- onephase(formula = tvol~1 ,data = grisons,
               phase_id =list(phase.col = "phase_id_3p",terrgrid.id = 2))

## calculate two-phase estimation:
reg2p_nex<- twophase(formula=formula.s0,
                     data=grisons,
                     phase_id=list(phase.col = "phase_id_3p", terrgrid.id = 2),
                     boundary_weights = "boundary_weights")

## calculate three-phase estimation:
reg3p_nex<- threephase(formula.s0, formula.s1, data = grisons,
                       phase_id = list(phase.col="phase_id_3p", s1.id = 1, terrgrid.id = 2),
                       boundary_weights = "boundary_weights")

## get summaries:
summary(reg2p_nex)
summary(reg3p_nex)

# merge to esttable-object:
etab.2p.glob<- estTable(est.list = list(op, reg2p_nex))
etab.3p.glob<- estTable(est.list = list(op, reg2p_nex, reg3p_nex))

# visualize estimation errors:
plot(etab.3p.glob)

# quantify gain:
etab.2p.glob<- estTable(est.list = list(op, reg2p_nex))
mphase.gain(etab.2p.glob)
mphase.gain(etab.3p.glob)



#########################################
# SMALL AREA THREE-PHASE ESTIMATIONS    #
#########################################















































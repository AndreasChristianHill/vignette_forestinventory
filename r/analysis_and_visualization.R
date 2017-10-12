#---------------------------------------------------------------------------------- #
#  Journal of Statistical Software                                                  #
#  Design-Based Global and Small Area Estimations for Multiphase Forest Inventories #
#  Andreas Hill & Alexander Massey                                                  #
#                                                                                   #
#  Code for running all examples given in                                           #
#  Section 7  "Analysis and Visualization"                                          #
#                                                                                   #
#---------------------------------------------------------------------------------- #


library(forestinventory)

## calculate some estimations (one-phase, two-phase, three-phase)

op <- onephase(formula = tvol~1, data = grisons,
               phase_id = list(phase.col = "phase_id_2p", terrgrid.id = 2),
               area = list(sa.col = "smallarea", areas = c("A", "B", "C", "D")))

extpsynth_2p<- twophase(formula = tvol ~ mean + stddev + max + q75, data = grisons,
                        phase_id = list(phase.col = "phase_id_2p", terrgrid.id = 2),
                        small_area = list(sa.col = "smallarea", 
                                          areas = c("A", "B","C", "D"),
                                          unbiased = TRUE))

psynth_2p <- twophase(formula = tvol ~ mean + stddev + max + q75, data = grisons,
                      phase_id = list(phase.col = "phase_id_2p", terrgrid.id = 2),
                      boundary_weights = "boundary_weights",
                      small_area = list(sa.col = "smallarea", 
                                        areas = c("A", "B", "C", "D"),
                                        unbiased = FALSE))
 
extpsynth_3p<- threephase(formula.s0 = tvol ~ mean,
                          formula.s1 = tvol ~ mean + stddev + max + q75, data = grisons,
                          phase_id = list(phase.col = "phase_id_3p", s1.id = 1, terrgrid.id = 2),
                          small_area=list(sa.col = "smallarea", areas = c("A", "B", "C", "D"),
                                          unbiased = TRUE),
                          boundary_weights = "boundary_weights")

psynth_3p<- threephase(formula.s0 = tvol ~ mean,
                       formula.s1 = tvol ~ mean + stddev + max + q75, data = grisons,
                       phase_id = list(phase.col = "phase_id_3p", s1.id = 1, terrgrid.id = 2),
                       small_area = list(sa.col = "smallarea", 
                                         areas = c("A", "B", "C", "D"),
                                         unbiased = FALSE),
                       boundary_weights = "boundary_weights")


#####################################################
# ----- estTable() -------------------------------- #
#####################################################


## aggregate all estimation results using the estTable()-function:
grisons.sae.table<- estTable(est.list = list(op, extpsynth_2p, psynth_2p,
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
grisons.sae.df<- data.frame(grisons.sae.table)
class(grisons.sae.df)


#####################################################
# ----- mphase.gain() ----------------------------- #
#####################################################

## analyse gain of best multi-phase estimation to one-phase estimation:
mphase.gain(grisons.sae.table, pref.vartype = "g_variance")



#####################################################
# ----- plot()-method ----------------------------- #
#####################################################

## plot point estimates and confidence intervals:
plot(grisons.sae.table, ncol = 2, yvar = "estimate") +
  ylab("Timber Volume [m3/ha]")

## plot estimation errors:
plot(grisons.sae.table, ncol = 2) 


#######
# END #
#######

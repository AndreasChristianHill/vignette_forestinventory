#---------------------------------------------------------------------------------- #
#  Journal of Statistical Software                                                  #
#  Design-Based Global and Small Area Estimations for Multiphase Forest Inventories #
#  Andreas Hill & Alexander Massey                                                  #
#                                                                                   #
#  Code for running all examples given in                                           #
#  Section 4  "Three-phase Estimators and their Application"                        #
#                                                                                   #
#---------------------------------------------------------------------------------- #

library(forestinventory)


#####################################################
# -------- Global Estimations --------------------- #
#####################################################


## load grisons data set:
data("grisons", package = "forestinventory")

## see structure of grisons:
str(grisons)

## see first 12 rows of grisons:
head(grisons, n = 12)

## define reduced and full regression model formulas:
formula.rm <- tvol ~ mean # reduced model applied to s0 phase
formula.fm <- tvol ~ mean + stddev + max + q75 # full model applied to s1 phase

## apply estimator:
reg3p_nex<- threephase(formula.s0 = formula.rm, 
                       formula.s1 = formula.fm, data = grisons,
                       phase_id = list(phase.col="phase_id_3p", s1.id = 1, 
                                       terrgrid.id = 2),
                       boundary_weights = "boundary_weights")

## get summary of estimation results:
summary(reg3p_nex)


#####################################################
# ------ Small Area Estimations ------------------- #
#####################################################


# --------------------------------- #
## extended synthetic estimator (extsynth)
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



# --------------------------------- #
## synthetic estimator (synth)
#  with boundary weight adjustment

synth_3p <- threephase(formula.rm, formula.fm, data = grisons,
                       phase_id = list(phase.col="phase_id_3p", s1.id = 1, terrgrid.id = 2),
                       small_area = list(sa.col = "smallarea", areas = c("A", "B"),
                                         unbiased = FALSE),
                       exhaustive = truemeans.G,
                       boundary_weights = "boundary_weights")

## get summary of estimation results:
summary(synth_3p)



# --------------------------------- #
## extended pseudo synthetic estimator (extpsynth)
#  with boundary weight adjustment

extpsynth_3p <- threephase(formula.rm, formula.fm, data = grisons,
                           phase_id = list(phase.col = "phase_id_3p", 
                                           s1.id = 1, terrgrid.id = 2),
                           small_area = list(sa.col = "smallarea", areas = c("A", "B"),
                                             unbiased = TRUE),
                           boundary_weights = "boundary_weights")


## get summary of estimation results:
summary(extpsynth_3p)


#######
# END #
#######


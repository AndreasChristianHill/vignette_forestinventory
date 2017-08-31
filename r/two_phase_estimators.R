#---------------------------------------------------------------------------------- #
#  Journal of Statistical Software                                                  #
#  Design-Based Global and Small Area Estimations for Multiphase Forest Inventories #
#  Andreas Hill & Alexander Massey                                                  #
#                                                                                   #
#  Code for running all examples given in                                           #
#  Section 3  "Two-phase Estimators and their Application"                          #
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


# --------------------------------- #
## non-exhaustive estimator 
#  with boundary weight adjustment

## apply estimator:
reg2p_nex <- twophase(formula=tvol ~ mean + stddev + max + q75,
                      data=grisons,
                      phase_id=list(phase.col = "phase_id_2p", terrgrid.id = 2),
                      boundary_weights = "boundary_weights")

## get summary of estimation results:
summary(reg2p_nex)



# --------------------------------- #
## exhaustive estimator 
#  with boundary weight adjustment

## retrieve order of columnames that the lm()-function processes a formula object:
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



#####################################################
# ------ Small Area Estimations ------------------- #
#####################################################

# --------------------------------- #
## pseudo small area estimator (psmall)
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


# --------------------------------- #
## extended pseudo synthetic estimator (extpsynth)
#  with boundary weight adjustment

## apply estimator:
extpsynth_2p <- twophase(formula = tvol ~ mean + stddev + max + q75, data = grisons,
                         phase_id = list(phase.col = "phase_id_2p", terrgrid.id = 2),
                         boundary_weights = "boundary_weights",
                         small_area = list(sa.col = "smallarea", areas = c("A", "B"),
                                           unbiased = TRUE))

## get data frame with estimation results:
extpsynth_2p$estimation


# --------------------------------- #
## pseudo synthetic estimator (extpsynth)
#  with boundary weight adjustment

## apply estimator:
psynth_2p <- twophase(formula = tvol ~ mean + stddev + max + q75, data = grisons,
                      phase_id = list(phase.col = "phase_id_2p", terrgrid.id = 2),
                      boundary_weights = "boundary_weights",
                      small_area = list(sa.col = "smallarea", areas = c("A", "B"),
                                        unbiased = FALSE))

## get data frame with estimation results:
psynth_2p$estimation


# --------------------------------- #
## extended synthetic estimator (extpsynth)
#  with boundary weight adjustment


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


#######
# END #
#######



library(forestinventory)
library(ggplot2)

op <- onephase(formula = tvol~1, data = grisons,
               phase_id = list(phase.col = "phase_id_2p", terrgrid.id = 2),
               area = list(sa.col = "smallarea", areas = c("A", "B", "C", "D")))


extpsynth_2p<- twophase(formula = tvol ~ mean + stddev + max + q75, data = grisons,
                        phase_id = list(phase.col = "phase_id_2p", terrgrid.id = 2),
                        small_area = list(sa.col = "smallarea", areas = c("A", "B","C", "D"),
                                          unbiased = TRUE))


psynth_2p <- twophase(formula = tvol ~ mean + stddev + max + q75, data = grisons,
                      phase_id = list(phase.col = "phase_id_2p", terrgrid.id = 2),
                      boundary_weights = "boundary_weights",
                      small_area = list(sa.col = "smallarea", areas = c("A", "B", "C", "D"),
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
          


grisons.sae.table.CI<- estTable(est.list = list(op, extpsynth_2p, psynth_2p,
                                                extpsynth_3p, psynth_3p),
                                sae = TRUE, 
                                vartypes = c("variance", "g_variance",  "ext_variance"))


grisons.sae.table<- estTable(est.list = list(op, extpsynth_2p, psynth_2p,
                                                extpsynth_3p, psynth_3p),
                                sae = TRUE, 
                                vartypes = c("variance", "g_variance",  "ext_variance"))



plot(grisons.sae.table.CI, ncol = 2, yvar = "estimate")

## save graphic for article:
ggsave("fig\\plot_est_ci.png", 
       plot(grisons.sae.table.CI, ncol = 2, yvar = "estimate") +
         ylab("Timber Volume [m3/ha]") +
         theme(axis.title.x =element_text(size=rel(6)), # size of axis-labels text
               axis.title.y =element_text(size=rel(6)),
               text = element_text(size=rel(7)), # text-size in panels
               axis.text.x = element_text(size=rel(6)),
               axis.text.y = element_text(size=rel(6)),
               legend.title=element_text(size=rel(7)),
               legend.text=element_text(size=rel(6))),
       width = 27, height = 17, dpi = 300)



plot(grisons.sae.table, ncol = 2, yvar = "error")


## save graphic for article:
ggsave("fig\\plot_error.png", 
       plot(grisons.sae.table, ncol = 2, yvar = "error") +
         theme(axis.title.x =element_text(size=rel(6)), # size of axis-labels text
               axis.title.y =element_text(size=rel(6)),
               text = element_text(size=rel(7)), # text-size in panels
               axis.text.x = element_text(size=rel(6)),
               axis.text.y = element_text(size=rel(6)),
               legend.title=element_text(size=rel(7)),
               legend.text=element_text(size=rel(6))),
       width = 27, height = 17, dpi = 300)


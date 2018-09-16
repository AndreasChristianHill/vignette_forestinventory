global.sims<- readRDS("D:\\Holzvorratschaetzung_RLP\\Artikel forestinventory\\vignette_forestinventory\\simuations covrates\\simres_global.rds")

library(ggplot2)

globalsim.fig<- 
ggplot(data=global.sims) + 
  geom_line(aes(x = n2, y=covrate.g, group=as.factor(n1), color=as.factor(n1)), size=rel(1)) +
  geom_point(aes(x = n2, y=covrate.g, group=as.factor(n1), color=as.factor(n1)),  size=rel(3)) +
  geom_hline(yintercept=0.95) +
  guides(color=guide_legend(title="Sample size n1"))+ 
  theme_bw() +
  theme(axis.title.x =element_text(size=rel(3)), # size of axis-labels text
        axis.title.y =element_text(size=rel(3)),
        text = element_text(size=rel(7)), # text-size in panels
        axis.text.x = element_text(size=rel(3)),
        axis.text.y = element_text(size=rel(3)),
        legend.title=element_text(size=rel(2.5)),
        legend.text=element_text(size=rel(2)),
        legend.key.size=unit(3, "line")) +
  xlab("Terrestrial sample size n2") +
  ylab("Coverage rate")


setwd("D:\\Holzvorratschaetzung_RLP\\Artikel forestinventory\\vignette_forestinventory\\fig\\")
ggsave("globalsim.png", 
       globalsim.fig, width = 10, height = 7, dpi = 600)







library(ggplot2)
ggplot(data=global.sims) + 
  geom_line(aes(x = sampling_fractions, y=covrate.g, group=as.factor(n1), color=as.factor(n1))) +
  geom_point(aes(x = sampling_fractions, y=covrate.g, group=as.factor(n1), color=as.factor(n1)))

# --------------------- #

simres_sae<- readRDS("D:\\Holzvorratschaetzung_RLP\\Artikel forestinventory\\vignette_forestinventory\\simuations covrates\\simres_sae.rds")
simres_sae_2<- readRDS("D:\\Holzvorratschaetzung_RLP\\Artikel forestinventory\\vignette_forestinventory\\simuations covrates\\simres_sae_2.rds")


simres_sae<- rbind(simres_sae, simres_sae_2)



library(ggplot2)
ggplot(data=simres_sae) + 
  geom_line(aes(x = avg.n2G, y=covrate.g)) +
  geom_point(aes(x = avg.n2G, y=covrate.g)) +
  geom_hline(yintercept=0.95) +
  facet_grid(~sampling_fractions) +
  theme(axis.title.x =element_text(size=rel(6)), # size of axis-labels text
        axis.title.y =element_text(size=rel(6)),
        text = element_text(size=rel(7)), # text-size in panels
        axis.text.x = element_text(size=rel(6)),
        axis.text.y = element_text(size=rel(6)),
        legend.title=element_text(size=rel(7)),
        legend.text=element_text(size=rel(6)),
        legend.key.size=unit(8, "line"))





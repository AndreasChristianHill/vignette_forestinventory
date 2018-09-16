
# -------------------------------------------------------------------------- #
# -------------------------------------------------------------------------- #

# function to create density surface:

target.suface <- function(x0, y0){
  30 + 13*x0 - 6*y0 - 4*x0^2 + 3*x0*y0 + 2*y0^2 + 6*cos(pi*x0)*sin(pi*y0)}
# The true mean value is derived from an integral can be calculated in analytically or in Matlab.
# The true mean value here is 39.17

# --- True spatial mean for Small area surface area: --- #
library(rmutil)
int2(target.suface, a = c(0.5, 0.3), b=c(2, 1.3))/(1*1.5) # 37.16243


# -------------------------------------------------------------------------- #
# -------------------------------------------------------------------------- #


# --- sample generator for simple sampling: --- #
sample.generator<- function(n1, n2, target.surface=target.surface){
  realization <- matrix(NA,n1,7)
  for(i in 1:n1){
    x0=2*runif(1)
    y0=3*runif(1)
    realization[i,1] <- target.suface(x0, y0)
    realization[i,2]<-x0
    realization[i,3]<-y0
    realization[i,4]<-x0*x0
    realization[i,5]<-x0*y0
    realization[i,6]<-y0*y0
    realization[i,7]<-0
  }
  realization <- as.data.frame(realization) # warning: still contains true response variable for all 1st phase
  names(realization) <- c("response","x","y","xx","xy","yy", "area")
  realization$phase <- 1
  realization$phase[sample(nrow(realization), n2)] <- 2
  realization$response[realization$phase == 1]<- NA
  realization$area[realization$x >= 0.3 & realization$x <= 1.3 & realization$y >= 0.5 & realization$y <= 2]<- 1
  return(realization)
}

# -------------------------------------------------------------------------- #
# -------------------------------------------------------------------------- #

# simulation for small area edge cases:

library(forestinventory)
num_replications <- 10000
n1.vec <- c(100, 150, 200, 250, 500, 750, 1000) 
sfrac <- 0.25
true_value.sae<- 37.16

# data.frame to store results:
coverage_rates <- data.frame(n1=integer(), n2=integer(), 
                             sampling_fractions=numeric(), 
                             avg.n2G=numeric(),
                             avg.n1G=numeric(),
                             avg.error.ext=numeric(),
                             avg.error.g=numeric(),
                             covrate.ext=numeric(),
                             covrate.g=numeric(),
                             no.valid.sims = integer())

# -------------- #
# simulations:

for(i in 1:length(n1.vec)){
  
  print(i)
  
  # variables to store temp. results:
  n2G<- integer()
  n1G<- integer()
  n2<- integer()
  error.ext<- numeric()
  error.g<- numeric()
  ci.ext.logical<- logical()
  ci.g.logical<- logical()
  no.valid.sims.<- integer()
  
  
  for(j in 1:num_replications){
    
    realization <- sample.generator(n1 = n1.vec[i], n2 = round(n1.vec[i]*sfrac))
    
    est <- twophase(formula = response ~ y + x + xx, data=realization,
                    phase_id = list(phase.col = "phase", terrgrid.id = 2),
                    small_area = list(sa.col = "area", areas = "1",
                                      unbiased = TRUE))
      
    ci<- confint(est)
    ci.ext.logical<- append(ci.ext.logical, ci$ci$ci_lower_ext <= true_value.sae & true_value.sae <= ci$ci$ci_upper_ext)
    ci.g.logical<- append(ci.g.logical, ci$ci$ci_lower_g <= true_value.sae & true_value.sae <= ci$ci$ci_upper_g)
    error.ext <- append(error.ext, sqrt(est$estimation$ext_variance)/est$estimation$estimate)
    error.g <- append(error.g, sqrt(est$estimation$g_variance)/est$estimation$estimate)
      
    n2G <- append(n2G, est$estimation$n2G)
    n1G <- append(n1G, est$estimation$n1G)
    n2 <- append(n2, est$estimation$n2)
    no.valid.sims. <- sum(n2G > 1)

  }
  
  # calculate cove.rates, avg. n2G etc:
  coverage_rates <- rbind(coverage_rates, data.frame(n1=n1.vec[i], n2=round(mean(n2)), 
                                                     sampling_fractions=sfrac, 
                                                     avg.n2G=round(mean(n2G)), 
                                                     avg.n1G=round(mean(n1G)), 
                                                     avg.error.ext=mean(error.ext, na.rm = TRUE),
                                                     avg.error.g=mean(error.g, na.rm = TRUE),
                                                     covrate.ext=mean(ci.ext.logical, na.rm = TRUE), 
                                                     covrate.g=mean(ci.g.logical, na.rm = TRUE),
                                                     no.valid.sims=no.valid.sims.))
  
} # end of sim


#   n1  n2 sampling_fractions avg.n2G avg.n1G avg.error.ext avg.error.g covrate.ext covrate.g no.valid.sims
#  100  12              0.125       3      25    0.04370493  0.03880794   0.9539212 0.9688253         15365
#  150  19              0.125       5      38    0.03583093  0.03271747   0.9453048 0.9444811         12140
#  200  25              0.125       6      50    0.03290806  0.03025852   0.9502630 0.9425118         10837
#  250  31              0.125       8      63    0.02890998  0.02690213   0.9512939 0.9439301          9506
#  500  62              0.125      15     125    0.02124846  0.02004751   0.9454056 0.9342031          7052
#  750  94              0.125      23     187    0.01726665  0.01638393   0.9526753 0.9410207          5663
# 1000 125              0.125      31     250    0.01490313  0.01416853   0.9478173 0.9386802          4925


saveRDS(coverage_rates, "D:\\Holzvorratschaetzung_RLP\\Artikel forestinventory\\vignette_forestinventory\\simuations covrates\\simres_sae_2.rds")





simres_sae<- readRDS("D:\\Holzvorratschaetzung_RLP\\Artikel forestinventory\\vignette_forestinventory\\simuations covrates\\simres_sae.rds")
simres_sae_2<- readRDS("D:\\Holzvorratschaetzung_RLP\\Artikel forestinventory\\vignette_forestinventory\\simuations covrates\\simres_sae_2.rds")

library(ggplot2)
ggplot(data=simres_sae[-1,]) + 
  geom_line(aes(x = avg.n2G, y=covrate.g)) +
  geom_point(aes(x = avg.n2G, y=covrate.g)) +
  geom_hline(yintercept=0.95)




# -------------------------------------------------------------------------- #
# -------------------------------------------------------------------------- #

# simulation for global cases:


run.global.sim <- function(num_replications, n1.vec){

  sfrac <- seq(0.02, 0.26, by=0.02) # fixed sampling fractions
  true_value<- 39.17
  library(forestinventory)
  
  
  # data.frame to store results:
  coverage_rates <- data.frame(n1=integer(), n2=integer(), 
                               sampling_fractions=numeric(), 
                               avg.error.ext=numeric(),
                               avg.error.g=numeric(),
                               covrate.ext=numeric(),
                               covrate.g=numeric(),
                               no.valid.sims = integer())
  
  # -------------- #
  # simulations:
  
  for(i in 1:length(sfrac)){ # loop over sfrac-values
    
    print(i)
    
    # temp. vars to store temp. results:
    n2<- integer()
    error.ext<- numeric()
    error.g<- numeric()
    ci.ext.logical<- logical()
    ci.g.logical<- logical()
    no.valid.sims.<- integer()
    
    
    for(j in 1:num_replications){ # loop over replications for fixed n1
      
      realization <- sample.generator(n1 = n1.vec, n2 = round(n1.vec*sfrac[i]))
      
      est <- twophase(formula = response ~ y + x + xx, data=realization,
                      phase_id = list(phase.col = "phase", terrgrid.id = 2))
      
      ci<- confint(est)
      ci.ext.logical<- append(ci.ext.logical, ci$ci$ci_lower_ext <= true_value & true_value <= ci$ci$ci_upper_ext)
      ci.g.logical<- append(ci.g.logical, ci$ci$ci_lower_g <= true_value & true_value <= ci$ci$ci_upper_g)
      error.ext <- append(error.ext, sqrt(est$estimation$ext_variance)/est$estimation$estimate)
      error.g<- append(error.g, sqrt(est$estimation$g_variance)/est$estimation$estimate)
      
      n2<- append(n2, est$estimation$n2)
      no.valid.sims. <- sum(n2 > 1)
      
    }
    
    # calculate cove.rates, avg. n2G etc:
    coverage_rates <- rbind(coverage_rates, data.frame(n1=n1.vec, n2=round(mean(n2)), 
                                                       sampling_fractions=sfrac[i], 
                                                       avg.error.ext=mean(error.ext, na.rm = TRUE),
                                                       avg.error.g=mean(error.g, na.rm = TRUE),
                                                       covrate.ext=mean(ci.ext.logical, na.rm = TRUE), 
                                                       covrate.g=mean(ci.g.logical, na.rm = TRUE),
                                                       no.valid.sims=no.valid.sims.))
    
  } # end of sim
  
  
  return(coverage_rates)

} # end of function


# ------- #

global.sims<- data.frame()
n1_sims<- c(250, 500, 750, 1000, 1500, 2000)

for (i in 1:length(n1_sims)){
  
  res<- run.global.sim(num_replications = 10000, n1.vec = n1_sims[i])

  global.sims <- rbind(global.sims, res)
  
}

saveRDS(global.sims, "D:\\Holzvorratschaetzung_RLP\\Artikel forestinventory\\vignette_forestinventory\\simuations covrates\\simres_global.rds")

# ------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------ #

# graphic

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









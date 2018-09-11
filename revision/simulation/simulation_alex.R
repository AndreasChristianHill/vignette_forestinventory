################

# Artificial data:

set.seed(0)
d <- data.frame(
  elv = (e<-runif(500, 300, 550)),     # explanatory variable (observed in s1)
  vol1 = rnorm(e, mean=600-e, sd=100), # response variables (to be observe in s2 only)
  vol2 = rnorm(e, mean=300, sd=100)*(e<310),
  resp3 = runif(e) < 0.03
)

# Coverage of true means of response variables by 95% confidence
# intervals obtained from 10000 samples s2 after two-phase fitting:

library(forestinventory)

# PROBLEM 1: Small sample size (n2=5)
true_value <- mean(d$vol1)
d$phase <- 1
cover <- replicate(1000, {
  d$phase[sample(length(d$phase), 5)] <- 2
  ci <- confint( twophase( vol1 ~ elv, data = d,
                           phase_id = list(phase.col="phase", terrgrid.id=2) ), level=0.95 )
  ci$ci$ci_lower_g <= true_value & true_value <= ci$ci$ci_upper_g
} )
mean(cover)

# PROBLEM 2: Effect of explanatory variable
true_value <- mean(d$vol2)
cover <- replicate(10000, {
  d$phase <- 1
  d$phase[sample(length(d$phase), 80)] <- 2
  ci <- confint( twophase( vol2 ~ elv, data = d,
                           phase_id = list(phase.col="phase", terrgrid.id=2) ), level=0.95 )
  ci$ci$ci_lower_g <= true_value & true_value <= ci$ci$ci_upper_g
} )
mean(cover)

# PROBLEM 3: Distribution of response
true_value <- mean(d$resp3)
cover <- replicate(10000, {
  d$phase <- 1
  d$phase[sample(length(d$phase), 80)] <- 2
  ci <- confint( twophase( resp3 ~ elv, data = d,
                           phase_id = list(phase.col="phase", terrgrid.id=2) ), level=0.95 )
  ci$ci$ci_lower_g <= true_value & true_value <= ci$ci$ci_upper_g
} )
mean(cover)



##################################
## Infinite Population Approach ##
##################################

target.suface <- function(x0, y0){
  30 + 13*x0 - 6*y0 - 4*x0^2 + 3*x0*y0 + 2*y0^2 + 6*cos(pi*x0)*sin(pi*y0)}
# The true mean value is derived from an integral can be calculated in analytically or in Matlab.
# The true mean value here is 39.17

# --- True spatial mean for entire surface area: --- #
library(rmutil)
int2(target.suface, a = c(0,0), b=c(3,2))/6 # 39.17

anker.points<- expand.grid(x=seq(0,2,by = 0.01), y=seq(0,3,0.01))
vec <- rep(NA, nrow(anker.points))
for (i in 1:length(vec)){
  vec[i]<- target.suface(anker.points[i,1], anker.points[i,2])
}
mean(vec) # 39.17

# True spatial mean for Small area surface area:
int2(target.suface, a = c(0.5, 0.3), b=c(2, 1.3))/(1*1.5) # 37.16243

anker.points<- expand.grid(x=seq(0.3,1.3,by = 0.001), y=seq(0.5,2,0.001))
vec <- rep(NA, nrow(anker.points))
for (i in 1:length(vec)){
  vec[i]<- target.suface(anker.points[i,1], anker.points[i,2])
}
mean(vec) # 37.16014


# sample generator for simple sampling:
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


# sample generator for cluster sampling:
sample.generator.cluster<- function(n1, n2, target.surface=target.surface){
  
  cluster<- function(start.x, start.y){
    clustxy<- matrix(data = NA, nrow = 4, ncol = 2)
    colnames(clustxy)<- c("x0","y0")
    dist_in_x<- 0.2
    dist_in_y<- 0.2
    clustxy[1,]<- c(start.x, start.y)
    clustxy[2,]<- c(start.x, start.y - dist_in_y)
    clustxy[3,]<- c(start.x + dist_in_x, start.y - dist_in_y)
    clustxy[4,]<- c(start.x + dist_in_x, start.y)
    return(as.data.frame(clustxy))
  }

  realization <- c()
  for(i in 1:n1){
    # cluster origins:
    x0=2*runif(1)
    y0=3*runif(1)
    
    # cluster:
    clust<- cluster(x0, y0)
    
    realization.t<- data.frame(response=target.suface(clust$x0, clust$y0),
               x = clust$x0,
               y = clust$y0,
               xx = clust$x0 * clust$x0,
               xy = clust$x0 * clust$y0,
               yy = clust$y0 * clust$y0,
               area = 0, 
               phase = 1,
               clustID = i)
    
    realization<- rbind(realization, realization.t)
  }

  # sample s2-phase (entire cluster):
  unique(realization$clustID)
  realization$phase[realization$clustID %in% sample(unique(realization$clustID), n2)]<- 2
  
  # set response for s1 to NA:
  realization$response[realization$phase == 1]<- NA

  # assign small area ID: 
  realization$area[realization$x >= 0.3 & realization$x <= 1.3 & realization$y >= 0.5 & realization$y <= 2]<- 1
  return(realization)
}



n1 <- 500 #Set your desired first phase sample size
n2 <- 0.25*n1 #Set your desired second phase sample size
realization <- sample.generator.cluster(n1, n2) #Draws independent sample realization

summary(twophase(formula = response ~ x + y + xx, data=realization,
                 phase_id = list(phase.col = "phase", terrgrid.id = 2),
                 cluster = "clustID",
                 small_area = list(sa.col = "area", areas = "1",
                                   unbiased = TRUE)))


# --------------------------------------------------------------------------------------- #

n1<- 50
sampling_fractions<- 0.25  

    n2G <- replicate(10000, {
      realization <- sample.generator(n1 = n1, n2 = round(n1*sampling_fractions))
      sum(realization$area & realization$phase==2)}
      )
    
mean(n2G) 


n1<- 50
sampling_fractions<- 0.35

n2G <- replicate(1000, {
  realization <- sample.generator.cluster(n1 = n1, n2 = round(n1*sampling_fractions))
  length(unique(realization$clustID[realization$area == 1 & realization$phase == 2]))}
)

mean(n2G) 



# -------------------------------------------------------------------------------------- #

# Set sample sizes
num_replications <- 20000
sampling_fractions <- 0.25 # 1:15/100
n1_seq <- c(40, 50, 70, 100, 200, 400) # 5:15*100
true_value.sae<- 37.16
coverage_rates <- data.frame(n1=integer(), n2=integer(), sampling_fractions=numeric(), coverage_rates=numeric())

# sae sim for simple sampling:
for(n1 in n1_seq){
  for(n2 in n1*sampling_fractions){
    cover <- replicate(num_replications, {
      realization <- sample.generator(n1, round(n2))
      ci <- confint( twophase(formula = response ~ x + y + xx, data=realization,
                              phase_id = list(phase.col = "phase", terrgrid.id = 2),
                              small_area = list(sa.col = "area", areas = "1",
                                                unbiased = TRUE)), level=0.95)
      ci$ci$ci_lower_g <= true_value.sae & true_value.sae <= ci$ci$ci_upper_g
    } )
    coverage_rates <- rbind(coverage_rates, data.frame(n1=n1, n2=n2, sampling_fractions=n2/n1, 
                                                       coverage_rates=mean(cover, na.rm = TRUE)))
  }
}

# -------------------------------------------------------------------------------------- #

# Set sample sizes
num_replications <- 20000
sampling_fractions <- 0.35 # 1:15/100
n1_seq <- 50 # 5:15*100
true_value.sae<- 37.16
coverage_rates <- data.frame(n1=integer(), n2=integer(), sampling_fractions=numeric(), coverage_rates=numeric())


# sae sim for cluster sampling:
for(n1 in n1_seq){
  for(n2 in n1*sampling_fractions){
    cover <- replicate(num_replications, {
      realization <- sample.generator.cluster(n1, round(n2))
      ci <- confint( twophase(formula = response ~ x + y + xx, data=realization,
                              phase_id = list(phase.col = "phase", terrgrid.id = 2),
                              cluster = "clustID",
                              small_area = list(sa.col = "area", areas = "1",
                                                unbiased = TRUE)), level=0.95)
      ci$ci$ci_lower_g <= true_value.sae & true_value.sae <= ci$ci$ci_upper_g
    } )
    coverage_rates <- rbind(coverage_rates, data.frame(n1=n1, n2=n2, sampling_fractions=n2/n1, 
                                                       coverage_rates=mean(cover, na.rm = TRUE)))
  }
}

# -------------------------------------------------------------------------------------- #

# coverage_rates
coverage_rates$n1 <- as.factor(coverage_rates$n1)
library(ggplot2)
ggplot(data=coverage_rates, aes(x=sampling_fractions, y=coverage_rates, group=n1)) +
  geom_line(aes(color=n1)) + geom_vline(xintercept=0.05) + geom_hline(yintercept=0.95) +
  geom_point(size=0.8)


t(rbind(n1=500,rbind(n2=sampling_fractions*n1,rbind(sampling_fractions=sampling_fractions,coverage_rates))))






################

# Artificial data:

set.seed(0)
d <- data.frame(
  elv = (e<-runif(500, 300, 550)),     # explanatory variable (observed in s1)
  vol1 = rnorm(e, mean=600-e, sd=100), # response variables (to be observe in s2 only)
  vol2 = rnorm(e, mean=300, sd=100)*(e<310),
  resp3 = as.numeric(runif(e) < 0.03)
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

sample.generator <- function(n1, n2, target.surface=target.surface){
  realization <- matrix(NA,n1,11)
  for(i in 1:n1){
    x0=2*runif(1)
    y0=3*runif(1)
    realization[i,1] <- target.suface(x0, y0)
    realization[i,2]<-x0
    realization[i,3]<-y0
    realization[i,4]<-x0*x0
    realization[i,5]<-x0*y0
    realization[i,6]<-y0*y0
    realization[i,7]<-x0 + rnorm(1, 1, 4)
    realization[i,8]<-y0 + rnorm(1, -3, 7)
    realization[i,9]<-x0*x0 + rnorm(1, 2, 3)
    realization[i,10]<-x0*y0 + rnorm(1, 0.5, 1)
    realization[i,11]<-y0*y0 + rnorm(1, -0.5, 2)
  }
  realization <- as.data.frame(realization) # warning: still contains true response variable for all 1st phase
  names(realization) <- c("response","x","y","xx","xy","yy","x_perturbed","y_perturbed","xx_perturbed","xy_perturbed","yy_perturbed")
  realization$phase <- 1
  realization$phase[sample(nrow(realization), n2)] <- 2
  realization
}


# Set sample sizes
num_replications <- 10000
sampling_fractions <- 1:15/100
n1_seq <- seq(250, 3000, by = 250)
true_value <- 39.17
coverage_rates <- data.frame(n1=integer(), n2=integer(), sampling_fractions=numeric(), coverage_rates=numeric())

for(n1 in n1_seq){
  for(n2 in n1*sampling_fractions){
    cover <- replicate(num_replications, {
      realization <- sample.generator(n1, n2)
      ci <- confint( twophase( response ~ x + y + xx, data = realization,
                               phase_id = list(phase.col="phase", terrgrid.id = 2) ), level=0.95 )
      ci$ci$ci_lower_g <= true_value & true_value <= ci$ci$ci_upper_g
    }
    )
    coverage_rates <- rbind(coverage_rates, data.frame(n1=n1, n2=n2, sampling_fractions=n2/n1, coverage_rates=mean(cover)))
  }
  print("sample size complete")
}



# Set sample sizes
num_replications <- 1000
sampling_fractions <- 1:15/100
n1_seq <- 5:15*100
true_value <- 39.17
coverage_rates <- data.frame(n1=integer(), n2=integer(), sampling_fractions=numeric(), coverage_rates=numeric())

for(n1 in n1_seq){
  for(n2 in n1*sampling_fractions){
    cover <- replicate(num_replications, {
      realization <- sample.generator(n1, n2)
      ci <- confint( twophase( response ~ x_perturbed + y_perturbed + xx_perturbed, data = realization,
                               phase_id = list(phase.col="phase", terrgrid.id = 2) ), level=0.95 )
      ci$ci$ci_lower_g <= true_value & true_value <= ci$ci$ci_upper_g
    }
    )
    coverage_rates2 <- rbind(coverage_rates2, data.frame(n1=n1, n2=n2, sampling_fractions=n2/n1, coverage_rates=mean(cover)))
  }
}


# coverage_rates

coverage_rates$n1 <- as.factor(coverage_rates$n1)
library(ggplot2)
ggplot(data=coverage_rates, aes(x=sampling_fractions, y=coverage_rates, group=n1)) +
  geom_line(aes(color=n1)) + geom_vline(xintercept=0.05) + geom_hline(yintercept=0.95) +
  geom_point(size=0.8) + labs(title="Simulated coverage rates \n by sample size and sampling fraction",
                            x ="Sampling fraction (n1/n2)", y = "Coverage rate")

coverage_rates2$n1 <- as.factor(coverage_rates2$n1)
ggplot(data=coverage_rates2, aes(x=sampling_fractions, y=coverage_rates, group=n1)) +
  geom_line(aes(color=n1)) + geom_vline(xintercept=0.05) + geom_hline(yintercept=0.95) +
  geom_point(size=0.8)


t(rbind(n1=500,rbind(n2=sampling_fractions*n1,rbind(sampling_fractions=sampling_fractions,coverage_rates))))



#1 Phase sampling
n1 <- 500 #Set your desired first phase sample size
n2 <- 5 #Set your desired second phase sample size
realization <- sample.generator(n1, n2) #Draws independent sample realization
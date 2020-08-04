set.seed(10)

library(statsr)
library(dplyr)
library(ggplot2)


data (ames)

n<-60
samp_60<- ames%>%
  sample_n (n)

#critical value for 95% CI and find its value of CI
Z_star_95 <- qnorm(0.975)

CI_samp60<- samp_60%>%
  summarise(lower_CI= mean(area) - Z_star_95*(sd(area)/sqrt(n)) , 
            upper_CI= mean(area) + Z_star_95*(sd(area)/sqrt(n)) )

# determine whether interval from sampling distribution (sample size 50) does capture the true population mean 

params <- ames %>%
  summarise(mu = mean(area))

CI_60rep<- ames %>%
  rep_sample_n(size=n , reps = 60, replace=TRUE)%>%
  summarise (lower = mean(area)-Z_star_95*(sd(area)/sqrt(n)), 
             upper = mean(area)+Z_star_95*(sd(area)/sqrt(n)))

CI_60rep<-CI_60rep%>%
  mutate(capture_mu= ifelse(lower<params$mu & upper>params$mu , "yes", "no"))

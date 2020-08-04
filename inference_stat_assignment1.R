library(statsr)
library(dplyr)
library(shiny)
library(ggplot2)

data(ames)


# summary statistic of ames
sumaarise_stat<-ames%>%
  summarise(mu = mean(area), pop_med=median(area), pop_sd= sd(area), pop_IQR=IQR(area), pop_q1= quantile(area, 0.25),
            pop_q3=quantile(area,0.75), pop_min=min (area), pop_max= max (area)
  )

# visualize the distribution of housing price
ggplot(data=ames, aes(x=price)) + 
  geom_histogram(binwidth=5000)

# simple random sample of size 50 and visualize the distribution
sample_50 <- ames%>%
  sample_n (size=50)

ggplot(data=sample_50, aes(x=price)) + 
  geom_histogram(binwidth=5000) + xlab("size 50")


# create sample size 50 (15000 samples with replacement)

sampl_dist_50<- ames %>%
  rep_sample_n(size=50, reps=15000, replace=TRUE)%>%
  summarise(x_bar= mean(area))

# Visualize the sampling distribution
ggplot(data=sampl_dist_50, aes(x=x_bar)) + 
  geom_histogram(binwidth = 20)




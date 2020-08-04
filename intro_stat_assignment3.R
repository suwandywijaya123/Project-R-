library(statsr)
library(dplyr)
library(ggplot2)

# Simulate the outcome base on probabilily with certain amount of sample
sim_basket <- c("H","M")%>%
  sample(size=200, replace = TRUE,c(0.45,0.55))

table(sim_basket)

# Counting streak lengths
streak_sim<- calc_streak(sim_basket)

# Visualize the distribution of these streak lengths
ggplot(data=streak_sim,  aes(x=length)) + geom_histogram(binwidth = 1)

# Calculate the median, IQR, max, min for the kobe_streak
streak_sim%>%
  summarise(median_streak=median(length), 
            IQR_streak=IQR(length),
            max_streak=max(length),
            min_streak=min(length))



library(statsr)
library(dplyr)
library(ggplot2)

data(atheism)
str (atheism)


#95% confidence interval for the proportion of atheists in the United States in 2012
us12 <- atheism %>%
  filter(nationality == "United States" , year == "2012")

us12%>%
  summarise(proportion_atheis= sum(response=="atheist")/n())

# hypothesis testing, 95% CI proportion of atheists
inference(y = response, data = us12, statistic = "proportion", 
          type = "ci", method = "theoretical", success = "atheist")

# plot on how the proportion affect the margin of error
p<- data.frame(p= seq(0,1,0.01))
n<-1000
d<- p%>%
  group_by(p)%>%
  mutate(ME=1.96*(sqrt(p*(1-p)/n)))

ggplot(d, aes(x=p, y=ME)) + geom_line()


spain_atheism<- atheism%>%
  filter (nationality=="Spain")

spain_atheism$year<-as.factor(spain_atheism$year)

# CI (95%) for the dif in proportion between the year for atheist
inference(y= response ,x = year , data = spain_atheism, 
          statistic = "proportion", type = "ci", method = "theoretical", 
          success = "atheist")  

# hypothesis testing relationship between year and atheism
inference(y= response ,x = year , data = spain_atheism, 
          statistic = "proportion", type = "ht", method = "theoretical", 
          success = "atheist", null=0, alternative="twosided") 


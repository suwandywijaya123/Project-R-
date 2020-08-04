library(statsr)
library(dplyr)
library(ggplot2)

data(nc)

str (nc)
summary (nc$gained)


# correlation between expectant mother with smoking habit and the weight gain of baby
smoke_weight_gain<- nc %>%
  select(habit, weight)%>%
  filter(!is.na(habit), !is.na(weight))

ggplot(smoke_weight_gain, aes(x=habit, y= weight, fill=habit)) + 
  geom_boxplot()

smoke_weight_gain%>%
  group_by(habit)%>%
  summarise(IQR_weight=IQR(weight), range_weight=max(weight)-min(weight))

# hypothesis testing on relationship between weight and habit
inference(y = weight, x = habit, data = nc, statistic = "mean", type = "ht", null = 0, 
          alternative = "twosided", method = "theoretical")

# CI (95%) for the difference between the weights of babies born to nonsmoking and smoking mothers
inference(y = weight, x = habit, data = nc, statistic = "mean", type = "ci", 
          method = "theoretical",order = c("smoker","nonsmoker"),conf_level = 0.95)

# value of 99% CI
inference(y = weeks, data = nc, statistic = "mean", type = "ci",method = "theoretical",
          conf_level = 0.99)


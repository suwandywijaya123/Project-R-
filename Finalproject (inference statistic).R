# Final project code. Exploring the gss data, create research questions and conduct the analysis study
data(gss)

library(ggplot2)
library(dplyr)
library(statsr)

data(gss)

#correlation study between education and different races

educated_race<- gss%>%
  select(race, degree, age)%>%
  filter (age >=30, !is.na(race), !is.na(degree))

educated_race_rate<- educated_race%>%
  group_by(race)%>%
  summarise (bachelordegree=sum(degree=="Bachelor")/n())%>%
  arrange(bachelordegree)


ggplot (educated_race_rate, aes (x=race, y=bachelordegree)) + 
          geom_bar(stat="identity") 

inference(data= educated_race, y=degree, x=race, type="ht", 
          statistic = "proportion", 
          success = "Bachelor", method="theoretical", 
          alternative="greater")
  
#correlation study between how easy to find the equivalent job and different races 
#amongst those with Bachelor degree

jobfind_race<- gss%>%
  select(race, jobfind, degree)%>%
  filter (degree == "Bachelor", !is.na(race), !is.na(jobfind) )

ggplot (jobfind_race, aes (factor(race), fill=factor(jobfind))) + 
  geom_bar(position="fill")

inference(data= jobfind_race, y=jobfind, x=race, type="ht", 
          statistic = "proportion", 
          success = "Very Easy", method="theoretical", 
          alternative="greater")


#correlation study between uneployment and different race 
#amongst those with Bachelor degree
  
unemp_race<- gss%>%
  select(race, unemp, degree)%>%
  filter (degree == "Bachelor", !is.na(race), !is.na(unemp) )

ggplot (unemp_race, aes (factor(race), fill=factor(unemp))) + 
  geom_bar(position="fill")

inference(data= unemp_race, y=unemp, x=race, type="ht", 
          statistic = "proportion", 
          success = "Very Easy", method="theoretical", 
          alternative="greater")

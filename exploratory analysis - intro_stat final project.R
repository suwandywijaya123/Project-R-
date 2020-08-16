# Final project code. Exploring the BRFSS data, create research questions and conduct the analysis study

load(brfss2013)

#qn 1
# correlation study between income rate and affordability of medical cost amongst the veteran

load(brfss2013)

rm(aff_rate_income)

aff_rate_income <- brfss2013 %>%
  filter (veteran3=="Yes", !is.na(income2))%>%
  select("medcost","income2", "veteran3")

colSums(is.na(aff_rate_income))
  
aff_rate_income<- aff_rate_income%>%
  group_by(income2)%>%
  summarise(cannot_aff_rate= sum((medcost=="Yes")/n(), na.rm=TRUE))
 
as_tibble(aff_rate_income)

ggplot (data = aff_rate_income, aes(x = income2, y = cannot_aff_rate)) +
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 45 , hjust=1,0))


#qn2 correlation between sleeping time and physical or mental health

m_p_health_sleeptime<- brfss2013%>%
  filter(sleptim1<=24,!is.na(sleptim1))%>%
  select("physhlth","menthlth","sleptim1")

boxplot(m_p_health_sleeptime$sleptim1)
summary(m_p_health_sleeptime$sleptim1)

m_p_health_sleeptime<- m_p_health_sleeptime%>%
  filter(sleptim1>1, sleptim1<=24,!is.na(sleptim1))

health_sleeptime<- m_p_health_sleeptime%>%
  group_by(sleptim1)%>%
  summarise(count= n(),"phyhealth_bad"= mean(physhlth, na.rm = TRUE), 
            "menhealth_bad"= mean(menthlth, na.rm = TRUE))

ggplot(health_sleeptime, aes(x=sleptim1)) + 
  geom_line(aes(y=phyhealth_bad, colour="phyhealth_bad(days)")) + 
  geom_line(aes(y=menhealth_bad, colour="menhealth_bad(days)"))


#qn3
#correlation between highblood and exercise

smoke_highblood<-brfss2013%>%
  filter(X_bmi5cat== "Normal weight", X_smoker3=="Never smoked", !is.na(X_pacat1))%>%
  select("X_pacat1", "X_rfhype5")

rate_highblood<- smoke_highblood%>%
  group_by(X_pacat1)%>%
  summarise(percentage_highblood= sum(X_rfhype5=="Yes", na.rm=TRUE)/n())


ggplot(data= rate_highblood, aes(x = X_pacat1, y= percentage_highblood)) + 
  geom_bar(stat="identity")

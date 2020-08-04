library(statsr)
library(dplyr)
library(ggplot2)

data(nycflights)


#Create a new data frame that includes flights headed to SFO in February, and save this data frame as sfo_feb_flights
#create sample size of 10
#visualize the spread of arrival delays for sfo_feb_flights 
assfo_feb_flight<-nycflights%>%
  filter(month==2 & dest=="SFO")

samplefight_10= sample_n(assfo_feb_flight, 10)

ggplot (data=assfo_feb_flight,aes(x=arr_delay)) +
  geom_histogram(binwidth=10,color='blue',fill='red') +
  labs(title="count of arrival delay") +
  labs(x="arrival delay") +
  theme(plot.title = element_text(hjust = 0.5))
  
#calculate the median and interquartile range for arr_delays of respective carrier
summary_statistic_1<-assfo_feb_flight%>%
  group_by(carrier)%>%
  summarise(median=median(arr_delay),
            IQR=IQR(arr_delay))%>%
  arrange((carrier))

# visualize the distributions of departure delays across months 
ggplot(nycflights, aes(x = factor(month), y = dep_delay)) +
  geom_boxplot()

#calculate the rate of on-time departure for the respective origin. 

nycflights_dep_type<-nycflights%>%
  mutate (dep_type=ifelse (dep_delay<5, 'OnTime','Delay') )

nycflights_dep_type%>%
  group_by(origin) %>%
  summarise(ot_dep_rate = sum(dep_type == "OnTime") / n()) %>%
  arrange(desc(ot_dep_rate))

# visualize the distribution on-time departure rate across the three airports 
ggplot(data = nycflights_dep_type, aes(x = origin, fill = dep_type)) +
  geom_bar()

#Mutate the data frame so that it includes a new variable that contains the average speed (in mph)
nycflights_speed<- nycflights%>%
  mutate(avg_speed= distance/air_time/60)

#analyse the tail number of the plane with the fastest avg_speed
nycflights_speed%>%
  select(tailnum,avg_speed)%>%
  arrange(desc(avg_speed))

#create scatterplot of avg_speed vs. distance to analyse its relationship.
ggplot(data=nycflights_speed, aes(x=distance, y=avg_speed)) + geom_point()

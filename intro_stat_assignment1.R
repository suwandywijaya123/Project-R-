library(dplyr)
library(ggplot2)
library(statsr)


# Retrieve the data present (total number of birth for male and female) from statsr
rm ('present')
data('present')


# To add new column to include total number of boys and girls, 
# more_boys (true if the years has more boys, false if the year has less boys)
present <- present %>%
  mutate ( total = boys + girls, more_boys=ifelse (boys>girls, 'True', 'False'))

# Total sum of column Total
sum(present$total)


# Plotting the total number of birth each years
ggplot (data=present, aes (x=year,y=total)) +
  geom_line ()

# To arrange the value of total birth in descending order
present<-present %>%
  arrange(desc(total)) 



          
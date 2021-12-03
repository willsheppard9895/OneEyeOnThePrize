library(tidyverse)

# import data
allData <- read_csv("C:/Users/cn13ws/OneDrive - University of Leeds/msc2021/data/data_exp_53563-v19_questionnaire-6dum.csv")

age <- allData %>%
  select(age_in_years)%>%
  na.omit()
ggplot(age, aes(x = age_in_years))+
  geom_histogram(binwidth = 2)+
  scale_x_continuous(breaks = seq(min(age$age_in_years), max(age$age_in_years), 2), 
                     lim = c(min(age$age_in_years), max(age$age_in_years)))

library(tidyverse)

# import data
allData <- read_csv("C:/Users/cn13ws/OneDrive - University of Leeds/msc2021/data/vsRaw.csv")

# create list of tests with only single targets
single <- c("Image1.PNG", "Image2.PNG", "Image3.PNG", "Image4.PNG")

# filter incorrect answers and multi target tests
data <- allData %>%
  filter(Correct == 1)%>%
  filter(VS_Puzzle %in% single)

# clean data
data <- data %>%
  group_by(VS_Puzzle, Eye.Condition)%>%
  filter(Reaction.Time < mean(Reaction.Time)+1.96*sd(Reaction.Time))%>%
  filter(Reaction.Time > .2)

ggplot(data, aes(x = Eye.Condition, y = Reaction.Time))+
  geom_boxplot()

t.test(data$Reaction.Time ~ data$Eye.Condition)

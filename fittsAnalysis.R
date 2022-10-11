library(tidyverse)
library(gapminder)
library(rstatix)
theme_set(theme_bw(16))
# import data
allData <- read_csv("C:/Users/cn13ws/OneDrive - University of Leeds/msc2021/data/fittsLong.csv")

# remove particularly long and unrealistically short response times
cleanData <- allData %>%
  group_by(Eye.Condition, structure)%>%
  mutate(upperLimit = mean(Reaction.Time) + 1.96*sd(Reaction.Time),
         lowerLimit = 0.2) %>%
  filter(Reaction.Time < upperLimit)%>%
  filter(Reaction.Time > lowerLimit)%>%
  ungroup()

cleanData$size <- as.factor(cleanData$size)
# lets have a look at the effects of size and distance
cleanData %>%
  group_by(size)%>%
  summarise(
    count = n(),
    mean = mean(Reaction.Time),
    sd = sd(Reaction.Time)
  )


# visualise the effect of size and calculate anova statistic
ggplot(cleanData, aes(x = size, y = Reaction.Time, group = size))+
  geom_boxplot()

# data not normally distributed across groups - Friendman test
ggplot(cleanData, aes(sample = Reaction.Time))+
  stat_qq()+
  stat_qq_line()+
  facet_grid(~size)

# calculate freidman stats

friedData <- cleanData%>%
  group_by(Participant.Private.ID, size)%>%
  summarise(rt = mean(Reaction.Time))

# check that id and group columns are factors
friedData$Participant.Private.ID <- as.factor(friedData$Participant.Private.ID)
friedData$size <- as.factor(friedData$size)


friedman.test(rt ~ size|Participant.Private.ID, data = friedData)


# caluclate screen distance of each position
cleanData <- cleanData %>%
  mutate(distance = sqrt(xR^2 + yR^2),
         position = as.factor(substr(structure, 1, 4)))

cleanData %>%
  group_by(position)%>%
  summarise(
    count = n(),
    mean = mean(Reaction.Time),
    sd = sd(Reaction.Time)
  )

ggplot(cleanData, aes(x = distance, y = Reaction.Time))+
  geom_point(alpha = .02)+
  geom_smooth(method = lm)

# calculate the effect effect of distanc eon rt as a regression coeficient
distLM <- lm(Reaction.Time ~ distance, data = cleanData)
summary(distLM)

# sumamrise data
allData %>%
  group_by(Eye.Condition)%>%
  summarise(
    count = n(),
    mean = mean(Reaction.Time),
    sd = sd(Reaction.Time)
  )




# seperate conditions
mono <- subset(allData, Eye.Condition == "Mono")
binoc <- subset(allData, Eye.Condition == "Binoc")


# filter extreme data points
monoSumm <- mono %>%
  group_by(structure)%>%
  filter(Reaction.Time < mean(Reaction.Time)+ 1.96*sd(Reaction.Time))%>%
  filter(Reaction.Time > 0.2)%>%
  summarise(Eye.Condition = first(Eye.Condition),
            Reaction.Time = mean(Reaction.Time))

binocSumm <- binoc %>%
  group_by(structure)%>%
  filter(Reaction.Time < mean(Reaction.Time)+ 1.96*sd(Reaction.Time))%>%
  filter(Reaction.Time > 0.2)%>%
  summarise(Eye.Condition = first(Eye.Condition),
            Reaction.Time = mean(Reaction.Time))

summ <- merge(binocSumm, monoSumm, by = "structure")

# visualise data
ggplot(allData, aes(x = Eye.Condition, y = Reaction.Time)) +
  geom_boxplot()

pd <- PairedData::paired(test1, test2)
plot(pd, type = "profile")


res <- t.test(binocSumm$Reaction.Time, monoSumm$Reaction.Time, paired = TRUE)
res

summ <- rbind(binocSumm, monoSumm)

ggplot(summ, aes(x = Eye.Condition, y = Reaction.Time))+
  geom_point()+
  geom_line(aes(group = structure))+
  xlab("Viewing Condition")+
  ylab("Response Time (ms)")+
  scale_x_discrete(labels = c("Binocular", "Monocular"))

ggsave("C:/Users/cn13ws/OneDrive - University of Leeds/msc2021/figures/rtFitts.png")

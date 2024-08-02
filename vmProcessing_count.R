library(tidyverse)
#library(quickpsy)

# import data
cs<- read_csv("C:/Users/cn13ws/OneDrive - University of Leeds/msc2021/data/csLong.csv")
va <- read_csv("C:/Users/cn13ws/OneDrive - University of Leeds/msc2021/data/vaLong.csv")

# clean up the data
cs <- cs %>%
  group_by(Participant.Private.ID)%>%
  mutate(rtUpper = mean(Reaction.Time)+1.96*sd(Reaction.Time),
         rtLower = 0.2
         )

csClean <- cs %>%
  filter(Reaction.Time < rtUpper)%>%
  filter(Reaction.Time > rtLower)

va <- va %>%
  group_by(Participant.Private.ID)%>%
  mutate(rtUpper = mean(Reaction.Time)+1.96*sd(Reaction.Time),
         rtLower = 0.2
  )

vaClean <- va %>%
  filter(Reaction.Time < rtUpper)%>%
  filter(Reaction.Time > rtLower)

# create sumamry measures

summaryCs <- cs%>%
  dplyr::group_by(Participant.Private.ID, Eye.Condition, contrast) %>%
  dplyr::summarise(Total.Attempts = sum(Attempt),
                   Total.Correct = sum(Correct),
                   Percentage.Correct = Total.Correct/Total.Attempts
  )

summaryVa <- vaClean %>%
  dplyr::group_by(Participant.Private.ID, Eye.Condition, logMAR) %>%
  dplyr::summarise(Total.Attempts = sum(Attempt),
                   Total.Correct = sum(Correct),
                   Percentage.Correct = Total.Correct/Total.Attempts
  )

# order data sets by particiapnt id and contrast/logmar
summaryCs <- summaryCs[order(summaryCs$Participant.Private.ID, summaryCs$Eye.Condition, summaryCs$contrast, decreasing = TRUE),]
summaryVa <- summaryVa[order(summaryVa$Participant.Private.ID, summaryVa$Eye.Condition, summaryVa$logMAR, decreasing = TRUE),]

# filter values where participants achieved less thna 50% accuracy
# t is short for threshold
# CS ONLY
t = .5

summaryCs <- summaryCs %>%
  filter(Percentage.Correct > t)

# create a value for the highest VA that a participant could ave scored i.e. higest logMAR score + .1
threshVa <- summaryVa %>%
  dplyr::group_by(Participant.Private.ID, Eye.Condition) %>%
  summarise(maxVA = max(logMAR)+.1, 
         totalCorrect = sum(Percentage.Correct))%>%
  summarise(Eye.Condition = Eye.Condition,
            VA = round(maxVA - totalCorrect/10, 2)
            )


# group by participant and condition - select lowest value for contrast/logMAR
threshCs <- summaryCs %>%
  group_by(Participant.Private.ID, Eye.Condition)%>%
  summarise(thresholdContrast = min(contrast))


# further process CS data to get contrast sensitivity
# 2 centres the scores
#log10(1/100) = -2, therefore some who fails at 100% contrast has a CS of 0.00 and someone who is succesful over 50% at 1% contrast gets a score of 2
threshCs <- threshCs %>%
  mutate(CS = 2 + log10(1/thresholdContrast))

# join the data frames on participant id and condition
thresh <- full_join(threshCs, threshVa, by = c("Participant.Private.ID", "Eye.Condition"))

# let visualise
# seems to be a pretty strong relatioship between CS and VA
ggplot(thresh, aes(x = CS, y = VA, color = Eye.Condition))+
  geom_point()+
  geom_smooth(method = lm)

ggplot(thresh, aes(y = CS, group = Eye.Condition, x = Eye.Condition))+
  geom_boxplot()+
  geom_jitter()

ggplot(thresh, aes(y = VA, group = Eye.Condition, x = Eye.Condition))+
  geom_boxplot()+
  geom_jitter()

# summary stats
thresh %>%
  group_by(Eye.Condition)%>%
  summarise(
    count = n(),
    meanCS = mean(CS),
    sdCS = sd(CS),
    meanVA = mean(VA),
    sdVA = sd(VA)
  )

# t test
t.test(CS ~ Eye.Condition, data = thresh, paired = TRUE)
t.test(VA ~ Eye.Condition, data = thresh, paired = TRUE)

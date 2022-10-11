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
test <- summaryVa %>%
  dplyr::group_by(Participant.Private.ID, Eye.Condition) %>%
  summarise(maxVA = max(logMAR)+.1, 
         totalCorrect = sum(Percentage.Correct))%>%
  summarise(VA = round(maxVA - totalCorrect/10, 2),
            Eye.Condition = Eye.Condition)

summaryVa <- summaryVa %>%
  filter(Percentage.Correct > t)

# group by participant and condition - select lowest value for contrast/logMAR
threshCs <- summaryCs %>%
  group_by(Participant.Private.ID, Eye.Condition)%>%
  summarise(thresholdContrast = min(contrast))

threshVa <- sumamryVa %>%
  group_by(Participant.Private.ID, Eye.Condition)%>%
  summarise(VA = min(logMAR))


# further process CS data to get contrast sensitivity
# 2 centres the scores
#log10(1/100) = -2, therefore some who fails at 100% contrast has a CS of 0.00 and someone who is succesful over 50% at 1% contrast gets a score of 2
threshCs <- threshCs %>%
  mutate(CS = 2 + log10(1/thresholdContrast))

# join the data frames on participant id and condition
thresh <- full_join(threshCs, threshVa, by = c("Participant.Private.ID", "Eye.Condition"))

# fit model
fitCs <- quickpsy(d = summaryCs, x = contrast, k = Percentage.Correct, 
                grouping = c("Participant.Private.ID", "Eye.Condition"), 
                xmin = 1, xmax = 100, guess = 1/26, lapse = 0.03, 
                prob = 0.17, parini = c(1, 25),
                log = TRUE)
plot(fitCs, color = Eye.Condition)#+
  #xlim(0, log2(100))

#print(fitCs$thresholds)
#csThreshHun <- as.data.frame(fitCs$thresholds)
csThresh <- as.data.frame(fitCs$thresholds)
csThresh <- csThresh %>%
  pivot_wider(id_cols = Participant.Private.ID, names_from = Eye.Condition, values_from = thre)
#write.csv(csThresh, "C:/Users/cn13ws/OneDrive - University of Leeds/msc2021/data/csThreshold.csv")

ggplot(csThresh, aes(x = Eye.Condition, y = thre))+
  geom_point()

fitVa <- quickpsy(d = summaryVa, x = logMAR, k = Percentage.Correct, 
                  grouping = c("Participant.Private.ID", "Eye.Condition"), 
                  xmin = -.3, xmax = 1.2, guess = 1/26, lapses = 0.03,
                  prob = 0.17, parini = c(0, .5))
plot(fitVa, color = Eye.Condition)+
  xlim(-.3, 1.2)
print(fitVa$thresholds)

vaThresh <- as.data.frame(fitVa$thresholds)

vaThresh <- vaThresh %>%
  pivot_wider(id_cols = Participant.Private.ID, names_from = Eye.Condition, values_from = thre)
write.csv(vaThresh, "C:/Users/cn13ws/OneDrive - University of Leeds/msc2021/data/vaThreshold.csv")

summaryVa$logMAR <- as.factor(summaryVa$logMAR)

ggplot(summaryVa, aes(x = logMAR, y = Percentage.Correct, color = Eye.Condition))+
  geom_point(alpha = 0.6)+
  facet_wrap(~Participant.Private.ID)

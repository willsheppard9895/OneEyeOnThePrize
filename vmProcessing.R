library(tidyverse)
library(quickpsy)

# import data
cs<- read_csv("C:/Users/cn13ws/OneDrive - University of Leeds/msc2021/data/csLong.csv")
va <- read_csv("C:/Users/cn13ws/OneDrive - University of Leeds/msc2021/data/vaLong.csv")

# clean up the data
cs <- cs %>%
  group_by(Participant.Private.ID)%>%
  mutate(rtUpper = mean(Reaction.Time)+1.96*sd(Reaction.Time),
         rtLower = mean(Reaction.Time)-1.96*sd(Reaction.Time),
         logContrast = log2(contrast)
         )

csClean <- cs %>%
  filter(Reaction.Time < rtUpper)%>%
  filter(Reaction.Time > rtLower)

va <- va %>%
  group_by(Participant.Private.ID)%>%
  mutate(rtUpper = mean(Reaction.Time)+1.96*sd(Reaction.Time),
         rtLower = mean(Reaction.Time)-1.96*sd(Reaction.Time)
  )

vaClean <- va %>%
  filter(Reaction.Time < rtUpper)%>%
  filter(Reaction.Time > rtLower)

# create sumamry measures

summaryCs <- cs%>%
  dplyr::group_by(Participant.Private.ID, Eye.Condition, contrast, logContrast) %>%
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


# fit model
fitCs <- quickpsy(d = summaryCs, x = contrast, k = Percentage.Correct, 
                grouping = c("Participant.Private.ID", "Eye.Condition"), 
                xmin = 1, xmax = 100, guess = 1/26, lapse = 0.03, 
                prob = 0.17, parini = c(0, log2(100)),
                log = TRUE)
plot(fitCs, color = Eye.Condition)+
  xlim(0, log2(100))

print(fitCs$thresholds)

csThresh <- as.data.frame(fitCs$thresholds)
csThresh <- csThresh %>%
  pivot_wider(id_cols = Participant.Private.ID, names_from = Eye.Condition, values_from = thre)
write.csv(csThresh, "C:/Users/cn13ws/OneDrive - University of Leeds/msc2021/data/csThreshold.csv")

ggplot(csThresh, aes(x = Eye.Condition, y = thre))+
  geom_point()

fitVa <- quickpsy(d = summaryVa, x = logMAR, k = Percentage.Correct, 
                  grouping = c("Participant.Private.ID", "Eye.Condition"), 
                  xmin = -.3, xmax = 1.2, guess = 1/26, lapses = 0.03,
                  prob = 0.17, parini = c(-0.3, 1.2))
plot(fitVa, color = Eye.Condition)+
  xlim(-.3, 1.2)
print(fitVa$thresholds)

vaThresh <- as.data.frame(fitVa$thresholds)

vaThresh <- vaThresh %>%
  pivot_wider(id_cols = Participant.Private.ID, names_from = Eye.Condition, values_from = thre)
#write.csv(vaThresh, "C:/Users/cn13ws/OneDrive - University of Leeds/msc2021/data/vaThreshold.csv")

summaryVa$logMAR <- as.factor(summaryVa$logMAR)

ggplot(summaryVa, aes(x = logMAR, y = Percentage.Correct, color = Eye.Condition))+
  geom_point(alpha = 0.6)+
  facet_wrap(~Participant.Private.ID)

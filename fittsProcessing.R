library(tidyverse)

# import data
allData <- read_csv("C:/Users/cn13ws/OneDrive - University of Leeds/msc2021/data/fittsLong.csv")

# create summary measures
fittsSummary <- allData %>%
  group_by(Participant.Private.ID, Eye.Condition, structure)%>%
  summarise(meanRt = mean(Reaction.Time),
            hand = first(Spreadsheet),
            size = first(size),
            xR = first(xR),
            yR = first(yR),
            xL = first(xL),
            yL = first(yL))

# extract position from structure
fittsSummary$pos <- sub(".....$", "", fittsSummary$structure)

# pivot wider by eye conditon and structure
# 1 row per participant
fittsWide <- fittsSummary %>%
  pivot_wider(id_cols = Participant.Private.ID ,names_from = c(Eye.Condition, structure), values_from = meanRt)

write.csv(fittsWide, "C:/Users/cn13ws/OneDrive - University of Leeds/msc2021/data/fittsWide.csv")

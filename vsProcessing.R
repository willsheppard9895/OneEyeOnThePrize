library(tidyverse)

#import data
vs <- read_csv("C:/Users/cn13ws/OneDrive - University of Leeds/msc2021/data/vsLong.csv")
xy <- read_csv("C:/Users/cn13ws/OneDrive - University of Leeds/msc2021/visualSearch/letterCoordsValues.csv")

# filter images into separate data sets
vsOne <-  vs %>%
  filter(VS_Puzzle =="Image1.PNG")
xyOne <- xy %>%
  filter(VS_Puzzle =="Image1.PNG")

vsTwo <- vs %>%
  filter(VS_Puzzle =="Image2.PNG")
xyTwo <- xy %>%
  filter(VS_Puzzle =="Image2.PNG")

vsThree <- vs %>%
  filter(VS_Puzzle =="Image3.PNG")
xyThree <- xy %>%
  filter(VS_Puzzle =="Image3.PNG")

vsFour <- vs %>%
  filter(VS_Puzzle =="Image4.PNG")
xyFour <- xy %>%
  filter(VS_Puzzle =="Image4.PNG")

vsFive <- vs %>%
  filter(VS_Puzzle =="Image5_three.PNG")
xyFive <- xy %>%
  filter(VS_Puzzle =="Image5_three.PNG")

vsSix <- vs %>%
  filter(VS_Puzzle =="Image6_many.PNG")
xySix <- xy %>%
  filter(VS_Puzzle =="Image6_many.PNG")

# images 1 - 4, create correct column
vsOne <- vsOne %>%
  mutate(Correct = case_when(
    X.Coordinate >= xyOne$xMin &
      X.Coordinate <= xyOne$xMax &
      Y.Coordinate >= xyOne$yMin &
      Y.Coordinate <= xyOne$yMax ~ 1,
    TRUE ~ 0
  ))

vsTwo <- vsTwo %>%
  mutate(Correct = case_when(
    X.Coordinate >= xyTwo$xMin &
      X.Coordinate <= xyTwo$xMax &
      Y.Coordinate >= xyTwo$yMin &
      Y.Coordinate <= xyTwo$yMax ~ 1,
    TRUE ~ 0
  ))

vsThree <- vsThree %>%
  mutate(Correct = case_when(
    X.Coordinate >= xyThree$xMin &
      X.Coordinate <= xyThree$xMax &
      Y.Coordinate >= xyThree$yMin &
      Y.Coordinate <= xyThree$yMax ~ 1,
    TRUE ~ 0
  ))

vsFour <- vsFour %>%
  mutate(Correct = case_when(
    X.Coordinate >= xyFour$xMin &
      X.Coordinate <= xyFour$xMax &
      Y.Coordinate >= xyFour$yMin &
      Y.Coordinate <= xyFour$yMax ~ 1,
    TRUE ~ 0
  ))

# image 5, 3 targets
for (row in 1:nrow(vsFive)) {
  boxOne <- xyFive[1, 2:5]
  boxTwo <- xyFive[2, 2:5]
  boxThree <- xyFive[3, 2:5]
  
  vsFive <- vsFive %>%
    mutate(
      One = case_when(
      X.Coordinate >= boxOne$xMin &
        X.Coordinate <= boxOne$xMax &
        Y.Coordinate >= boxOne$yMin &
        Y.Coordinate <= boxOne$yMax ~ 1,
      TRUE ~ 0
    ),
    Two = case_when(
      X.Coordinate >= boxTwo$xMin &
        X.Coordinate <= boxTwo$xMax &
        Y.Coordinate >= boxTwo$yMin &
        Y.Coordinate <= boxTwo$yMax ~ 1,
      TRUE ~ 0
    ),
    Three = case_when(
      X.Coordinate >= boxThree$xMin &
        X.Coordinate <= boxThree$xMax &
        Y.Coordinate >= boxThree$yMin &
        Y.Coordinate <= boxThree$yMax ~ 1,
      TRUE ~ 0
    ) 
      
    )}


vsFive <- vsFive %>%
  mutate(Correct = (One + Two + Three)) %>%
  select(-One, -Two, -Three)

#xyBind <- bind_rows(vsOne, vsTwo, vsThree, vsFour, vsFive)

# image 6, 5 targets
for (row in 1:nrow(vsSix)) {
  boxOne <- xySix[1, 2:5]
  boxTwo <- xySix[2, 2:5]
  boxThree <- xySix[3, 2:5]
  boxFour <- xySix[4, 2:5]
  boxFive <- xySix[5, 2:5]
  
  vsSix <- vsSix %>%
    mutate(
      One = case_when(
        X.Coordinate >= boxOne$xMin &
          X.Coordinate <= boxOne$xMax &
          Y.Coordinate >= boxOne$yMin &
          Y.Coordinate <= boxOne$yMax ~ 1,
        TRUE ~ 0
      ),
      Two = case_when(
        X.Coordinate >= boxTwo$xMin &
          X.Coordinate <= boxTwo$xMax &
          Y.Coordinate >= boxTwo$yMin &
          Y.Coordinate <= boxTwo$yMax ~ 1,
        TRUE ~ 0
      ),
      Three = case_when(
        X.Coordinate >= boxThree$xMin &
          X.Coordinate <= boxThree$xMax &
          Y.Coordinate >= boxThree$yMin &
          Y.Coordinate <= boxThree$yMax ~ 1,
        TRUE ~ 0
      ) ,
      Four = case_when(
        X.Coordinate >= boxFour$xMin &
          X.Coordinate <= boxFour$xMax &
          Y.Coordinate >= boxFour$yMin &
          Y.Coordinate <= boxFour$yMax ~ 1,
        TRUE ~ 0
      ),
      Five = case_when(
        X.Coordinate >= boxFive$xMin &
          X.Coordinate <= boxFive$xMax &
          Y.Coordinate >= boxFive$yMin &
          Y.Coordinate <= boxFive$yMax ~ 1,
        TRUE ~ 0
      )
      
    )}

vsSix <- vsSix %>%
  mutate(Correct = (One + Two + Three + Four + Five)) %>%
  select(-One, -Two, -Three, -Four, - Five)

vsBind <- bind_rows(vsOne, vsTwo, vsThree, vsFour, vsFive, vsSix)

write.csv(vsBind, "C:/Users/cn13ws/OneDrive - University of Leeds/msc2021/data/vsRaw.csv")

single <- c("Image1.PNG", "Image2.PNG", "Image3.PNG", "Image4.PNG")
three <- "Image5_three.PNG"
many <- "Image6_many.PNG"

vsFilt <- vsBind %>%
  filter(Correct == 1)%>%
  mutate(condition = case_when(
    VS_Puzzle %in% single ~"single",
    VS_Puzzle %in% three ~ "three",
    VS_Puzzle %in% many ~ "many"
  ))

vsFilt <- vsFilt %>%
  group_by(Participant.Private.ID, Eye.Condition, condition)%>%
  summarise(RT = mean(Reaction.Time))

vsWide <- vsFilt %>%
  pivot_wider(id_cols = Participant.Private.ID,
              names_from = c(Eye.Condition, condition), 
              values_from = RT)

vsWide <- vsWide %>%
  select(Participant.Private.ID, contains("single"))
write.csv(vsWide, "C:/Users/cn13ws/OneDrive - University of Leeds/msc2021/data/vsWide.csv")

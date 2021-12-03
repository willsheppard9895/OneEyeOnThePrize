library(tidyverse)

# create lists of names of files for each condition
# nameing: task_eyeCondition_firstOrSecondAttempt
# i.e. vaOneSecond
# means - visual acuity task, with one eye and this is the second time that they have completed the tasks
# fn = file names
# cs - Contrast Sensitivity, va - Visual Acuity, fitts - Fitts' Law (clicking task), vs - Visual Search


setwd("C:/Users/cn13ws/OneDrive - University of Leeds/msc2021/data")

fn_all <- c("questionnaire-6dum", 
            "task-j73i", "task-bfvh", "task-n858", "task-3x99", "task-38w1", 
            "task-pi5h", "task-hfl1", "task-opgx", "task-lwqo", "task-h6z8",
            "task-v321", "task-cbqt", "task-8uqp", "task-fp5t", "task-rnwg", 
            "task-8lqq", "task-w9qt", "task-t7pr", "task-yvet", "task-sm8i",
            "task-hg5t", "task-sqpr",
            "task-i37h",
            "task-27g9", "task-sdmv", "task-div7", "task-1dcg", "task-mobr",
            "task-qr76", "task-tbvc", "task-te92", "task-bhrz", "task-21io",
            "task-rk85", "task-8dap", "task-3zz8", "task-f2kk", "task-5peh",
            "task-6lcm", "task-xngf", "task-qpfg", "task-ryyz", "task-zjdw",
            "task-osyu", "task-zuoj",
            "task-3u5y",
            "task-bp17", "task-nq8y", "task-nu2s", "task-1cqs", "task-bzq9", 
            "task-dyf7", "task-6x8e", "task-fj3l", "task-ba6q", "task-l16k",
            "task-xddp", "task-6v3o", "task-kna9", "task-i4cd", "task-zip7",
            "task-z8mr", "task-2cta", "task-gepb", "task-2hnp", "task-zcvm",
            "task-p5uw", "task-tpox",
            "task-xa72",
            "task-o9ch", "task-28bf", "task-fwg1", "task-z7iw", "task-md11",
            "task-do91", "task-wsb8", "task-h3db", "task-jslo", "task-m4vj",
            "task-wsav", "task-4pqm", "task-ghuw", "task-1i17", "task-j3lk",
            "task-wdxj", "task-w9bs", "task-6cxl", "task-yrfp", "task-tm55",
            "task-sgnv", "task-43se",
            "task-9zzh")

fn_mainQ <- "questionnaire-6dum"

fn_csOneFirst <- c("task-j73i", "task-bfvh", "task-n858", "task-3x99", "task-38w1", 
                   "task-pi5h", "task-hfl1", "task-opgx", "task-lwqo", "task-h6z8") 


fn_vaOneFirst <- c("task-v321", "task-cbqt", "task-8uqp", "task-fp5t", "task-rnwg", 
                   "task-8lqq", "task-w9qt", "task-t7pr", "task-yvet", "task-sm8i")

fn_fittsOneFirst <- c("task-hg5t", "task-sqpr")

fn_vsOneFirst <- "task-i37h"

fn_vaOneSecond <- c("task-27g9", "task-sdmv", "task-div7", "task-1dcg", "task-mobr",
                    "task-qr76", "task-tbvc", "task-te92", "task-bhrz", "task-21io")

fn_csOneSecond <- c("task-rk85", "task-8dap", "task-3zz8", "task-f2kk", "task-5peh",
                    "task-6lcm", "task-xngf", "task-qpfg", "task-ryyz", "task-zjdw")

fn_fittsOneSecond <- c("task-osyu", "task-zuoj")

fn_vsOneSecond <- "task-3u5y"

fn_csBothFirst <- c("task-bp17", "task-nq8y", "task-nu2s", "task-1cqs", "task-bzq9", "
                    task-dyf7", "task-6x8e", "task-fj3l", "task-ba6q", "task-l16k")

fn_vaBothFirst <- c("task-xddp", "task-6v3o", "task-kna9", "task-i4cd", "task-zip7",
                    "task-z8mr", "task-2cta", "task-gepb", "task-2hnp", "task-zcvm")

fn_fittsBothFirst <- c("task-p5uw", "task-tpox")

fn_vsBothFirst <- "task-xa72"

fn_vaBothSecond <- c("task-o9ch", "task-28bf", "task-fwg1", "task-z7iw", "task-md11",
                     "task-do91", "task-wsb8", "task-h3db", "task-jslo", "task-m4vj")

fn_csBothSecond <- c("task-wsav", "task-4pqm", "task-ghuw", "task-1i17", "task-j3lk",
                     "task-wdxj", "task-w9bs", "task-6cxl", "task-yrfp", "task-tm55")

fn_fittsBothSecond <- c("task-sgnv", "task-43se")

fn_vsBothSecond <- "task-9zzh"



#  import all relevant files
for(i in fn_all){
  filepath <- file.path("./",paste("data_exp_53563-v19_",i,".csv",sep=""))
  assign(i, read.csv(filepath))
}

####### combine data frames #######
# CS one eye
`task-n858`$Reaction.Time <- as.numeric(`task-n858`$Reaction.Time)
csOneFirst <- bind_rows(`task-j73i`, `task-bfvh`, `task-n858`, `task-3x99`, `task-38w1`, 
                        `task-pi5h`, `task-hfl1`, `task-opgx`,
                        `task-lwqo`, `task-h6z8`
                        )
csBothSecond <- bind_rows(`task-wsav`, `task-4pqm`, `task-ghuw`, `task-1i17`, `task-j3lk`,
                          `task-wdxj`, `task-w9bs`, `task-6cxl`, `task-yrfp`, `task-tm55`)
csMono <- bind_rows(csOneFirst, csBothSecond)

# Cs both eyes
csOneSecond <- bind_rows(`task-rk85`, `task-8dap`, `task-3zz8`, `task-f2kk`, `task-5peh`,
                         `task-6lcm`, `task-xngf`, `task-qpfg`, `task-ryyz`, `task-zjdw`)
csBothFirst <- bind_rows(`task-bp17`, `task-nq8y`, `task-nu2s`, `task-1cqs`, `task-bzq9`, 
                         `task-dyf7`, `task-6x8e`, `task-fj3l`, `task-ba6q`, `task-l16k`)
csBinoc <- bind_rows(csOneSecond, csBothFirst)

# va one eye
vaOneFirst <- bind_rows(`task-v321`, `task-cbqt`, `task-8uqp`, `task-fp5t`, `task-rnwg`, 
                        `task-8lqq`, `task-w9qt`, `task-t7pr`, `task-yvet`, `task-sm8i`
                        )
vaBothSecond <- bind_rows(`task-o9ch`, `task-28bf`, `task-fwg1`, `task-z7iw`, `task-md11`,
                          `task-do91`, `task-wsb8`, `task-h3db`, `task-jslo`, `task-m4vj`)
vaMono <- bind_rows(vaOneFirst, vaBothSecond)

# va both eyes
vaOneSecond <- bind_rows(`task-27g9`, `task-sdmv`, `task-div7`, `task-1dcg`, `task-mobr`,
                         `task-qr76`, `task-tbvc`, `task-te92`, `task-bhrz`, `task-21io`)
vaBothFirst <- bind_rows(`task-xddp`, `task-6v3o`, `task-kna9`, `task-i4cd`, `task-zip7`,
                         `task-z8mr`, `task-2cta`, `task-gepb`, `task-2hnp`, `task-zcvm`)
vaBinoc <- bind_rows(vaOneSecond, vaBothFirst)

# fitts one eye
fittsOneFirst <- bind_rows(`task-hg5t`, `task-sqpr`)
fittsBothSecond <- bind_rows(`task-sgnv`, `task-43se`)
fittsMono <- bind_rows(fittsOneFirst, fittsBothSecond)

# fitts both eyes
fittsOneSecond <- bind_rows(`task-osyu`, `task-zuoj`)
fittsBothFirst <- bind_rows(`task-p5uw`, `task-tpox`)
fittsBinoc <- bind_rows(fittsOneSecond, fittsBothFirst)

# vs one eye
vsOneFirst <- `task-i37h`
vsBothSecond <- `task-9zzh`
vsMono <- bind_rows(vsOneFirst, vsBothSecond)

#vs both eyes
vsOneSecond <- `task-3u5y`
vsBothFirst <- `task-xa72`
vsBinoc <- bind_rows(vsOneSecond, vsBothFirst)

## add a new 'eyeCondition' column filled with mono or binoc 
csMono$Eye.Condition <- rep("Mono", length(csMono$UTC.Timestamp))
csBinoc$Eye.Condition <- rep("Binoc", length(csBinoc$UTC.Timestamp))

vaMono$Eye.Condition <- rep("Mono", length(vaMono$UTC.Timestamp))
vaBinoc$Eye.Condition <- rep("Binoc", length(vaBinoc$UTC.Timestamp))

fittsMono$Eye.Condition <- rep("Mono", length(fittsMono$UTC.Timestamp))
fittsBinoc$Eye.Condition <- rep("Binoc", length(fittsBinoc$UTC.Timestamp))

vsMono$Eye.Condition <- rep("Mono", length(vsMono$UTC.Timestamp))
vsBinoc$Eye.Condition <- rep("Binoc", length(vsBinoc$UTC.Timestamp))

# Bind data frames
allCs <- bind_rows(csMono, csBinoc)
allVa <- bind_rows(vaMono, vaBinoc)
allFitts <- bind_rows(fittsMono, fittsBinoc)
allVs <- bind_rows(vsMono, vsBinoc)

##### filter relevant rows #####
allCs <- allCs %>%
  filter(Zone.Type == "response_text_entry")

allVa <- allVa %>%
  filter(Zone.Type == "response_text_entry")


allFitts <- allFitts %>%
  filter(Screen.Name == "target")


allVs <- allVs %>%
  filter(Response == "click")


## select relevant columns
# create data frame for ACQ then remove
# a monocular and binocular score for each pps
selectCs <- allCs %>%
  select(Participant.Private.ID, Eye.Condition,
         Task.Name, Trial.Number, Screen.Name,
         letter, Response,
         Attempt, Correct, Incorrect,
         Reaction.Time,
         ACQ, ACQ_answer)
csACQ <- selectCs %>%
  filter(Screen.Name == "Screen 1")
selectCs <- selectCs %>%
  filter(Screen.Name != "Screen 1") %>%
  select(-ACQ, - ACQ_answer)

# change screen name to numeric
selectCs$Screen.Name <- sub("^.....", "", selectCs$Screen.Name)
selectCs$Screen.Name <- as.numeric(selectCs$Screen.Name)

# create contrast and remove screen.name
selectCs <- selectCs %>%
  mutate(contrast = 101 - Screen.Name)

selectCs <- selectCs %>%
  select(-Screen.Name)

write.csv(selectCs, "C:/Users/cn13ws/OneDrive - University of Leeds/msc2021/data/csLong.csv")
write.csv(csACQ, "C:/Users/cn13ws/OneDrive - University of Leeds/msc2021/data/csACQ.csv")

# a monocular and binocular score for each pps
selectVa <- allVa %>%
  select(Participant.Private.ID, Eye.Condition,
         Task.Name, Trial.Number, Screen.Name,
         letter, Response,
         Attempt, Correct, Incorrect,
         Reaction.Time,
         ACQ, ACQ_answer)
vaACQ <- selectVa %>%
  filter(Screen.Name == "Screen 1")
selectVa <- selectVa %>%
  filter(Screen.Name != "Screen 1") %>%
  select(-ACQ, - ACQ_answer)

# revalue screen name to logMAR and then drop Screen.Name
selectVa <- selectVa %>%
  mutate(logMAR = case_when(
    Screen.Name == "Level1" ~ 1.1,
    Screen.Name == "Level2" ~ 1,
    Screen.Name == "Level3" ~ 0.9,
    Screen.Name == "Level4" ~ 0.8,
    Screen.Name == "Level5" ~ 0.7,
    Screen.Name == "Level6" ~ 0.6,
    Screen.Name == "Level7" ~ 0.5,
    Screen.Name == "Level8" ~ 0.4,
    Screen.Name == "Level9" ~ 0.3,
    Screen.Name == "Level10" ~ 0.2,
    Screen.Name == "Level11" ~ 0.1,
    Screen.Name == "Level12" ~ 0.0,
    Screen.Name == "Level13" ~ -0.1,
    Screen.Name == "Level14" ~ -0.2,
    Screen.Name == "Level15" ~ -0.3
  ))

selectVa <- selectVa %>%
  select(-Screen.Name)

write.csv(selectVa, "C:/Users/cn13ws/OneDrive - University of Leeds/msc2021/data/vaLong.csv")
write.csv(vaACQ, "C:/Users/cn13ws/OneDrive - University of Leeds/msc2021/data/vaACQ.csv")

# one row per participant x size x position (distance)
# pivot_wider()
selectFitts <- allFitts %>%
  select(Participant.Private.ID, Eye.Condition,
         Task.Name, Spreadsheet, Trial.Number,
         structure, xR, yR, xL, yL, size,
         Reaction.Time)

write.csv(selectFitts, "C:/Users/cn13ws/OneDrive - University of Leeds/msc2021/data/fittsLong.csv")

# mean RT of first 4 searchers 
# find 3 - number and time
# find as many as you can, number + total time
selectVs <- allVs %>%
  select(Participant.Private.ID, Eye.Condition,
         Trial.Number, Attempt, Reaction.Time,
         X.Coordinate, Y.Coordinate, VS_Puzzle)

write.csv(selectVs, "C:/Users/cn13ws/OneDrive - University of Leeds/msc2021/data/vsLong.csv")

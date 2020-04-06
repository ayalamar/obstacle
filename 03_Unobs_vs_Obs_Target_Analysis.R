# Comparing learning rate for OBS and unOBS targets
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggbeeswarm)

obssingle <- read.csv('data/DualObs_Data0305_SINGLESONLY.csv', header = TRUE)
obssingle <- tbl_df(obssingle)
obsdual <- read.csv('data/DualObs_Data0305_DUALONLY.csv', header = TRUE)
obsdual <- tbl_df(obsdual)

# LOOKING ONLY AT OBSTRUCTED TARGETS
obs_single_train <- obssingle %>%
  filter(condition %in% c('train_CCW', 'train_CW')) 

## CW trials - leftward obstacle - obstructed targets are 120, 105
obs_single_train_CW <- obs_single_train %>%
  filter(rotationval == -1) %>%
  filter(targetang %in% c(120, 105)) # obstructed targets only

obs_single_train_CW.summary <- obs_single_train_CW %>%
  group_by(trial) %>%
  summarise(Mean_RMSExy = mean(RMSExy, na.rm = TRUE), 
            SD_RMSExy = sd(RMSExy, na.rm = TRUE), 
            SEM_RMSExy = SD_RMSExy/sqrt(length(unique(subject))))

## CCW trials - rightward obstacle - obstructed targets are 60, 75
obs_single_train_CCW <- obs_single_train %>%
  filter(rotationval == 1) %>%
  filter(targetang %in% c(60, 75))

obs_single_train_CCW.summary <- obs_single_train_CCW %>%
  group_by(trial) %>%
  summarise(Mean_RMSExy = mean(RMSExy, na.rm = TRUE), 
            SD_RMSExy = sd(RMSExy, na.rm = TRUE), 
            SEM_RMSExy = SD_RMSExy/sqrt(length(unique(subject))))


# make tbls of just obstructed target sets (follow same format as previous code for unobs target sets)
# need to compare block 2 of obstructed and unobstructed target sets (RM t-test for block 2)
# maybe do a line plot comparing them


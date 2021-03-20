# Comparing learning rate for OBS and unOBS targets
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggbeeswarm)
library(OneR)

obssingle <- read.csv('data/DualObs_Data0305_SINGLESONLY.csv', header = TRUE)
obssingle <- tbl_df(obssingle)
obsdual <- read.csv('data/DualObs_Data0305_DUALONLY.csv', header = TRUE)
obsdual <- tbl_df(obsdual)

obsdual <- obsdual %>%
  filter(subject != "sg0528") # participant didn't finish
################################################################################
# SINGLE CONDITIONS 
obs_single_train <- obssingle %>%
  filter(condition %in% c('train_CCW', 'train_CW')) 
# MAKE TBLS OF OBSTRUCTED TARGETS
## CW trials - leftward obstacle - obstructed targets are 120, 105
obs_single_train_CW <- obs_single_train %>%
  filter(rotationval == 1) %>%
  filter(targetang %in% c(120, 105)) %>% # obstructed targets only
  mutate(obstructed = 1)

## CCW trials - rightward obstacle - obstructed targets are 60, 75
obs_single_train_CCW <- obs_single_train %>%
  filter(rotationval == -1) %>%
  filter(targetang %in% c(60, 75)) %>%
  mutate(obstructed = 1)

# MAKE TBLS OF UNOBSTRUCTED TARGET SETS
## CW trials - leftward obstacle - UNobstructed targets are 60, 75
unobs_single_train_CW <- obs_single_train %>%
  filter(rotationval == 1) %>%
  filter(targetang %in% c(60, 75)) %>% # UNobstructed targets only
  mutate(obstructed = 0)

## CCW trials - rightward obstacle - UNobstructed targets are 120, 105
unobs_single_train_CCW <- obs_single_train %>%
  filter(rotationval == -1) %>%
  filter(targetang %in% c(120, 105)) %>%
  mutate(obstructed = 0)

# combine all tbls to one big one to do stats
df <- bind_rows(obs_single_train_CW, obs_single_train_CCW,
                unobs_single_train_CW, unobs_single_train_CCW)

dfob <- df %>%
  filter(obstructed == 1) %>% # OBSTRUCTED
  group_by(subject) %>%
  mutate(trialn = 1:n(),
         allbinno = bin(trialn,
                        nbins = 72,
                        labels = c(1:72)))
dfun <- df %>%
  filter(obstructed == 0) %>% # UNOBSTRUCTED
  group_by(subject) %>%
  mutate(trialn = 1:n(),
         allbinno = bin(trialn,
                        nbins = 72,
                        labels = c(1:72)))

  
# need to compare block 2 of obstructed and unobstructed target sets (RM t-test for block 2)
# summary df of obstructed targets block 2
df_obs_b2 <- dfob %>%
  filter(allbinno == 2) %>%
  summarise(Mean_RMSExy = mean(RMSExy, na.rm = TRUE))
# summary df of UNobstructed targets block 2
df_unobs_b2 <- dfun %>%
  filter(allbinno == 2) %>% 
  summarise(Mean_RMSExy = mean(RMSExy, na.rm = TRUE))

t.test(df_obs_b2$Mean_RMSExy, df_unobs_b2$Mean_RMSExy,
       paired = TRUE) # compare obstructed vs unobstructed training, block 2

# compare OBSTRUCTED block 1 and block last 
df_obs_b1 <- dfob %>%
  filter(allbinno == 1) %>%
  summarise(Mean_RMSExy = mean(RMSExy, na.rm = TRUE))

df_obs_blast <- dfob %>%
  group_by(subject) %>%
  filter(allbinno == max(as.numeric(allbinno))) %>%
  summarise(Mean_RMSExy = mean(RMSExy, na.rm = TRUE))

t.test(df_obs_b1$Mean_RMSExy,
       df_obs_blast$Mean_RMSExy,
       paired = TRUE)

#compare UNobstructed block1 and blocklast
df_unobs_b1 <- dfun %>%
  filter(allbinno == 1) %>%
  summarise(Mean_RMSExy = mean(RMSExy, na.rm = TRUE))

df_unobs_blast <- dfun %>%
  group_by(subject) %>%
  filter(allbinno == max(as.numeric(allbinno))) %>%
  summarise(Mean_RMSExy = mean(RMSExy, na.rm = TRUE))

t.test(df_unobs_b1$Mean_RMSExy,
       df_unobs_blast$Mean_RMSExy,
       paired = TRUE)

# save some tbls for the BIG ANOVA
s1<- df_unobs_b1 %>%
  mutate(obstructed = 0, block = 'first', group = 'single')
s2<- df_unobs_blast %>%
  mutate(obstructed = 0, block = 'last', group = 'single')
s3<- df_obs_b1 %>%
  mutate(obstructed =1, block = 'first', group = 'single')
s4<- df_obs_blast %>%
  mutate(obstructed =1, block = 'last', group = 'single')
################################################################################
# DUAL CONDITIONS - same as above ^
obs_dual_train <- obsdual %>%
  filter(condition %in% c('train_dual')) 

obs_dual_train_CW <- obs_dual_train %>%
  filter(rotationval == 1) %>%
  filter(targetang %in% c(120, 105)) %>% # obstructed targets only
  mutate(obstructed = 1) 

obs_dual_train_CCW <- obs_dual_train %>%
  filter(rotationval == -1) %>%
  filter(targetang %in% c(60, 75)) %>%
  mutate(obstructed = 1)

unobs_dual_train_CW <- obs_dual_train %>%
  filter(rotationval == 1) %>%
  filter(targetang %in% c(60, 75)) %>% # UNobstructed targets only
  mutate(obstructed = 0)

unobs_dual_train_CCW <- obs_dual_train %>%
  filter(rotationval == -1) %>%
  filter(targetang %in% c(120, 105)) %>%
  mutate(obstructed = 0)

# combine all tbls to one big one to do stats
df <- bind_rows(obs_dual_train_CW, obs_dual_train_CCW,
                unobs_dual_train_CW, unobs_dual_train_CCW)

# block TRIALS separately (since both unobstructed and obstructed
# appear within the same training set)
dfob <- df %>%
  filter(obstructed == 1) %>% # OBSTRUCTED
  group_by(subject) %>%
  mutate(trialn = 1:n(),
         allbinno = bin(trialn,
                        nbins = 72,
                        labels = c(1:72)))
dfun <- df %>%
  filter(obstructed == 0) %>% # UNOBSTRUCTED
  group_by(subject) %>%
  mutate(trialn = 1:n(),
         allbinno = bin(trialn,
                        nbins = 72,
                        labels = c(1:72)))
# analyse block 2
# summary df of obstructed and UNobstructed targets block 2
df_obs_b2 <- dfob %>%
  filter(allbinno == 2) %>%
  summarise(Mean_RMSExy = mean(RMSExy, na.rm = TRUE))

df_unobs_b2 <- dfun %>%
  filter(allbinno == 2) %>% 
  summarise(Mean_RMSExy = mean(RMSExy, na.rm = TRUE))

t.test(df_obs_b2$Mean_RMSExy, df_unobs_b2$Mean_RMSExy,
       paired = TRUE)

# analyze adaptation for obstructed
df_obs_b1 <- dfob %>%
  filter(allbinno == 1) %>%
  summarise(Mean_RMSExy = mean(RMSExy, na.rm = TRUE))

df_obs_blast <- dfob %>%
  filter(allbinno == 72)%>%
  summarise(Mean_RMSExy = mean(RMSExy, na.rm = TRUE))

t.test(df_obs_b1$Mean_RMSExy, df_obs_blast$Mean_RMSExy,
       paired = TRUE)

# analyze adaptation for UNobstructed
df_unobs_b1 <- dfun %>%
  filter(allbinno == 1) %>%
  summarise(Mean_RMSExy = mean(RMSExy, na.rm = TRUE))

df_unobs_blast <- dfun %>%
  filter(allbinno == 72)%>%
  summarise(Mean_RMSExy = mean(RMSExy, na.rm = TRUE))

t.test(df_unobs_b1$Mean_RMSExy, df_unobs_blast$Mean_RMSExy,
       paired = TRUE)

# big 2 x 2 x 2 anova
s5<- df_unobs_b1 %>%
  mutate(obstructed = 0, block = 'first', group = 'dual')
s6<- df_unobs_blast %>%
  mutate(obstructed = 0, block = 'last', group = 'dual')
s7<- df_obs_b1 %>%
  mutate(obstructed =1, block = 'first', group = 'dual')
s8<- df_obs_blast %>%
  mutate(obstructed =1, block = 'last', group = 'dual')

dfanova <- bind_rows(s1,s2,s3,s4,s5,s6,s7,s8)
library(ez)

dfanova$obstructed <- as.character(dfanova$obstructed)
bigmod <- ezANOVA(data = dfanova,
                  dv = Mean_RMSExy,
                  wid = .(subject),
                  within= .(obstructed, block),
                  between = group)
summary(bigmod)

################################
################################
################################

# SINGLE OBSTACLE ANALYSIS

library(ggplot2)
library(dplyr)
library(tidyr)
library(ggbeeswarm)

################################
################################
################################

obssingle <- read.csv('data/DualObs_Data0305_SINGLESONLY.csv', header = TRUE)
obssingle <- tbl_df(obssingle)

################################
################################
################################

# ALL TRAINING DATA - VISUOMOTOR ADAPTATION - ROTATIONS COLLAPSED

obs_single_train <- obssingle %>%
  filter(condition %in% c('train_CCW', 'train_CW')) 

taskmeans <- obs_single_train %>%
  group_by(subject) %>%
  group_by(trial) %>% 
  summarise(Mean_RMSExy = mean(RMSExy, na.rm = TRUE),
            SD_RMSExy = sd(RMSExy, na.rm = TRUE), 
            SEM_RMSExy = SD_RMSExy/sqrt(length(unique(subject))))

ggplot(data = taskmeans, aes(x = trial, y = Mean_RMSExy)) +
  geom_line() + 
  geom_ribbon(aes(ymin = Mean_RMSExy - SEM_RMSExy, ymax = Mean_RMSExy + SEM_RMSExy),
              alpha = 0.4) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  ylim(0, 4) +
  ylab("Mean RMSExy") +
  xlab("Trial") +
  ggtitle('Single Obstacle: Training with Obstructed and Unobstructed Targets')

## stats
block1 <- obs_single_train %>%
  group_by(subject) %>% 
  filter(trial %in% min(trial)) %>%
  summarise(RMSE = mean(RMSExy, na.rm = TRUE), block = 1)

blocklast <- obs_single_train %>%
  group_by(subject) %>%
  filter(trial %in% c(max(trial)-4, max(trial)-3, max(trial)-2, max(trial)-1, max(trial))) %>% 
  summarise(RMSE = mean(RMSExy, na.rm = TRUE), block = 2)

t.test(block1$RMSE, blocklast$RMSE, alternative = "greater", paired = TRUE)

################################
################################
################################

# FROM HERE ON, ONLY ANALYSE UNOBSTRUCTED TARGET SETS!!!
# ANALYZE LEARNING FOR EACH OBSTACLE AND ONLY "FREE" TARGET SETS

## CW trials - leftward obstacle - analyze only 90, 75, and 60 degree targets
obs_single_train_CW <- obs_single_train %>%
  filter(rotationval == -1) %>%
  filter(targetang %in% c(90, 75, 60))

# trial-by-trial (mean per trial)
obs_single_train_CW.summary <- obs_single_train_CW %>%
  group_by(trial) %>%
  summarise(Mean_RMSExy = mean(RMSExy, na.rm = TRUE), 
            SD_RMSExy = sd(RMSExy, na.rm = TRUE), 
            SEM_RMSExy = SD_RMSExy/sqrt(length(unique(subject))))

## CCW trials - rightward obstacle - analyze only 120, 105, and 90 degree targets
obs_single_train_CCW <- obs_single_train %>%
  filter(rotationval == 1) %>%
  filter(targetang %in% c(120, 105, 90))

# trial-by-trial (mean per trial)
obs_single_train_CCW.summary <- obs_single_train_CCW %>%
  group_by(trial) %>%
  summarise(Mean_RMSExy = mean(RMSExy, na.rm = TRUE), 
            SD_RMSExy = sd(RMSExy, na.rm = TRUE), 
            SEM_RMSExy = SD_RMSExy/sqrt(length(unique(subject))))

unobs_df <- rbind(obs_single_train_CCW, obs_single_train_CW) # only unobstructed targets per rotation/obstacle

for (rot in sort(unique(unobs_df$rotationval))){
  
  ppdf1 <- unobs_df %>%
    filter(rotationval == rot) %>%
    group_by(subject) %>%
    filter(trial %in% c(0,1,2)) %>%
    mutate(block = 1, blockmean = mean(RMSExy, na.rm = TRUE)) %>%
    select(subject, block, blockmean) %>%
    distinct(subject, .keep_all = TRUE)
  
  ppdf2 <- unobs_df %>%
    filter(rotationval == rot) %>%
    group_by(subject) %>%
    filter(trial %in% c(3,4,5)) %>%
    mutate(block = 2, blockmean = mean(RMSExy, na.rm = TRUE)) %>%
    select(subject, block, blockmean) %>%
    distinct(subject, .keep_all = TRUE)
  
  ppdf3 <- unobs_df %>%
    filter(rotationval == rot) %>%
    group_by(subject) %>%
    filter(trial %in% c(max(trial)-2, max(trial)-1, max(trial))) %>%
    mutate(block = 7, blockmean = mean(RMSExy, na.rm = TRUE)) %>%
    select(subject, block, blockmean) %>%
    distinct(subject, .keep_all = TRUE)
  
  ppdf <- rbind(ppdf1, ppdf2, ppdf3)
  
  ppdf.summary <- ppdf %>%
    group_by(block) %>%
    summarise(group_mean = mean(blockmean, na.rm = TRUE),
              group_sd = sd(blockmean, na.rm = TRUE),
              group_sem = group_sd/sqrt(length(unique(subject))))
  
  plot_title <- sprintf('Single Training: Rotation %s', rot)
  
  block_train <- ggplot(data = ppdf,
                        aes(x = block, y = blockmean)) +
    geom_line(aes(x=block, y=blockmean, colour=as.factor(subject)), alpha = 0.1) +
    # geom_beeswarm(alpha = 0.8,
    #               dodge.width = 2,
    #               cex = 3,
    #               stroke = 0.3,
    #               aes(color=block)) +  
    geom_line(data = ppdf.summary, 
              aes(x = block, y = group_mean)) +
    geom_ribbon(data = ppdf.summary, 
                aes(x = block, ymin = group_mean - group_sem, ymax = group_mean + group_sem),
                alpha=0.4,
                inherit.aes = FALSE) +
    ylim(0,6) +
    xlim(0.5,7.5) +
    coord_fixed(ratio = 2) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          legend.title = element_blank(), legend.position = "none") +
    ylab("Reaching error (RMSExy)") +
    xlab("Block") +
    ggtitle(plot_title) 
  
  print(block_train)
  t.test(ppdf1$blockmean, ppdf3$blockmean, paired = TRUE)
  
}

# # plot of trial-by-trial learning curves - per rotation
# ggplot(data = obs_dual_train_CW.summary,
#        aes(x = trialct, y = Mean_RMSExy)) +
#   geom_line(color = 'salmon1') + 
#   geom_ribbon(aes(ymin = Mean_RMSExy-SEM_RMSExy, ymax = Mean_RMSExy+SEM_RMSExy),
#               alpha = 0.4) +
#   geom_line(data= obs_dual_train_CCW.summary, aes(x = trialct, y = Mean_RMSExy),
#             position = "jitter",
#             color = 'chartreuse1') + 
#   geom_ribbon(data = obs_dual_train_CCW.summary,
#               aes(ymin = Mean_RMSExy - SEM_RMSExy, ymax = Mean_RMSExy + SEM_RMSExy),
#               alpha = 0.4) +
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         axis.line = element_line(colour = "black")) +
#   ylab("Mean RMSExy") +
#   xlab("Trial") +
#   ggtitle('Dual Obstacle: Training with Unobstructed Targets') +
#   ylim(0, 3.5) 

################################
################################
################################

# ANALYZE REACH AFTEREFFECTS - again, only unobstructed target sets

## LEFT obstacle - analyze only 90, 75, and 60 degree targets
obs_single_NC_CW <- obssingle %>%
  filter(condition %in% c("nocur_1", "nocur_2")) %>%
  filter(rotationval == -1) %>% 
  filter(targetang %in% c(90, 75, 60)) %>%
  group_by(subject) %>%
  mutate(angmaxvel_n = angmaxvel*1) %>%
  ungroup()

## RIGHT obstacle - analyze only 120, 105, and 90 degree targets
obs_single_NC_CCW <- obssingle %>%
  filter(condition %in% c("nocur_1", "nocur_2")) %>%
  filter(rotationval == 1) %>%
  filter(targetang %in% c(120, 105, 90)) %>%
  group_by(subject) %>%
  mutate(angmaxvel_n = angmaxvel*-1) %>%
  ungroup()

unobs_NC_df <- rbind(obs_single_NC_CW, obs_single_NC_CCW)

# AE analysis but with normalized errors
unobs_NC1_collapsed <- unobs_NC_df %>%
  filter(condition == "nocur_1") %>%
  group_by(subject) %>%
  filter(trial %in% c(max(trial) - 4, max(trial) - 3, max(trial) - 2, max(trial) -1 , max(trial))) %>%
  summarise(pv_nc1 = mean(angmaxvel_n, na.rm=TRUE))

unobs_NC2_collapsed <- unobs_NC_df %>%
  filter(condition == "nocur_2") %>%
  group_by(subject) %>%
  filter(trial == min(trial)) %>%
  summarise(pv_nc2 = mean(angmaxvel_n, na.rm=TRUE))

t.test(unobs_NC2_collapsed$pv_nc2 - unobs_NC1_collapsed$pv_nc1, mu = 0, alternative = "greater")

for (rot in sort(unique(unobs_NC_df$rotationval))){
  
  print(rot)
  #last block of baseline no-cursors
  nocur1 <- unobs_NC_df %>%
    filter(rotationval == rot) %>%
    filter(condition == "nocur_1") %>%
    group_by(subject) %>%
    filter(trial %in% c(max(trial)-7, max(trial)-6, max(trial)-5, max(trial)-4, max(trial)-3, max(trial)-2, max(trial)-1, max(trial))) %>%
    summarise(pv = mean(angmaxvel, na.rm=TRUE)) %>%
    mutate(task = 'nocur1', block = 1)
  # boxplot(nocur1$pv)
  
  #first trial of no-cursor after dual training
  nocur2 <- unobs_NC_df %>%
    filter(rotationval == rot) %>%
    filter(condition == "nocur_2") %>%
    group_by(subject) %>%
    filter(trial %in% min(trial)) %>% 
    summarise(pv = mean(angmaxvel, na.rm=TRUE)) %>%
    mutate(task = 'nocur2', block = 2)
  # boxplot(nocur2$pv)
  
  nocurdf <- rbind(nocur1, nocur2)
  nocurdf$task <- factor(nocurdf$task)
  nocurdf$subject <- factor(nocurdf$subject)
  
  if (rot == 1){
    
    print(rot)
    
    nocur1$AE <- nocur2$pv - nocur1$pv # for t stats
    t.test(nocur1$AE, mu = 0, alternative = "greater")
    
  } else {
    
    nocur1$AE <- nocur2$pv - nocur1$pv # for t stats
    t.test(nocur1$AE, mu = 0, alternative = "less")
  }
  
  # plot AE for each rotation
  plot_title <- sprintf('Single Training AE: Rotation %s', rot)
  
  AE.summary <- nocur1 %>% 
    summarise(Mean_pv = mean(AE, na.rm = TRUE), 
              SD_pv = sd(AE, na.rm = TRUE), 
              SEM_pv = SD_pv/sqrt(length(unique(subject)))) %>%
    mutate(block = 1)
  
  AE_bars <- ggplot(data = AE.summary,
                    aes(x = block, y = Mean_pv)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.2) +
    geom_errorbar(data = AE.summary, mapping = aes(x = block, y = Mean_pv, 
                                                   ymin = Mean_pv - SEM_pv, ymax = Mean_pv + SEM_pv),
                  width = 0.05, size = 0.5, color = "black",
                  position = position_dodge(width = 0.9)) +
    geom_beeswarm(data = nocur1, aes(x = block, y = AE),
                  alpha = 0.8,
                  cex = 2,
                  stroke = 0.3,
                  color = 'blue') +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          legend.title = element_blank(),
          legend.position = "none") +
    coord_fixed(ratio = 1/60) +
    xlim(0.5, 1.5) +
    ylim(-60, 60) +
    ylab("Angular error (Degrees)") +
    ggtitle(plot_title) 
  
  print(AE_bars)
}

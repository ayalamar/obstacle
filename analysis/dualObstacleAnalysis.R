##################################################################
##################### obstacle analysis script ##################
library(ggplot2)
library(dplyr)

#################### dual group ###################################

obsdual <- read.csv('data/DualObs_Data0305_DUALONLY.csv', header = TRUE)
obsdual <- tbl_df(obsdual)

# training data
obs_dual_train <- obsdual %>% filter(condition == 'train_dual') # looking only at training
taskmeans<- obs_dual_train %>% group_by(subject) %>% group_by(trial) %>% 
  summarise(Mean_RMSExy = mean(RMSExy, na.rm=TRUE), SD_RMSExy = sd(RMSExy, na.rm=TRUE), 
            SEM_RMSExy = SD_RMSExy/sqrt(length(unique(subject))))

# plot - ALL training data
ggplot(data=taskmeans, aes(x=trial, y=Mean_RMSExy)) +
  geom_line() + 
  geom_ribbon(aes(ymin=Mean_RMSExy-SEM_RMSExy, ymax=Mean_RMSExy+SEM_RMSExy),
              alpha=0.4) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylim(0, 3.5) +
  ylab("Mean Root-mean-square-error") +
  xlab("Trial") +
  ggtitle('Dual Obstacle Experiment: Training')

# analyzing adaptation for each obstacle (and thus, each "free" target set - due to very large reach berth)
# CW trials - leftward obstacle - analyze only 90, 75, and 60 degree targets
obs_dual_train_CW <- obs_dual_train %>% filter(rotationval == -1) %>%
  filter(targetang %in% c(90, 75, 60)) %>%
  group_by(subject) %>%
  mutate(trialct = 1:n()) %>%
  ungroup()

obs_dual_train_CW.summary <- obs_dual_train_CW %>%
  group_by(trialct) %>%
  summarise(Mean_RMSExy = mean(RMSExy, na.rm = TRUE), 
            SD_RMSExy = sd(RMSExy, na.rm=TRUE), 
            SEM_RMSExy = SD_RMSExy/sqrt(length(unique(subject))))

obs_dual_train_CCW <- obs_dual_train %>% filter(rotationval == 1) %>%
  filter(targetang %in% c(120, 105, 90)) %>%
  group_by(subject) %>%
  mutate(trialct = 1:n()) %>%
  ungroup()

obs_dual_train_CW.summary <- obs_dual_train_CW %>%
  group_by(trialct) %>%
  summarise(Mean_RMSExy = mean(RMSExy, na.rm = TRUE), 
            SD_RMSExy = sd(RMSExy, na.rm=TRUE), 
            SEM_RMSExy = SD_RMSExy/sqrt(length(unique(subject))))

obs_dual_train_CCW.summary <- obs_dual_train_CCW %>%
  group_by(trialct) %>%
  summarise(Mean_RMSExy = mean(RMSExy, na.rm = TRUE), 
            SD_RMSExy = sd(RMSExy, na.rm=TRUE), 
            SEM_RMSExy = SD_RMSExy/sqrt(length(unique(subject))))

ggplot(data= obs_dual_train_CW.summary, aes(x=trialct, y=Mean_RMSExy)) +
  geom_line(color='salmon1') + 
  geom_ribbon(aes(ymin=Mean_RMSExy-SEM_RMSExy, ymax=Mean_RMSExy+SEM_RMSExy),
              alpha=0.4) +
  geom_line(data= obs_dual_train_CCW.summary, aes(x=trialct, y=Mean_RMSExy), position="jitter",color='chartreuse1') + 
  geom_ribbon(data= obs_dual_train_CCW.summary,aes(ymin=Mean_RMSExy-SEM_RMSExy, ymax=Mean_RMSExy+SEM_RMSExy), alpha=0.4) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylim(0, 3.5) 

##################################################################
##################################################################
##################################################################
# analyze visuomotor adaptation (dv -- RMSExy)

block1<- obs_dual_train %>% filter(trial %in% c(0)) %>% group_by(subject) %>% summarise(RMSE = mean(RMSExy, na.rm=TRUE), block = mean(block5))
blocklast<- obs_dual_train %>% filter(trial %in% c(355,356,357,358,359)) %>% group_by(subject) %>% summarise(RMSE = mean(RMSExy, na.rm=TRUE), block = ceiling(mean(block5)))
adaptdf<- rbind(block1,blocklast)

adaptdf$block <- factor(adaptdf$block)
adaptdf$subject <- factor(adaptdf$subject)

RM_RMSE <- aov(RMSE ~ block + Error(subject/block), data=adaptdf)
summary(RM_RMSE)
# the effect of block is significant (F(1,22)=8.352, p=0.0085)

# NOTE --- analyze per target location AND TAG OBSTRUCTED AND NOT
for (rot in sort(unique(obs_dual_train$rotationval))){
  
  for (target in sort(unique(obs_dual_train$targetang))){
    
    taskmeans<- obs_dual_train %>% filter(rotationval == rot | targetang == target) %>% group_by(subject) %>% group_by(trial) %>% 
      summarise(Mean_RMSExy = mean(RMSExy, na.rm=TRUE), SD_RMSExy = sd(RMSExy, na.rm=TRUE), 
                SEM_RMSExy = SD_RMSExy/sqrt(length(unique(subject))))
    
    taskplot <- ggplot(data=taskmeans, aes(x=trial, y=Mean_RMSExy)) +
      geom_line() + 
      geom_ribbon(aes(ymin=Mean_RMSExy-SEM_RMSExy, ymax=Mean_RMSExy+SEM_RMSExy),
                  alpha=0.4) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black")) +
      ylim(0, 3.5) +
      geom_smooth(method="lm", formula= (Mean_RMSExy ~ exp(trial)), se=FALSE, linetype = 1)
    
    print(taskplot)
    # DO ANOVA ON BLOCK 2 ACROSS TARGETS
    
  }
}


##################################################################
##################################################################
##################################################################
# nocursor data
obs_dual_AE <- obsdual %>% filter(condition == c("nocur_1", "nocur_2")) # only nocursors 
# AEmeans<- obs_dual_AE %>% group_by(subject) %>% group_by(trial) %>% 
#   summarise(Mean_angmaxvel = mean(angmaxvel, na.rm=TRUE), SD_angmaxvel = sd(angmaxvel, na.rm=TRUE), 
#             SEM_angmaxvel = SD_angmaxvel/sqrt(length(unique(subject))))

for (rot in sort(unique(obs_dual_AE$rotationval))){
  
  nocur1<- obs_dual_AE %>% filter(rotationval == rot) %>% filter(condition == "nocur_1") %>% filter(trial %in% c(25,26,27,28,29)) %>%
    group_by(subject) %>% summarise(pv = mean(angmaxvel, na.rm=TRUE), block = ceiling(mean(block5))) %>% mutate(task = 'nocur1')
  # boxplot(nocur1$pv)
  nocur2<- obs_dual_AE %>% filter(rotationval == rot) %>% filter(condition == "nocur_2") %>% filter(trial %in% c(0, 1, 2, 3, 4)) %>% 
    group_by(subject) %>% summarise(pv = mean(angmaxvel, na.rm=TRUE), block = ceiling(mean(block5))) %>% mutate(task = 'nocur2')
  # boxplot(nocur2$pv)
  nocurdf<- rbind(nocur1,nocur2)
  
  nocurdf$task <- factor(nocurdf$task)
  nocurdf$subject <- factor(nocurdf$subject)
  
  AE_dual_n <- nocur2$pv - nocur1$pv
  t.test(AE_dual_n, mu = 0)
  # dual CCW - t(17) = -2.29, p=0.035
  # dual CW t(20) = 2.11, p = 0.047
  
}

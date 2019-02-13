### obstacle analysis script
library(ggplot2)
library(dplyr)

## dual group 

obsdual <- read.csv('DualObs_Data0305_DUALONLY.csv', header = TRUE)
obsdual <- tbl_df(obsdual)

# training data
obs_dual_train <- obsdual %>% filter(condition == 'train_dual') # looking only at training
taskmeans<- obs_dual_train %>% group_by(subject) %>% group_by(trial) %>% 
  summarise(Mean_RMSExy = mean(RMSExy, na.rm=TRUE), SD_RMSExy = sd(RMSExy, na.rm=TRUE), 
            SEM_RMSExy = SD_RMSExy/sqrt(length(unique(subject))))
  # plot
taskplot <- ggplot(data=taskmeans, aes(x=trial, y=Mean_RMSExy)) +
  geom_line() + 
  geom_ribbon(aes(ymin=Mean_RMSExy-SEM_RMSExy, ymax=Mean_RMSExy+SEM_RMSExy),
              alpha=0.4) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylim(0, 3.5) +
  ggtitle('dual obs')

print(taskplot)

  # some basic statistics 
  # adaptation (dv -- RMSE)

block1<- obs_dual_train %>% filter(trial %in% c(0)) %>% group_by(subject) %>% summarise(RMSE = mean(RMSExy, na.rm=TRUE), block = mean(block5))
blocklast<- obs_dual_train %>% filter(trial %in% c(355,356,357,358,359)) %>% group_by(subject) %>% summarise(RMSE = mean(RMSExy, na.rm=TRUE), block = ceiling(mean(block5)))
adaptdf<- rbind(block1,blocklast)

adaptdf$block <- factor(adaptdf$block)
adaptdf$subject <- factor(adaptdf$subject)

RM_RMSE <- aov(RMSE ~ block + Error(subject/block), data=adaptdf)
summary(RM_RMSE)
# the effect of block is significant (F(1,22)=8.352, p=0.0085)

# NOTE --- analyze per target location

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

####################################################################
########################## single controls #########################
# load

obssingle <- read.csv('DualObs_Data0305_SINGLESONLY.csv', header = TRUE)
obssingle <- tbl_df(obssingle) # make to tibble
obssingle <- obssingle %>% filter(condition == c('train_CW', 'train_CCW'))

# separate groups

  for (group in sort(unique(obssingle$rotgroup))) {
    
    dfname<- sprintf('rotgroup%d_train_means', group)
    print(dfname)
    
    obs_temp <- obssingle %>% filter(rotgroup == group) # looking only at training
    #obs_single_ccw <- obssingle %>% filter(condition == 'train_CCW')
    
    # get means 
    
    taskmeans<- obs_temp %>% group_by(subject) %>% group_by(trial) %>% 
      summarise(Mean_RMSExy = mean(RMSExy, na.rm=TRUE), SD_RMSExy = sd(RMSExy, na.rm=TRUE), 
                SEM_RMSExy = SD_RMSExy/sqrt(length(unique(subject))))
    
    # get adaptation plot
    
    taskplot <- ggplot(data=taskmeans, aes(x=trial, y=Mean_RMSExy)) +
      geom_line() + 
      geom_ribbon(aes(ymin=Mean_RMSExy-SEM_RMSExy, ymax=Mean_RMSExy+SEM_RMSExy),
                  alpha=0.4) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black")) +
      ylim(0, 3.5) +
      ggtitle(dfname)
    
    print(taskplot)
    
    # some basic statistics (dv -- RMSE)
    
    block1<- obs_temp %>% filter(trial %in% c(0)) %>% group_by(subject) %>% summarise(RMSE = mean(RMSExy, na.rm=TRUE), block = mean(block5), rot = mean(rotationval))
    blocklast<- obs_temp %>% filter(trial %in% c(175,176,177,178,179)) %>% group_by(subject) %>% summarise(RMSE = mean(RMSExy, na.rm=TRUE), block = ceiling(mean(block5)), rot = mean(rotationval))
    # boxplot(blocklast$RMSE) # outlier
    adaptdf<- rbind(block1,blocklast)
    # adaptdf <- adaptdf[-c(20),] 
    
    adaptdf$block <- factor(adaptdf$block)
    adaptdf$subject <- factor(adaptdf$subject)
    
    RM_pv <- aov(RMSE ~ block + Error(subject/block), data=adaptdf)
    print(summary(RM_pv))
    
    ## CCW - (F(1,9) = 20.81, p=0.00136)
    ## CW - (F(1,8) = 17.57, p=0.00303)
    
  }

# nocursor data
obs_single_AE <- obssingle %>% filter(condition == c("nocur_1", "nocur_2")) # only nocursors 
# AEmeans<- obs_single_AE %>% group_by(subject) %>% group_by(trial) %>% 
#   summarise(Mean_angmaxvel = mean(angmaxvel, na.rm=TRUE), SD_angmaxvel = sd(angmaxvel, na.rm=TRUE), 
#             SEM_angmaxvel = SD_angmaxvel/sqrt(length(unique(subject))))

for (rot in sort(unique(obs_single_AE$rotationval))){
 
  nocur1<- obs_single_AE %>% filter(rotgroup == rot) %>% filter(condition == "nocur_1") %>% filter(trial %in% c(25,26,27,28,29)) %>%
    group_by(subject) %>% summarise(pv = mean(angmaxvel, na.rm=TRUE), block = ceiling(mean(block5))) %>% mutate(task = 'nocur1')
  # boxplot(nocur1$pv)
  # nocur1 <- nocur1[-c(2,3),] # outliers
  nocur2<- obs_single_AE %>% filter(rotgroup == rot) %>% filter(condition == "nocur_2") %>% filter(trial %in% c(0, 1, 2, 3, 4)) %>% 
    group_by(subject) %>% summarise(pv = mean(angmaxvel, na.rm=TRUE), block = ceiling(mean(block5))) %>% mutate(task = 'nocur2')
  # boxplot(nocur2$pv)
  # nocur2 <- nocur2[-c(15),] # outlying participant from training
  nocurdf<- rbind(nocur1,nocur2)
  
  nocurdf$task <- factor(nocurdf$task)
  nocurdf$subject <- factor(nocurdf$subject)
  
  ggplot(nocurdf, aes(task,pv)) +
    + geom_point()
  
  AE_single_n <- nocur2$pv - nocur1$pv
  t.test(AE_single_n, mu = 0)
 
  
  # single CW t = 1.9947, df = 12, p-value = 0.0693
  # single CCW t = -1.564, df = 10, p-value = 0.1489

  ## Compare between CW and CCW groups and collapse if possible
}
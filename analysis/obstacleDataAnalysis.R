### obstacle analysis script
library(ggplot2)
library(dplyr)

## dual group 

obsdual <- read.csv('DualObs_Data0305_DUALONLY.csv', header = TRUE)

obsdual <- tbl_df(obsdual)
obs_dual_train <- obsdual %>% filter(condition == 'train_dual') # looking only at training

# get means

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
  ylim(0, 3.5)

print(taskplot)

# some basic statistics 
# adaptation (dv -- RMSE)

block1<- obs_dual_train %>% filter(trial %in% c(0)) %>% group_by(subject) %>% summarise(RMSE = mean(RMSExy, na.rm=TRUE), block = mean(block5))
blocklast<- obs_dual_train %>% filter(trial %in% c(357,358,359)) %>% group_by(subject) %>% summarise(RMSE = mean(RMSExy, na.rm=TRUE), block = mean(block5))
adaptdf<- rbind(block1,blocklast)

adaptdf$block <- factor(adaptdf$block)
adaptdf$subject <- factor(adaptdf$subject)

RM_pv <- aov(RMSE ~ block + Error(subject/block), data=adaptdf)
summary(RM_pv)

# analyze per target location

## single controls

# load

obssingle <- read.csv('DualObs_Data0305_SINGLESONLY.csv', header = TRUE)
obssingle <- tbl_df(obssingle) # make to tibble

# separate groups
singleAnalysis <- function() {
  
  ### NOTE TO SELF - FIX LOOP
  for (rotgroup in sort(unique(obssingle$rotationval))) {
    
    dfname<- sprintf('rotgroup%d_train_means', rotgroup)
    print(dfname)
    
    obs_temp <- obssingle %>% filter(condition == c('train_CW', 'train_CCW'))
  
    obs_temp<- obs_temp %>% filter(rotationval == rotgroup) # looking only at training
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
    
    block1<- obs_temp %>% filter(trial %in% c(0)) %>% group_by(subject) %>% summarise(RMSE = mean(RMSExy, na.rm=TRUE), block = mean(block5))
    blocklast<- obs_temp %>% filter(trial %in% c(177,178,179)) %>% group_by(subject) %>% summarise(RMSE = mean(RMSExy, na.rm=TRUE), block = mean(block5))
    adaptdf<- rbind(block1,blocklast)
    
    adaptdf$block <- factor(adaptdf$block)
    adaptdf$subject <- factor(adaptdf$subject)
    
    RM_pv <- aov(RMSE ~ block + Error(subject/block), data=adaptdf)
    print(summary(RM_pv))
  }
}




# obstacle analysis script
library(ggplot2)
library(dplyr)

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
  ylim(1.5, 3.5)

print(taskplot)

# some basic statistics (dv -- RMSE)

block1<- obs_dual_train %>% filter(trial %in% c(0)) %>% group_by(subject) %>% summarise(RMSE = mean(RMSExy, na.rm=TRUE), block = mean(block5))
blocklast<- obs_dual_train %>% filter(trial %in% c(357,358,359)) %>% group_by(subject) %>% summarise(RMSE = mean(RMSExy, na.rm=TRUE), block = mean(block5))
adaptdf<- rbind(block1,blocklast)

adaptdf$block <- factor(adaptdf$block)
adaptdf$subject <- factor(adaptdf$subject)

RM_pv <- aov(RMSE ~ block + Error(subject/block), data=adaptdf)
summary(RM_pv)


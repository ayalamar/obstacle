# obstacle script
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
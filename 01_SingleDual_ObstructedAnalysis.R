# ANALYZE ALL TARGETS (OBSTRUCTED + UNOBSTRUCTED SETS)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggbeeswarm)

obssingle <- read.csv('data/DualObs_Data0305_SINGLESONLY.csv', header = TRUE)
obssingle <- tbl_df(obssingle)
obsdual <- read.csv('data/DualObs_Data0305_DUALONLY.csv', header = TRUE)
obsdual <- tbl_df(obsdual)

# per trial plots - collapsed
taskmeans_s <- obssingle %>%
  filter(condition %in% c('train_CCW', 'train_CW')) %>%
  group_by(subject) %>%
  group_by(trial) %>% 
  summarise(Mean_RMSExy = mean(RMSExy, na.rm = TRUE),
            SD_RMSExy = sd(RMSExy, na.rm = TRUE), 
            SEM_RMSExy = SD_RMSExy/sqrt(length(unique(subject)))) %>%
  mutate(group = 'single')

taskmeans_d <- obsdual %>%
  filter(condition == 'train_dual') %>%
  group_by(rotationval, subject) %>%
  mutate(trial = 1:n()) %>% # collapsed across rotations in the dual group
  ungroup() %>%
  group_by(trial) %>% 
  summarise(Mean_RMSExy = mean(RMSExy, na.rm = TRUE),
            SD_RMSExy = sd(RMSExy, na.rm = TRUE), 
            SEM_RMSExy = SD_RMSExy/sqrt(length(unique(subject)))) %>%
  mutate(group = 'dual')

taskmeans <- rbind(taskmeans_s,taskmeans_d)

ggplot(data = taskmeans, aes(x = trial, y = Mean_RMSExy)) +
  geom_line() + 
  geom_ribbon(aes(ymin = Mean_RMSExy - SEM_RMSExy, ymax = Mean_RMSExy + SEM_RMSExy),
              alpha = 0.4) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  ylim(0, 4) +
  ylab("Reaching error (RMSE)") +
  scale_x_continuous(name="Trial", limits=c(0, 180), breaks = seq(0,180,30)) +
  ggtitle('Training All Targets') +
  facet_grid(cols = vars(group)) +
  coord_fixed(ratio=30)

# per block plots - collapsed
blockdf_s <- obssingle %>%
  filter(condition %in% c('train_CCW', 'train_CW')) %>%
  filter(block5 %in% c(0, 1, 35)) %>%
  group_by(subject, block5) %>%
  mutate(Mean_RMSExy = mean(RMSExy, na.rm = TRUE)) %>%
  ungroup()%>%
  group_by(block5) %>%
  summarise(GroupMean_RMSExy = mean(RMSExy, na.rm = TRUE),
            SD_RMSExy = sd(RMSExy, na.rm = TRUE), 
            SEM_RMSExy = SD_RMSExy/sqrt(length(unique(subject))),
            group = 'single')

blockdf_d <- obsdual %>%
  filter(condition == 'train_dual') %>%
  filter(block5 %in% c(0, 1, 71)) %>%
  group_by(subject, block5) %>%
  mutate(Mean_RMSExy = mean(RMSExy, na.rm = TRUE)) %>%
  ungroup()%>%
  group_by(block5) %>%
  summarise(GroupMean_RMSExy = mean(RMSExy, na.rm = TRUE),
            SD_RMSExy = sd(RMSExy, na.rm = TRUE), 
            SEM_RMSExy = SD_RMSExy/sqrt(length(unique(subject))),
            group = 'dual')

blockdf_d$block5[which(blockdf_d$block5 == 71)] <- 35 #match block numbers

blockdf <- rbind(blockdf_s, blockdf_d)
blockdf$block5[which(blockdf$block5 == 1)] <- 7
ggplot(data = blockdf, aes(x = block5, y = GroupMean_RMSExy)) +
  geom_line() + 
  geom_point() +
  geom_ribbon(aes(ymin = GroupMean_RMSExy - SEM_RMSExy, ymax = GroupMean_RMSExy + SEM_RMSExy),
              alpha = 0.4) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  ylim(0, 4) +
  ylab("Reaching error (RMSE)") +
  scale_x_continuous(name="Block (5 trials)", limits=c(0, 35)) +
  ggtitle('Training All Targets') +
  facet_grid(cols = vars(group)) +
  coord_fixed(ratio=20)

# plot reach AE singles
NC_df <- obssingle %>%
  filter(condition %in% c("nocur_1", "nocur_2")) %>%
  drop_na(subject)

for (rot in c(-1,1)){
  
  #last block of baseline no-cursors
  nocur1 <- NC_df %>%
    filter(rotationval == rot) %>%
    filter(condition == "nocur_1") %>%
    group_by(subject) %>%
    filter(trial %in% c(max(trial)-7, max(trial)-6, max(trial)-5, max(trial)-4, max(trial)-3, max(trial)-2, max(trial)-1, max(trial))) %>%
    summarise(pv = mean(angmaxvel, na.rm = TRUE)) %>%
    mutate(task = 'nocur1', block = 1)
  
  #first trial of no-cursor after training
  nocur2 <- NC_df %>%
    filter(rotationval == rot) %>%
    filter(condition == "nocur_2") %>%
    group_by(subject) %>%
    filter(trial %in% min(trial)) %>% 
    summarise(pv = mean(angmaxvel, na.rm = TRUE)) %>%
    mutate(task = 'nocur2', block = 2)
  
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
  plot_title <- sprintf('Single AE %s', rot)
  
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
                  alpha = 0.5,
                  cex = 2,
                  stroke = 0.5,
                  color = 'gray50') +
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
    ggtitle(plot_title) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  
  print(AE_bars)
  
}

# plot reach AE duals
NC_df <- obsdual %>%
  filter(condition %in% c("nocur_1", "nocur_2")) %>%
  drop_na(subject)

for (rot in c(-1,1)){
  
  #last block of baseline no-cursors
  nocur1 <- NC_df %>%
    filter(rotationval == rot) %>%
    filter(condition == "nocur_1") %>%
    group_by(subject) %>%
    filter(trial %in% c(max(trial)-7, max(trial)-6, max(trial)-5, max(trial)-4, max(trial)-3, max(trial)-2, max(trial)-1, max(trial))) %>%
    summarise(pv = mean(angmaxvel, na.rm = TRUE)) %>%
    mutate(task = 'nocur1', block = 1)
  
  #first trial of no-cursor after training
  nocur2 <- NC_df %>%
    filter(rotationval == rot) %>%
    filter(condition == "nocur_2") %>%
    group_by(subject) %>%
    filter(trial %in% min(trial)) %>% 
    summarise(pv = mean(angmaxvel, na.rm = TRUE)) %>%
    mutate(task = 'nocur2', block = 2)
  
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
  plot_title <- sprintf('Dual AE %s', rot)
  
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
                  alpha = 0.5,
                  cex = 2,
                  stroke = 0.5,
                  color = 'gray50') +
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
    ggtitle(plot_title) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  
  print(AE_bars)
  
}



rm(list = ls(all = TRUE))

# Load packages.
# Data manipulation.
library(data.table)
library(dplyr)
library(stringr)
library(rio)
library(tidyverse)
library(janitor)
library(parallel)
library(tidyr)

# Plots.
library(ggplot2)
library(cowplot)
library(ggsignif)
library(lemon)

# Analyses.
library(afex)
library(lme4)
library(lmerTest)

# Read in files.
file_names <- list.files(path = ".", pattern = "*.csv", all.files = FALSE,
                         full.names = FALSE, recursive = FALSE)

# Create data frame.
df <- data.frame()

# Loop to create combined dataframe.
for (i in file_names) {
  data <- fread(i, header = TRUE, sep = ",")
  data$response <- lead(data$response_response, 1) #shifts responses one row up since OpenSesame logs them a row down
  data$response_time <- lead(data$response_time_response, 1) #shifts RTs one row up since OpenSesame logs them a row down
  df <- rbind(df, data,fill=T)
}

#### Clean up data. ####

# Remove row (one per subject) that has a NA in response, as a result of shifting RTs up
df <- df %>% drop_na(response)

# Remove columns related to audio onset that were in for debugging purposes
df <- select(df,-9:-26)

# Remove columns where responses/RTs were logged incorrectly by OpenSesame
df <- select(df,-6:-7)

# Create variable to indicate if it was male or female talker on trial
df$talker <- ifelse(grepl("m150",df$current_stimulus),"m150","m160")

# Create variable to indicate which sort of expectations the participant had
df$Expectations <- NA
df$Expectations[which(as.numeric(substr(df$subject_nr, 2, 3)) %% 8 == 1 | 
                        as.numeric(substr(df$subject_nr, 2, 3)) %% 8 == 2 | 
                        as.numeric(substr(df$subject_nr, 2, 3)) %% 8 == 3 | 
                        as.numeric(substr(df$subject_nr, 2, 3)) %% 8 == 4)] <- "One Voice Instructions"
df$Expectations[which(as.numeric(substr(df$subject_nr, 2, 3)) %% 8 == 5 | 
                        as.numeric(substr(df$subject_nr, 2, 3)) %% 8 == 6 | 
                        as.numeric(substr(df$subject_nr, 2, 3)) %% 8 == 7 | 
                        as.numeric(substr(df$subject_nr, 2, 3)) %% 8 == 0)] <- "Two Voice Instructions"

# Indicate which item number this was within a trial
df$itemNo <- row(as.matrix(df$current_stimulus)) %% 16; df$itemNo[which(df$itemNo==0)] <- 16

# Create variable to add row number for next step
df <- df[order(as.numeric(Trial)),]
df <- df[order(subject_nr),]
df$row <- as.numeric(1:length(df$Trial))

# Responses within lag time (150 ms in M&N 2007) are counted as responses to the previous item
df$corrected_rt <- as.numeric(df$response_time)
df$corrected_rt[which(df$response_time<=150 & df$itemNo > 1)-1] <- df$corrected_rt[which(df$response_time<=150 & df$itemNo > 1)] + 750
df$corrected_rt[which(df$corrected_rt<=150)] <- NA

# Set subject number as factor
df$subject_nr <- as.factor(df$subject_nr)

# Keep only trials in which audio matches target.
df$trial_stimulus <- substr(df$current_stimulus,5,nchar(df$current_stimulus)-4)
df$trialtype <- ifelse(df$Target==df$trial_stimulus,"Target","Distractor")

# Calculate accuracy in obvious way, noting hits/correct rejections as accurate and misses/false alarms as wrong
df$accuracy <- ifelse(df$trialtype=="Target"&df$response=="space"|df$trialtype=="Distractor"&df$response=="None",1,0)

# Separate target trials for RT analysis.                       
target.trials <- subset(df,df$Target==df$trial_stimulus)

# Remove target trials where participants hit wrong key.
target.trials <- subset(target.trials,target.trials$response=="None"|target.trials$response=="space")

# Identify trials where subjects made a correct response immediately after the target item
a <- (which(df$Target==df$trial_stimulus&df$response=="None")+1) # Grab indices for post-target item
missed.targets <- df[a] # Subset data to post-target items
missed.targets <- subset(missed.targets, as.numeric(response_time) <= 150) # Subset post-target items to those with RT <= 150 ms
missed.targets <- subset(missed.targets,response=="space") # Remove trials where participants didn't make spacebar response to the post-target items

# Calculate accuracy for complex trials (response in trial after target)
df$accuracy <- ifelse(df$row %in% (missed.targets$row-1),1,df$accuracy) # If subjects made a correct response shortly after the target, mark their performance to the target correct
df$accuracy <- ifelse(df$row %in% missed.targets$row,NA,df$accuracy) # And then null out the accuracy for the post-target trial

# Now we grab just the target trials that have a space response
target.trials <- subset(target.trials, response!="None")

# And merge them with the complex trials to have our set of critical trials that we'll analyze
critical.trials <- rbind(target.trials,missed.targets)

# Set variables as appropriate types.
critical.trials$Condition <- as.factor(critical.trials$Condition)
critical.trials$Talker <- as.factor(critical.trials$Talker)
critical.trials$response <- as.factor(critical.trials$response)
critical.trials$response_time <- as.numeric(critical.trials$response_time)
critical.trials$subject_nr <- as.factor(critical.trials$subject_nr)

# Order by row number.
critical.trials <- critical.trials[order(row),]

#  Now we can remove the trials after the target.
critical.trials <- subset(critical.trials,Target==trial_stimulus)

# Calculate accuracy.
performance <- df %>%
  group_by(subject_nr,Expectations) %>%
  summarize(acc=mean(na.omit(accuracy)))

# Make a list of subjects to exclude
exclude <- filter(performance,acc<=0.9)

critical.trials <- droplevels(critical.trials %>% 
                                filter(!subject_nr %in% exclude$subject_nr))

df.goodSubj <- droplevels(df %>% 
                            filter(!subject_nr %in% exclude$subject_nr))



# New accuracy analysis with only included subjects

performance.goodSubj <- performance %>% 
                                filter(!subject_nr %in% exclude$subject_nr)
#### Outcomes of interest. ####

acc_model_slope <- mixed(accuracy ~ Condition * Expectations + (Condition||subject_nr),
                         data=df.goodSubj,family="binomial",method="LRT",expand_re = TRUE,
                         control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 1500000)))
acc_model_int <- mixed(accuracy ~ Condition * Expectations + (1|subject_nr),
                       data=df.goodSubj,family="binomial",method="LRT",expand_re = TRUE,
                       control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 1500000)))

anova(acc_model_slope, acc_model_int) #slope model fits better

acc_model_slope
summary(acc_model_slope)


# Average performance by conditions
avg.performance <- performance %>%
  filter(!subject_nr %in% exclude$subject_nr) %>% 
  group_by(Expectations) %>%
  summarize(avg=mean(acc), n_subj = length(subject_nr))

# Calculate reaction time by condition and talker.
rt <- critical.trials %>%
  group_by(Expectations, Condition) %>%
  summarise(rt=mean(response_time),SD=sd(response_time))

#### Plots####

# What is distribution of RTs?
ggplot(critical.trials, aes(corrected_rt)) + geom_density()
# non-transformed RTs look reasonable

# Visualize RTs by subjects
qqnorm(critical.trials$corrected_rt); qqline(critical.trials$corrected_rt); 

# Plotting prep
# Mean RTs by subject
rt_summary <- Rmisc::summarySE(data = critical.trials, measurevar = "corrected_rt", 
                               groupvars = c("subject_nr","Expectations", "Condition"), na.rm = TRUE)

# Ensure factors are set appropriately
rt_summary$Expectations <- as.factor(rt_summary$Expectations)

# Mean RTs by group to get CI's for error bar
rt_summary_byGroup <- Rmisc::summarySE(data = critical.trials, measurevar = "corrected_rt",
                                       groupvars = c("Expectations", "Condition"), na.rm = TRUE)
rt_summary_byGroup$CondNum <- ifelse(rt_summary_byGroup$Condition=="Blocked",1,2)
rt_summary_byGroup$Expectations <- as.factor(rt_summary_byGroup$Expectations)


# For plotting purposes, create numeric version of condition and variable to allow for offsetting of points
rt_summary$CondNum <- ifelse(rt_summary$Condition=="Blocked",1,2)
rt_summary$xPos <- ifelse(rt_summary$Condition=="Blocked",1.25,1.75)


theme_set(theme_bw(base_size = 20))

# Create behavioral data figure.
behav_fig <- ggplot(rt_summary, aes(CondNum, corrected_rt, fill=Condition)) + 
  geom_boxplot(aes(group=Condition,fill=Condition),width=0.4) +
  geom_point(data=rt_summary,aes(x=xPos), color = "black") + 
  geom_line(data=rt_summary,aes(group=subject_nr,x=xPos),stat="summary", color = "black") +
  stat_summary(fun.y=mean, geom="point", shape=1, size=3) +
  scale_x_continuous("Condition",breaks=c(1,2),labels=c("Blocked","Mixed")) +
  scale_fill_manual(values=c("#ff6e26","#26b7ff")) + 
  facet_rep_wrap(.~Expectations,ncol=2,repeat.tick.labels=TRUE) +
  labs(y = "Reaction time (ms)") + coord_cartesian(ylim = c(375,725)) + 
  theme(legend.position = "none",strip.text = element_text(size = 18), axis.title.x = element_blank())
behav_fig
ggsave("TalkerExpectations_E1_n88.png", width = 20, height = 10, units = "cm")


# Create summary barplot of the MTPC for each level of expectations.
rt_differences <- rt[1:3] %>% spread(Condition, rt, drop=TRUE)
rt_differences$diff <- rt_differences$Mixed-rt_differences$Blocked

MTPC <- ggplot(rt_differences,aes(x=as.factor(Expectations),y=diff)) +
  geom_bar(stat="identity") + labs(y="Multi-talker processing cost (ms)", x="Expectations") + 
  theme(legend.position = "none") + ylim(0,25) + 
  geom_signif(xmin=1,xmax=1,y_position=23,tip_length=0,annotation ="***",textsize=8) +
  geom_signif(xmin=3,xmax=3,y_position=23,tip_length=0,annotation ="***",textsize=8)


behav_fig
MTPC


# Mixed effects models
# Backward stepping with Gamma distribution
rt_model_slope_gamma <- mixed(corrected_rt ~ Condition * Expectations + (Condition||subject_nr),
      data=critical.trials,family=Gamma(link="identity"),method="LRT",expand_re = TRUE,
      control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 1500000)))

rt_model_int_gamma <- mixed(corrected_rt ~ Condition * Expectations + (1|subject_nr),
                            data=critical.trials,family=Gamma(link="identity"),method="LRT",expand_re = TRUE,
                            control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 1500000)))


anova(rt_model_slope_gamma,rt_model_int_gamma) #slope model fits better

# Backward stepping with inverse Gaussian distribution
rt_model_slope_invgauss <- mixed(corrected_rt ~ Condition * Expectations + (Condition||subject_nr),
                              data=critical.trials,family=inverse.gaussian(link="identity"),method="LRT",expand_re = TRUE,
                              control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 1500000)))

rt_model_int_invgauss <- mixed(corrected_rt ~ Condition * Expectations + (1|subject_nr),
                            data=critical.trials,family=inverse.gaussian(link="identity"),method="LRT",expand_re = TRUE,
                            control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 1500000)))

anova(rt_model_slope_invgauss,rt_model_int_invgauss) #slope model fits better

# Compare Gamma and inverse Gaussian distributions
anova(rt_model_slope_gamma,rt_model_slope_invgauss) #models fit equally well

rt_model_int_gamma

rt_model_slope_invgauss


# ANOVA (for direct comparison with M&N)

aov_model <- aov(corrected_rt ~ Condition * Expectations, data=rt_summary)
summary(aov_model)

save.image("TalkerExpectations_E1_2020.12.01.RData")

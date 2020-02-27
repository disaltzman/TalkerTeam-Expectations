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

# Remove columns related to audio onset that were in for debugging purposes
df <- select(df,-9:-26)

# Remove columns where responses/RTs were logged incorrectly by OpenSesame
df <- select(df,-6:-7)

# Create variable to indicate if it was male or female talker on trial
df$talker <- ifelse(grepl("m150",df$current_stimulus),"m150","m160")

# Create variable to indicate which sort of expectations the participant had
df$Expectations <- NA
df$Expectations[which((df$subject_nr-100) %% 8 == 1 | (df$subject_nr-100) %% 8 == 2 | 
                        (df$subject_nr-100) %% 8 == 3 | (df$subject_nr-100) %% 8 == 4)] <- "One Voice Instructions"
df$Expectations[which((df$subject_nr-100) %% 8 == 5 | (df$subject_nr-100) %% 8 == 6 | 
                        (df$subject_nr-100) %% 8 == 7 | (df$subject_nr-100) %% 8 == 0)] <- "Two Voice Instructions"

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
target.trials <- subset(df,df$Target==df$trial_stimulus)

# Remove target trials where participants hit wrong key.
target.trials <- subset(target.trials,target.trials$response=="None"|target.trials$response=="space")

# Keep trials where participant responded immediately after the target trial.
a <- (which(df$Target==df$trial_stimulus&df$response=="None")+1)
missed.targets <- df[a]
missed.targets <- subset(missed.targets, as.numeric(response_time) <= 150)

# Remove trials where participants didn't respond in the post-target trial.
b <- (which(missed.targets$response=="None"))
c <- missed.targets[b,row]-1
target.trials <- subset(target.trials,!(row %in% c))
missed.targets <- subset(missed.targets,response=="space")

target.trials <- subset(target.trials, response!="None")

# Combine these together into single data frame.
critical.trials <- rbind(target.trials,missed.targets)

# Set variables as appropriate types.
critical.trials$Condition <- as.factor(critical.trials$Condition)
critical.trials$Talker <- as.factor(critical.trials$Talker)
critical.trials$response <- as.factor(critical.trials$response)
critical.trials$response_time <- as.numeric(critical.trials$response_time)
critical.trials$subject_nr <- as.factor(critical.trials$subject_nr)

# Order by row number.
critical.trials <- critical.trials[order(row),]

#  Now we can now remove the trials after the target.
critical.trials <- subset(critical.trials,Target==trial_stimulus)

#### Outcomes of interest. ####

#### Accuracy calculations ####
all.targets <- subset(df,df$Target==df$trial_stimulus)
all.targets$Accuracy <- ifelse(all.targets$row %in% critical.trials$row,1,0)

acc_model_slope <- mixed(Accuracy ~ Condition * Expectations + (Condition||subject_nr),
                        data=all.targets,family="binomial",method="LRT",expand_re = TRUE,
                        control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 1500000)))
acc_model_int <- mixed(Accuracy ~ Condition * Expectations + (1|subject_nr),
                        data=all.targets,family="binomial",method="LRT",expand_re = TRUE,
                        control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 1500000)))

anova(acc_model_slope, acc_model_int) #slope model fits better

acc_model_slope
summary(acc_model_slope)

# Calculate accuracy based on the number of critical trials they responded to out of 384.
performance <- critical.trials %>%
  group_by(subject_nr,Expectations) %>%
  summarize(n_corr=length(Trial))

# Divide by number of total trials.
performance$accuracy <- (performance$n_corr/384)

# Make a list of subjects to exclude
exclude <- sort(as.numeric(levels(droplevels(performance$subject_nr[which(performance$accuracy <= 0.9)]))))

critical.trials <- droplevels(critical.trials %>% 
                                filter(!subject_nr %in% exclude))


# Average performance by conditions
avg.performance <- performance %>%
  filter(!subject_nr %in% exclude) %>% 
  group_by(Expectations) %>%
  summarize(avg=mean(accuracy), n_subj = length(subject_nr))

# Calculate reaction time by condition and talker.
rt <- critical.trials %>%
  group_by(Expectations, Condition) %>%
  summarise(rt=mean(response_time),SD=sd(response_time))

#### Plots####

# What is distribution of RTs?
ggplot(critical.trials, aes(corrected_rt)) + geom_density()
ggplot(critical.trials, aes(log(corrected_rt))) + geom_density()
# non-transformed RTs look reasonable

# Visualize RTs by subjects
qqnorm(critical.trials$corrected_rt); qqline(critical.trials$corrected_rt); 
qqnorm(log(critical.trials$corrected_rt)); qqline(log(critical.trials$corrected_rt)); 

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
  theme(legend.position = "none",strip.text = element_text(size = 18))

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
anova(rt_model_slope_gamma,rt_model_slope_invgauss) #inverse Gaussian fits better


rt_model_slope_invgauss
summary(rt_model_slope_invgauss)


# ANOVA (for direct comparison with M&N)
aov_model <- aov(corrected_rt ~ Condition * Expectations, data=critical.trials)
summary(aov_model)

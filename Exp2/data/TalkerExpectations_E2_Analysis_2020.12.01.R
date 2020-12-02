# Basic R setup ----
rm(list = ls(all = TRUE))

# Load packages.
# Loading data.
library(here)
library(bit64)

# Data manipulation.
library(data.table)
library(Rmisc)
library(dplyr)
library(stringr)
library(rio)
library(tidyverse)
library(janitor)

# Plots.
library(ggplot2)
library(cowplot)
library(psych)
library(lemon)

# Analyses.
library(afex)
library(lme4)
library(lmerTest)

theme_set(theme_bw(base_size = 20))

# Load Data ----
# Read in files
file_names <- list.files(path = "./data", pattern = "*.csv", all.files = FALSE,
                         full.names = TRUE, recursive = FALSE)

# Create data frame
df <- data.frame()

# Loop to create combined dataframe
for (i in file_names) {
  data <- fread(i, header = TRUE, sep = ",", fill = TRUE)
  df <- rbind(df, data)
}

# Clean up environment
rm(data); rm(file_names); rm(i) 

# Data Housekeeping ----
# Make variable names syntactically valid - everything is machine readable now
df <- clean_names(df)

# Determine cb condition from spreadsheet_name column
df$spreadsheet_name <- substr(df$spreadsheet_name, 
                              nchar(df$spreadsheet_name), nchar(df$spreadsheet_name))

# Give columns more informative names
colnames(df)[which(colnames(df)=="randomiser_czzd")] <- "expectations"
colnames(df)[which(colnames(df)=="spreadsheet_name")] <- "cb"
colnames(df)[which(colnames(df)=="participant_public_id")] <- "subject"
colnames(df)[which(colnames(df)=="checkpoint_7g4v")] <- "headphone_check"
colnames(df)[which(colnames(df)=="fname")] <- "audio"

# Store responses to debriefing questionnaire separately
debrief <- droplevels(subset(df, screen_name == "Q1" | screen_name == "Q2 & Q3"))

# Select only rows/columns we want
df <- dplyr::filter(df,df$trial_number != "BEGIN TASK")
df <- dplyr::filter(df,df$trial_number != "END TASK")
df <- dplyr::filter(df,df$zone_type == "response_keyboard_single")
df <- dplyr::filter(df,df$attempt == 1)

df <- select(df, subject, headphone_check, expectations, cb, trial_number, 
             condition, talker, audio, 
             response, correct_response, correct, reaction_time)

debrief <- select(debrief, subject, expectations, headphone_check, 
                  screen_name, zone_type, response)

# Set variable type
to.factor <- c('subject','headphone_check','expectations','cb','condition','talker','audio', 'response', 'correct_response') 
df[, to.factor] <- lapply(df[, to.factor], as.factor)

to.numeric <- c('trial_number','correct','reaction_time')
df[, to.numeric] <- lapply(df[, to.numeric], as.numeric)

# Load Demographics ----
# Read in files
file_names <- list.files(path = "./data/questionnaire", pattern = "*.csv", all.files = FALSE,
                         full.names = TRUE, recursive = FALSE)

# Create data frame
demographics <- data.frame()

# Loop to create combined dataframe
for (i in file_names) {
  data <- fread(i, header = TRUE, sep = ",")
  demographics <- rbind(demographics, data)
}

# Clean up environment
rm(data); rm(file_names); rm(i) 

# Clean up demographics
demographics <- clean_names(demographics)
colnames(demographics)[which(colnames(demographics)=="participant_public_id")] <- "subject"
demographics <- select(demographics, subject, question_key, response)
demographics <- separate(demographics, question_key, c(NA, "question_key",NA), sep = "-")
demographics <- dplyr::filter(demographics,
                              demographics$question_key == "sex" | 
                                demographics$question_key == "race" | 
                                demographics$question_key == "ethnicity")
demographics <- unique(demographics)
demographics <- demographics %>% 
  pivot_wider(names_from = question_key, values_from = response) # R will complain if there's some subjects with multiple responses (e.g., multiple selections for race), but it'll work fine if you just output the demographics to the R console

# Count Good Subjects ----
# How many subjects do we have?
subj_count_all <- nrow(df %>% group_by(subject)  %>% dplyr::summarise(count = length(subject)))
subj_count_all

# Select only subjects who passed the headphone check 
df <- filter(df, headphone_check == "Pass")
debrief <- filter(debrief, headphone_check == "Pass")

# How many subjects were taken out?
subj_count_HC <- nrow(df %>% group_by(subject)  %>% dplyr::summarise(count = length(subject)))
subj_count_all-subj_count_HC

# For remanining subjects, look at accuracy by subject
acc_summary <- df %>% 
  group_by(expectations, cb, subject) %>% 
  dplyr::summarise(accuracy = mean(correct))

# How many subjects had less than 90% accuracy on task?
excl_Acc <- acc_summary$subject[which(acc_summary$accuracy < 0.9)]
length(excl_Acc)

# Drop subjects with less than 90% accuracy on task
df <- df %>% dplyr::filter(!subject %in% excl_Acc)
debrief <- debrief %>% dplyr::filter(!subject %in% excl_Acc)

# Update accuracy summary so that we only have the good subjects included
acc_summary <- df %>% 
  group_by(expectations, cb, subject) %>% 
  dplyr::summarise(accuracy = mean(correct))

# Check how many subjects in each condition
cb_summary <- df %>%
  group_by(expectations, cb, subject) %>% 
  dplyr::summarise(count = length(subject)) %>% 
  dplyr::summarise(count = length(count))
cb_summary
sum(cb_summary$count) # Number of usable participants

# Plots ----

# Plotting prep
# Mean RTs by subject
rt_summary <- Rmisc::summarySE(data = df, measurevar = "reaction_time", 
                               groupvars = c("subject","expectations", "condition"), na.rm = TRUE)
levels(rt_summary$expectations) <- c("One Voice Instructions","Two Voice Instructions")

# Mean RTs by group to get CI's for error bar
rt_summary_byGroup <- Rmisc::summarySE(data = df, measurevar = "reaction_time",
                                       groupvars = c("expectations", "condition"), na.rm = TRUE)
rt_summary_byGroup$condNum <- ifelse(rt_summary_byGroup$condition=="blocked",1,2)
rt_summary_byGroup$expectations <- as.factor(rt_summary_byGroup$expectations)
rt_summary_byGroup

# For plotting purposes, create numeric version of condition and variable to allow for offsetting of points
rt_summary$condNum <- ifelse(rt_summary$condition=="blocked",1,2)
rt_summary$xPos <- ifelse(rt_summary$condition=="blocked",1.25,1.75)


# Create behavioral data figure.
behav_fig <- ggplot(rt_summary, aes(condNum, reaction_time, fill=condition)) + 
  geom_boxplot(aes(group=condition,fill=condition),width=0.4) +
  geom_point(data=rt_summary,aes(x=xPos), color = "black") + 
  geom_line(data=rt_summary,aes(group=subject,x=xPos),stat="summary", color = "black") +
  stat_summary(fun.y=mean, geom="point", shape=1, size=3) +
  scale_x_continuous("Condition",breaks=c(1,2),labels=c("Blocked","Mixed")) +
  scale_fill_manual(values=c("#ff6e26","#26b7ff")) + 
  facet_rep_wrap(.~expectations,ncol=2,repeat.tick.labels=TRUE) +
  labs(y = "Reaction time (ms)") + 
  theme(legend.position = "none",strip.text = element_text(size = 18), axis.title.x = element_blank())
behav_fig
ggsave("TalkerExpectations_E2_n88.png", width = 20, height = 10, units = "cm")


# Statistical Analyses ----

# Analyze accuracy data
acc_model_slope <- mixed(correct ~ condition * expectations + (condition||subject),
                         data=df, family="binomial", method="LRT", expand_re = TRUE,
                         control = glmerControl(optimizer="bobyqa", calc.derivs = FALSE, 
                                                optCtrl = list(maxfun = 1500000)))
acc_model_int <- mixed(correct ~ condition * expectations + (1|subject),
                       data=df, family="binomial", method="LRT", expand_re = TRUE,
                       control = glmerControl(optimizer="bobyqa", calc.derivs = FALSE, 
                                              optCtrl = list(maxfun = 1500000)))

anova(acc_model_slope, acc_model_int) 

acc_model_slope


# Analyze RT data

# Mixed effects models
# Backward stepping with Gamma distribution
rt_model_slope_gamma <- mixed(reaction_time ~ condition * expectations + (condition||subject),
                              data=df, family=Gamma(link="identity"), method="LRT", expand_re = TRUE,
                              control = glmerControl(optimizer="bobyqa", calc.derivs = FALSE,
                                                     optCtrl = list(maxfun = 1500000)))

rt_model_int_gamma <- mixed(reaction_time ~ condition * expectations + (1|subject),
                            data=df, family=Gamma(link="identity"), method="LRT", expand_re = TRUE,
                            control = glmerControl(optimizer="bobyqa", calc.derivs = FALSE, 
                                                   optCtrl = list(maxfun = 1500000)))


anova(rt_model_slope_gamma,rt_model_int_gamma) #gamma slope is better fit

# Backward stepping with inverse Gaussian distribution
rt_model_slope_invgauss <- mixed(reaction_time ~ condition * expectations + (condition||subject),
                                 data=df, family=inverse.gaussian(link="identity"), method="LRT", expand_re = TRUE,
                                 control = glmerControl(optimizer="bobyqa", calc.derivs = FALSE, 
                                                        optCtrl = list(maxfun = 1500000)))

rt_model_int_invgauss <- mixed(reaction_time ~ condition * expectations + (1|subject),
                               data=df, family = inverse.gaussian(link="identity"), method="LRT", expand_re = TRUE,
                               control = glmerControl(optimizer="bobyqa", calc.derivs = FALSE, 
                                                      optCtrl = list(maxfun = 1500000)))

anova(rt_model_slope_invgauss,rt_model_int_invgauss) #invgauss slope is better fit

# Compare Gamma and inverse Gaussian distributions
anova(rt_model_slope_gamma,rt_model_slope_invgauss) # models fit equally well

# Go with gamma model
rt_model_slope_gamma

# ANOVA for RT data (for direct comparison with M&N)
aov_model <- aov(reaction_time ~ condition * expectations, data=df)
summary(aov_model)

# Analyze debriefing data ----

# Tidy up debrief df
debrief <- dplyr::filter(debrief,debrief$zone_type != "continue_button")
debrief$screen_name[which(debrief$screen_name=="Q2 & Q3" & debrief$zone_type == "response_text_area")] <- "Q2"
debrief$screen_name[which(debrief$screen_name=="Q2 & Q3" & debrief$zone_type == "response_rating_scale_likert_active")] <- "Q3"
debrief <- select(debrief, subject, expectations, screen_name, response)
debrief <- spread(debrief, screen_name, response)

# Save workspace ----
save.image("exp2-results-2020.12.01.RData")

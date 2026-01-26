rm(list = ls())
cat("\014")
#dev.off()
library(tidyr)
library(ggplot2)
library(dplyr)
library(rstatix)
library(readr)
library(lmerTest)
library(lmtest)
library(lme4)
library(emmeans)
library(psych)
library(tidyverse)
library(effsize)
library(effectsize)
library(betareg)
library(performance)
library(lattice)
library(Hmisc)
library(lsr)
library(glmmTMB)
library(corrplot)

#reset path if needed
setwd(here::here())


#changing R contrast default for ANOVA and Analysis of Deviance Type 3 tests
options(contrasts = c("contr.sum", "contr.poly"))


#load data
aggregate_data <- read_csv('./data/3min_Aggregated_Combined.csv')
aggregate_data$Participant_Code <- 
  as.factor(aggregate_data$Participant_Code)
aggregate_data$Condition <- 
  factor(aggregate_data$Condition, levels = c("SP", "HA"))
aggregate_data$Speech_Number <- 
  factor(aggregate_data$Speech_Number, levels = c("SPEECH001", "SPEECH002"))


#demographics
aggregate_data_demo <- 
  aggregate_data %>% 
  filter(Speech_Number == "SPEECH001")
mean(aggregate_data_demo$prearrival_Age, na.rm=TRUE)
sd(aggregate_data_demo$prearrival_Age, na.rm=TRUE)
table(aggregate_data_demo$prearrival_Age)
table(aggregate_data_demo$prearrival_Gender)


#Function to check there are two speeches for each participant
check_two_values_per_group <- function(df) {
  check <- aggregate(df$Speech_Number, by = list(df$Participant_Code), FUN = function(x) length(unique(x)))
  colnames(check) <- c("col1", "unique_col2_count")
  all_have_two_values <- all(check$unique_col2_count == 2)
  if (all_have_two_values) {
    message("All groups in column 1 have exactly 2 unique values in column 2.")
  } else {
    message("Not all groups in column 1 have exactly 2 unique values in column 2.")
    print(check)
  }
}


#EEG
##alpha
aggregate_data_EEG <- 
  aggregate_data %>%
  filter(!is.na(alphabeta_ratio))
aggregate_data_EEG$Participant_Code <- 
  as.factor(aggregate_data_EEG$Participant_Code)
check_two_values_per_group(aggregate_data_EEG)
hist(aggregate_data_EEG$alphabeta_ratio)
aggregate_data_EEG %>%
  group_by(Condition) %>%
  get_summary_stats(alphabeta_ratio, type = "mean_sd")
hist(aggregate_data_EEG$alphabeta_ratio)
alphabeta_ratio <- 
  lmer(alphabeta_ratio ~ Condition + Speech_Number + (1 | Participant_Code), data = aggregate_data_EEG)
anova(alphabeta_ratio, type="2")

###check model
performance(alphabeta_ratio)
rand(alphabeta_ratio)
plot(resid(alphabeta_ratio) ~ fitted(alphabeta_ratio)) 
alphabeta_ratio_residuals <- resid(alphabeta_ratio)
hist(alphabeta_ratio_residuals, breaks = 10, main = "Histogram of Residuals", xlab = "Residuals")
qqnorm(alphabeta_ratio_residuals)
qqline(alphabeta_ratio_residuals, col = "red")
shapiro.test(alphabeta_ratio_residuals)
alphabeta_ratio_ranef_model <- ranef(alphabeta_ratio, condVar = TRUE)
qqmath(alphabeta_ratio_ranef_model)


#Audio
##data cleaning
aggregate_data_audio <- 
  aggregate_data %>%
  filter(!is.na(arousal))
valid_participants <- 
  aggregate_data_audio %>%
  group_by(Participant_Code) %>%
  dplyr::summarize(has_speech001 = any(Speech_Number == "SPEECH001"),
            has_speech002 = any(Speech_Number == "SPEECH002")) %>%
  filter(has_speech001 & has_speech002) %>%
  pull(Participant_Code)
aggregate_data_audio <- 
  aggregate_data_audio %>% 
  filter(Participant_Code %in% valid_participants)
aggregate_data_audio$Participant_Code <- 
  as.factor(aggregate_data_audio$Participant_Code)
check_two_values_per_group(aggregate_data_audio)

##Arousal
hist(aggregate_data_audio$arousal)
aggregate_data_audio %>%
  group_by(Condition) %>%
  get_summary_stats(arousal, type = "mean_sd")
arousal_model <-
  lmer(arousal ~ Condition + Speech_Number + (1|Participant_Code), data = aggregate_data_audio)
anova(arousal_model, type="2")
###check model
performance(arousal_model)
rand(arousal_model)
plot(resid(arousal_model) ~ fitted(arousal_model)) 
arousal_residuals <- resid(arousal_model)
hist(arousal_residuals, breaks = 10, main = "Histogram of Residuals", xlab = "Residuals")
qqnorm(arousal_residuals)
qqline(arousal_residuals, col = "red")
shapiro.test(arousal_residuals)
arounsal_ranef_model <- ranef(arousal_model, condVar = TRUE)
qqmath(arounsal_ranef_model)
###ggplot
arousal_data <- 
  aggregate_data_audio %>%
  group_by(Condition) %>%
  get_summary_stats(arousal, type = "full")
ggplot(arousal_data, aes(x = Condition, y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), width = 0.2, position = position_dodge(0.7)) +
  theme_minimal() +
  scale_fill_manual(values = c("SP" = "lightgray", "HA" = "black")) +
  theme_classic(base_size = 16) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, .4), breaks = seq(0, .4, by = 0.2)) + # Prevents the y-axis from extending below zero
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),  # Removes x axis labels
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),  # Removes x axis labels
        #axis.ticks.y=element_blank(),
        axis.line.x=element_line(color="black"))

##Dominance
hist(aggregate_data_audio$dominance)
aggregate_data_audio %>%
  group_by(Condition) %>%
  get_summary_stats(dominance, type = "mean_sd")
dominance_model <-
  lmer(dominance ~ Condition + Speech_Number + (1|Participant_Code), data = aggregate_data_audio)
anova(dominance_model, type="2")
###check model
performance(dominance_model)
rand(dominance_model)
plot(resid(dominance_model) ~ fitted(dominance_model)) 
dominance_residuals <- resid(dominance_model)
hist(dominance_residuals, breaks = 10, main = "Histogram of Residuals", xlab = "Residuals")
qqnorm(dominance_residuals)
qqline(dominance_residuals, col = "red")
shapiro.test(dominance_residuals)
dominance_ranef_model <- ranef(dominance_model, condVar = TRUE)
qqmath(dominance_ranef_model)
###ggplot
dominance_data <- 
  aggregate_data_audio %>%
  group_by(Condition) %>%
  get_summary_stats(dominance, type = "full")
ggplot(dominance_data, aes(x = Condition, y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), width = 0.2, position = position_dodge(0.7)) +
  theme_minimal() +
  scale_fill_manual(values = c("SP" = "lightgray", "HA" = "black")) +
  theme_classic(base_size = 16) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, .45), breaks = seq(0, .45, by = 0.1)) + # Prevents the y-axis from extending below zero
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),  # Removes x axis labels
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),  # Removes x axis labels
        #axis.ticks.y=element_blank(),
        axis.line.x=element_line(color="black"))

##Valence
hist(aggregate_data_audio$valence)
aggregate_data_audio %>%
  group_by(Condition) %>%
  get_summary_stats(valence, type = "mean_sd")
valence_model <-
  lmer(valence ~ Condition + Speech_Number + (1|Participant_Code), data = aggregate_data_audio)
anova(valence_model, type="2")
###check model
performance(valence_model)
rand(valence_model)
plot(resid(valence_model) ~ fitted(valence_model)) 
valence_residuals <- resid(valence_model)
hist(valence_residuals, breaks = 10, main = "Histogram of Residuals", xlab = "Residuals")
qqnorm(valence_residuals)
qqline(valence_residuals, col = "red")
shapiro.test(valence_residuals) #shows significance due to one data point, but data generally resembles a normal pattern
valence_ranef_model <- ranef(valence_model, condVar = TRUE)
qqmath(valence_ranef_model)

##Words per minute
hist(aggregate_data_audio$WordsPerMin)
aggregate_data_audio %>%
  group_by(Condition) %>%
  get_summary_stats(WordsPerMin, type = "mean_sd")
wpm_model <-
  lmer(WordsPerMin ~ Condition + Speech_Number + (1 | Participant_Code), data = aggregate_data_audio)
anova(wpm_model, type="2")
###check model
performance(wpm_model)
rand(wpm_model)
plot(resid(wpm_model) ~ fitted(wpm_model)) 
wpm_residuals <- resid(wpm_model)
hist(wpm_residuals, breaks = 10, main = "Histogram of Residuals", xlab = "Residuals")
qqnorm(wpm_residuals)
qqline(wpm_residuals, col = "red")
shapiro.test(wpm_residuals)
wpm_ranef_model <- ranef(wpm_model, condVar = TRUE)
qqmath(wpm_ranef_model)
###ggplot
WordsPerMin_data <- 
  aggregate_data_audio %>%
  group_by(Condition) %>%
  get_summary_stats(WordsPerMin, type = "full")
ggplot(WordsPerMin_data, aes(x = Condition, y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), width = 0.2, position = position_dodge(0.7)) +
  theme_minimal() +
  scale_fill_manual(values = c("SP" = "lightgray", "HA" = "black")) +
  theme_classic(base_size = 16) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 140), breaks = seq(0, 140, by = 40)) + # Prevents the y-axis from extending below zero
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),  # Removes x axis labels
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),  # Removes x axis labels
        #axis.ticks.y=element_blank(),
        axis.line.x=element_line(color="black"))


#Breathing
aggregate_data_breathing <- aggregate_data %>%
  filter(!is.na(Average_BPM))
aggregate_data_breathing$Participant_Code <- 
  as.factor(aggregate_data_breathing$Participant_Code)
check_two_values_per_group(aggregate_data_breathing)
hist(aggregate_data_breathing$Average_BPM)
aggregate_data_breathing %>%
  group_by(Condition) %>%
  get_summary_stats(Average_BPM, type = "mean_sd")
BreathingMean <- 
  lmer(Average_BPM ~ Condition + Speech_Number + (1 | Participant_Code), data = aggregate_data_breathing)
anova(BreathingMean, type="2")
###check model
performance(BreathingMean)
rand(BreathingMean)
plot(resid(BreathingMean) ~ fitted(BreathingMean))
breathing_residuals <- resid(BreathingMean)
hist(breathing_residuals, breaks = 10, main = "Histogram of Residuals", xlab = "Residuals")
qqnorm(breathing_residuals)
qqline(breathing_residuals, col = "red")
shapiro.test(breathing_residuals)
breathing_ranef_model <- ranef(BreathingMean, condVar = TRUE)
qqmath(breathing_ranef_model)


#Motion
##Openness
aggregate_data_motion <- 
  aggregate_data %>%
  filter(!is.na(expressiveness))
valid_participants3 <- 
  aggregate_data_motion %>%
  group_by(Participant_Code) %>%
  dplyr::summarize(has_speech001 = any(Speech_Number == "SPEECH001"),
                   has_speech002 = any(Speech_Number == "SPEECH002")) %>%
  filter(has_speech001 & has_speech002) %>%
  pull(Participant_Code)
aggregate_data_motion <- 
  aggregate_data_motion %>% 
  filter(Participant_Code %in% valid_participants3)
aggregate_data_motion$Participant_Code <- 
  as.factor(aggregate_data_motion$Participant_Code)
check_two_values_per_group(aggregate_data_motion)
hist(aggregate_data_motion$openness)
aggregate_data_motion %>%
  group_by(Condition) %>%
  get_summary_stats(openness, type = "mean_sd")
motion_model <- 
  lmer(openness ~ Condition + Speech_Number + (1 | Participant_Code), 
       data = aggregate_data_motion)
anova(motion_model, type="2")
###check model
performance(motion_model)
rand(motion_model)
plot(resid(motion_model) ~ fitted(motion_model)) 
motion_residuals <- resid(motion_model)
hist(motion_residuals, breaks = 10, main = "Histogram of Residuals", xlab = "Residuals")
qqnorm(motion_residuals)
qqline(motion_residuals, col = "red")
shapiro.test(motion_residuals)
motion_ranef_model <- ranef(motion_model, condVar = TRUE)
qqmath(motion_ranef_model)


##Expressiveness
hist(aggregate_data_motion$expressiveness)
aggregate_data_motion %>%
  group_by(Condition) %>%
  get_summary_stats(expressiveness, type = "mean_sd")
motion_model2 <- 
  lmer(expressiveness ~ Condition + Speech_Number + (1 | Participant_Code), 
       data = aggregate_data_motion)
anova(motion_model2, type="2")
###check model
performance(motion_model2)
rand(motion_model2)
plot(resid(motion_model2) ~ fitted(motion_model2)) 
motion_residuals2 <- resid(motion_model2)
hist(motion_residuals2, breaks = 10, main = "Histogram of Residuals", xlab = "Residuals")
qqnorm(motion_residuals2)
qqline(motion_residuals2, col = "red")
shapiro.test(motion_residuals2)
motion_ranef_model2 <- ranef(motion_model2, condVar = TRUE)
qqmath(motion_ranef_model2)


#HeartRate
aggregate_data_HR <- 
  aggregate_data %>%
  filter(!is.na(Average_HeartRate))
check_two_values_per_group(aggregate_data_HR)
aggregate_data_HR$Participant_Code <- 
  as.factor(aggregate_data_HR$Participant_Code)
hist(aggregate_data_HR$Average_HeartRate)
aggregate_data_HR %>%
  group_by(Condition) %>%
  get_summary_stats(Average_HeartRate, type = "mean_sd")
HeartRate <- 
  lmer(Average_HeartRate ~ Condition + Speech_Number + (1 | Participant_Code), data = aggregate_data_HR)
anova(HeartRate, type="2")
###check model
performance(HeartRate)
rand(HeartRate)
plot(resid(HeartRate) ~ fitted(HeartRate)) 
HeartRate_residuals <- resid(HeartRate)
hist(HeartRate_residuals, breaks = 10, main = "Histogram of Residuals", xlab = "Residuals")
qqnorm(HeartRate_residuals)
qqline(HeartRate_residuals, col = "red")
shapiro.test(HeartRate_residuals)
HeartRate_ranef_model <- ranef(HeartRate, condVar = TRUE)
qqmath(HeartRate_ranef_model)


#Pupil Dilation
aggregate_data_PD <- 
  aggregate_data %>%
  filter(!is.na(Average_PD))
check_two_values_per_group(aggregate_data_PD)
aggregate_data_PD$Participant_Code <- 
  as.factor(aggregate_data_PD$Participant_Code)
hist(aggregate_data_PD$Average_PD)
aggregate_data_PD %>%
  group_by(Condition) %>%
  get_summary_stats(Average_PD, type = "mean_sd")
PupilDilation <- 
  lmer(Average_PD ~ Condition + Speech_Number + (1 | Participant_Code), data = aggregate_data_PD)
anova(PupilDilation, type="2")
###check model
performance(PupilDilation)
rand(PupilDilation)
plot(resid(PupilDilation) ~ fitted(PupilDilation)) 
PD_residuals <- resid(PupilDilation)
hist(PD_residuals, breaks = 10, main = "Histogram of Residuals", xlab = "Residuals")
qqnorm(PD_residuals)
qqline(PD_residuals, col = "red")
shapiro.test(PD_residuals)
PD_ranef_model <- ranef(PupilDilation, condVar = TRUE)
qqmath(PD_ranef_model)


#Gaze
##Ratio of gaze on audience vs. monitor
aggregate_data_gaze <- 
  aggregate_data %>%
  filter(!is.na(AudienceVsMonitor))
valid_participants2 <- 
  aggregate_data_gaze %>%
  group_by(Participant_Code) %>%
  dplyr::summarize(has_speech001 = any(Speech_Number == "SPEECH001"),
            has_speech002 = any(Speech_Number == "SPEECH002")) %>%
  filter(has_speech001 & has_speech002) %>%
  pull(Participant_Code)
aggregate_data_gaze <- 
  aggregate_data_gaze %>% 
  filter(Participant_Code %in% valid_participants2)
aggregate_data_gaze$Participant_Code <- 
  as.factor(aggregate_data_gaze$Participant_Code)
check_two_values_per_group(aggregate_data_gaze)
hist(aggregate_data_gaze$AudienceVsMonitor)
aggregate_data_gaze %>%
  group_by(Condition) %>%
  get_summary_stats(AudienceVsMonitor, type = "mean_sd")
aggregate_data_gaze$AudienceVsMonitor_log <- 
  log1p(aggregate_data_gaze$AudienceVsMonitor)
hist(aggregate_data_gaze$AudienceVsMonitor_log)
gaze_model <- 
  lmer(AudienceVsMonitor_log ~ Condition + Speech_Number + (1 | Participant_Code), 
       data = aggregate_data_gaze)
anova(gaze_model, type="2")
###check model
performance(gaze_model)
rand(gaze_model)
plot(resid(gaze_model) ~ fitted(gaze_model)) 
gaze_residuals <- resid(gaze_model)
hist(gaze_residuals, breaks = 10, main = "Histogram of Residuals", xlab = "Residuals")
qqnorm(gaze_residuals)
qqline(gaze_residuals, col = "red")
shapiro.test(gaze_residuals)
gaze_ranef_model <- ranef(gaze_model, condVar = TRUE)
qqmath(gaze_ranef_model)


##Gaze dispersion
aggregate_data_gaze2 <- 
  aggregate_data %>%
  filter(!is.na(EyeDispersion))
aggregate_data_gaze2$Participant_Code <- 
  as.factor(aggregate_data_gaze2$Participant_Code)
check_two_values_per_group(aggregate_data_gaze2)
hist(aggregate_data_gaze2$EyeDispersion)
aggregate_data_gaze2 %>%
  group_by(Condition) %>%
  get_summary_stats(EyeDispersion, type = "mean_sd")
aggregate_data_gaze2$EyeDispersion_log <- 
  log1p(aggregate_data_gaze2$EyeDispersion)
hist(aggregate_data_gaze2$EyeDispersion_log)
gaze_model2 <- 
  lmer(EyeDispersion_log ~ Condition + Speech_Number + (1 | Participant_Code), 
       data = aggregate_data_gaze2)
anova(gaze_model2, type="2")
###check model
performance(gaze_model2)
rand(gaze_model2)
plot(resid(gaze_model2) ~ fitted(gaze_model2)) 
gaze_residuals2 <- resid(gaze_model2)
hist(gaze_residuals2, breaks = 10, main = "Histogram of Residuals", xlab = "Residuals")
qqnorm(gaze_residuals2)
qqline(gaze_residuals2, col = "red")
shapiro.test(gaze_residuals2)
gaze_ranef_model2 <- ranef(gaze_model2, condVar = TRUE)
qqmath(gaze_ranef_model2)


#SelfReport
aggregate_data_postsurvey <- 
  aggregate_data %>%
  filter(!is.na(postspeech_Duration))
valid_participants4 <- 
  aggregate_data_postsurvey %>%
  group_by(Participant_Code) %>%
  dplyr::summarize(has_speech001 = any(Speech_Number == "SPEECH001"),
            has_speech002 = any(Speech_Number == "SPEECH002")) %>%
  filter(has_speech001 & has_speech002) %>%
  pull(Participant_Code)
aggregate_data_postsurvey <- aggregate_data_postsurvey %>% 
  filter(Participant_Code %in% valid_participants4)
aggregate_data_postsurvey$Participant_Code <- 
  as.factor(aggregate_data_postsurvey$Participant_Code)
check_two_values_per_group(aggregate_data_postsurvey)

##FeelingManikin1
hist(aggregate_data_postsurvey$postspeech_FeelingManikin1)
aggregate_data_postsurvey %>%
  group_by(Condition) %>%
  get_summary_stats(postspeech_FeelingManikin1, type = "mean_sd")
feelingManikin1_model <- 
  lmer(postspeech_FeelingManikin1 ~ Condition + Speech_Number + (1 | Participant_Code), 
       data = aggregate_data_postsurvey)
anova(feelingManikin1_model, type="2")
###check model
performance(feelingManikin1_model)
rand(feelingManikin1_model)


##FeelingManikin2
hist(aggregate_data_postsurvey$postspeech_FeelingManikin2)
aggregate_data_postsurvey %>%
  group_by(Condition) %>%
  get_summary_stats(postspeech_FeelingManikin2, type = "mean_sd")
feelingManikin2_model <- 
  lmer(postspeech_FeelingManikin2 ~ Condition + Speech_Number + (1 | Participant_Code), 
       data = aggregate_data_postsurvey)
anova(feelingManikin2_model, type="2")
###check model
performance(feelingManikin2_model)
rand(feelingManikin2_model)


##FeelingManikin3
hist(aggregate_data_postsurvey$postspeech_FeelingManikin3)
aggregate_data_postsurvey %>%
  group_by(Condition) %>%
  get_summary_stats(postspeech_FeelingManikin3, type = "mean_sd")
feelingManikin3_model <- 
  lmer(postspeech_FeelingManikin3 ~ Condition + Speech_Number + (1 | Participant_Code), 
       data = aggregate_data_postsurvey)
anova(feelingManikin3_model, type="2")
###check model
performance(feelingManikin3_model)
rand(feelingManikin3_model)


##Symptoms - Language & Behavior
hist(aggregate_data_postsurvey$postspeech_Symptoms_Lang_Beh)
aggregate_data_postsurvey %>%
  group_by(Condition) %>%
  get_summary_stats(postspeech_Symptoms_Lang_Beh, type = "mean_sd")
langbehsymptoms_model <- 
  lmer(postspeech_Symptoms_Lang_Beh ~ Condition + Speech_Number + (1 | Participant_Code), 
       data = aggregate_data_postsurvey)
anova(langbehsymptoms_model, type="2")
###check model
performance(langbehsymptoms_model)
rand(langbehsymptoms_model)
###ggplot
langbehsymptoms_data <- 
  aggregate_data_postsurvey %>%
  group_by(Condition) %>%
  get_summary_stats(postspeech_Symptoms_Lang_Beh, type = "full")
ggplot(langbehsymptoms_data, aes(x = Condition, y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), width = 0.2, position = position_dodge(0.7)) +
  theme_minimal() +
  scale_fill_manual(values = c("SP" = "lightgray", "HA" = "black")) +
  theme_classic(base_size = 16) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 30), breaks = seq(0, 30, by = 10)) + # Prevents the y-axis from extending below zero
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),  # Removes x axis labels
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),  # Removes x axis labels
        #axis.ticks.y=element_blank(),
        axis.line.x=element_line(color="black"))

##Symptoms - Thought
hist(aggregate_data_postsurvey$postspeech_Symptoms_Thought)
aggregate_data_postsurvey %>%
  group_by(Condition) %>%
  get_summary_stats(postspeech_Symptoms_Thought, type = "mean_sd")
thoughtsymptoms_model <- 
  lmer(postspeech_Symptoms_Thought ~ Condition + Speech_Number + (1 | Participant_Code), 
       data = aggregate_data_postsurvey)
anova(thoughtsymptoms_model, type="2")
###check model
performance(thoughtsymptoms_model)
rand(thoughtsymptoms_model)
###ggplot
thoughtsymptoms_data <- 
  aggregate_data_postsurvey %>%
  group_by(Condition) %>%
  get_summary_stats(postspeech_Symptoms_Thought, type = "full")
ggplot(thoughtsymptoms_data, aes(x = Condition, y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), width = 0.2, position = position_dodge(0.7)) +
  theme_minimal() +
  scale_fill_manual(values = c("SP" = "lightgray", "HA" = "black")) +
  theme_classic(base_size = 16) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 40), breaks = seq(0, 40, by = 10)) + # Prevents the y-axis from extending below zero
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),  # Removes x axis labels
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),  # Removes x axis labels
        #axis.ticks.y=element_blank(),
        axis.line.x=element_line(color="black"))

##Symptoms - Physiological
hist(aggregate_data_postsurvey$postspeech_Symptoms_1_Physiological)
aggregate_data_postsurvey %>%
  group_by(Condition) %>%
  get_summary_stats(postspeech_Symptoms_1_Physiological, type = "mean_sd")
physiological_model <- 
  lmer(postspeech_Symptoms_1_Physiological ~ Condition + Speech_Number + (1 | Participant_Code), 
       data = aggregate_data_postsurvey)
anova(physiological_model, type="2")
###check model
performance(physiological_model)
rand(physiological_model)


##CDRS - Cognitive Demand
hist(aggregate_data_postsurvey$postspeech_CDRS_Cognitive_Demand)
aggregate_data_postsurvey %>%
  group_by(Condition) %>%
  get_summary_stats(postspeech_CDRS_Cognitive_Demand, type = "mean_sd")
CogDemand_model <- 
  lmer(postspeech_CDRS_Cognitive_Demand ~ Condition + Speech_Number + (1 | Participant_Code), 
       data = aggregate_data_postsurvey)
anova(CogDemand_model, type="2")
###check model
performance(CogDemand_model)
rand(CogDemand_model)
###ggplot
CogDemand_data <- 
  aggregate_data_postsurvey %>%
  group_by(Condition) %>%
  get_summary_stats(postspeech_CDRS_Cognitive_Demand, type = "full")
ggplot(CogDemand_data, aes(x = Condition, y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), width = 0.2, position = position_dodge(0.7)) +
  theme_minimal() +
  scale_fill_manual(values = c("SP" = "lightgray", "HA" = "black")) +
  theme_classic(base_size = 16) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 18), breaks = seq(0, 18, by = 6)) + # Prevents the y-axis from extending below zero
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),  # Removes x axis labels
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),  # Removes x axis labels
        #axis.ticks.y=element_blank(),
        axis.line.x=element_line(color="black"))

##CDRS - Exertional Demand
hist(aggregate_data_postsurvey$postspeech_CDRS_Exertional_Demand)
aggregate_data_postsurvey %>%
  group_by(Condition) %>%
  get_summary_stats(postspeech_CDRS_Exertional_Demand, type = "mean_sd")
exertiondemand_model <- 
  lmer(postspeech_CDRS_Exertional_Demand ~ Condition + Speech_Number + (1 | Participant_Code), 
       data = aggregate_data_postsurvey)
anova(exertiondemand_model, type="2")
###check model
performance(exertiondemand_model)
rand(exertiondemand_model)
###ggplot
exertiondemand_data <- 
  aggregate_data_postsurvey %>%
  group_by(Condition) %>%
  get_summary_stats(postspeech_CDRS_Exertional_Demand, type = "full")
ggplot(exertiondemand_data, aes(x = Condition, y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), width = 0.2, position = position_dodge(0.7)) +
  theme_minimal() +
  scale_fill_manual(values = c("SP" = "lightgray", "HA" = "black")) +
  theme_classic(base_size = 16) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 8), breaks = seq(0, 8, by = 4)) + # Prevents the y-axis from extending below zero
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),  # Removes x axis labels
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),  # Removes x axis labels
        #axis.ticks.y=element_blank(),
        axis.line.x=element_line(color="black"))

##CDRS - Social Demand
hist(aggregate_data_postsurvey$postspeech_CDRS_Social_Demand)
aggregate_data_postsurvey %>%
  group_by(Condition) %>%
  get_summary_stats(postspeech_CDRS_Social_Demand, type = "mean_sd")
socialdemand_model <- 
  lmer(postspeech_CDRS_Social_Demand ~ Condition + Speech_Number + (1 | Participant_Code), 
       data = aggregate_data_postsurvey)
anova(socialdemand_model, type="2")
###check model
performance(socialdemand_model)
rand(socialdemand_model)
###ggplot
socialdemand_data <- 
  aggregate_data_postsurvey %>%
  group_by(Condition) %>%
  get_summary_stats(postspeech_CDRS_Social_Demand, type = "full")
ggplot(socialdemand_data, aes(x = Condition, y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), width = 0.2, position = position_dodge(0.7)) +
  theme_minimal() +
  scale_fill_manual(values = c("SP" = "lightgray", "HA" = "black")) +
  theme_classic(base_size = 16) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 18), breaks = seq(0, 18, by = 6)) + # Prevents the y-axis from extending below zero
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),  # Removes x axis labels
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),  # Removes x axis labels
        #axis.ticks.y=element_blank(),
        axis.line.x=element_line(color="black"))


##correlations
aggregate_data_corr <- 
  aggregate_data %>%
  filter(!(is.na(prearrival_Duration) | is.na(arrival_Duration) 
           | is.na(postspeech_Duration) | is.na(poststudy_Duration)))
valid_participants6 <- 
  aggregate_data_corr %>%
  group_by(Participant_Code) %>%
  dplyr::summarize(has_speech001 = any(Speech_Number == "SPEECH001"),
                   has_speech002 = any(Speech_Number == "SPEECH002")) %>%
  filter(has_speech001 & has_speech002) %>%
  pull(Participant_Code)
aggregate_data_corr <- 
  aggregate_data_corr %>% 
  filter(Participant_Code %in% valid_participants6)
aggregate_data_corr$Participant_Code <- 
  as.factor(aggregate_data_corr$Participant_Code)
check_two_values_per_group(aggregate_data_corr) 
aggregate_data_corr <- 
  aggregate_data_corr[, !names(aggregate_data_corr) %in% 
                        c("Speech_Number", "alpha", "beta", "postspeech_participant_label")]
agg_data_corr_wide <- 
  aggregate_data_corr %>%
  pivot_wider(names_from = Condition, values_from = c(3:14, 154:230))
SP_cols <- 
  names(agg_data_corr_wide)[str_ends(names(agg_data_corr_wide), "_SP")]
col_names <- str_remove(SP_cols, "_SP")
for (name in col_names) {
  SP_col <- paste0(name, "_SP")
  HA_col <- paste0(name, "_HA")
  diff_col <- paste0(name, "_Diff")
  agg_data_corr_wide[[diff_col]] <- 
    agg_data_corr_wide[[SP_col]] - agg_data_corr_wide[[HA_col]]
}

# agg_data_corr_only <- 
#   agg_data_corr_wide[ , c(80:87, 129:130, 199, 210, 211, 213:215, 394:405, 407:409, 
#                           426, 474:477, 482)]
# corr_estimate <- rcorr(as.matrix(agg_data_corr_only), type = "pearson")
# corr_estimate_matrix <- corr_estimate$r
# p_value_mat <- corr_estimate$P
# corr_matrix_full <- matrix(
#   paste0(round(corr_estimate_matrix, 2), " (p=", signif(p_value_mat, 3), ")"),
#   nrow = nrow(corr_estimate_matrix),
#   dimnames = dimnames(corr_estimate_matrix)
# )

agg_data_corr_only <- 
  agg_data_corr_wide[ , c(81, 199, 210, 211, 213, 215, 394:405, 407:409, 
                          426, 474:477, 482)]
new_order <- c(1, 6:27, 2:5)
agg_data_corr_rename <- agg_data_corr_only[, new_order]
#print(corr_matrix_full)

colnames(agg_data_corr_rename)[colnames(agg_data_corr_rename) == 
                                 "prearrival_Confidence_Speaker"] <- "SpeakerConfidence"
colnames(agg_data_corr_rename)[colnames(agg_data_corr_rename) == 
                                 "poststudy_Recommend"] <- "LTR"
colnames(agg_data_corr_rename)[colnames(agg_data_corr_rename) == 
                                 "poststudy_Spatial_Presence_SL"] <- "PresenceSL"
colnames(agg_data_corr_rename)[colnames(agg_data_corr_rename) == 
                                 "poststudy_Spatial_Presence_PA"] <- "PresencePA"
colnames(agg_data_corr_rename)[colnames(agg_data_corr_rename) == 
                                 "poststudy_Immersion"] <- "Immersion"
colnames(agg_data_corr_rename)[colnames(agg_data_corr_rename) == 
                                 "changeSTAI_STATE"] <- "STAI_STATE"
colnames(agg_data_corr_rename)[colnames(agg_data_corr_rename) == 
                                 "arousal_Diff"] <- "arousal"
colnames(agg_data_corr_rename)[colnames(agg_data_corr_rename) == 
                                 "dominance_Diff"] <- "dominance"
colnames(agg_data_corr_rename)[colnames(agg_data_corr_rename) == 
                                 "valence_Diff"] <- "valence"
colnames(agg_data_corr_rename)[colnames(agg_data_corr_rename) == 
                                 "WordsPerMin_Diff"] <- "wpm"
colnames(agg_data_corr_rename)[colnames(agg_data_corr_rename) == 
                                 "Average_BPM_Diff"] <- "bpm"
colnames(agg_data_corr_rename)[colnames(agg_data_corr_rename) == 
                                 "alphabeta_ratio_Diff"] <- "alphabeta"
colnames(agg_data_corr_rename)[colnames(agg_data_corr_rename) == 
                                 "EyeDispersion_Diff"] <- "Gaze_Dis"
colnames(agg_data_corr_rename)[colnames(agg_data_corr_rename) == 
                                 "AudienceVsMonitor_Diff"] <- "Gaze_AudMon"
colnames(agg_data_corr_rename)[colnames(agg_data_corr_rename) == 
                                 "Average_HeartRate_Diff"] <- "HR"
colnames(agg_data_corr_rename)[colnames(agg_data_corr_rename) == 
                                 "expressiveness_Diff"] <- "Motion_Express"
colnames(agg_data_corr_rename)[colnames(agg_data_corr_rename) == 
                                 "openness_Diff"] <- "Motion_Open"
colnames(agg_data_corr_rename)[colnames(agg_data_corr_rename) == 
                                 "Average_PD_Diff"] <- "PD"
colnames(agg_data_corr_rename)[colnames(agg_data_corr_rename) == 
                                 "postspeech_FeelingManikin1_Diff"] <- "Manikin1"
colnames(agg_data_corr_rename)[colnames(agg_data_corr_rename) == 
                                 "postspeech_FeelingManikin2_Diff"] <- "Manikin2"
colnames(agg_data_corr_rename)[colnames(agg_data_corr_rename) == 
                                 "postspeech_FeelingManikin3_Diff"] <- "Manikin3"
colnames(agg_data_corr_rename)[colnames(agg_data_corr_rename) == 
                                 "postspeech_Symptoms_1_Physiological_Diff"] <- "Symptom_Phys"
colnames(agg_data_corr_rename)[colnames(agg_data_corr_rename) == 
                                 "postspeech_CDRS_Cognitive_Demand_Diff"] <- "CDRS_Cog"
colnames(agg_data_corr_rename)[colnames(agg_data_corr_rename) == 
                                 "postspeech_CDRS_Exertional_Demand_Diff"] <- "CDRS_Exert"
colnames(agg_data_corr_rename)[colnames(agg_data_corr_rename) == 
                                 "postspeech_CDRS_Social_Demand_Diff"] <- "CDRS_Social"
colnames(agg_data_corr_rename)[colnames(agg_data_corr_rename) == 
                                 "postspeech_Symptoms_Lang_Beh_Diff"] <- "Symptom_Behave"
colnames(agg_data_corr_rename)[colnames(agg_data_corr_rename) == 
                                 "postspeech_Symptoms_Thought_Diff"] <- "Symptom_Thought"
corr_estimate <- rcorr(as.matrix(agg_data_corr_rename))
corr_estimate_matrix <- corr_estimate$r
p_value_mat <- corr_estimate$P
diag(p_value_mat) <- 1
corrplot(
  corr_estimate_matrix,
  method = "color",
  type = "upper",
  order = "original",
  pch.cex = 0.9,
  pch.col = 'grey20',
  #addCoef.col = "black",   
  #number.cex = 0.8,
  tl.col = "black",
  tl.srt = 45,
  #tl.pos = "n",
  p.mat = p_value_mat,
  sig.level = c(0.001, 0.01, 0.05),
  insig = 'label_sig',
  #mar = c(0, 0, 0, 0),
  #cl.pos = "n"
)
corrplot(
  corr_estimate_matrix,
  method = "color",
  type = "upper",
  order = "original",
  pch.cex = 0.9,
  pch.col = 'grey20',
  #addCoef.col = "black",   
  #number.cex = 0.8,
  tl.col = "black",
  tl.srt = 45,
  tl.pos = "n",
  p.mat = p_value_mat,
  sig.level = c(0.001, 0.01, 0.05),
  insig = 'label_sig',
  #mar = c(0, 0, 0, 0),
  #cl.pos = "n"
)


##Other outcomes
aggregate_data_arrivalpostsurvey <- 
  aggregate_data %>%
  filter(!is.na(postspeech_Duration) & !is.na(arrival_Duration))
valid_participants5 <- 
  aggregate_data_arrivalpostsurvey %>%
  group_by(Participant_Code) %>%
  dplyr::summarize(has_speech001 = any(Speech_Number == "SPEECH001"),
                   has_speech002 = any(Speech_Number == "SPEECH002")) %>%
  filter(has_speech001 & has_speech002) %>%
  pull(Participant_Code)
aggregate_data_arrivalpostsurvey <- 
  aggregate_data_arrivalpostsurvey %>% 
  filter(Participant_Code %in% valid_participants5)
check_two_values_per_group(aggregate_data_arrivalpostsurvey)

df_pre_post_STAI_STATE <- data.frame(
  Participant_Code = aggregate_data_arrivalpostsurvey$Participant_Code,
  Speaker_Anxiety = aggregate_data_arrivalpostsurvey$prearrival_Confidence_Speaker,
  arrival_STAI_STATE = aggregate_data_arrivalpostsurvey$arrival_STAI_STATE,
  poststudy_STAI_STATE = aggregate_data_arrivalpostsurvey$poststudy_STAI_STATE)
df_pre_post_STAI_STATE_pivot <- df_pre_post_STAI_STATE %>%
  pivot_longer(
    cols = ends_with("STAI_STATE"), 
    names_to = "Group", 
    values_to = "STAI_STATE"
  )

t.test(STAI_STATE ~ Group, data = df_pre_post_STAI_STATE_pivot, paired = TRUE)
ChangeAnxiety <- 
  lmer(STAI_STATE ~ Group*Speaker_Anxiety + (1|Participant_Code), 
       data = df_pre_post_STAI_STATE_pivot)
anova(ChangeAnxiety, Type="III")
df_pre_post_STAI_STATE_pivot %>%
  group_by(Group) %>%
  get_summary_stats(STAI_STATE, type = "mean_sd")


ggplot(df_pre_post_STAI_STATE_pivot, aes(x = Speaker_Anxiety, y = STAI_STATE, group = Group, color = Group)) +
  geom_smooth(method = "lm", se = TRUE) +    # regression line with confidence interval
  labs(title = "Relationship between x and y",
       x = "Speaker Anxiety",
       y = "State Anxiety")
# slopes <- emtrends(ChangeAnxiety, ~ Group, var = "Speaker_Anxiety")
# slopes          # shows slope (dy/dtime) per group
# pairs(slopes, adjust = "tukey")

mean(aggregate_data_postsurvey$poststudy_Recommend)
SD(aggregate_data_postsurvey$poststudy_Recommend)
# 
# mean(aggregate_data_postsurvey$poststudy_Spatial_Presence_SL)
# SD(aggregate_data_postsurvey$poststudy_Spatial_Presence_SL)
# 
# mean(aggregate_data_postsurvey$poststudy_Spatial_Presence_PA)
# SD(aggregate_data_postsurvey$poststudy_Spatial_Presence_PA)
# 
# mean(aggregate_data_postsurvey$poststudy_Immersion)
# SD(aggregate_data_postsurvey$poststudy_Immersion)
# 
# mean(aggregate_data_postsurvey$poststudy_Virtual_Sickness)
# SD(aggregate_data_postsurvey$poststudy_Virtual_Sickness)


##Manipulation Check
t.test(postspeech_Audience_Perceptions_1_Attentive ~ Condition, 
       data = aggregate_data_postsurvey, paired = TRUE)
cohens_d(postspeech_Audience_Perceptions_1_Attentive ~ Condition, 
         data = aggregate_data_postsurvey, paired = TRUE, ci = 0.95)
aggregate_data_postsurvey %>%
  group_by(Condition) %>%
  get_summary_stats(postspeech_Audience_Perceptions_1_Attentive, type = "mean_sd")

t.test(postspeech_Audience_Perceptions_2_Friendly ~ Condition, 
       data = aggregate_data_postsurvey, paired = TRUE)
cohens_d(postspeech_Audience_Perceptions_2_Friendly ~ Condition, 
        data = aggregate_data_postsurvey, paired = TRUE, ci = 0.95)
aggregate_data_postsurvey %>%
  group_by(Condition) %>%
  get_summary_stats(postspeech_Audience_Perceptions_2_Friendly, type = "mean_sd")

t.test(postspeech_Audience_Perceptions_3_Interest ~ Condition, 
       data = aggregate_data_postsurvey, paired = TRUE)
cohens_d(postspeech_Audience_Perceptions_3_Interest ~ Condition, 
        data = aggregate_data_postsurvey, paired = TRUE, ci = 0.95)
aggregate_data_postsurvey %>%
  group_by(Condition) %>%
  get_summary_stats(postspeech_Audience_Perceptions_3_Interest, type = "mean_sd")

t.test(postspeech_Audience_Perceptions_8_Lively ~ Condition, 
       data = aggregate_data_postsurvey, paired = TRUE)
cohens_d(postspeech_Audience_Perceptions_8_Lively ~ Condition, 
        data = aggregate_data_postsurvey, paired = TRUE, ci = 0.95)
aggregate_data_postsurvey %>%
  group_by(Condition) %>%
  get_summary_stats(postspeech_Audience_Perceptions_8_Lively, type = "mean_sd")

t.test(postspeech_Audience_Perceptions_9_Polite ~ Condition, 
       data = aggregate_data_postsurvey, paired = TRUE)
cohens_d(postspeech_Audience_Perceptions_9_Polite ~ Condition,
        data = aggregate_data_postsurvey, paired = TRUE, ci = 0.95)
aggregate_data_postsurvey %>%
  group_by(Condition) %>%
  get_summary_stats(postspeech_Audience_Perceptions_9_Polite, type = "mean_sd")

t.test(postspeech_Audience_Perceptions_4_Rude ~ Condition, 
       data = aggregate_data_postsurvey, paired = TRUE)
cohens_d(postspeech_Audience_Perceptions_4_Rude ~ Condition, 
        data = aggregate_data_postsurvey, paired = TRUE, ci = 0.95)
aggregate_data_postsurvey %>%
  group_by(Condition) %>%
  get_summary_stats(postspeech_Audience_Perceptions_4_Rude, type = "mean_sd")

t.test(postspeech_Audience_Perceptions_6_Cold ~ Condition, 
       data = aggregate_data_postsurvey, paired = TRUE)
cohens_d(postspeech_Audience_Perceptions_6_Cold ~ Condition, 
        data = aggregate_data_postsurvey, paired = TRUE, ci = 0.95)
aggregate_data_postsurvey %>%
  group_by(Condition) %>%
  get_summary_stats(postspeech_Audience_Perceptions_6_Cold, type = "mean_sd")

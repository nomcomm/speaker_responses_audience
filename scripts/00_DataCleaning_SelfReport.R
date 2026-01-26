rm(list = ls())
cat("\014")
library(tidyr)
library(ggplot2)
library(dplyr)
library(readxl)
library(readr)
library(tidyverse)
library(stringr)
library(openxlsx)

setwd(here::here())

#SelfReport
prearrival <- read_csv('./data/Datafiles/Survey_PreArrival.csv')
arrival <- read_csv('./data/Datafiles/Survey_Arrival.csv')
postspeech <- read_csv('./data/Datafiles/Survey_PostSpeech.csv')
poststudy <- read_csv('./data/Datafiles/Survey_PostStudy.csv')
subjectlog <- read_excel('./data/Datafiles/SubjectLog.xlsx')

##prearrival cleaning
prearrival$Fear_Neg_Eval_2_UnconcernedaboutOthersR <- 6 - prearrival$Fear_Neg_Eval_2_UnconcernedaboutOthers
prearrival$Fear_Neg_Eval_4_RarelyWorrySelfImpressionR <- 6 - prearrival$Fear_Neg_Eval_4_RarelyWorrySelfImpression
prearrival$Fear_Neg_Eval_7_OthersNotBothersomeR <- 6 - prearrival$Fear_Neg_Eval_7_OthersNotBothersome
prearrival$Fear_Neg_Eval_10_JudgeNoImpactR <- 6 - prearrival$Fear_Neg_Eval_10_JudgeNoImpact
prearrival$Fear_Neg_Eval <- 
  rowSums(prearrival[, c("Fear_Neg_Eval_1_WorryaboutOthers",
                         "Fear_Neg_Eval_2_UnconcernedaboutOthersR",
                         "Fear_Neg_Eval_3_AfraidShortcomings",
                         "Fear_Neg_Eval_4_RarelyWorrySelfImpressionR",
                         "Fear_Neg_Eval_5_AfraidOthersApproval",
                         "Fear_Neg_Eval_6_AfraidFindFault",
                         "Fear_Neg_Eval_7_OthersNotBothersomeR",
                         "Fear_Neg_Eval_8_WorryOthersThought",
                         "Fear_Neg_Eval_9_UsualWorryImpression",
                         "Fear_Neg_Eval_10_JudgeNoImpactR",
                         "Fear_Neg_Eval_11_TooConcernedOthers",
                         "Fear_Neg_Eval_12_WorrySayDoWrong")], na.rm = TRUE)
speaker_cols <- grep("^Confidence_Speaker_", names(prearrival), value = TRUE)
prearrival$Confidence_Speaker <- rowMeans(prearrival[, speaker_cols], na.rm = TRUE)
prearrival$Big_Five_Extroversion <- 
  (6 - prearrival$Big_Five_1_Reserved) + prearrival$Big_Five_6_OutgoingSociable
prearrival$Big_Five_Agreeableness <- 
  (prearrival$Big_Five_2_Trusting) + (6 - prearrival$Big_Five_7_FindFaultinOthers)
prearrival$Big_Five_Conscientiousness <- 
  (6 - prearrival$Big_Five_3_TendstobeLazy) + (prearrival$Big_Five_8_ThoroughJob)
prearrival$Big_Five_Neuroticism <- 
  (6 - prearrival$Big_Five_4_HandleStressWell) + (prearrival$Big_Five_9_NervousEasily)
prearrival$Big_Five_Openness <- 
  (6 - prearrival$Big_Five_5_FewArtInterest) + (prearrival$Big_Five_10_ActiveImagination)
sum_step1 <- rowSums(prearrival[, c("PS_Anxiety_1_TenseNervousPrep",
                                    "PS_Anxiety_2_TenseSpeechPresent",
                                    "PS_Anxiety_3_ConfusedJumbledSpeech",
                                    "PS_Anxiety_5_SpeechToughtAnxious",
                                    "PS_Anxiety_9_NervousSpeechAssignment",
                                    "PS_Anxiety_10_HandsTremble",
                                    "PS_Anxiety_13_ConstantFearForget",
                                    "PS_Anxiety_14_AnxiousQuestions",
                                    "PS_Anxiety_19_SweatBeforeSpeech",
                                    "PS_Anxiety_20_HeartBeatsSpeech",
                                    "PS_Anxiety_21_WaitingAnxiety",
                                    "PS_Anxiety_22_TenseRigidSpeech",
                                    "PS_Anxiety_23_LittleTimeAnxious",
                                    "PS_Anxiety_25_BreatheFasterBeforeSpeech",
                                    "PS_Anxiety_27_PoorerSpeechesNervous",
                                    "PS_Anxiety_28_AnxiousSpeechAssignment",
                                    "PS_Anxiety_29_MistakeDifficult",
                                    "PS_Anxiety_30_ImportantSpeechHelplessness",
                                    "PS_Anxiety_31_NoSleepBeforeSpeech",
                                    "PS_Anxiety_32_HeartBeatFastSpeech",
                                    "PS_Anxiety_33_NervousWhileWaiting",
                                    "PS_Anxiety_34_NervousForgetSpeech")], na.rm = TRUE)
sum_step2 <- rowSums(prearrival[, c("PS_Anxiety_4_SpeechPleasantExperience",
                                    "PS_Anxiety_6_NoFearSpeech",
                                    "PS_Anxiety_7_NotNervousSpeechStart",
                                    "PS_Anxiety_8_LookForwardSpeech",
                                    "PS_Anxiety_11_RelaxedSpeech",
                                    "PS_Anxiety_12_EnjoyPreparing",
                                    "PS_Anxiety_15FaceSpeakingConf",
                                    "PS_Anxiety_16_CompleteControl",
                                    "PS_Anxiety_17_SpeechMindClear",
                                    "PS_Anxiety_18_DoNotDread",
                                    "PS_Anxiety_24_CanControlAnxiety",
                                    "PS_Anxiety_26_ComfortableRelaxed")], na.rm = TRUE)
prearrival$PS_Anxiety <- 72 - sum_step2 + sum_step1

##arrival cleaning
cols_to_recode <- grep("^PANAS-SF_", names(arrival), value = TRUE)
arrival[cols_to_recode] <- lapply(arrival[cols_to_recode], function(col) {
  col - 1
})

arrival$PANAS_PA <- rowSums(arrival[, c("PANAS-SF_17_Attentive",
                                        "PANAS-SF_1_Interested",
                                        "PANAS-SF_12_Alert",
                                        "PANAS-SF_3_Excited",
                                        "PANAS-SF_9_Enthusiastic",
                                        "PANAS-SF_14_Inspired",
                                        "PANAS-SF_10_Proud",
                                        "PANAS-SF_16_Determined",
                                        "PANAS-SF_5_Strong",
                                        "PANAS-SF_19_Active")], na.rm = TRUE)
arrival$PANAS_NA <- rowSums(arrival[, c("PANAS-SF_2_Distressed",
                                        "PANAS-SF_4_Upset",
                                        "PANAS-SF_8_Hostile",
                                        "PANAS-SF_11_Irritable",
                                        "PANAS-SF_7_Scared",
                                        "PANAS-SF_20_Afraid",
                                        "PANAS-SF_13_Ashamed",
                                        "PANAS-SF_6_Guily",
                                        "PANAS-SF_15_Nervous",
                                        "PANAS-SF_18_Jittery"
                                        )], na.rm = TRUE)

cols_to_recode2 <- grep("^STAI-STATE_", names(arrival), value = TRUE)
arrival[cols_to_recode2] <- lapply(arrival[cols_to_recode2], function(col) {
  col - 1
})
arrival$STAI_STATE_1_CalmR <- 5 - arrival$'STAI-STATE_1_Calm'
arrival$STAI_STATE_2_SecureR <- 5 - arrival$'STAI-STATE_2_Secure'
arrival$STAI_STATE_5_EaseR <- 5 - arrival$'STAI-STATE_5_Ease'
arrival$STAI_STATE_8_SatisfiedR <- 5 - arrival$'STAI-STATE_8_Satisfied'
arrival$STAI_STATE_11_SelfConfidentR <- 5 - arrival$'STAI-STATE_11_SelfConfident'
arrival$STAI_STATE_15_RelaxedR <- 5 - arrival$'STAI-STATE_15_Relaxed'
arrival$STAI_STATE_16_ContentR <- 5 - arrival$'STAI-STATE_16_Content'
arrival$STAI_STATE_19_SteadyR <- 5 - arrival$'STAI-STATE_19_Steady'
arrival$STAI_STATE_20_PleasantR <- 5 - arrival$'STAI-STATE_20_Pleasant'
arrival$STAI_STATE <- rowSums(arrival[, c("STAI_STATE_1_CalmR",
                                          "STAI_STATE_2_SecureR",
                                          "STAI-STATE_3_Tense",
                                          "STAI-STATE_4_Strained",
                                          "STAI_STATE_5_EaseR",
                                          "STAI-STATE_6_Upset",
                                          "STAI-STATE_7_Worrying",
                                          "STAI_STATE_8_SatisfiedR",
                                          "STAI-STATE_9_Frightened",
                                          "STAI-STATE_10_Uncomfortable",
                                          "STAI_STATE_11_SelfConfidentR",
                                          "STAI-STATE_12_Nervous",
                                          "STAI-STATE_13_Jittery",
                                          "STAI-STATE_14_Indecisive",
                                          "STAI_STATE_15_RelaxedR",
                                          "STAI_STATE_16_ContentR",
                                          "STAI-STATE_17_Worried",
                                          "STAI-STATE_18_Confused",
                                          "STAI_STATE_19_SteadyR",
                                          "STAI_STATE_20_PleasantR")], na.rm = TRUE)

##postspeech
postspeech <- postspeech %>%
  mutate(
    Participant_Code = str_extract(participant_label, "^[^_]+"),
    Speech_Number = str_extract(participant_label, "(?<=_)[^_]+$")
  )
postspeech$FeelingManikin3 <- recode(postspeech$FeelingManikin3,
                            `1` = 1,
                            `4` = 2,
                            `5` = 3,
                            `6` = 4,
                            `7` = 5)
cols_to_recode3 <- grep("^SummaryEvaluations_", names(postspeech), value = TRUE)
postspeech[cols_to_recode3] <- lapply(postspeech[cols_to_recode3], function(col) {
  col - 7
})
recode_map <- c(`1` = 1, `3` = 2, `5` = 3, `6` = 4, `8` = 5, `9` = 6, `10` = 7)
postspeech <- postspeech  %>%
  mutate(across(starts_with("Nasa_Load_"), ~ recode(., !!!recode_map)))
postspeech$CDRS_Cognitive_Demand <- 
  postspeech$Demand2_1_PresentinginVRDemanding + 
  postspeech$Demand2_2_ThinkHardWhilePresenting + 
  postspeech$Demand2_3_VRMentallyDemanding + 
  postspeech$Demand2_4_ChallengesAffectedPresentation
postspeech$CDRS_Exertional_Demand <- 
  postspeech$Demand2_7_PhysicallyExhausted + 
  postspeech$Demand2_8_StrainedAfterPresentation
postspeech$CDRS_Social_Demand <- 
  postspeech$Demand2_9_AwareofAudience + 
  postspeech$Demand2_10_AudienceImpactedPres +  
  postspeech$Demand2_11_SociallyDemanding
postspeech$Symptoms_Lang_Beh <- rowSums(postspeech[, c("Symptoms_2_Missspoke_Stuttered",
                                                       "Symptoms_3_VoiceTrembled",
                                                       "Symptoms_4_LangIncoherent",
                                                       "Symptoms_5_MouthDry",
                                                       "Symptoms_6_Wobbly",
                                                       "Symptoms_7_HeartRaced",
                                                       "Symptoms_8_BreathingChanged",
                                                       "Symptoms_9_Blushed",
                                                       "Symptoms_10_DisplaceAct")], na.rm = TRUE)

postspeech$Symptoms_17_IntheZoneR <- 8-postspeech$Symptoms_17_IntheZone
postspeech$Symptoms_18_FeltCompetentR <- 8-postspeech$Symptoms_18_FeltCompetent
postspeech$Symptoms_19_BoostConfidenceR <- 8-postspeech$Symptoms_19_BoostConfidence
postspeech$Symptoms_20_EnjoyedSpotlightR <- 8-postspeech$Symptoms_20_EnjoyedSpotlight

postspeech$Symptoms_Thought <- 
  rowSums(postspeech[, c("Symptoms_11_UnwantedThoughts",
                         "Symptoms_12_Embarrassed",
                         "Symptoms_13_Overwhelmed",
                         "Symptoms_14_Lost_TrainofThought",
                         "Symptoms_15_NegativeEvaluation",
                         "Symptoms_16_ThoughtAboutOthers",
                         "Symptoms_17_IntheZoneR",
                         "Symptoms_18_FeltCompetentR",
                         "Symptoms_19_BoostConfidenceR",
                         "Symptoms_20_EnjoyedSpotlightR")], na.rm = TRUE)

##poststudy
poststudy[cols_to_recode2] <- lapply(poststudy[cols_to_recode2], function(col) {
  col - 1
})
poststudy$STAI_STATE_1_CalmR <- 5 - poststudy$'STAI-STATE_1_Calm'
poststudy$STAI_STATE_2_SecureR <- 5 - poststudy$'STAI-STATE_2_Secure'
poststudy$STAI_STATE_5_EaseR <- 5 - poststudy$'STAI-STATE_5_Ease'
poststudy$STAI_STATE_8_SatisfiedR <- 5 - poststudy$'STAI-STATE_8_Satisfied'
poststudy$STAI_STATE_11_SelfConfidentR <- 5 - poststudy$'STAI-STATE_11_SelfConfident'
poststudy$STAI_STATE_15_RelaxedR <- 5 - poststudy$'STAI-STATE_15_Relaxed'
poststudy$STAI_STATE_16_ContentR <- 5 - poststudy$'STAI-STATE_16_Content'
poststudy$STAI_STATE_19_SteadyR <- 5 - poststudy$'STAI-STATE_19_Steady'
poststudy$STAI_STATE_20_PleasantR <- 5 - poststudy$'STAI-STATE_20_Pleasant'
poststudy$STAI_STATE <- rowSums(poststudy[, c("STAI_STATE_1_CalmR",
                                              "STAI_STATE_2_SecureR",
                                              "STAI-STATE_3_Tense",
                                              "STAI-STATE_4_Strained",
                                              "STAI_STATE_5_EaseR",
                                              "STAI-STATE_6_Upset",
                                              "STAI-STATE_7_Worrying",
                                              "STAI_STATE_8_SatisfiedR",
                                              "STAI-STATE_9_Frightened",
                                              "STAI-STATE_10_Uncomfortable",
                                              "STAI_STATE_11_SelfConfidentR",
                                              "STAI-STATE_12_Nervous",
                                              "STAI-STATE_13_Jittery",
                                              "STAI-STATE_14_Indecisive",
                                              "STAI_STATE_15_RelaxedR",
                                              "STAI_STATE_16_ContentR",
                                              "STAI-STATE_17_Worried",
                                              "STAI-STATE_18_Confused",
                                              "STAI_STATE_19_SteadyR",
                                              "STAI_STATE_20_PleasantR")], na.rm = TRUE)
poststudy$Spatial_Presence_SL <- 
  rowMeans(poststudy[, c("Spatial_Presence_1_InEnvironment",
                         "Spatial_Presence_2_TookPartofPresentation",
                         "Spatial_Presence_3_LocationShifted",
                         "Spatial_Presence_4_PhysicallyPresent",
                         "Spatial_Presence_5_SteppedintoDiffPlace",
                         "Spatial_Presence_6_ThingsHappeningAroundMe",
                         "Spatial_Presence_7_MiddleofAction",
                         "Spatial_Presence_8_ObjectsSurround",
                         "Spatial_Presence_9_WasActuallyThere",
                         "Spatial_Presence_10_ObjectsAroundBody")], na.rm = TRUE)
poststudy$Spatial_Presence_PA <- 
  rowMeans(poststudy[, c("Spatial_Presence_11_CouldDoSomeObject",
                         "Spatial_Presence_12_CouldBeActive",
                         "Spatial_Presence_13_ActInEnvironment",
                         "Spatial_Presence_14_CouldReachObjects",
                         "Spatial_Presence_15_MoveAroundObjects",
                         "Spatial_Presence_16_JumpintoAction",
                         "Spatial_Presence_17_CouldActuallyTouchObject",
                         "Spatial_Presence_18_DoWhatever",
                         "Spatial_Presence_19_HaveEffect",
                         "Spatial_Presence_20_MoveFreely")], na.rm = TRUE)
recode_map2 <- c(`1` = 1, `2` = 2, `3` = 3, `4` = 4, `6` = 5)
poststudy <- poststudy %>%
  mutate(across(starts_with("Immersion_"), ~ recode(., !!!recode_map2)))
poststudy$Immersion_5_AwareofDeviceR <- 8 - poststudy$Immersion_5_AwareofDevice
poststudy$Immersion <- 
  rowMeans(poststudy[, c("Immersion_1_FeltReal",
                         "Immersion_2_ImmersedwithSenses",
                         "Immersion_3_Absorbed",
                         "Immersion_4_ForgotaboutRealEnv",
                         "Immersion_5_AwareofDevice",
                         "Immersion_6_LostTime")], na.rm = TRUE)
poststudy$Virtual_Sickness <- 
  rowMeans(poststudy[, c("Virtual_Sickness_1_Discomfort",
                         "Virtual_Sickness_2_Fatigue",
                         "Virtual_Sickness_3_Eyestrain",
                         "Virtual_Sickness_4_DifficultyFocusing",
                         "Virtual_Sickness_5_Headache",
                         "Virtual_Sickness_6_FullnessofHead",
                         "Virtual_Sickness_7_BlurredVision",
                         "Virtual_Sickness_8_Dizzy",
                         "Virtual_Sickness_9_Vertigo")], na.rm = TRUE)
poststudy$Recommend <- recode(poststudy$Recommend,
                                     `1` = 1,
                                     `2` = 2,
                                     `3` = 3,
                                     `5` = 4,
                                     `6` = 5,
                                     `7` = 6,
                                     `8` = 7)
rename_columns <- function(df, name) {
  df %>% 
    rename_with(
      .fn = ~ifelse(.x == "Participant_Code", .x, paste(name, .x, sep = "_")),
      .cols = -Participant_Code
    )
}
list_dfs <- list(
  prearrival = prearrival,
  arrival = arrival,
  postspeech = postspeech,
  poststudy = poststudy
)
list_dfs_renamed <- map2(list_dfs, names(list_dfs), rename_columns)
list2env(list_dfs_renamed, envir = .GlobalEnv)
dataframes <- list(prearrival, arrival, postspeech, poststudy)
df <- reduce(dataframes, full_join, by = "Participant_Code")
df <- df %>%
  select(Participant_Code, postspeech_Speech_Number, everything()) %>%
  rename(Speech_Number = postspeech_Speech_Number)
subjectlog_pivot <- subjectlog %>%
  pivot_longer(
    cols = starts_with("SPEECH"), 
    names_to = "Speech_Number", 
    values_to = "Condition"
  )
df_total <- 
  full_join(df, subjectlog_pivot, by = c("Participant_Code" = "Participant_Code", "Speech_Number" = "Speech_Number"))
df_total <- df_total %>%
  select(Participant_Code, Speech_Number, Condition, everything())
df_total <- df_total %>%
  mutate(poststudy_Recommend = case_when(
    poststudy_Recommend == 6 ~ 5,
    poststudy_Recommend == 7 ~ 6,
    poststudy_Recommend == 8 ~ 7,
    TRUE ~ poststudy_Recommend  # This keeps all other values as they are
  ))
df_total$changeSTAI_STATE <- 
  df_total$poststudy_STAI_STATE - df_total$arrival_STAI_STATE
write.csv(df_total, file = "./data/CleanedFiles/Aggregate/SelfReport.csv", row.names = FALSE)

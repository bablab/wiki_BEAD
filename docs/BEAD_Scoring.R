#Body & Emotion Awareness Differences (BEAD) Data Scoring

library(Rmisc)
library(ggplot2)
library(dplyr)
library(psych)
library(purrr)

#Create Participant List: 1) Only all Questionnaires complete; 2) If repeat participant, only keep first dataset

#Read in completion info
complete <- read.csv("/Users/paulsavoca/Library/CloudStorage/Box-Box/BABLAB/Studies/BEAD/Data/BEAD_Completion.csv")

#Keep only complete participants
complete <- complete[(complete$eac_complete == 2),]

#Remove Duplicate participants
complete$duplicate <- duplicated(complete$sona_id)
complete <- complete[!(complete$duplicate == TRUE),]

#Finalized Participant List
BEAD_ppt_list <- complete[c("record_id", "sona_id")]

#write.csv(BEAD_ppt_list, "BEAD_ppt_list.csv")

#Read in All Raw Data
cesd <- read.csv("/Users/paulsavoca/Library/CloudStorage/Box-Box/BABLAB/Studies/BEAD/Data/BEAD_CESD_Raw.csv")
ctq <- read.csv("/Users/paulsavoca/Library/CloudStorage/Box-Box/BABLAB/Studies/BEAD/Data/BEAD_CTQ_Raw.csv")
demos <- read.csv("/Users/paulsavoca/Library/CloudStorage/Box-Box/BABLAB/Studies/BEAD/Data/BEAD_Demos_Raw.csv")
eac <- read.csv("/Users/paulsavoca/Library/CloudStorage/Box-Box/BABLAB/Studies/BEAD/Data/BEAD_EAC_Raw.csv")
ias <- read.csv("/Users/paulsavoca/Library/CloudStorage/Box-Box/BABLAB/Studies/BEAD/Data/BEAD_IAS_Raw.csv")
icq <- read.csv("/Users/paulsavoca/Library/CloudStorage/Box-Box/BABLAB/Studies/BEAD/Data/BEAD_ICQ_Raw.csv")
maia <- read.csv("/Users/paulsavoca/Library/CloudStorage/Box-Box/BABLAB/Studies/BEAD/Data/BEAD_MAIA2_Raw.csv")
puberty <- read.csv("/Users/paulsavoca/Library/CloudStorage/Box-Box/BABLAB/Studies/BEAD/Data/BEAD_Puberty_Raw.csv")
quic <- read.csv("/Users/paulsavoca/Library/CloudStorage/Box-Box/BABLAB/Studies/BEAD/Data/BEAD_QUIC_Raw.csv")
stai <- read.csv("/Users/paulsavoca/Library/CloudStorage/Box-Box/BABLAB/Studies/BEAD/Data/BEAD_STAI_Raw.csv")
tas <- read.csv("/Users/paulsavoca/Library/CloudStorage/Box-Box/BABLAB/Studies/BEAD/Data/BEAD_TAS20_Raw.csv")

##Center for Epidemiological Studies - Depression Scale (CES-D) Scoring
#Rescale all responses from 1-4 to 0-3
cesd[2:21] <- cesd[2:21] - 1

#Change "Prefer Not to Answer" Responses by setting to NA
cesd[2:21][cesd[2:21]== 4] <- NA

#Reverse Score items: 4, 8, 12, 16
cesd$cesd_4 <- recode(cesd$cesd_4, '0' = 3, '1' = 2, "2" = 1, "3" = 0)
cesd$cesd_8 <- recode(cesd$cesd_8, '0' = 3, '1' = 2, "2" = 1, "3" = 0)
cesd$cesd_12 <- recode(cesd$cesd_12, '0' = 3, '1' = 2, "2" = 1, "3" = 0)
cesd$cesd_16 <- recode(cesd$cesd_16, '0' = 3, '1' = 2, "2" = 1, "3" = 0)

#CES-D Score: Total Score = Sum of all responses after reverse scoring
cesd$CESD_Total <- rowMeans(cesd[2:21], na.rm = T) * 20

##Childhood Trauma Questionnaire (CTQ) Scoring

#Set all NA to 0
ctq[is.na(ctq)] <- 0
ctq$Cumulative_Trauma <- (ctq$ctq_1b + ctq$ctq_2b + ctq$ctq_3b + ctq$ctq_4b +
                          ctq$ctq_5b + ctq$ctq_6c)

##Demographics Scoring/Cleaning

##Emotions as a Child (EAC) Scoring
#For EAC we collapsed across three negative emotions (angry/fearful/sad) in the questions -- only 15 items total then
#Two subscores are calculated based on Guo et al., 2017 Factor Analysis (https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6218930/)
#Supportive Subscale - Items: 1, 3, 6, 7, 10, 11, 12, 14R, 15
#Unsupportive Subscale - Items: 2, 4, 5, 8, 9, 13


#Change "Prefer Not to Answer"  to NA
eac[3:17][eac[3:17] == 6] <- NA

#Reverse Score Item 14
eac$eac_14R <- dplyr::recode(eac$eac_14, '1' = 5, '2' = 4, '3' =3, '4' = 2, '5' = 1)

#Calculate Supportive Subscale
eac$EAC_Supportive <- rowMeans(eac[c("eac_1", "eac_3", "eac_7", "eac_10",
                                     "eac_10", "eac_11", "eac_12", "eac_14R", 
                                     "eac_15")], na.rm = T) * 9

#Calculate Unsupportive Subscale 
eac$EAC_Unsupportive <- rowMeans(eac[c("eac_2", "eac_4", "eac_5", "eac_8",
                                       "eac_9", "eac_13")], na.rm = T) * 6

##Interoceptive Accuracy Scale (IAS) Scoring

#Change "Prefer Not to Answer"  to NA
ias[2:22][ias[2:22]== 6] <- NA

#IAS Scoring Sum of all items; mean imputation for NAs
ias$IAS_Total <- rowMeans(ias[2:22], na.rm = T) * 21

##Interoceptive Confusion Questionnaire (ICQ) Scoring

#Change "Prefer Not to Answer"  to NA
icq[2:21][icq[2:21] == 6] <- NA

#All items are responded to on a scale from 1 (Does not describe me) to 5 (Describes me very well). 
#Reverse Scored Items: 4, 9, 12, 14, 16, 18, 19 
#marked with * are reverse scored (5 = 1, 4 = 2, 2 = 4, 1 = 5). 
#An interoceptive confusion total score, ranging between 20 and 100, was calculated by summing all responses following reverse scoring (Brewer et al., 2016).

#Reverse Scoring
icq$icq_4r <- dplyr::recode(icq$icq_4, '1' = 5, '2' = 4, '3' =3, '4' = 2, '5' = 1)
icq$icq_9r <- dplyr::recode(icq$icq_9, '1' = 5, '2' = 4, '3' =3, '4' = 2, '5' = 1)
icq$icq_12r <- dplyr::recode(icq$icq_12, '1' = 5, '2' = 4, '3' =3, '4' = 2, '5' = 1)
icq$icq_14r <- dplyr::recode(icq$icq_14, '1' = 5, '2' = 4, '3' =3, '4' = 2, '5' = 1)
icq$icq_16r <- dplyr::recode(icq$icq_16, '1' = 5, '2' = 4, '3' =3, '4' = 2, '5' = 1)
icq$icq_18r <- dplyr::recode(icq$icq_18, '1' = 5, '2' = 4, '3' =3, '4' = 2, '5' = 1)
icq$icq_19r <- dplyr::recode(icq$icq_19, '1' = 5, '2' = 4, '3' =3, '4' = 2, '5' = 1)

#Total score with mean imputation
icq$Interoceptive_Confusion <- rowMeans(icq[c("icq_1", "icq_2", "icq_3", "icq_4r", "icq_5", 
                                              "icq_6", "icq_7", "icq_8", "icq_9r", "icq_10", 
                                              "icq_11", "icq_12r", "icq_13", "icq_14r", "icq_15", 
                                              "icq_16r", "ic1_17", "icq_18r", "icq_19r", "icq_20")],
                                              na.rm = T) * 20

##Multidimensional Assessment of Interoceptive Awareness (MAIA-2) Scoring
#1. Noticing: Awareness of uncomfortable, comfortable, and neutral body sensations
#Q1______ + Q2______ + Q3______ + Q4______ / 4 = ___________
#2. Not-Distracting: Tendency not to ignore or distract oneself from sensations of pain or discomfort
#Q5(R)____ + Q6(R)____+ Q7(R)____+ Q8(R)____+Q9(R)____+Q10(R) / 6 = ___________
#3. Not-Worrying: Tendency not to worry or experience emotional distress with sensations of pain or discomfort
#Q11(R)______ + Q12(R)______ + Q13______ + Q14______ + Q15 (R) / 5 = ___________
#4. Attention Regulation: Ability to sustain and control attention to body sensations
#Q16_____ + Q17_____ + Q18_____ + Q19_____ + Q20_____ + Q21_____ + Q22_____ / 7 = ________
#5. Emotional Awareness: Awareness of the connection between body sensations and emotional states
#Q23_____ + Q24_____ + Q25_____ + Q26_____ + Q27_____ / 5 = ___________
#6. Self-Regulation: Ability to regulate distress by attention to body sensations
#Q28_____ + Q29_____ + Q30_____ + Q31_____ / 4= ___________
#7. Body Listening: Active listening to the body for insight
#Q32_____ + Q33_____ + Q34_____ / 3= ___________
#8. Trusting: Experience of one’s body as safe and trustworthy
#Q35_____ + Q36_____ + Q37_____ / 3= ___________
#Note: (R): reverse-score (5 – x) items 5, 6, 7, 8, 9 and 10 on Not-Distracting, and items 11, 12 and 15 on NotWorrying.

#Reverse Scored Items:
maia$maia_5 <- 5 - maia$maia_5
maia$maia_6 <- 5 - maia$maia_6
maia$maia_7 <- 5 - maia$maia_7
maia$maia_8 <- 5 - maia$maia_8
maia$maia_9 <- 5 - maia$maia_9
maia$maia_10 <- 5 - maia$maia_10
maia$maia_11 <- 5 - maia$maia_11
maia$maia_12 <- 5 - maia$maia_12
maia$maia_15 <- 5 - maia$maia_15

#Prefer Not to Answer to NA
maia[2:38][maia[2:38]== 6] <- NA
maia[2:38][maia[2:38]== -1] <- NA

#Cacluate Sub-Scales
maia$Noticing <- rowMeans(maia[2:5], na.rm = T)
maia$Not_Distracting <- rowMeans(maia[6:11], na.rm = T)
maia$Not_Worrying <- rowMeans(maia[12:16], na.rm = T)
maia$Attention_Regulation <- rowMeans(maia[17:23], na.rm = T)
maia$Emotional_Awareness <- rowMeans(maia[24:28], na.rm = T)
maia$Self_Regulation <- rowMeans(maia[29:32], na.rm = T)
maia$Body_Listening <- rowMeans(maia[33:35], na.rm = T)
maia$Trusting <- rowMeans(maia[36:38], na.rm = T)


##Puberty & Contraceptive Scoring/Cleaning
# cfm (clarefmccann@g.ucla.edu)

# function to reverse score negatively valenced start items 
Reverse_code_5_likert <- function(response) {
  
  Reverse_code_5_likert = ifelse(response == 1, 5, 
                                 ifelse(response == 2, 4,
                                        ifelse(response == 3, 3,
                                               ifelse(response == 4, 2,
                                                      ifelse(response == 5, 1, response)))))
  return(Reverse_code_5_likert)
}

# read in data and select affective items

data <- puberty %>% select(record_id, pcq_4_1,	pcq_4_2,	pcq_4_3,	pcq_4_4,	pcq_4_5,	pcq_4_6,	pcq_4_7,	pcq_4_8,	pcq_4_9,	pcq_4_10,	pcq_4_11,	pcq_5_1,	pcq_5_2,	pcq_5_3,	pcq_5_4)

# reverse scoring so that higher values on measure score = more positive appraisals of puberty 

data$pcq_4_1R <- Reverse_code_5_likert(data$pcq_4_1) 
data$pcq_4_2R <- Reverse_code_5_likert(data$pcq_4_2) 
data$pcq_4_3R <- Reverse_code_5_likert(data$pcq_4_3) 
data$pcq_4_4R <- Reverse_code_5_likert(data$pcq_4_4) 
data$pcq_4_6R <- Reverse_code_5_likert(data$pcq_4_6) 
data$pcq_4_7R <- Reverse_code_5_likert(data$pcq_4_7)
data$pcq_4_8R <- Reverse_code_5_likert(data$pcq_4_8)
data$pcq_4_10R <- Reverse_code_5_likert(data$pcq_4_10)
data$pcq_4_11R <- Reverse_code_5_likert(data$pcq_4_11)

# calculating scores 

# subscale 1 -- appraisals of the start of puberty 

data$PAM_Affect_S1 <- rowSums(data[,paste("pcq_4_", c("1R","2R","3R","4R", 5, "6R", "7R", "8R", 9, "10R", "11R"), sep = "")], na.rm = FALSE)

# subscale 2 -- appraisals of changes brought on by puberty

data$PAM_Affect_S2 <- rowSums(data[,paste("pcq_5_", c(1,2,3,4), sep = "")], na.rm = FALSE)

# calculating z-scores for summary score since questions were asked on a different scale 

PAM_Affect_Mean_S1 <- mean(data$PAM_Affect_S1, na.rm = TRUE)
PAM_Affect_Mean_S2 <- mean(data$PAM_Affect_S2, na.rm = TRUE)
PAM_Affect_SD_S1 <- sd(data$PAM_Affect_S1, na.rm = TRUE)
PAM_Affect_SD_S2 <- sd(data$PAM_Affect_S2, na.rm = TRUE)

data$PAM_Affect_S1_zscore <- (data$PAM_Affect_S1 - PAM_Affect_Mean_S1) / PAM_Affect_SD_S1
data$PAM_Affect_S2_zscore <- (data$PAM_Affect_S2 - PAM_Affect_Mean_S2) / PAM_Affect_SD_S2

data$PAM_Affect_Total <- data$PAM_Affect_S1_zscore + data$PAM_Affect_S2_zscore

##Questionnaire of Uncertainty in Childhood (QUIC) Scoring

#Change"Prefer Not to Answer"  to NA
quic[9:46][quic[9:46]== 3] <- NA

#QUIC Scoring:
#Parental monitoring and involvement = 1R + 3R + 4R + 5R + 6R + 7R + 9R + 10R + 14R
#Parental predictability = 2 + 8R + 11 + 12 + 15R + 16 + 17R + 31 + 32 + 33 + 34 + 35
#Parental environment = 18 + 19 + 21 + 22 + 28R + 29 + 30
#Physical environment = 13 + 20 + 26 + 27 + 36R + 37 + 38
#Safety and security = 23 + 24 + 25
#Overall = Sum of all subscales.

#Fix all 'No' responses to be 0, instead of 2
quic[9:46][quic[9:46]== 2] <- 0

#Reverse Score Item
quic$quic_1r <- recode(quic$quic_1, '0' = 1, '1' = 0)
quic$quic_3r <- recode(quic$quic_3, '0' = 1, '1' = 0)
quic$quic_4r <- recode(quic$quic_4, '0' = 1, '1' = 0)
quic$quic_5r <- recode(quic$quic_5, '0' = 1, '1' = 0)
quic$quic_6r <- recode(quic$quic_6, '0' = 1, '1' = 0)
quic$quic_7r <- recode(quic$quic_7, '0' = 1, '1' = 0)
quic$quic_8r <- recode(quic$quic_8, '0' = 1, '1' = 0)
quic$quic_9r <- recode(quic$quic_9, '0' = 1, '1' = 0)
quic$quic_10r <- recode(quic$quic_10, '0' = 1, '1' = 0)
quic$quic_14r <- recode(quic$quic_14, '0' = 1, '1' = 0)
quic$quic_15r <- recode(quic$quic_15, '0' = 1, '1' = 0)
quic$quic_17r <- recode(quic$quic_17, '0' = 1, '1' = 0)
quic$quic_28r <- recode(quic$quic_28, '0' = 1, '1' = 0)
quic$quic_36r <- recode(quic$quic_36, '0' = 1, '1' = 0)

#Sub-scales
#Parental monitoring and involvement = 1R + 3R + 4R + 5R + 6R + 7R + 9R + 10R + 14R
quic$Parental_Monitoring <- rowMeans(quic[c("quic_1r", "quic_3r","quic_4r","quic_5r",
                                            "quic_6r", "quic_7r", "quic_9r", "quic_10r", 
                                            "quic_14r")], na.rm = T) * 9

#Parental predictability = 2 + 8R + 11 + 12 + 15R + 16 + 17R + 31 + 32 + 33 + 34 + 35
quic$Parental_Predictablity <- rowMeans(quic[c("quic_2", "quic_8r","quic_11","quic_12","quic_15r",
                                               "quic_16", "quic_17r", "quic_31", "quic_32", 
                                               "quic_33", "quic_34", "quic_35")], na.rm = T) * 12

#Parental environment = 18 + 19 + 21 + 22 + 28R + 29 + 30
quic$Parental_Environment <- rowMeans(quic[c("quic_18", "quic_19","quic_21","quic_22","quic_28r",
                                             "quic_29", "quic_30")], na.rm = T) * 7

#Physical environment = 13 + 20 + 26 + 27 + 36R + 37 + 38
quic$Physical_Environment <- rowMeans(quic[c("quic_13", "quic_20","quic_26","quic_27","quic_36r","quic_37",
                                             "quic_38")], na.rm = T) * 7

#Safety and security = 23 + 24 + 25
quic$Safety <- rowMeans(quic[c("quic_23", "quic_24","quic_25")], na.rm = T) * 3


#QUIC Total
quic$QUIC_Total <- quic$Parental_Monitoring + quic$Parental_Predictablity + quic$Parental_Environment +
  quic$Physical_Environment + quic$Safety

##State Trait Anxiety Inventory - Trait (STAI-T) Scoring

#Set Prefer Not to Answer to NA
stai[2:21][stai[2:21] == 5] <- NA

#Reversed items are: 1,2, 5, 8, 10, 11,15, 16, 19, 20
stai$stait_1 <- dplyr::recode(stai$stait_1, '1' = 4, '2' = 3, '3' =2, '4' = 1)
stai$stait_2 <- dplyr::recode(stai$stait_2, '1' = 4, '2' = 3, '3' =2, '4' = 1)
stai$stait_5 <- dplyr::recode(stai$stait_5, '1' = 4, '2' = 3, '3' =2, '4' = 1)
stai$stait_8 <- dplyr::recode(stai$stait_8, '1' = 4, '2' = 3, '3' =2, '4' = 1)
stai$stait_10 <- dplyr::recode(stai$stait_10, '1' = 4, '2' = 3, '3' =2, '4' = 1)
stai$stait_11 <- dplyr::recode(stai$stait_11, '1' = 4, '2' = 3, '3' =2, '4' = 1)
stai$stait_15 <- dplyr::recode(stai$stait_15, '1' = 4, '2' = 3, '3' =2, '4' = 1)
stai$stait_16 <- dplyr::recode(stai$stait_16, '1' = 4, '2' = 3, '3' =2, '4' = 1)
stai$stait_19 <- dplyr::recode(stai$stait_19, '1' = 4, '2' = 3, '3' =2, '4' = 1)
stai$stait_20 <- dplyr::recode(stai$stait_20, '1' = 4, '2' = 3, '3' =2, '4' = 1)

stai$Anxiety_Total <- rowMeans(stai[2:21], na.rm = T) * 20

##Toronto Alexithymia Scale (TAS-20) Scoring
#Prefer not to answer to NA
tas[2:21][tas[2:21]== 6] <- NA


#TAS-20 Reverse Scored Answers: items 4, 5, 10, 18 and 19
tas$tas_4r <- recode(tas$tas_4, '1' = 5, '2' = 4, "3" = 3, "4" = 2, "5" = 1)
tas$tas_5r <- recode(tas$tas_5, '1' = 5, '2' = 4, "3" = 3, "4" = 2, "5" = 1)
tas$tas_10r <- recode(tas$tas_10, '1' = 5, '2' = 4, "3" = 3, "4" = 2, "5" = 1)
tas$tas_18r <- recode(tas$tas_18, '1' = 5, '2' = 4, "3" = 3, "4" = 2, "5" = 1)
tas$tas_19r <- recode(tas$tas_19, '1' = 5, '2' = 4, "3" = 3, "4" = 2, "5" = 1)

#TAS-20 Scoring
tas$Diff_Describe <- rowMeans(tas[c("tas_2", "tas_4r","tas_11","tas_12","tas_17")], na.rm = T) * 5

tas$Diff_Ident <- rowMeans(tas[c("tas_1", "tas_3","tas_6","tas_7",
                                 "tas_9","tas_13", "tas_14" )], na.rm = T) * 7

tas$Extern_Orient <- rowMeans(tas[c("tas_5r", "tas_8","tas_10r","tas_15",
                                    "tas_16","tas_18r", "tas_19r", "tas_20" )], na.rm = T) * 8

tas$TAS_Total <- tas$Diff_Describe + tas$Diff_Ident + tas$Extern_Orient

##Data QA/QC

#Assess Attention through repeated answers 
#-- Participant attention was assessed by excluding participants who responded with the same answer for all questions
#-- on the: IAS, MAIA-2, ICQ, TAS, STAI, QUIC, CES-D or EAC
#-- Raw (non-reverse scoring was used for checking b/c only for attentiveness)

ias$AllEqual_IAS <- apply(ias[2:22], 1, function(row) all(row == row[1]))
maia$AllEqual_MAIA <- apply(maia[2:38], 1, function(row) all(row == row[1]))
icq$AllEqual_ICQ <- apply(icq[2:21], 1, function(row) all(row == row[1]))
tas$AllEqual_TAS <- apply(tas[2:21], 1, function(row) all(row == row[1]))
stai$AllEqual_STAI <- apply(stai[2:21], 1, function(row) all(row == row[1]))
quic$AllEqual_QUIC <- apply(quic[9:46], 1, function(row) all(row == row[1]))
cesd$AllEqual_CESD <- apply(cesd[2:21], 1, function(row) all(row == row[1]))
eac$AllEqual_EAC <- apply(eac[3:17], 1, function(row) all(row == row[1]))

#Assess Percentage of NAs per Questionnaire
ias$PerctNAs_IAS <- rowSums(is.na(ias[2:22]))/21
maia$PerctNAs_MAIA <- rowSums(is.na(maia[2:38]))/37
icq$PerctNAs_ICQ <- rowSums(is.na(icq[2:21]))/20
tas$PerctNAs_TAS <- rowSums(is.na(tas[2:21]))/20
stai$PerctNAs_STAI <- rowSums(is.na(stai[2:21]))/20
quic$PerctNAs_QUIC <- rowSums(is.na(quic[9:46]))/38
cesd$PerctNAs_CESD <- rowSums(is.na(cesd[2:21]))/20
eac$PerctNAs_EAC <- rowSums(is.na(eac[3:17]))/15

##Master Data Spreadsheet
#Combine all scored data with total scores, subscales, & other categorical info/data
bead_questionnaires <- list(cesd, ias, maia, quic, ctq, demos, icq, eac, puberty,
                            stai, tas)

bead_questionnaires <- bead_questionnaires %>% reduce(full_join, by='record_id')

#Keep only included participants
bead_all <- merge(BEAD_ppt_list, bead_questionnaires, by='record_id')

#Write CSV with all Data
#write.csv(bead_all, file = "All_BEAD_Data_Combined.csv")

#Remove Inattentive Participants
#bead_clean <- 

##Questionnaire Alphas/Betas
#IAS
ias_alpha <- psych::alpha(ias[2:22])
ias_alpha$total
psych::iclust(ias[2:22], 1)

#MAIA-2
#Alpha by Subscale...
iclust(maia[2:38], 8)

#QUIC
quic_alpha <- psych::alpha(quic[9:46])

iclust(quic[9:46], 1)

#STAI
stai_alpha <- psych::alpha(stai[2:21])
stai_alpha$total
iclust(stai[2:21],1)




library(Rmisc)
library(ggplot2)
library(dplyr)
library(psych)
library(purrr)
library(ggsignif)
library(fastDummies)
library(ggpubr)

##Read in Data
bead <- read.csv("/Users/paulsavoca/Library/CloudStorage/Box-Box/BABLAB/Studies/BEAD/Data/Data_Summaries/Summary_BEAD_Data_Combined.csv")

#Replace PAM Scores
pam_updated <- read.csv("/Users/paulsavoca/Library/CloudStorage/Box-Box/BABLAB/Studies/BEAD/Data/Data_Summaries/updated_pam_forbead.csv")
pam_updated <- pam_updated[c("record_id", "total_PAM", "PAM_Affect_S1", "PAM_Affect_S2")]
bead <- merge(bead, pam_updated, by = "record_id")

#Initial Sample Size (N=198)

##Data Cleaning
#Remove Prefer Not to Answer on > 20% of items for a given questionnaire (n = 7)
bead <- bead[!(bead$PerctNAs_IAS > 0.2),]
bead <- bead[!(bead$PerctNAs_TAS > 0.2),]
bead <- bead[!(bead$PerctNAs_CESD > 0.2),]
bead <- bead[!(bead$PerctNAs_EAC > 0.2),]

#Remove Outliers (+/- 3SDs) (n = 1 {IAS})
bead <- bead[!(bead$IAS_Total > (mean(bead$IAS_Total) + 3*sd(bead$IAS_Total))),]
bead <- bead[!(bead$IAS_Total < (mean(bead$IAS_Total) - 3*sd(bead$IAS_Total))),]

bead <- bead[!(bead$TAS_Total > (mean(bead$TAS_Total) + 3*sd(bead$TAS_Total))),]
bead <- bead[!(bead$TAS_Total < (mean(bead$TAS_Total) - 3*sd(bead$TAS_Total))),]

bead <- bead[!(bead$CESD_Total > (mean(bead$CESD_Total) + 3*sd(bead$CESD_Total))),]
bead <- bead[!(bead$CESD_Total < (mean(bead$CESD_Total) - 3*sd(bead$CESD_Total))),]

bead <- bead[!(bead$EAC_Supportive > (mean(bead$EAC_Supportive) + 3*sd(bead$EAC_Supportive))),]
bead <- bead[!(bead$EAC_Supportive < (mean(bead$EAC_Supportive) - 3*sd(bead$EAC_Supportive))),]

bead <- bead[!(bead$EAC_Unsupportive > (mean(bead$EAC_Unsupportive) + 3*sd(bead$EAC_Unsupportive))),]
bead <- bead[!(bead$EAC_Unsupportive < (mean(bead$EAC_Unsupportive) - 3*sd(bead$EAC_Unsupportive))),]

#For Analysis: N = 190; n = 72 male; n = 118 female

#Create Gender Label Column
bead$Gender <- ifelse(bead$participant_gender == 2, "Female", "Male")

##Histograms & Descriptives & Group Differences (by Gender) of Important Variables

#IAS
hist(bead$IAS_Total)
summarySE(bead, measurevar = "IAS_Total", groupvars = "participant_gender")
t.test(bead$IAS_Total ~ bead$participant_gender)

#TAS-20
hist(bead$TAS_Total)
summarySE(bead, measurevar = "TAS_Total", groupvars = "participant_gender")
t.test(bead$TAS_Total ~ bead$participant_gender)

#CES-D
hist(bead$CESD_Total)
summarySE(bead, measurevar = "CESD_Total", groupvars = "participant_gender")
t.test(bead$CESD_Total ~ bead$participant_gender)

#EAC Supportive
hist(bead$EAC_Supportive)
summarySE(bead, measurevar = "EAC_Supportive", groupvars = "participant_gender")
t.test(bead$EAC_Supportive ~ bead$participant_gender)

#EAC Unsupportive
hist(bead$EAC_Unsupportive)
summarySE(bead, measurevar = "EAC_Unsupportive", groupvars = "participant_gender")
t.test(bead$EAC_Unsupportive ~ bead$participant_gender)

#PAM
hist(bead$total_PAM)
summarySE(bead, measurevar = "total_PAM", groupvars = "participant_gender")
t.test(bead$total_PAM ~ bead$participant_gender) #Significantly different between groups

#Age
hist(bead$participant_age)
summarySE(bead, measurevar = "participant_age", groupvars = "participant_gender")
t.test(bead$participant_age ~ bead$participant_gender)

#Ethnicity
hist(bead$participant_ethnicity)
table(bead$participant_ethnicity, bead$participant_gender)
chisq.test(bead$participant_ethnicity, bead$participant_gender)

##Potential Covariate Relationships

#Age -- Not associated with IAS, TAS or CES-D
cor.test(bead$IAS_Total, bead$participant_age)
cor.test(bead$TAS_Total, bead$participant_age)
cor.test(bead$CESD_Total, bead$participant_age)
cor.test(bead$total_PAM, bead$participant_age) #Significant negative correlation with age
cor.test(bead$EAC_Supportive, bead$participant_age)
cor.test(bead$EAC_Unsupportive, bead$participant_age)

#Race
bead$participant_ethnicity <- as.factor(bead$participant_ethnicity)
summary(aov(bead$IAS_Total ~ bead$participant_ethnicity)) #Marginal Association (p = 0.07)
summary(aov(bead$TAS_Total ~ bead$participant_ethnicity)) #Marginal Association (p = 0.07)
summary(aov(bead$CESD_Total ~ bead$participant_ethnicity)) #Significant Association (p = 0.03)
summary(aov(bead$total_PAM ~ bead$participant_ethnicity)) #Significant Association (p = 0.04)
summary(aov(bead$EAC_Supportive ~ bead$participant_ethnicity)) #Significant Association (p = 0.03)
summary(aov(bead$EAC_Unsupportive ~ bead$participant_ethnicity)) #Significant Association (p = 0.015)

#Create Race Covariate
# 1 = White, 2 = Latinx/Histpanic, 3 = Middle Eastern, 4 = African, 5 = Caribbean,
# 6 = South Asian, 7 = East Asian, 8 = Other, 9 = Prefer Not to Answer
# To Reduce Groups combine: South & East Asian = Asian; Caribbean, Other & NA = Other

bead$ethincity_cov <- ifelse(bead$participant_ethnicity == 1, "White",
                             ifelse(bead$participant_ethnicity == 2, "Hispanic",
                                    ifelse(bead$participant_ethnicity ==3, "MiddleEastern",
                                           ifelse(bead$participant_ethnicity == 4, "African",
                                                  ifelse(bead$participant_ethnicity == 5, "Other",
                                                         ifelse(bead$participant_ethnicity == 6, "Asian",
                                                                ifelse(bead$participant_ethnicity == 7, "Asian",
                                                                       ifelse(bead$participant_ethnicity == 8, "Other",
                                                                              ifelse(bead$participant_ethnicity == 9, "Other", "NA"
                                                                              )))))))))

#With New Grouping still marginal association with IAS; Signf with TAS, CES-D, PAM, EAC_S 
summary(aov(bead$IAS_Total ~ bead$ethincity_cov))
summary(aov(bead$TAS_Total ~ bead$ethincity_cov))
summary(aov(bead$CESD_Total ~ bead$ethincity_cov))
summary(aov(bead$total_PAM ~ bead$ethincity_cov))
summary(aov(bead$EAC_Supportive ~ bead$ethincity_cov))
summary(aov(bead$EAC_Unsupportive ~ bead$ethincity_cov))

chisq.test(bead$ethincity_cov, bead$participant_gender)

#Dummy Code Ethnicity
bead <- dummy_cols(bead, 
                   select_columns = c("ethincity_cov"))

##Start of Analysis

#Parenting to Puberty
summary(lm(total_PAM ~ EAC_Supportive + participant_gender + ethincity_cov + participant_age, data = bead))

PAM__EACs <- ggplot(data = bead, aes(x = EAC_Supportive, y = total_PAM))+
  geom_point(aes(color = Gender), alpha = .5)+
  geom_smooth(method = 'lm', color = "black", se = F)+
  theme_classic()+
  theme(text = element_text(size=12))+
  ylab("Puberty Affective Apprasial")+
  xlab("Supportive Emotional Parenting (EAC Supportive)")

summary(lm(total_PAM ~ EAC_Unsupportive + participant_gender + ethincity_cov + participant_age, data = bead))

PAM__EACu <- ggplot(data = bead, aes(x = EAC_Unsupportive, y = total_PAM))+
  geom_point(aes(color = Gender), alpha = .5)+
  geom_smooth(method = 'lm', color = "black", se = F)+
  theme_classic()+
  theme(text = element_text(size=12))+
  ylab("Puberty Affective Apprasial")+
  xlab("Unsupportive Emotional Parenting (EAC Unsupportive)")

#Puberty to Interoception
summary(lm(IAS_Total ~ total_PAM + participant_gender + ethincity_cov + participant_age, data = bead))

IAS__PAM <- ggplot(data = bead, aes(x = total_PAM, y = IAS_Total))+
  geom_point(aes(color = Gender), alpha = .5)+
  geom_smooth(method = 'lm', color = "black", se = F)+
  theme_classic()+
  theme(text = element_text(size=12))+
  xlab("Puberty Affective Apprasial")+
  ylab("Interoceptive Sensibility (IAS)")

#Puberty to Alexithymia
summary(lm(TAS_Total ~ total_PAM + participant_gender + ethincity_cov + participant_age, data = bead))

TAS__PAM <- ggplot(data = bead, aes(x = total_PAM, y = TAS_Total))+
  geom_point(aes(color = Gender), alpha = .5)+
  geom_smooth(method = 'lm', color = "black", se = F)+
  theme_classic()+
  theme(text = element_text(size=12))+
  xlab("Puberty Affective Apprasial")+
  ylab("Alexithymia (TAS-20)")

#Interoception & Alexithymia
summary(lm(TAS_Total ~ IAS_Total + participant_gender + ethincity_cov + participant_age, data = bead))

TAS__IAS <- ggplot(data = bead, aes(x = IAS_Total, y = TAS_Total))+
  geom_point(aes(color = Gender), alpha = .5)+
  geom_smooth(method = 'lm', color = "black", se = F)+
  theme_classic()+
  theme(text = element_text(size=12))+
  xlab("Interoceptive Sensibility (IAS)")+
  ylab("Alexithymia (TAS-20)")

#Interoception to Depression
summary(lm(CESD_Total ~ IAS_Total + participant_gender + ethincity_cov + participant_age, data = bead))

CESD__IAS <- ggplot(data = bead, aes(x = IAS_Total, y = CESD_Total))+
  geom_point(aes(color = Gender), alpha = .5)+
  geom_smooth(method = 'lm', color = "black", se = F)+
  theme_classic()+
  theme(text = element_text(size=12))+
  ylab("Depression (CES-D)")+
  xlab("Interoceptive Sensibility (IAS)")

#Alexithymia to Depression
summary(lm(CESD_Total ~ TAS_Total + participant_gender + ethincity_cov + participant_age, data = bead))

CESD__TAS <- ggplot(data = bead, aes(x = TAS_Total, y = CESD_Total))+
  geom_point(aes(color = Gender), alpha = .5)+
  geom_smooth(method = 'lm', color = "black", se = F)+
  theme_classic()+
  theme(text = element_text(size=12))+
  ylab("Depression (CES-D)")+
  xlab("Alexithymia (TAS)")

#Direct Depression Relationships
summary(lm(CESD_Total ~  EAC_Supportive+ participant_gender + ethincity_cov + participant_age, data = bead))
summary(lm(CESD_Total ~ EAC_Unsupportive + participant_gender + ethincity_cov + participant_age, data = bead))
summary(lm(CESD_Total ~ total_PAM + participant_gender + ethincity_cov + participant_age, data = bead))


#Reliabilities
#TAS Total Score alpha = 0.83
psych::alpha(bead_all[c("tas_1","tas_2","tas_3", "tas_6","tas_7","tas_8",                                   
                        "tas_9","tas_11", "tas_12" ,"tas_13",  "tas_14",                                  
                        "tas_15","tas_16","tas_17","tas_20","tas_4r","tas_5r","tas_10r",
                        "tas_18r","tas_19r")])

#IAS Total Score alpha = 0.90
psych::alpha(bead_all[28:48])

#CES-D alpha = 0.90
psych::alpha(bead_all[4:23])

#EAC Supportive alpha = 0.87
psych::alpha(bead_all[c("eac_1","eac_3","eac_6","eac_7","eac_10",
                        "eac_11","eac_12","eac_14R","eac_15")])

#EAC Unsupportive alpha = 0.84
psych::alpha(bead_all[c("eac_2","eac_4","eac_5","eac_8","eac_9",
                        "eac_13")])

###Figures for MS###

#EAC and PAM Plots
PAM__EACs <- ggplot(data = bead, aes(x = EAC_Supportive, y = total_PAM))+
  geom_point(alpha = .5)+
  geom_smooth(method = 'lm', color = "black", se = F)+
  theme_classic()+
  theme(text = element_text(size=12))+
  ylab("Puberty Affective Apprasial")+
  xlab("Supportive Emotional Caregiving (EAC Supportive)")


PAM__EACu <- ggplot(data = bead, aes(x = EAC_Unsupportive, y = total_PAM))+
  geom_point(alpha = .5)+
  geom_smooth(method = 'lm', color = "black", se = F)+
  theme_classic()+
  theme(text = element_text(size=12))+
  ylab("Puberty Affective Apprasial")+
  xlab("Unsupportive Emotional Caregiving (EAC Unsupportive)")


fig1 <- ggarrange(PAM__EACs, PAM__EACu,
                  labels = c("A", "B"),
                  ncol = 2, nrow = 1)

#PAM and IAS/TAS-20 Plots
TAS__PAM <- ggplot(data = bead, aes(x = total_PAM, y = TAS_Total))+
  geom_point(alpha = .5)+
  geom_smooth(method = 'lm', color = "black", se = F)+
  theme_classic()+
  theme(text = element_text(size=12))+
  xlab("Puberty Affective Apprasial")+
  ylab("Alexithymia (TAS-20)")

IAS__PAM <- ggplot(data = bead, aes(x = total_PAM, y = IAS_Total))+
  geom_point(alpha = .5)+
  geom_smooth(method = 'lm', color = "black", se = F)+
  theme_classic()+
  theme(text = element_text(size=12))+
  xlab("Puberty Affective Apprasial")+
  ylab("Interoceptive Sensibility (IAS)")

fig2 <- ggarrange(IAS__PAM,TAS__PAM,
                  labels = c("A", "B"),
                  ncol = 2, nrow = 1)

#TAS, IAS, & CES-D Plots
TAS__IAS <- ggplot(data = bead, aes(x = IAS_Total, y = TAS_Total))+
  geom_point(alpha = .5)+
  geom_smooth(method = 'lm', color = "black", se = F)+
  theme_classic()+
  theme(text = element_text(size=12))+
  xlab("Interoceptive Sensibility (IAS)")+
  ylab("Alexithymia (TAS-20)")

CESD__IAS <- ggplot(data = bead, aes(x = IAS_Total, y = CESD_Total))+
  geom_point(alpha = .5)+
  geom_smooth(method = 'lm', color = "black", se = F)+
  theme_classic()+
  theme(text = element_text(size=12))+
  ylab("Depression (CES-D)")+
  xlab("Interoceptive Sensibility (IAS)")

CESD__TAS <- ggplot(data = bead, aes(x = TAS_Total, y = CESD_Total))+
  geom_point(alpha = .5)+
  geom_smooth(method = 'lm', color = "black", se = F)+
  theme_classic()+
  theme(text = element_text(size=12))+
  ylab("Depression (CES-D)")+
  xlab("Alexithymia (TAS)")

fig3 <- ggarrange(TAS__IAS, CESD__IAS, CESD__TAS,
                  labels = c("A", "B", "C"),
                  ncol = 3, nrow = 1)

#Direct CES-D Plots
CESD__EACs <- ggplot(data = bead, aes(x = EAC_Supportive, y = CESD_Total))+
  geom_point(alpha = .5)+
  geom_smooth(method = 'lm', color = "black", se = F)+
  theme_classic()+
  theme(text = element_text(size=12))+
  ylab("Depression (CES-D)")+
  xlab("Supportive Emotional Caregiving (EAC Supportive)")

CESD__EACu <- ggplot(data = bead, aes(x = EAC_Unsupportive, y = CESD_Total))+
  geom_point(alpha = .5)+
  geom_smooth(method = 'lm', color = "black", se = F)+
  theme_classic()+
  theme(text = element_text(size=12))+
  ylab("Depression (CES-D)")+
  xlab("Unsupportive Emotional Caregiving (EAC Unsupportive)")

CESD__PAM <- ggplot(data = bead, aes(x = total_PAM, y = CESD_Total))+
  geom_point(alpha = .5)+
  geom_smooth(method = 'lm', color = "black", se = F)+
  theme_classic()+
  theme(text = element_text(size=12))+
  ylab("Depression (CES-D)")+
  xlab("Puberty Affective Apprasial")

fig4 <- ggarrange(CESD__EACs, CESD__EACu, CESD__PAM,
                  labels = c("A", "B", "C"),
                  ncol = 3, nrow = 1)

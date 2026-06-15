##Finalized Analysis PWS - 07/2025##
library(ggplot2)
library(ggpubr)
library(lmtest)
library(psych)
library(gtsummary)
library(pwr)


#Find Outlier Function
id_outliers <- function(data, survey_col) {
  # Convert to numeric
  data[[survey_col]] <- as.numeric(as.character(data[[survey_col]]))
  
  # Calculate mean and sd for the survey column
  mean_val <- mean(data[[survey_col]], na.rm = TRUE)
  sd_val <- sd(data[[survey_col]], na.rm = TRUE)
  
  # Define outlier thresholds
  threshold_upper <- mean_val + 3 * sd_val
  threshold_lower <- mean_val - 3 * sd_val
  
  # Flagging Outliers
  data[[paste0("Outlier_", survey_col)]] <- ifelse(data[[survey_col]] < threshold_lower | 
                                                     data[[survey_col]] > threshold_upper, 
                                                   "outlier", "not outlier")
  return(data)
}

#Read in Scored Data
csv_data <- read.csv("/Users/paulsavoca/Library/CloudStorage/Box-Box/BEAD/Data/Data_Summaries/All_BEAD_Data_Combined.csv")

##Starting N = 198

#Remove more than 20% 'Prefer Not to Answer' Responses on IAS, MAIA, QUIC & ICQ
csv_data$PerctNAs_IAS <- as.numeric(as.character(csv_data$PerctNAs_IAS))
csv_data$PerctNAs_MAIA <- as.numeric(as.character(csv_data$PerctNAs_MAIA))
csv_data$PerctNAs_QUIC <- as.numeric(as.character(csv_data$PerctNAs_QUIC))
csv_data$PerctNAs_ICQ <- as.numeric(as.character(csv_data$PerctNAs_ICQ))

csv_filtered <- csv_data[!(is.na(csv_data$PerctNAs_IAS) | csv_data$PerctNAs_IAS > 0.2 |
                             is.na(csv_data$PerctNAs_MAIA) | csv_data$PerctNAs_MAIA > 0.2 |
                             is.na(csv_data$PerctNAs_QUIC) | csv_data$PerctNAs_QUIC > 0.2 |
                             is.na(csv_data$PerctNAs_ICQ) | csv_data$PerctNAs_ICQ > 0.2), ]

## n = 192 (after 20% NA Removal)

#Remove Outliers (+/- 3SDs)
csv_filtered <- id_outliers(csv_filtered, "IAS_Total")
csv_filtered <- id_outliers(csv_filtered, "Interoceptive_Confusion")
csv_filtered <- id_outliers(csv_filtered, "Noticing")
csv_filtered <- id_outliers(csv_filtered, "Not_Distracting")
csv_filtered <- id_outliers(csv_filtered, "Not_Worrying")
csv_filtered <- id_outliers(csv_filtered, "Attention_Regulation")
csv_filtered <- id_outliers(csv_filtered, "Emotional_Awareness")
csv_filtered <- id_outliers(csv_filtered, "Self_Regulation")
csv_filtered <- id_outliers(csv_filtered, "Body_Listening")
csv_filtered <- id_outliers(csv_filtered, "Trusting")
csv_filtered <- id_outliers(csv_filtered, "Cumulative_Trauma")
csv_filtered <- id_outliers(csv_filtered, "QUIC_Total")

cleaned_data_final <- csv_filtered[!(csv_filtered$Outlier_IAS_Total == "outlier" |
                                       csv_filtered$Outlier_Interoceptive_Confusion == "outlier" |
                                       csv_filtered$Outlier_Noticing == "outlier" |
                                       csv_filtered$Outlier_Not_Distracting == "outlier" |
                                       csv_filtered$Outlier_Not_Worrying == "outlier" |
                                       csv_filtered$Outlier_Attention_Regulation == "outlier" |
                                       csv_filtered$Outlier_Emotional_Awareness == "outlier" |
                                       csv_filtered$Outlier_Self_Regulation == "outlier" |
                                       csv_filtered$Outlier_Body_Listening == "outlier" |
                                       csv_filtered$Outlier_Trusting == "outlier" |
                                       csv_filtered$Outlier_Cumulative_Trauma == "outlier" |
                                       csv_filtered$Outlier_QUIC_Total == "outlier"),]

## n = 186 (after Outlier Removal)

#Descriptive Statistics & Covariate Checks
mean(cleaned_data_final$participant_age)
sd(cleaned_data_final$participant_age)

table(cleaned_data_final$participant_gender)
table(cleaned_data_final$participant_gender)/186

table(cleaned_data_final$participant_ethnicity)
table(cleaned_data_final$participant_ethnicity)/186

mean(cleaned_data_final$QUIC_Total)
sd(cleaned_data_final$QUIC_Total)

mean(cleaned_data_final$Cumulative_Trauma)
sd(cleaned_data_final$Cumulative_Trauma)

mean(cleaned_data_final$IAS_Total)
sd(cleaned_data_final$IAS_Total)

mean(cleaned_data_final$Interoceptive_Confusion)
sd(cleaned_data_final$Interoceptive_Confusion)

mean(cleaned_data_final$Noticing)
sd(cleaned_data_final$Noticing)

mean(cleaned_data_final$Not_Distracting)
sd(cleaned_data_final$Not_Distracting)

mean(cleaned_data_final$Not_Worrying)
sd(cleaned_data_final$Not_Worrying)

mean(cleaned_data_final$Attention_Regulation)
sd(cleaned_data_final$Attention_Regulation)

mean(cleaned_data_final$Emotional_Awareness)
sd(cleaned_data_final$Emotional_Awareness)

mean(cleaned_data_final$Self_Regulation)
sd(cleaned_data_final$Self_Regulation)

mean(cleaned_data_final$Body_Listening)
sd(cleaned_data_final$Body_Listening)

mean(cleaned_data_final$Trusting)
sd(cleaned_data_final$Trusting)

#Correlation Matrix
corr_vars <- cleaned_data_final[c("QUIC_Total", "Cumulative_Trauma", "IAS_Total", "Interoceptive_Confusion",
                                  "Noticing", "Not_Distracting", "Not_Worrying", "Attention_Regulation",
                                  "Emotional_Awareness", "Self_Regulation", "Body_Listening", "Trusting")]
cor_matrix <- cor(corr_vars, use = "complete.obs")
print(cor_matrix)

my_data <- as.matrix(corr_vars[, c("QUIC_Total", "Cumulative_Trauma", "IAS_Total", "Interoceptive_Confusion",
                                  "Noticing", "Not_Distracting", "Not_Worrying", "Attention_Regulation",
                                  "Emotional_Awareness", "Self_Regulation", "Body_Listening", "Trusting")])

# Calculate correlations and p-values
results <- rcorr(my_data, type = "pearson") # or "spearman"


#Standardize Continuous Variables
cleaned_data_final <-
  cleaned_data_final %>%
  JSmediation::standardize_variable(c(IAS_Total, Cumulative_Trauma, Interoceptive_Confusion, participant_age,
                                      Noticing, Not_Distracting, Not_Worrying, Attention_Regulation,
                                      Emotional_Awareness, Self_Regulation, Body_Listening, Trusting,
                                      QUIC_Total), suffix = "std")

cleaned_data_final$participant_ethnicity <- as.factor(cleaned_data_final$participant_ethnicity)
cleaned_data_final$participant_gender <- as.factor(cleaned_data_final$participant_gender)

#QUIC x Intero Regressions (Standardized)
ias.quic <- lm(cleaned_data_final$IAS_Total_std ~ cleaned_data_final$QUIC_Total_std + cleaned_data_final$participant_gender + cleaned_data_final$participant_ethnicity)
summary(ias.quic)

icq.quic <- lm(cleaned_data_final$Interoceptive_Confusion_std ~ cleaned_data_final$QUIC_Total_std + cleaned_data_final$participant_gender + cleaned_data_final$participant_ethnicity)
summary(icq.quic)

noticing.quic <- lm(cleaned_data_final$Noticing_std ~ cleaned_data_final$QUIC_Total_std + cleaned_data_final$participant_gender + cleaned_data_final$participant_ethnicity)
summary(noticing.quic)
not_distracting.quic <- lm(cleaned_data_final$Not_Distracting_std ~ cleaned_data_final$QUIC_Total_std + cleaned_data_final$participant_gender + cleaned_data_final$participant_ethnicity)
summary(not_distracting.quic)
not_worrying.quic <- lm(cleaned_data_final$Not_Worrying_std ~ cleaned_data_final$QUIC_Total_std + cleaned_data_final$participant_gender + cleaned_data_final$participant_ethnicity)
summary(not_worrying.quic)
attention_regulation.quic <- lm(cleaned_data_final$Attention_Regulation_std ~ cleaned_data_final$QUIC_Total_std + cleaned_data_final$participant_gender + cleaned_data_final$participant_ethnicity)
summary(attention_regulation.quic)
emotional_awareness.quic <- lm(cleaned_data_final$Emotional_Awareness_std ~ cleaned_data_final$QUIC_Total_std + cleaned_data_final$participant_gender + cleaned_data_final$participant_ethnicity)
summary(emotional_awareness.quic)
self_regulation.quic <- lm(cleaned_data_final$Self_Regulation_std ~ cleaned_data_final$QUIC_Total_std + cleaned_data_final$participant_gender + cleaned_data_final$participant_ethnicity)
summary(self_regulation.quic)
body_listening.quic <- lm(cleaned_data_final$Body_Listening_std ~ cleaned_data_final$QUIC_Total_std + cleaned_data_final$participant_gender + cleaned_data_final$participant_ethnicity)
summary(body_listening.quic)
trusting.quic <- lm(cleaned_data_final$Trusting_std ~ cleaned_data_final$QUIC_Total_std + cleaned_data_final$participant_gender + cleaned_data_final$participant_ethnicity)
summary(trusting.quic)

#Benjamini-Hochberg (FDR)
maia.ps <- c(0.0204, 0.00183, 0.953643, 0.00659, 0.2398, 0.1638, 0.2785, 4.39e-06)
p.adjust(maia.ps, method = "BH")

#CTQ x Intero Regressions (Standardized)
ias.ctq <- lm(cleaned_data_final$IAS_Total_std ~ cleaned_data_final$Cumulative_Trauma_std + cleaned_data_final$participant_gender + cleaned_data_final$participant_ethnicity)
summary(ias.ctq)

icq.ctq <- lm(cleaned_data_final$Interoceptive_Confusion_std ~ cleaned_data_final$Cumulative_Trauma_std + cleaned_data_final$participant_gender + cleaned_data_final$participant_ethnicity)
summary(icq.ctq)

noticing.ctq <- lm(cleaned_data_final$Noticing_std ~ cleaned_data_final$Cumulative_Trauma_std + cleaned_data_final$participant_gender + cleaned_data_final$participant_ethnicity)
summary(noticing.ctq)
not_distracting.ctq <- lm(cleaned_data_final$Not_Distracting_std ~ cleaned_data_final$Cumulative_Trauma_std + cleaned_data_final$participant_gender + cleaned_data_final$participant_ethnicity)
summary(not_distracting.ctq)
not_worrying.ctq <- lm(cleaned_data_final$Not_Worrying_std ~ cleaned_data_final$Cumulative_Trauma_std + cleaned_data_final$participant_gender + cleaned_data_final$participant_ethnicity)
summary(not_worrying.ctq)
attention_regulation.ctq <- lm(cleaned_data_final$Attention_Regulation_std ~ cleaned_data_final$Cumulative_Trauma_std + cleaned_data_final$participant_gender + cleaned_data_final$participant_ethnicity)
summary(attention_regulation.ctq)
emotional_awareness.ctq <- lm(cleaned_data_final$Emotional_Awareness_std ~ cleaned_data_final$Cumulative_Trauma_std + cleaned_data_final$participant_gender + cleaned_data_final$participant_ethnicity)
summary(emotional_awareness.ctq)
self_regulation.ctq <- lm(cleaned_data_final$Self_Regulation_std ~ cleaned_data_final$Cumulative_Trauma_std + cleaned_data_final$participant_gender + cleaned_data_final$participant_ethnicity)
summary(self_regulation.ctq)
body_listening.ctq <- lm(cleaned_data_final$Body_Listening_std ~ cleaned_data_final$Cumulative_Trauma_std + cleaned_data_final$participant_gender + cleaned_data_final$participant_ethnicity)
summary(body_listening.ctq)
trusting.ctq <- lm(cleaned_data_final$Trusting_std ~ cleaned_data_final$Cumulative_Trauma_std + cleaned_data_final$participant_gender + cleaned_data_final$participant_ethnicity)
summary(trusting.ctq)

maia.ps2 <- c(0.0645, 0.1322, 0.25883, 0.7205, 0.00668, 0.8726, 0.8703, 0.51322)
p.adjust(maia.ps2, method = "BH")


#Chi-Square Test

# Create the data frame
df <- data.frame(
  Construct = c("Unpredictability", "Trauma"),
  Negative_Associations = c(6, 0),
  Positive_Associations = c(0, 1),
  Non_Significant_Associations = c(4, 9)
)

# Full 2x3 table
full_table <- as.matrix(df[, 2:4])
rownames(full_table) <- df$Construct
full_table

chisq.test(full_table)  # Chi-square test
fisher.test(full_table)

#Graphs
#QUIC Graphs
QUICxIAS <- ggplot(data = cleaned_data_final, aes(x=(QUIC_Total), y=IAS_Total))+
  geom_point(size = 2, alpha = 0.5, color = "orange")+
  geom_smooth(method = 'lm', se=F, color="black")+
  xlab("QUIC Total Score")+
  ylab("IAS Total Score")+
  theme_classic()+
  theme(text = element_text(size=15), plot.title = element_text(hjust = 0.5, size = 25, face = "bold"))+
  ggtitle("**")

QUICxICQ <- ggplot(data = cleaned_data_final, aes(x=(QUIC_Total), y=Interoceptive_Confusion))+
  geom_point(size = 2, alpha = 0.5, color = "orange")+
  geom_smooth(method = 'lm', se=F, color="black")+
  xlab("QUIC Total Score")+
  ylab("ICQ Total Score")+
  theme_classic()+
  theme(text = element_text(size=15), plot.title = element_text(hjust = 0.5, size = 25, face = "bold"))+
  ggtitle("***")

QUICxNoticing <- ggplot(data = cleaned_data_final, aes(x=(QUIC_Total), y=Noticing))+
  geom_point(size = 2, alpha = 0.5, color = "orange")+
  geom_smooth(method = 'lm', se=F, color="black")+
  xlab("QUIC Total Score")+
  ylab("Noticing (MAIA-2)")+
  theme_classic()+
  theme(text = element_text(size=15), plot.title = element_text(hjust = 0.5, size = 25, face = "bold"))+
  ggtitle("*")

QUICxNotDisctracting <- ggplot(data = cleaned_data_final, aes(x=(QUIC_Total), y=Not_Distracting))+
  geom_point(size = 2, alpha = 0.5, color = "orange")+
  geom_smooth(method = 'lm', se=F, color="black")+
  xlab("QUIC Total Score")+
  ylab("Not Disctracting (MAIA-2)")+
  theme_classic()+
  theme(text = element_text(size=15), plot.title = element_text(hjust = 0.5, size = 25, face = "bold"))+
  ggtitle("**")

QUICxNotWorrying <- ggplot(data = cleaned_data_final, aes(x=(QUIC_Total), y=Not_Worrying))+
  geom_point(size = 2, alpha = 0.5)+
  geom_smooth(method = 'lm', se=F, color="black")+
  xlab("QUIC Total Score")+
  ylab("Not Worrying (MAIA-2)")+
  theme_classic()+
  theme(text = element_text(size=15), plot.title = element_text(hjust = 0.5, size = 25, face = "bold"))+
  ggtitle("")

QUICxAttentionRegulation <- ggplot(data = cleaned_data_final, aes(x=(QUIC_Total), y=Attention_Regulation))+
  geom_point(size = 2, alpha = 0.5, color = "orange")+
  geom_smooth(method = 'lm', se=F, color="black")+
  xlab("QUIC Total Score")+
  ylab("Attention Regulation (MAIA-2)")+
  theme_classic()+
  theme(text = element_text(size=15), plot.title = element_text(hjust = 0.5, size = 25, face = "bold"))+
  ggtitle("*")

QUICxEmotionalAwareness <- ggplot(data = cleaned_data_final, aes(x=(QUIC_Total), y=Emotional_Awareness))+
  geom_point(size = 2, alpha = 0.5)+
  geom_smooth(method = 'lm', se=F, color="black")+
  xlab("QUIC Total Score")+
  ylab("Emotional Awareness (MAIA-2)")+
  theme_classic()+
  theme(text = element_text(size=15), plot.title = element_text(hjust = 0.5, size = 25, face = "bold"))+
  ggtitle("")

QUICxSelfRegulation <- ggplot(data = cleaned_data_final, aes(x=(QUIC_Total), y=Self_Regulation))+
  geom_point(size = 2, alpha = 0.5)+
  geom_smooth(method = 'lm', se=F, color="black")+
  xlab("QUIC Total Score")+
  ylab("Self-Regulation (MAIA-2)")+
  theme_classic()+
  theme(text = element_text(size=15), plot.title = element_text(hjust = 0.5, size = 25, face = "bold"))+
  ggtitle("")

QUICxBodyListening <- ggplot(data = cleaned_data_final, aes(x=(QUIC_Total), y=Body_Listening))+
  geom_point(size = 2, alpha = 0.5)+
  geom_smooth(method = 'lm', se=F, color="black")+
  xlab("QUIC Total Score")+
  ylab("Body Listening (MAIA-2)")+
  theme_classic()+
  theme(text = element_text(size=15), plot.title = element_text(hjust = 0.5, size = 25, face = "bold"))+
  ggtitle("")

QUICxTrusting <- ggplot(data = cleaned_data_final, aes(x=(QUIC_Total), y=Trusting))+
  geom_point(size = 2, alpha = 0.5, color = "orange")+
  geom_smooth(method = 'lm', se=F, color="black")+
  xlab("QUIC Total Score")+
  ylab("Trusting (MAIA-2)")+
  theme_classic()+
  theme(text = element_text(size=15), plot.title = element_text(hjust = 0.5, size = 25, face = "bold"))+
  ggtitle("***")

ggarrange(QUICxIAS,QUICxICQ, QUICxNoticing,QUICxNotDisctracting, QUICxNotWorrying,
          QUICxAttentionRegulation, QUICxEmotionalAwareness, QUICxSelfRegulation, QUICxBodyListening,QUICxTrusting,   
          ncol = 5,nrow = 2, common.legend = T, legend = "bottom",
          labels = c("A", "B", "C", "D", "E","F","G", "H", "I", "J"))

#ggsave(filename = "Figure_1_rev.pdf", path = "/Users/paulsavoca/Desktop", width = 15, height = 10)

#CTQ Graphs
CTQxIAS <- ggplot(data = cleaned_data_final, aes(x=(Cumulative_Trauma), y=IAS_Total))+
  geom_point(size = 2, alpha = 0.5)+
  geom_smooth(method = 'lm', se=F, color="black")+
  xlab("CTQ Score")+
  ylab("IAS Total Score")+
  theme_classic()+
  theme(text = element_text(size=15), plot.title = element_text(hjust = 0.5, size = 25, face = "bold"))+
  ggtitle("")

CTQxICQ <- ggplot(data = cleaned_data_final, aes(x=(Cumulative_Trauma), y=Interoceptive_Confusion))+
  geom_point(size = 2, alpha = 0.5)+
  geom_smooth(method = 'lm', se=F, color="black")+
  xlab("CTQ Score")+
  ylab("ICQ Total Score")+
  theme_classic()+
  theme(text = element_text(size=15), plot.title = element_text(hjust = 0.5, size = 25, face = "bold"))+
  ggtitle("")

CTQxNoticing <- ggplot(data = cleaned_data_final, aes(x=(Cumulative_Trauma), y=Noticing))+
  geom_point(size = 2, alpha = 0.5)+
  geom_smooth(method = 'lm', se=F, color="black")+
  xlab("CTQ Score")+
  ylab("Noticing (MAIA-2)")+
  theme_classic()+
  theme(text = element_text(size=15), plot.title = element_text(hjust = 0.5, size = 25, face = "bold"))+
  ggtitle("")

CTQxNotDisctracting <- ggplot(data = cleaned_data_final, aes(x=(Cumulative_Trauma), y=Not_Distracting))+
  geom_point(size = 2, alpha = 0.5)+
  geom_smooth(method = 'lm', se=F, color="black")+
  xlab("CTQ Score")+
  ylab("Not Disctracting (MAIA-2)")+
  theme_classic()+
  theme(text = element_text(size=15), plot.title = element_text(hjust = 0.5, size = 25, face = "bold"))+
  ggtitle("")

CTQxNotWorrying <- ggplot(data = cleaned_data_final, aes(x=(Cumulative_Trauma), y=Not_Worrying))+
  geom_point(size = 2, alpha = 0.5)+
  geom_smooth(method = 'lm', se=F, color="black")+
  xlab("CTQ Score")+
  ylab("Not Worrying (MAIA-2)")+
  theme_classic()+
  theme(text = element_text(size=15), plot.title = element_text(hjust = 0.5, size = 25, face = "bold"))+
  ggtitle("")

CTQxAttentionRegulation <- ggplot(data = cleaned_data_final, aes(x=(Cumulative_Trauma), y=Attention_Regulation))+
  geom_point(size = 2, alpha = 0.5)+
  geom_smooth(method = 'lm', se=F, color="black")+
  xlab("CTQ Score")+
  ylab("Attention Regulation (MAIA-2)")+
  theme_classic()+
  theme(text = element_text(size=15), plot.title = element_text(hjust = 0.5, size = 25, face = "bold"))+
  ggtitle("")

CTQxEmotionalAwareness <- ggplot(data = cleaned_data_final, aes(x=(Cumulative_Trauma), y=Emotional_Awareness))+
  geom_point(size = 2, alpha = 0.5, color = "blue")+
  geom_smooth(method = 'lm', se=F, color="black")+
  xlab("CTQ Score")+
  ylab("Emotional Awareness (MAIA-2)")+
  theme_classic()+
  theme(text = element_text(size=15, ), plot.title = element_text(hjust = 0.5, size = 25, face = "bold"))+
  ggtitle("†")

CTQxSelfRegulation <- ggplot(data = cleaned_data_final, aes(x=(Cumulative_Trauma), y=Self_Regulation))+
  geom_point(size = 2, alpha = 0.5)+
  geom_smooth(method = 'lm', se=F, color="black")+
  xlab("CTQ Score")+
  ylab("Self-Regulation (MAIA-2)")+
  theme_classic()+
  theme(text = element_text(size=15), plot.title = element_text(hjust = 0.5, size = 25, face = "bold"))+
  ggtitle("")

CTQxBodyListening <- ggplot(data = cleaned_data_final, aes(x=(Cumulative_Trauma), y=Body_Listening))+
  geom_point(size = 2, alpha = 0.5)+
  geom_smooth(method = 'lm', se=F, color="black")+
  xlab("CTQ Score")+
  ylab("Body Listening (MAIA-2)")+
  theme_classic()+
  theme(text = element_text(size=15), plot.title = element_text(hjust = 0.5, size = 25, face = "bold"))+
  ggtitle("")

CTQxTrusting <- ggplot(data = cleaned_data_final, aes(x=(Cumulative_Trauma), y=Trusting))+
  geom_point(size = 2, alpha = 0.5)+
  geom_smooth(method = 'lm', se=F, color="black")+
  xlab("CTQ Score")+
  ylab("Trusting (MAIA-2)")+
  theme_classic()+
  theme(text = element_text(size=15), plot.title = element_text(hjust = 0.5, size = 25, face = "bold"))+
  ggtitle("")

ggarrange(CTQxIAS,CTQxICQ, CTQxNoticing,CTQxNotDisctracting, CTQxNotWorrying,
          CTQxAttentionRegulation, CTQxEmotionalAwareness, CTQxSelfRegulation, CTQxBodyListening,CTQxTrusting,   
          ncol = 5,nrow = 2, common.legend = T, legend = "bottom",
          labels = c("A", "B", "C", "D", "E","F","G", "H", "I", "J"))

#ggsave(filename = "Figure_2_rev.pdf", path = "/Users/paulsavoca/Desktop", width = 15, height = 10, device = cairo_pdf)

##Regression Output Tables
ias.quic.tab <- tbl_regression(ias.quic, pvalue_fun = label_style_pvalue(digits = 3))
icq.quic.tab <- tbl_regression(icq.quic, pvalue_fun = label_style_pvalue(digits = 3))
noticing.quic.tab <- tbl_regression(noticing.quic, pvalue_fun = label_style_pvalue(digits = 3))
not_distracting.quic.tab <- tbl_regression(not_distracting.quic, pvalue_fun = label_style_pvalue(digits = 3))
not_worrying.quic.tab <- tbl_regression(not_worrying.quic, pvalue_fun = label_style_pvalue(digits = 3))
attention_regulation.quic.tab <- tbl_regression(attention_regulation.quic, pvalue_fun = label_style_pvalue(digits = 3))
emotional_awareness.quic.tab <- tbl_regression(emotional_awareness.quic, pvalue_fun = label_style_pvalue(digits = 3))
self_regulation.quic.tab <- tbl_regression(self_regulation.quic, pvalue_fun = label_style_pvalue(digits = 3))
body_listening.quic.tab <- tbl_regression(body_listening.quic, pvalue_fun = label_style_pvalue(digits = 3))
trusting.quic.tab <- tbl_regression(trusting.quic, pvalue_fun = label_style_pvalue(digits = 3))

tbl_merge(
  tbls = list(ias.quic.tab, icq.quic.tab, noticing.quic.tab,not_distracting.tab, not_worrying.tab,
              attention_regulation.tab, emotional_awareness.tab, self_regulation.tab, body_listening.tab, trusting.tab),
  tab_spanner = c(
    "**IAS**",
    "**ICQ**",
    "**Noticing**",
    "**Not Distracting**",
    "**Not Worrying**",
    "**Attention Regulation**",
    "**Emotional Awareness**",
    "**Self-Regulation**",
    "**Body Listening**",
    "**Trusting**"
  )
)

ias.ctq.tab <- tbl_regression(ias.ctq, pvalue_fun = label_style_pvalue(digits = 3))
icq.ctq.tab <- tbl_regression(icq.ctq, pvalue_fun = label_style_pvalue(digits = 3))
noticing.ctq.tab <- tbl_regression(noticing.ctq, pvalue_fun = label_style_pvalue(digits = 3))
not_distracting.ctq.tab <- tbl_regression(not_distracting.ctq, pvalue_fun = label_style_pvalue(digits = 3))
not_worrying.ctq.tab <- tbl_regression(not_worrying.ctq, pvalue_fun = label_style_pvalue(digits = 3))
attention_regulation.ctq.tab <- tbl_regression(attention_regulation.ctq, pvalue_fun = label_style_pvalue(digits = 3))
emotional_awareness.ctq.tab <- tbl_regression(emotional_awareness.ctq, pvalue_fun = label_style_pvalue(digits = 3))
self_regulation.ctq.tab <- tbl_regression(self_regulation.ctq, pvalue_fun = label_style_pvalue(digits = 3))
body_listening.ctq.tab <- tbl_regression(body_listening.ctq, pvalue_fun = label_style_pvalue(digits = 3))
trusting.ctq.tab <- tbl_regression(trusting.ctq, pvalue_fun = label_style_pvalue(digits = 3))

tbl_merge(
  tbls = list(ias.ctq.tab, icq.ctq.tab, noticing.ctq.tab,not_distracting.ctq.tab, not_worrying.ctq.tab,
              attention_regulation.ctq.tab, emotional_awareness.ctq.tab, self_regulation.ctq.tab, body_listening.ctq.tab, trusting.ctq.tab),
  tab_spanner = c(
    "**IAS**",
    "**ICQ**",
    "**Noticing**",
    "**Not Distracting**",
    "**Not Worrying**",
    "**Attention Regulation**",
    "**Emotional Awareness**",
    "**Self-Regulation**",
    "**Body Listening**",
    "**Trusting**"
  )
)

##Regression Diagnostics
#Homoscedasticity
bptest(ias.quic)
bptest(icq.quic)
bptest(noticing.quic)
bptest(not_distracting.quic)
bptest(not_worrying.quic) 
bptest(attention_regulation.quic)
bptest(emotional_awareness.quic)
bptest(self_regulation.quic)
bptest(body_listening.quic)
bptest(trusting.quic)

bptest(ias.ctq)
bptest(icq.ctq)
bptest(noticing.ctq)
bptest(not_distracting.ctq)
bptest(not_worrying.ctq) 
bptest(attention_regulation.ctq) 
bptest(emotional_awareness.ctq)
bptest(self_regulation.ctq)
bptest(body_listening.ctq)
bptest(trusting.ctq)

#Normality of Resid
hist(ias.quic$residuals)
hist(icq.quic$residuals)
hist(noticing.quic$residuals)
hist(not_distracting.quic$residuals)
hist(not_worrying.quic$residuals) 
hist(attention_regulation.quic$residuals)
hist(emotional_awareness.quic$residuals)
hist(self_regulation.quic$residuals)
hist(body_listening.quic$residuals)
hist(trusting.quic$residuals)

hist(ias.ctq$residuals)
hist(icq.ctq$residuals)
hist(noticing.ctq$residuals)
hist(not_distracting.ctq$residuals)
hist(not_worrying.ctq$residuals) 
hist(attention_regulation.ctq$residuals) 
hist(emotional_awareness.ctq$residuals)
hist(self_regulation.ctq$residuals)
hist(body_listening.ctq$residuals)
hist(trusting.ctq$residuals)

##Questionnaire Relability
ias.items <- cleaned_data_final[28:48] #No reverse scored Items
psych::alpha(ias.items)

maia.qs <- cleaned_data_final[53:89] #MAIA Items already have reverse scoring computed
noticing.items <- cleaned_data_final[53:56]
psych::alpha(noticing.items)
not_distracting.items <- cleaned_data_final[57:62]
psych::alpha(not_distracting.items)
not_worrying.items <- cleaned_data_final[63:67]
psych::alpha(not_worrying.items)
attention_regulation.items <- cleaned_data_final[68:74]
psych::alpha(attention_regulation.items)
emotional_awareness.items <- cleaned_data_final[75:79]
psych::alpha(emotional_awareness.items)
self_regulation.items <- cleaned_data_final[80:83]
psych::alpha(self_regulation.items)
body_listening.items <- cleaned_data_final[84:86]
psych::alpha(body_listening.items)
trusting.items <- cleaned_data_final[87:89]
psych::alpha(trusting.items)


icq.items <- cleaned_data_final[c("icq_1", "icq_2","icq_3","icq_4r","icq_5",
                             "icq_6","icq_7","icq_8","icq_9r","icq_10",
                             "icq_11","icq_12r","icq_13","icq_14r","icq_15",
                             "icq_16r","ic1_17","icq_18r","icq_19r","icq_20")]
psych::alpha(icq.items)

quic.items <- cleaned_data_final[c("quic_1r","quic_2","quic_3r","quic_4r","quic_5r","quic_6r","quic_7r","quic_8r","quic_9r",
                                   "quic_10r","quic_11","quic_12","quic_13","quic_14r","quic_15r","quic_16","quic_17r","quic_18",
                                   "quic_19","quic_20","quic_21","quic_22","quic_23","quic_24","quic_25","quic_26","quic_27",
                                   "quic_28r","quic_29","quic_30","quic_31","quic_32","quic_33","quic_34","quic_35","quic_36r",
                                   "quic_37","quic_38")]
psych::alpha(quic.items)

#Power Analysis
results <- pwr.f2.test(u = 3, f2 = 0.08, sig.level = 0.05, power = 0.90)

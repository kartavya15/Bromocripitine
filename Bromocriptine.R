require(tidyverse)
require(stringr)
require(lubridate)
require(mgcv)
require(metRology)

library(tidyverse)
library(stringr)
library(lubridate)
library(mgcv)
library(metRology)

# Load Dataset
Sharma <- read.csv("~/Desktop/Sharma/Sharma.csv", stringsAsFactors = FALSE)
Demo <- read.csv("~/Desktop/Sharma/Demo.csv", stringsAsFactors = FALSE)
Demo2 <- read.csv("~/Desktop/Sharma/Demo2.csv", stringsAsFactors = FALSE)

# Rename the Positive culture variable
colnames(Demo2)[39] <- "NoPosCult"

# Merge the dataset to include positive culture variable
Demo <- left_join(Demo, select(Demo2, CASE_ID, NoPosCult), by = "CASE_ID")

# Convert variables to numeric data
Demo$TR_BRAIN_INJURY <- as.numeric(Demo$TR_BRAIN_INJURY)
Demo$NTR_SUBARACHNOID_HEM <- as.numeric(Demo$NTR_SUBARACHNOID_HEM)
Demo$NTR_INTRACEREBRAL_HEM <- as.numeric(Demo$NTR_INTRACEREBRAL_HEM)
Demo$CEREB_INFARCT <- as.numeric(Demo$CEREB_INFARCT)
Demo$SUBDURAL_HEM <- as.numeric(Demo$SUBDURAL_HEM)
Demo$SUBARACH_HEM <- as.numeric(Demo$SUBARACH_HEM)
Demo$OTHER_NTR_INTRACRAN_HEM <- as.numeric(Demo$OTHER_NTR_INTRACRAN_HEM)

# Create an indicator variable for central fever (1 = central fever, 0 = otherwise)
Demo <- mutate(Demo, Group = ifelse(Indication.for.bromocriptine.Use == "CENTRAL FEVER", 1, 0))

# Create date and time variables that are appropriate for determining time differences
Demo$Date1 <- gsub(" .*", "", Demo$FD_BROMO_DTTM)
Demo$Date2 <- gsub(" .*", "", Demo$LD_BROMO_DTTM)
Demo$Time1 <- gsub(".* ", "", Demo$FD_BROMO_DTTM)
Demo$Time2 <- gsub(".* ", "", Demo$LD_BROMO_DTTM)
Demo$Time1 <- sapply(Demo$Time1, function(x) ifelse(nchar(x) == 4, paste(0, x, ":00", sep = ""),
                                                    paste(x, ":00", sep = "")))
Demo$Time2 <- sapply(Demo$Time2, function(x) ifelse(nchar(x) == 4, paste(0, x, ":00", sep = ""),
                                                    paste(x, ":00", sep = "")))
Date1 <- rep(NA, nrow(Demo))
Date2 <- rep(NA, nrow(Demo))
for(i in 1:nrow(Demo)){
  hold <- Demo$Date1[i]
  Month <- gsub("/.*", "", hold)
  Day <- gsub("/.*", "", substring(hold, nchar(Month) + 2))
  Year <- gsub(".*/", "", hold)
  Month <- ifelse(nchar(Month) == 1, paste(0, Month, sep = ""), Month)
  Day <- ifelse(nchar(Day) == 1, paste(0, Day, sep = ""), Day)
  Date1[i] <- paste(Month, "/", Day, "/", "20", Year, sep = "")
  hold2 <- Demo$Date2[i]
  Month2 <- gsub("/.*", "", hold2)
  Day2 <- gsub("/.*", "", substring(hold2, nchar(Month2) + 2))
  Year2 <- gsub(".*/", "", hold2)
  Month2 <- ifelse(nchar(Month2) == 1, paste(0, Month2, sep = ""), Month2)
  Day2 <- ifelse(nchar(Day2) == 1, paste(0, Day2, sep = ""), Day2)
  Date2[i] <- paste(Month2, "/", Day2, "/", "20", Year2, sep = "")
}

Demo$Date1 <- Date1
Demo$Date2 <- Date2

Demo <- mutate(Demo, DateTime1 = paste(Date1, Time1))
Demo <- mutate(Demo, DateTime2 = paste(Date2, Time2))

Demo$DateTime1 <- mdy_hms(Demo$DateTime1)
Demo$DateTime2 <- mdy_hms(Demo$DateTime2)

# Determine time difference between first date and last date of bromocriptine administration
Demo$TimeDif <- interval(Demo$DateTime1, Demo$DateTime2) / days(1)


##############################################################################################################
# 
#                                         TABLE 1
#
##############################################################################################################
# The below code generates the summary statistics for the table requested
n <- c(nrow(filter(Demo, Group == 1)), 
       nrow(filter(Demo, Group == 1 & NoPosCult == 1)))
Age_mean_sd <- c(paste(round(mean(filter(Demo, Group == 1)$AGE), 2), 
                       " (",
                       round(sd(filter(Demo, Group == 1)$AGE), 2), 
                       ")", sep = ""),
                 paste(round(mean(filter(Demo, Group == 1 & NoPosCult == 1)$AGE), 2), 
                       " (",
                       round(sd(filter(Demo, Group == 1 & NoPosCult == 1)$AGE), 2), 
                       ")", sep = ""))
Age_median_iqr <- c(paste(round(median(filter(Demo, Group == 1)$AGE_AT_ADMISSION), 2), 
                          " (",
                          round(quantile(filter(Demo, Group == 1)$AGE_AT_ADMISSION, probs = 0.25), 2),
                          ", ",
                          round(quantile(filter(Demo, Group == 1)$AGE_AT_ADMISSION, probs = 0.75), 2),
                          ")", sep = ""),
                    paste(round(median(filter(Demo, Group == 1 & NoPosCult == 1)$AGE_AT_ADMISSION), 2), 
                          " (",
                          round(quantile(filter(Demo, Group == 1 & NoPosCult == 1)$AGE_AT_ADMISSION, probs = 0.25), 2),
                          ", ",
                          round(quantile(filter(Demo, Group == 1 & NoPosCult == 1)$AGE_AT_ADMISSION, probs = 0.75), 2),
                          ")", sep = ""))
Women_N_Percent <- c(paste(sum(filter(Demo, Group == 1)$SEX == "FEMALE"),
                           ", ",
                           round(mean(filter(Demo, Group == 1)$SEX == "FEMALE") * 100, 2),
                           "%", sep = ""),
                     paste(sum(filter(Demo, Group == 1 & NoPosCult == 1)$SEX == "FEMALE"),
                           ", ",
                           round(mean(filter(Demo, Group == 1 & NoPosCult == 1)$SEX == "FEMALE") * 100, 2),
                           "%", sep = ""))
WHITE_N_Percent <- c(paste(sum(filter(Demo, Group == 1)$RACE == "WHITE"),
                           ", ",
                           round(mean(filter(Demo, Group == 1)$RACE == "WHITE") * 100, 2),
                           "%", sep = ""),
                     paste(sum(filter(Demo, Group == 1 & NoPosCult == 1)$RACE == "WHITE"),
                           ", ",
                           round(mean(filter(Demo, Group == 1 & NoPosCult == 1)$RACE == "WHITE") * 100, 2),
                           "%", sep = ""))
BLACK_N_Percent <- c(paste(sum(filter(Demo, Group == 1)$RACE == "BLACK"),
                           ", ",
                           round(mean(filter(Demo, Group == 1)$RACE == "BLACK") * 100, 2),
                           "%", sep = ""),
                     paste(sum(filter(Demo, Group == 1 & NoPosCult == 1)$RACE == "BLACK"),
                           ", ",
                           round(mean(filter(Demo, Group == 1 & NoPosCult == 1)$RACE == "BLACK") * 100, 2),
                           "%", sep = ""))
ASIAN_N_Percent <- c(paste(sum(filter(Demo, Group == 1)$RACE == "ASIAN"),
                           ", ",
                           round(mean(filter(Demo, Group == 1)$RACE == "ASIAN") * 100, 2),
                           "%", sep = ""),
                     paste(sum(filter(Demo, Group == 1 & NoPosCult == 1)$RACE == "ASIAN"),
                           ", ",
                           round(mean(filter(Demo, Group == 1 & NoPosCult == 1)$RACE == "ASIAN") * 100, 2),
                           "%", sep = ""))
UNKNOWN_N_Percent <- c(paste(sum(filter(Demo, Group == 1)$RACE == "UNKNOWN"),
                             ", ",
                             round(mean(filter(Demo, Group == 1)$RACE == "UNKNOWN") * 100, 2),
                             "%", sep = ""),
                       paste(sum(filter(Demo, Group == 1 & NoPosCult == 1)$RACE == "UNKNOWN"),
                             ", ",
                             round(mean(filter(Demo, Group == 1 & NoPosCult == 1)$RACE == "UNKNOWN") * 100, 2),
                             "%", sep = ""))
TR_BRAIN_INJURY_N_Percent <- c(paste(sum(filter(Demo, Group == 1)$TR_BRAIN_INJURY, na.rm = TRUE),
                                     ", ",
                                     round(mean(filter(Demo, Group == 1)$TR_BRAIN_INJURY, na.rm = TRUE) * 100, 2),
                                     "%", sep = ""),
                               paste(sum(filter(Demo, Group == 1 & NoPosCult == 1)$TR_BRAIN_INJURY, na.rm = TRUE),
                                     ", ",
                                     round(mean(filter(Demo, Group == 1 & NoPosCult == 1)$TR_BRAIN_INJURY, na.rm = TRUE) * 100, 2),
                                     "%", sep = ""))
NTR_SUBARACHNOID_HEM_N_Percent <- c(paste(sum(filter(Demo, Group == 1)$NTR_SUBARACHNOID_HEM, na.rm = TRUE),
                                          ", ",
                                          round(mean(filter(Demo, Group == 1)$NTR_SUBARACHNOID_HEM, na.rm = TRUE) * 100, 2),
                                          "%", sep = ""),
                                    paste(sum(filter(Demo, Group == 1 & NoPosCult == 1)$NTR_SUBARACHNOID_HEM, na.rm = TRUE),
                                          ", ",
                                          round(mean(filter(Demo, Group == 1 & NoPosCult == 1)$NTR_SUBARACHNOID_HEM, na.rm = TRUE) * 100, 2),
                                          "%", sep = ""))
NTR_INTRACEREBRAL_HEM_N_Percent <- c(paste(sum(filter(Demo, Group == 1)$NTR_INTRACEREBRAL_HEM, na.rm = TRUE),
                                           ", ",
                                           round(mean(filter(Demo, Group == 1)$NTR_INTRACEREBRAL_HEM, na.rm = TRUE) * 100, 2),
                                           "%", sep = ""),
                                     paste(sum(filter(Demo, Group == 1 & NoPosCult == 1)$NTR_INTRACEREBRAL_HEM, na.rm = TRUE),
                                           ", ",
                                           round(mean(filter(Demo, Group == 1 & NoPosCult == 1)$NTR_INTRACEREBRAL_HEM, na.rm = TRUE) * 100, 2),
                                           "%", sep = ""))
CEREB_INFARCT_N_Percent <- c(paste(sum(filter(Demo, Group == 1)$CEREB_INFARCT, na.rm = TRUE),
                                   ", ",
                                   round(mean(filter(Demo, Group == 1)$CEREB_INFARCT, na.rm = TRUE) * 100, 2),
                                   "%", sep = ""),
                             paste(sum(filter(Demo, Group == 1 & NoPosCult == 1)$CEREB_INFARCT, na.rm = TRUE),
                                   ", ",
                                   round(mean(filter(Demo, Group == 1 & NoPosCult == 1)$CEREB_INFARCT, na.rm = TRUE) * 100, 2),
                                   "%", sep = ""))
SUBDURAL_HEM_N_Percent <- c(paste(sum(filter(Demo, Group == 1)$SUBDURAL_HEM, na.rm = TRUE),
                                  ", ",
                                  round(mean(filter(Demo, Group == 1)$SUBDURAL_HEM, na.rm = TRUE) * 100, 2),
                                  "%", sep = ""),
                            paste(sum(filter(Demo, Group == 1 & NoPosCult == 1)$SUBDURAL_HEM, na.rm = TRUE),
                                  ", ",
                                  round(mean(filter(Demo, Group == 1 & NoPosCult == 1)$SUBDURAL_HEM, na.rm = TRUE) * 100, 2),
                                  "%", sep = ""))
SUBARACH_HEM_N_Percent <- c(paste(sum(filter(Demo, Group == 1)$SUBARACH_HEM, na.rm = TRUE),
                                  ", ",
                                  round(mean(filter(Demo, Group == 1)$SUBARACH_HEM, na.rm = TRUE) * 100, 2),
                                  "%", sep = ""),
                            paste(sum(filter(Demo, Group == 1 & NoPosCult == 1)$SUBARACH_HEM, na.rm = TRUE),
                                  ", ",
                                  round(mean(filter(Demo, Group == 1 & NoPosCult == 1)$SUBARACH_HEM, na.rm = TRUE) * 100, 2),
                                  "%", sep = ""))
OTHER_NTR_INTRACRAN_HEM_N_Percent <- c(paste(sum(filter(Demo, Group == 1)$OTHER_NTR_INTRACRAN_HEM, na.rm = TRUE),
                                             ", ",
                                             round(mean(filter(Demo, Group == 1)$OTHER_NTR_INTRACRAN_HEM, na.rm = TRUE) * 100, 2),
                                             "%", sep = ""),
                                       paste(sum(filter(Demo, Group == 1 & NoPosCult == 1)$OTHER_NTR_INTRACRAN_HEM, na.rm = TRUE),
                                             ", ",
                                             round(mean(filter(Demo, Group == 1 & NoPosCult == 1)$OTHER_NTR_INTRACRAN_HEM, na.rm = TRUE) * 100, 2),
                                             "%", sep = ""))

LOS_DAYS_median_iqr <- c(paste(round(median(filter(Demo, Group == 1)$LOS_DAYS), 2), 
                               " (",
                               round(quantile(filter(Demo, Group == 1)$LOS_DAYS, probs = 0.25), 2),
                               ", ",
                               round(quantile(filter(Demo, Group == 1)$LOS_DAYS, probs = 0.75), 2),
                               ")", sep = ""),
                         paste(round(median(filter(Demo, Group == 1 & NoPosCult == 1)$LOS_DAYS), 2), 
                               " (",
                               round(quantile(filter(Demo, Group == 1 & NoPosCult == 1)$LOS_DAYS, probs = 0.25), 2),
                               ", ",
                               round(quantile(filter(Demo, Group == 1 & NoPosCult == 1)$LOS_DAYS, probs = 0.75), 2),
                               ")", sep = ""))
OutcomeGood_N_Percent <- c(paste(sum(filter(Demo, Group == 1 & MORTALITY == 0)$Outcome == "Good", na.rm = TRUE),
                                 ", ",
                                 round(mean(filter(Demo, Group == 1 & MORTALITY == 0)$Outcome == "Good", na.rm = TRUE) * 100, 2),
                                 "%", sep = ""),
                           paste(sum(filter(Demo, Group == 1 & NoPosCult == 1 & MORTALITY == 0)$Outcome == "Good", na.rm = TRUE),
                                 ", ",
                                 round(mean(filter(Demo, Group == 1 & NoPosCult == 1 & MORTALITY == 0)$Outcome == "Good", na.rm = TRUE) * 100, 2),
                                 "%", sep = ""))
OutcomePoor_N_Percent <- c(paste(sum(filter(Demo, Group == 1 & MORTALITY == 0)$Outcome == "Poor", na.rm = TRUE),
                                 ", ",
                                 round(mean(filter(Demo, Group == 1 & MORTALITY == 0)$Outcome == "Poor", na.rm = TRUE) * 100, 2),
                                 "%", sep = ""),
                           paste(sum(filter(Demo, Group == 1 & NoPosCult == 1 & MORTALITY == 0)$Outcome == "Poor", na.rm = TRUE),
                                 ", ",
                                 round(mean(filter(Demo, Group == 1 & NoPosCult == 1 & MORTALITY == 0)$Outcome == "Poor", na.rm = TRUE) * 100, 2),
                                 "%", sep = ""))
MORTALITY_N_Percent <- c(paste(sum(filter(Demo, Group == 1)$MORTALITY , na.rm = TRUE),
                               ", ",
                               round(mean(filter(Demo, Group == 1)$MORTALITY , na.rm = TRUE) * 100, 2),
                               "%", sep = ""),
                         paste(sum(filter(Demo, Group == 1 & NoPosCult == 1)$MORTALITY , na.rm = TRUE),
                               ", ",
                               round(mean(filter(Demo, Group == 1 & NoPosCult == 1)$MORTALITY , na.rm = TRUE) * 100, 2),
                               "%", sep = ""))
TimeDif_median_iqr <- c(paste(round(median(filter(Demo, Group == 1)$TimeDif), 2), 
                              " (",
                              round(quantile(filter(Demo, Group == 1)$TimeDif, probs = 0.25), 2),
                              ", ",
                              round(quantile(filter(Demo, Group == 1)$TimeDif, probs = 0.75), 2),
                              ")", sep = ""),
                        paste(round(median(filter(Demo, Group == 1 & NoPosCult == 1)$TimeDif), 2), 
                              " (",
                              round(quantile(filter(Demo, Group == 1 & NoPosCult == 1)$TimeDif, probs = 0.25), 2),
                              ", ",
                              round(quantile(filter(Demo, Group == 1 & NoPosCult == 1)$TimeDif, probs = 0.75), 2),
                              ")", sep = ""))
BROMO_ADMIN_TOTAL_median_iqr <- c(paste(round(median(filter(Demo, Group == 1)$BROMO_ADMIN_TOTAL), 2), 
                                        " (",
                                        round(quantile(filter(Demo, Group == 1)$BROMO_ADMIN_TOTAL, probs = 0.25), 2),
                                        ", ",
                                        round(quantile(filter(Demo, Group == 1)$BROMO_ADMIN_TOTAL, probs = 0.75), 2),
                                        ")", sep = ""),
                                  paste(round(median(filter(Demo, Group == 1 & NoPosCult == 1)$BROMO_ADMIN_TOTAL), 2), 
                                        " (",
                                        round(quantile(filter(Demo, Group == 1 & NoPosCult == 1)$BROMO_ADMIN_TOTAL, probs = 0.25), 2),
                                        ", ",
                                        round(quantile(filter(Demo, Group == 1 & NoPosCult == 1)$BROMO_ADMIN_TOTAL, probs = 0.75), 2),
                                        ")", sep = ""))


tb <- rbind(n,
            Age_mean_sd,
            Age_median_iqr,
            Women_N_Percent,
            WHITE_N_Percent,
            BLACK_N_Percent,
            ASIAN_N_Percent,
            UNKNOWN_N_Percent,
            TR_BRAIN_INJURY_N_Percent,
            NTR_SUBARACHNOID_HEM_N_Percent,
            NTR_INTRACEREBRAL_HEM_N_Percent,
            CEREB_INFARCT_N_Percent,
            SUBDURAL_HEM_N_Percent,
            SUBARACH_HEM_N_Percent,
            OTHER_NTR_INTRACRAN_HEM_N_Percent,
            LOS_DAYS_median_iqr,
            OutcomeGood_N_Percent,
            OutcomePoor_N_Percent,
            MORTALITY_N_Percent,
            TimeDif_median_iqr,
            BROMO_ADMIN_TOTAL_median_iqr)

##############################################################################################################

# MedAdm is a data frame of medicines administered
MedAdm <- filter(Sharma, str_detect(DATA_TYPE, "MEDICATION ADMIN") == 1 | 
                   str_detect(DATA_TYPE, "COOLING MEASURE") == 1)

# Bromo is a data frame of only bromocriptine administrations
Bromo <- filter(MedAdm, str_detect(ACTION, regex('bromocriptine', ignore_case = T)))

# Non-bromocriptine medication administrations
MedAdm <- filter(MedAdm, str_detect(ACTION, regex('dexamethasone', ignore_case = T)) |
                   str_detect(ACTION, regex('methylprednisone', ignore_case = T)) | 
                   str_detect(ACTION, regex('prednisone', ignore_case = T)) | 
                   (str_detect(ACTION, regex('acetaminophen', ignore_case = T)) & 
                      str_detect(ACTION, regex('hydro', ignore_case = T)) == FALSE) | 
                   (str_detect(ACTION, regex('acetaminophen', ignore_case = T)) & 
                      str_detect(ACTION, regex('codeine', ignore_case = T)) == FALSE) | 
                   str_detect(ACTION, regex('ibuprofen', ignore_case = T)) | 
                   str_detect(ACTION, regex('naproxen', ignore_case = T)) | 
                   str_detect(ACTION, regex('celecoxib', ignore_case = T)) | 
                   str_detect(ACTION, regex('aspirin', ignore_case = T)) | 
                   str_detect(RESULT, regex('Cooling device documented', ignore_case = T)))

# Temperature data frame
Temp <- filter(Sharma, DATA_TYPE == "TEMPERATURE")

# Create an indicator variable to indicate if the temperature obtained was invasive (1) or not (0)
Temp <- mutate(Temp, Invasive = ifelse(ADDTL_1 %in% c("BLADDER",
                                                      "ESOPHAGEAL",
                                                      "RECTAL",
                                                      "ORAL",
                                                      "ORAL; reported to nurse.",
                                                      "BLADDER; Simultaneous filing. User may be unaware of other data.",
                                                      "BLADDER; cooling machine",                                          
                                                      "ESOPHAGEAL; ice packs on, fan on",                                   
                                                      "ESOPHAGEAL; Simultaneous filing. User may be unaware of other data.",
                                                      "BLADDER; b",                                                       
                                                      "BLADDER; cooling blsnket",                                           
                                                      "BLADDER; cooling blanket"), 
                                       1, 0))

# Indicator variable for surface temperature 
Temp <- mutate(Temp, Surface = ifelse(ADDTL_1 %in% c("TEMPORAL ARTERY",
                                                     "TYMPANIC",
                                                     "AXILLARY"), 
                                      1, 0))

# Indicator variable to denote if temperature method was unknown
Temp <- mutate(Temp, UnknownTempMethod = ifelse(ADDTL_1 %in% c("$null$"), 
                                                1, 0))
Temp$RESULT <- as.numeric(Temp$RESULT)

# Construct final data frame for temperatures and remove extraneous variables
Temp <- left_join(Temp, dplyr::select(Demo, CASE_ID, Group, AGE_AT_ADMISSION, 
                                      SEX, Outcome, NoPosCult), by = "CASE_ID")
Temp <- Temp[,c(1:3, 5, 7, 8, 11:17, 19)]
colnames(Temp) <- c("ID", "Start_Bromo", "Stop_Bromo", "Time", "Result", 
                    "Additional_1", "Bromo", "Invasive", "Surface", "UnknownTempMethod", 
                    "Group", "Age", "Sex", "NoPosCult")
Temp <- filter(Temp, ID %in% Demo$CASE_ID)

# Indicator variable for sex (Female = 1, Male = 0)
Temp <- mutate(Temp, Female = ifelse(Sex == "FEMALE", 1, 0))

# Restrict data to only Central Fever subjects.
Temp <- filter(Temp, Group == 1)

# Adjust time data
Temp$Start_Bromo <- mdy_hm(Temp$Start_Bromo)
Temp$Time <- mdy_hm(Temp$Time)
Temp$Stop_Bromo <- mdy_hm(Temp$Stop_Bromo)
Temp$NewTime <- interval(Temp$Start_Bromo, Temp$Time) %/% minutes(1)
Temp$NewTime <- Temp$NewTime/60
Temp$Hours <- hour(Temp$Time)
Temp$Minutes <- minute(Temp$Time)
Temp <- mutate(Temp, NewHours = Hours + Minutes/60)
Temp$NewDays <- interval(Temp$Start_Bromo, Temp$Time) %/% days(1)

holdDF <- data.frame(ID = unique(Temp$ID), 
                     RBL_Date = sapply(unique(Temp$ID), function(x) min(filter(Temp, ID == x & NewTime >= 0 & 
                                                                                 Result <= 100)$Time)))
Temp <- left_join(Temp, holdDF, by = "ID")
Temp$RBL_Date <- as_datetime(Temp$RBL_Date)

MedAdm$RECORDED_TIME <- mdy_hm(MedAdm$RECORDED_TIME)

MedAdm2 <- NULL
for(i in 1:nrow(Temp)){
  hold <- Temp[i,]
  hold2 <- filter(MedAdm, CASE_ID == hold$ID & 
                    RECORDED_TIME < hold$Time)
  if(nrow(hold2) > 0){
    hold2 <- hold2[which.max(hold2$RECORDED_TIME),]
    hold2$Time <- hold$Time
    MedAdm2 <- rbind(MedAdm2, hold2)
  } 
}

colnames(MedAdm2)[c(1, 5, 12)] <- c("ID", "AP_Time", "Time")

# Create an indicator variable for cooling administration
MedAdm2 <- mutate(MedAdm2, Cooling = ifelse(ACTION == "THERMOREGULATION - COOLING",
                                            1, 0))

# Work with time data
Bromo$Time2 <- mdy(gsub(" .*", "", Bromo$RECORDED_TIME))
Bromo$ADDTL_1 <- as.numeric(Bromo$ADDTL_1)
Bromo2 <- NULL
for(i in 1:length(unique(Bromo$CASE_ID))){
  hold <- filter(Bromo, CASE_ID == unique(Bromo$CASE_ID)[i])
  for(j in 1:length(unique(hold$Time2))){
    hold2 <- filter(hold, Time2 == unique(hold$Time2)[j])
    holdDF <- data.frame(CASE_ID = unique(hold2$CASE_ID),
                         Date = unique(hold2$Time2),
                         ADDTL_1 = sum(hold2$ADDTL_1, na.rm = TRUE))
    Bromo2 <- rbind(Bromo2, holdDF)
  }
}

Bromo2 <- left_join(Bromo2, unique(select(Demo, CASE_ID, Group, NoPosCult)), by = "CASE_ID")
Bromo2 <- filter(Bromo2, CASE_ID %in% Demo$CASE_ID)

##############################################################################################################
#
#                                     ADD ADDITIONAL DATE FOR TABLE 1
#
##############################################################################################################

BROMO_DOSE_median_range <- c(paste(round(median(filter(Bromo2, Group == 1)$ADDTL_1), 2), 
                                   " (",
                                   round(quantile(filter(Bromo2, Group == 1)$ADDTL_1, probs = 0), 2),
                                   ", ",
                                   round(quantile(filter(Bromo2, Group == 1)$ADDTL_1, probs = 1), 2),
                                   ")", sep = ""),
                             paste(round(median(filter(Bromo2, Group == 1 & NoPosCult == 1)$ADDTL_1), 2), 
                                   " (",
                                   round(quantile(filter(Bromo2, Group == 1 & NoPosCult == 1)$ADDTL_1, probs = 0), 2),
                                   ", ",
                                   round(quantile(filter(Bromo2, Group == 1 & NoPosCult == 1)$ADDTL_1, probs = 1), 2),
                                   ")", sep = ""))

tb <- rbind(tb, BROMO_DOSE_median_range)

##############################################################################################################

Temp <- left_join(Temp, select(filter(MedAdm2, is.na(AP_Time) == 0), ID, Time, AP_Time, Cooling), by = c("ID", "Time"))

holdTemp <- NULL
for(i in 1:length(unique(Temp$ID))){
  hold <- filter(Temp, ID == unique(Temp$ID)[i])
  holdDF <- data.frame(CASE_ID = unique(hold$ID),
                       AP = (nrow(filter(hold, is.na(AP_Time) == 0 & Cooling == 0)) > 0) + 0.0)
  holdTemp <- rbind(holdTemp, holdDF)
}


holdTemp <- left_join(holdTemp, unique(select(Demo, CASE_ID, Group, NoPosCult)), by = "CASE_ID")
holdTemp <- filter(holdTemp, CASE_ID %in% Demo$CASE_ID)

##############################################################################################################
#
#                                         ADD ADDITIONAL DATA TO TABLE 1
#
##############################################################################################################

Antipyretic_N_Percent <- c(paste(sum(filter(holdTemp, Group == 1)$AP, na.rm = TRUE),
                                 ", ",
                                 round(mean(filter(holdTemp, Group == 1)$AP, na.rm = TRUE) * 100, 2),
                                 "%", sep = ""),
                           paste(sum(filter(holdTemp, Group == 1 & NoPosCult == 1)$AP, na.rm = TRUE),
                                 ", ",
                                 round(mean(filter(holdTemp, Group == 1 & NoPosCult == 1)$AP, na.rm = TRUE) * 100, 2),
                                 "%", sep = ""))
tb <- rbind(tb, Antipyretic_N_Percent)

##############################################################################################################

Temp$AP_Time_Dif <- interval(Temp$AP_Time, Temp$Time) / minutes(1)
Temp$AP_Time_Dif <- Temp$AP_Time_Dif/60
Temp0 <- NULL
for(i in 1:length(unique(Temp$ID))){
  hold <- filter(Temp, ID == unique(Temp$ID)[i] & NewTime <= 0)
  hold <- arrange(hold, abs(NewTime))
  Temp0 <- rbind(Temp0, hold[1,])
}

Temp0 <- select(Temp0, ID, Result, NewTime)
colnames(Temp0) <- c("ID", "BL_Temp", "BL_Time")

Temp <- left_join(Temp, Temp0, by = "ID")

Temp <- mutate(Temp, Cooling = ifelse(is.na(Cooling) == 1, 0, Cooling))
Temp <- mutate(Temp, Antipyretic = ifelse(is.na(AP_Time_Dif) == 0 & AP_Time_Dif < 8 & Cooling == 0, 
                                          1, 0))
Temp <- mutate(Temp, Cooling = ifelse(Cooling == 1 & AP_Time_Dif > 6, 0, Cooling))
Temp$zAge <- (Temp$Age - mean(sapply(unique(Temp$ID), function(x) filter(Temp, ID == x)$Age[1])))/sd(sapply(unique(Temp$ID), function(x) filter(Temp, ID == x)$Age[1]))
Temp <- mutate(Temp, Age50 = ifelse(Age > 50, 1, 0))


holdTemp2 <- NULL
for(i in 1:length(unique(Temp$ID))){
  hold <- filter(Temp, ID == unique(Temp$ID)[i])
  holdDF <- data.frame(CASE_ID = unique(hold$ID),
                       Invasive = (nrow(filter(hold, Invasive == 1)) > 0) + 0.0,
                       Surface = (nrow(filter(hold, Surface == 1)) > 0) + 0.0,
                       UnknownTempMethod = (nrow(filter(hold, UnknownTempMethod == 1)) > 0) + 0.0)
  holdTemp2 <- rbind(holdTemp2, holdDF)
}

holdTemp2 <- left_join(holdTemp2, unique(select(Demo, CASE_ID, Group, NoPosCult)), by = "CASE_ID")
holdTemp2 <- filter(holdTemp2, CASE_ID %in% Demo$CASE_ID)

##############################################################################################################
#
#                                       ADD ADDITIONAL DATA FOR TABLE 1
#
##############################################################################################################

Invasive_N_Percent <- c(paste(sum(filter(holdTemp2, Group == 1)$Invasive, na.rm = TRUE),
                              ", ",
                              round(mean(filter(holdTemp2, Group == 1)$Invasive, na.rm = TRUE) * 100, 2),
                              "%", sep = ""),
                        paste(sum(filter(holdTemp2, Group == 1 & NoPosCult == 1)$Invasive, na.rm = TRUE),
                              ", ",
                              round(mean(filter(holdTemp2, Group == 1 & NoPosCult == 1)$Invasive, na.rm = TRUE) * 100, 2),
                              "%", sep = ""))
Surface_N_Percent <- c(paste(sum(filter(holdTemp2, Group == 1)$Surface, na.rm = TRUE),
                             ", ",
                             round(mean(filter(holdTemp2, Group == 1)$Surface, na.rm = TRUE) * 100, 2),
                             "%", sep = ""),
                       paste(sum(filter(holdTemp2, Group == 1 & NoPosCult == 1)$Surface, na.rm = TRUE),
                             ", ",
                             round(mean(filter(holdTemp2, Group == 1 & NoPosCult == 1)$Surface, na.rm = TRUE) * 100, 2),
                             "%", sep = ""))
UnknownTempMethod_N_Percent <- c(paste(sum(filter(holdTemp2, Group == 1)$UnknownTempMethod, na.rm = TRUE),
                                       ", ",
                                       round(mean(filter(holdTemp2, Group == 1)$UnknownTempMethod, na.rm = TRUE) * 100, 2),
                                       "%", sep = ""),
                                 paste(sum(filter(holdTemp2, Group == 1 & NoPosCult == 1)$UnknownTempMethod, na.rm = TRUE),
                                       ", ",
                                       round(mean(filter(holdTemp2, Group == 1 & NoPosCult == 1)$UnknownTempMethod, na.rm = TRUE) * 100, 2),
                                       "%", sep = ""))
tb <- rbind(tb, Invasive_N_Percent, 
            Surface_N_Percent, 
            UnknownTempMethod_N_Percent)

##############################################################################################################

theme_set(theme_bw())
ggplot(data = filter(Temp, NewTime >= -72 & 
                       NewTime <= 72 & 
                       Additional_1 != "$null$" & 
                       Bromo != "POST"), aes(x = NewTime, y = (Result))) + 
  #geom_line(aes(group = ID)) + 
  geom_smooth() + 
  labs(x = "Time from Bromocriptine (in Hours)",
       y = expression("Temperature " ( degree~F)))

Temp$logAge <- scale(Temp$Age)

Temp <- filter(Temp, NoPosCult == 1)

# Impute missing data based on the adjacent measurements
Temp0 <- rep(NA, length(unique(Temp$ID)))
Temp1 <- rep(NA, length(unique(Temp$ID)))
Temp2 <- rep(NA, length(unique(Temp$ID)))
Temp3 <- rep(NA, length(unique(Temp$ID)))
for(i in 1:length(Temp0)){
  hold <- filter(Temp, ID == unique(Temp$ID)[i])
  hold <- arrange(hold, NewTime)
  hold0b <- tail(filter(hold, NewTime <= 0), 1)
  hold0a <- head(filter(hold, NewTime > 0), 1)
  hold1b <- tail(filter(hold, NewTime <= 24), 1)
  hold1a <- head(filter(hold, NewTime > 24), 1)
  hold2b <- tail(filter(hold, NewTime <= 48), 1)
  hold2a <- head(filter(hold, NewTime > 48), 1)
  hold3b <- tail(filter(hold, NewTime <= 72), 1)
  hold3a <- head(filter(hold, NewTime > 72), 1)
  if(hold0b$NewTime == 0){
    Temp0[i] <- hold0b$Result
  } else if (nrow(hold0a) == 0){
    Temp0[i] <- NA
  } else {
    Temp0[i] <- hold0b$Result + 
      ((0 - hold0b$NewTime)) * (hold0a$Result - hold0b$Result)/(hold0a$NewTime - hold0b$NewTime)
  }
  if(hold1b$NewTime == 0){
    Temp1[i] <- hold1b$Result
  } else if (nrow(hold1a) == 0){
    Temp1[i] <- NA
  } else {
    Temp1[i] <- hold1b$Result + 
      ((24 - hold1b$NewTime)) * (hold1a$Result - hold1b$Result)/(hold1a$NewTime - hold1b$NewTime)
  }
  if(hold2b$NewTime == 0){
    Temp2[i] <- hold2b$Result
  } else if (nrow(hold2a) == 0){
    Temp2[i] <- NA
  } else {
    Temp2[i] <- hold2b$Result + 
      ((48 - hold2b$NewTime)) * (hold2a$Result - hold2b$Result)/(hold2a$NewTime - hold2b$NewTime)
  }
  if(hold3b$NewTime == 0){
    Temp3[i] <- hold3b$Result
  } else if (nrow(hold3a) == 0){
    Temp3[i] <- NA
  } else {
    Temp3[i] <- hold3b$Result + 
      ((72 - hold3b$NewTime)) * (hold3a$Result - hold3b$Result)/(hold3a$NewTime - hold3b$NewTime)
  }
}

wilcox.test(Temp0, Temp1, paired = TRUE)
wilcox.test(Temp0, Temp2, paired = TRUE)
wilcox.test(Temp0, Temp3, paired = TRUE)

plotDF <- data.frame(ID = unique(Temp$ID),
                     Temp0,
                     Temp1, 
                     Temp2, 
                     Temp3)

T_Full <- gather(plotDF, key = "Key", value = "Temperature", 3:5)
#T_Full <- mutate(T_Full, Time = ifelse(Key == "Temp0", 0, NA))
T_Full <- mutate(T_Full, Time = ifelse(Key == "Temp1", 1, NA))
T_Full <- mutate(T_Full, Time = ifelse(Key == "Temp2", 2, Time))
T_Full <- mutate(T_Full, Time = ifelse(Key == "Temp3", 3, Time))

T_Full <- mutate(T_Full, Dif = Temp0 - Temperature)
mean(filter(T_Full, Time == 1)$Dif, na.rm = TRUE)
sd(filter(T_Full, Time == 1)$Dif, na.rm = TRUE)
mean(filter(T_Full, Time == 2)$Dif, na.rm = TRUE)
sd(filter(T_Full, Time == 2)$Dif, na.rm = TRUE)
mean(filter(T_Full, Time == 3)$Dif, na.rm = TRUE)
sd(filter(T_Full, Time == 3)$Dif, na.rm = TRUE)

ggplot(data = T_Full, aes(x = factor(Time), y = Temperature)) + 
  geom_boxplot() + 
  labs(x = "Time from Bromocriptine (in Days)",
       y = expression("Temperature " ( degree~F)))


# Fit the generalized additive model
fit_gam <- gam(Result ~ s(NewTime, bs = "cr", k = 6) +
                 s(NewTime, by = Female, bs = "cr", k = 6) +
                 s(NewTime, by = Age, bs = "cr", k = 6) +
                 Invasive + 
                 Cooling + 
                 Antipyretic,
               data = filter(Temp, NewTime >= -24 & 
                               (NewTime/24) <= 10 & 
                               Bromo != "POST" & 
                               Additional_1 != "$null$"),
               method = "REML", 
               family = scat(), 
               select = TRUE)

summary(fit_gam)
gam.check(fit_gam)

# Generate data to show differences between males and females
testdata1 <- data.frame(NewTime = seq(-24, 240),
                        Age = mean(sapply(unique(Temp$ID), function(x) filter(Temp, ID == x)$Age[1])),
                        Female = 0,
                        Invasive = 0,
                        Cooling = 0,
                        Antipyretic = 0)
testdata2 <- data.frame(NewTime = seq(-24, 240),
                        Age = mean(sapply(unique(Temp$ID), function(x) filter(Temp, ID == x)$Age[1])),
                        Female = 1,
                        Invasive = 0,
                        Cooling = 0,
                        Antipyretic = 0)
testdata <- rbind(testdata1, 
                  testdata2)


testdata <- mutate(testdata, Bromo = ifelse(NewTime < 0, "PRE", "DURING"))

fits <- predict(fit_gam, newdata = testdata, type = "response", se = T)
df.res <- df.residual(fit_gam)
crit.t <- qt.scaled(0.975, df = 64.291, sd = 1.135, lower.tail = FALSE)

predicts = data.frame(testdata, fits) %>%
  mutate(lower = fit - crit.t*se.fit,
         upper = fit + crit.t*se.fit)

predicts <- mutate(predicts, Gender = ifelse(Female == 1, "Female", "Male"))
#predicts <- mutate(predicts, AgeGroup = ifelse(Age50 == 1, "Age > 50", "Age < 50"))

ggplot(data = predicts, aes(x = NewTime/24, y = fit)) + 
  facet_grid(Gender ~ .) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "gray90") + 
  geom_line() + 
  labs(x = "Time from Bromocriptine (in Days)",
       y = expression("Temperature " ( degree~F))) + 
  scale_x_continuous(breaks = seq(-1, 10)) + 
  scale_y_continuous(breaks = seq(97, 100, by = 0.5))

# Plot the trend of temperature over time
p1 <- plot(fit_gam, seWithMean = TRUE, #xlim = c(-3, 22), 
           select = 1, 
           shade = TRUE,
           xlab = "Time from Bromocriptine (in Days)",
           ylab = expression("Temperature " ( degree~F)))


NewTemp <- filter(Temp, NewTime >= -24 & 
                    (NewTime/24) <= 10 & 
                    Bromo != "POST" & 
                    Additional_1 != "$null$")

pdat <- testdata
p2 <- predict(fit_gam, newdata = pdat, type = "terms", se.fit = TRUE)
pdat$p2 <- p2$fit[,4]
pdat$se2 <- p2$se.fit[,4]
df.res <- df.residual(fit_gam)
crit.t <- qt(0.975, df.res, lower.tail = FALSE)
pdat <- transform(pdat,
                  upper = p2 + (crit.t * se2),
                  lower = p2 - (crit.t * se2))
pdat <- mutate(pdat, upper = upper - filter(pdat, NewTime == 0)$p2)
pdat <- mutate(pdat, lower = lower - filter(pdat, NewTime == 0)$p2)
pdat <- mutate(pdat, p2 = p2 - filter(pdat, NewTime == 0)$p2)


Term <- "NewTime"

# Functions to compute derivatives different than 0
Deriv <- function(mod, n = 200, eps = 1e-7, newdata, term) {
  if(inherits(mod, "gamm"))
    mod <- mod$gam
  m.terms <- attr(terms(mod), "term.labels")
  if(missing(newdata)) {
    newD <- sapply(model.frame(mod)[, m.terms, drop = FALSE],
                   function(x) seq(min(x), max(x), length = n))
    names(newD) <- m.terms
  } else {
    newD <- newdata
  }
  newDF <- data.frame(newD) ## needs to be a data frame for predict
  X0 <- predict(mod, newDF, type = "lpmatrix")
  newDF <- newDF + eps
  X1 <- predict(mod, newDF, type = "lpmatrix")
  Xp <- (X1 - X0) / eps
  Xp.r <- NROW(Xp)
  Xp.c <- NCOL(Xp)
  ## dims of bs
  bs.dims <- sapply(mod$smooth, "[[", "bs.dim") - 1
  ## number of smooth terms
  t.labs <- attr(mod$terms, "term.labels")
  ## match the term with the the terms in the model
  if(!missing(term)) {
    want <- grep(term, t.labs)
    if(!identical(length(want), length(term)))
      stop("One or more 'term's not found in model!")
    t.labs <- t.labs[want]
  }
  nt <- length(t.labs)
  ## list to hold the derivatives
  lD <- vector(mode = "list", length = nt)
  names(lD) <- t.labs
  for(i in seq_len(nt)) {
    Xi <- Xp * 0
    want <- grep(t.labs[i], colnames(X1))
    Xi[, want] <- Xp[, want]
    df <- Xi %*% coef(mod)
    df.sd <- rowSums(Xi %*% mod$Vp * Xi)^.5
    lD[[i]] <- list(deriv = df, se.deriv = df.sd)
  }
  class(lD) <- "Deriv"
  lD$gamModel <- mod
  lD$eps <- eps
  lD$eval <- newD - eps
  lD ##return
}

confint.Deriv <- function(object, term, alpha = 0.05, ...) {
  l <- length(object) - 3
  term.labs <- names(object[seq_len(l)])
  if(missing(term)) {
    term <- term.labs
  } else { ## how many attempts to get this right!?!?
    ##term <- match(term, term.labs)
    ##term <- term[match(term, term.labs)]
    term <- term.labs[match(term, term.labs)]
  }
  if(any(miss <- is.na(term)))
    stop(paste("'term'", term[miss], "not a valid model term."))
  res <- vector(mode = "list", length = length(term))
  names(res) <- term
  residual.df <- df.residual(object$gamModel)
  tVal <- qt(1 - (alpha/2), residual.df)
  ##for(i in term.labs[term]) {
  for(i in term) {
    upr <- object[[i]]$deriv + tVal * object[[i]]$se.deriv
    lwr <- object[[i]]$deriv - tVal * object[[i]]$se.deriv
    res[[i]] <- list(upper = drop(upr), lower = drop(lwr))
  }
  res$alpha = alpha
  res
}

signifD <- function(x, d, upper, lower, eval = 0) {
  miss <- upper > eval & lower < eval
  incr <- decr <- x
  want <- d > eval
  incr[!want | miss] <- NA
  want <- d < eval
  decr[!want | miss] <- NA
  list(incr = incr, decr = decr)
}

plot.Deriv <- function(x, alpha = 0.05, polygon = TRUE,
                       sizer = FALSE, term,
                       eval = 0, lwd = 3,
                       col = "lightgrey", border = col,
                       ylab, xlab, main, ...) {
  l <- length(x) - 3
  ## get terms and check specified (if any) are in model
  term.labs <- names(x[seq_len(l)])
  if(missing(term)) {
    term <- term.labs
  } else {
    term <- term.labs[match(term, term.labs)]
  }
  if(any(miss <- is.na(term)))
    stop(paste("'term'", term[miss], "not a valid model term."))
  if(all(miss))
    stop("All terms in 'term' not found in model.")
  l <- sum(!miss)
  nplt <- n2mfrow(l)
  tVal <- qt(1 - (alpha/2), df.residual(x$gamModel))
  if(missing(ylab))
    ylab <- expression(italic(hat(f)*"'"*(x)))
  if(missing(xlab)) {
    xlab <- attr(terms(x$gamModel), "term.labels")
    names(xlab) <- xlab
  }
  if (missing(main)) {
    main <- term
    names(main) <- term
  }
  ## compute confidence interval
  CI <- confint(x, term = term)
  ## plots
  layout(matrix(seq_len(l), nrow = nplt[1], ncol = nplt[2]))
  for(i in term) {
    upr <- CI[[i]]$upper
    lwr <- CI[[i]]$lower
    ylim <- range(upr, lwr)
    plot(x$eval[,i], x[[i]]$deriv, type = "n",
         ylim = ylim, ylab = ylab, xlab = xlab[i], main = main[i], ...)
    if(isTRUE(polygon)) {
      polygon(c(x$eval[,i], rev(x$eval[,i])),
              c(upr, rev(lwr)), col = col, border = border)
    } else {
      lines(x$eval[,i], upr, lty = "dashed")
      lines(x$eval[,i], lwr, lty = "dashed")
    }
    abline(h = 0, ...)
    if(isTRUE(sizer)) {
      lines(x$eval[,i], x[[i]]$deriv, lwd = 1)
      S <- signifD(x[[i]]$deriv, x[[i]]$deriv, upr, lwr,
                   eval = eval)
      lines(x$eval[,i], S$incr, lwd = lwd, col = "blue")
      lines(x$eval[,i], S$decr, lwd = lwd, col = "red")
    } else {
      lines(x$eval[,i], x[[i]]$deriv, lwd = 2)
    }
  }
  layout(1)
  invisible(x)
}

m2.d <- Deriv(fit_gam)
m2.dci <- confint.Deriv(m2.d, term = Term)
m2.dsig <- signifD(pdat$p2, d = m2.d[[Term]]$deriv,
                   m2.dci[[Term]]$upper, m2.dci[[Term]]$lower)

# Generate a plot of temperatures at 0, 24, 48, and 72 hours
ggplot(data = filter(predicts, NewTime %in% c(0, 24, 48, 72)), 
       aes(x = NewTime, y = fit)) + 
  facet_grid(Gender ~ .) + 
  geom_line() + 
  geom_errorbar(aes(ymin = lower, ymax = upper), size = 0.5, width = 2) + 
  labs(x = "Time from Bromocriptine (in Hours)",
       y = expression("Temperature " ( degree~F))) + 
  scale_x_continuous(breaks = c(0, 24, 48, 72))




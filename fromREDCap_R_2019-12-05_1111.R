#Clear existing data and graphics
rm(list = ls())
graphics.off()
#Load Hmisc library
library(Hmisc)
#Read Data
data = read.csv('fromREDCap_DATA_2019-12-05_1111.csv')
#Setting Labels

label(data$record_id) = "Record ID"
label(data$redcap_event_name) = "Event Name"
label(data$visit_date) = "Visit Date"
label(data$randomization) = "Randomization"
label(data$sex) = "Sex"
label(data$age) = "Age"
label(data$social_connectedness) = "Social Connectedness"
label(data$comments) = "Comments"
                 
#Setting Factors(will create new variable for factors)
data$redcap_event_name.factor = factor(data$redcap_event_name, levels = c("baseline", "followup"))
data$randomization.factor = factor(data$randomization,levels = c("0", "1"))
data$sex.factor = factor(data$sex,levels = c("0", "1", "666", "999"))
data$social_connectedness.factor = factor(data$social_connectedness, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "666", "999"))

levels(data$redcap_event_name.factor) = c("Baseline","Follow-up")
levels(data$randomization.factor) = c("Control","Treatment")
levels(data$sex.factor) = c("Male","Female","Ambiguous","Missing")
levels(data$social_connectedness) = c("0", "1", "2", "3", "4", "5", "6", "7", "Ambiguous","Missing")

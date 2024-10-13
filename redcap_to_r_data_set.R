##############
# Libraries  #
##############
package_loader <- function(x, ...) {
    if (x %in% rownames(installed.packages()) == FALSE) install.packages(x)
    library(x, ...)
}

packages <- c("Hmisc", "lubridate")

invisible(sapply(X = packages, FUN = package_loader, character.only = TRUE))

####################
# CSV Data Setup   #
####################
set.seed(123)

d <- matrix(nrow = 50, ncol = 8)
d <- data.frame(d)
colnames(d) <- c("record_id", "redcap_event_name", "visit_date", "randomization", "sex", "age", "social_connectedness", "comments")

d$record_id <- rep(c(1:(dim(d)[1]/2)), 2)
d <- d[order(d$record_id), ]

d$redcap_event_name <- rep(c("baseline", "followup"), dim(d)[1]/2)

d$visit_date <- as.Date(d$visit_date, origin = "1970-01-01")
d[which(d$redcap_event_name == "baseline"), "visit_date"] <- sample(seq(as.Date('2015-06-12', format = "%Y-%m-%d", origin = "1970-01-01"), as.Date('2016-10-03', format = "%Y-%m-%d", origin = "1970-01-01"), by = "day"), dim(d)[1]/2)
d[which(d$redcap_event_name == "followup"), "visit_date"] <- sample(seq(as.Date('2015-12-22', format = "%Y-%m-%d", origin = "1970-01-01"), as.Date('2018-05-05', format = "%Y-%m-%d", origin = "1970-01-01"), by = "day"), dim(d)[1]/2)

negInd <- which((d[which(d$redcap_event_name == "followup"), "visit_date"] - d[which(d$redcap_event_name == "baseline"), "visit_date"]) <= 0)

for (i in 1:length(unique(negInd))) {
    b <- d[which(d$record_id == negInd[i]), "visit_date"][1]
    d[which(d$record_id == negInd[i]), "visit_date"][2] <- b + days(1)
}

d[which(d$redcap_event_name == "baseline"), "randomization"] <- sample(x = c(0, 1), size = dim(d)[1]/2, prob = rep(1/2, 2), replace = TRUE)

d[which(d$redcap_event_name == "baseline"), "sex"] <- sample(x = c(1, 2), size = dim(d)[1]/2, prob = rep(1/2, 2), replace = TRUE)
d[which(d$redcap_event_name == "baseline" & (d$record %in% sample(x = unique(d$record_id), size = ceiling((dim(d)[1]/2)*0.1), replace = FALSE))), "sex"] <- 999

d[which(d$redcap_event_name == "baseline"), "age"] <- floor(rnorm(n = dim(d)[1]/2, mean = 40, sd = 10))
d[which(d$redcap_event_name == "baseline" & (d$record %in% sample(x = unique(d$record_id), size = ceiling((dim(d)[1]/2)*0.1), replace = FALSE))), "age"] <- 666
d[which(d$redcap_event_name == "baseline" & (d$record %in% sample(x = unique(d$record_id), size = ceiling((dim(d)[1]/2)*0.1), replace = FALSE))), "age"] <- 999

d[which(d$redcap_event_name == "baseline"), "social_connectedness"] <- rpois(25, 2)
d[which(d$redcap_event_name == "baseline" & (d$social_connectedness > 7)), "social_connectedness"] <- 7
d[which(d$redcap_event_name == "baseline" & (d$record %in% sample(x = unique(d$record_id), size = ceiling((dim(d)[1]/2)*0.1), replace = FALSE))), "social_connectedness"] <- 666
d[which(d$redcap_event_name == "baseline" & (d$record %in% sample(x = unique(d$record_id), size = ceiling((dim(d)[1]/2)*0.1), replace = FALSE))), "social_connectedness"] <- 999

d[which(d$redcap_event_name == "followup"), "social_connectedness"] <- rpois(25, 4)
d[which(d$redcap_event_name == "followup" & (d$social_connectedness > 7)), "social_connectedness"] <- 7
d[which(d$redcap_event_name == "followup" & (d$record %in% sample(x = unique(d$record_id), size = ceiling((dim(d)[1]/2)*0.1), replace = FALSE))), "social_connectedness"] <- 666
d[which(d$redcap_event_name == "followup" & (d$record %in% sample(x = unique(d$record_id), size = ceiling((dim(d)[1]/2)*0.1), replace = FALSE))), "social_connectedness"] <- 999

d$comments <- sample(x = c(" ", "They're inside the form factor, use the mobile RX card to attach their capacitor!",  "My PNG microchip is down, our only choice is to shut down and connect the digital panel!", "The thing is, I can't complete my model because my grandma drank it, probably silently."), size = 50, replace = TRUE)

write.csv(d, file = "fromREDCap_DATA_2019-12-05_1111.csv", row.names = FALSE)
data <- read.csv(file = "fromREDCap_DATA_2019-12-05_1111.csv", stringsAsFactors = FALSE)

#################
# R File Setup  #
#################

redcapLines <- c("#Clear existing data and graphics
rm(list=ls())
graphics.off()
#Load Hmisc library
library(Hmisc)
#Read Data
data=read.csv('fromREDCap_DATA_2019-12-05_1111.csv')
#Setting Labels

label(data$record_id)=\"Record ID\"
label(data$redcap_event_name)=\"Event Name\"
label(data$visit_date) =\"Visit Date\"
label(data$randomization)=\"Randomization\"
label(data$sex)=\"Sex\"
label(data$age)=\"Age\"
label(data$social_connectedness)=\"Social Connectedness\"
label(data$comments)=\"Comments\"
                 
#Setting Factors(will create new variable for factors)
data$redcap_event_name.factor = factor(data$redcap_event_name,levels=c(\"baseline\", \"followup\"))
data$randomization.factor = factor(data$randomization,levels=c(\"0\", \"1\"))
data$sex.factor = factor(data$sex,levels=c(\"0\", \"1\", \"666\", \"999\"))
data$social_connectedness.factor = factor(data$social_connectedness,levels=c(\"0\", \"1\", \"2\", \"3\", \"4\", \"5\", \"6\", \"7\", \"666\", \"999\"))

levels(data$redcap_event_name.factor)=c(\"Baseline\",\"Follow-up\")
levels(data$randomization.factor)=c(\"Control\",\"Treatment\")
levels(data$sex.factor)=c(\"Male\",\"Female\",\"Ambiguous\",\"Missing\")
levels(data$social_connectedness)=c(\"0\", \"1\", \"2\", \"3\", \"4\", \"5\", \"6\", \"7\", \"Ambiguous\",\"Missing\")")

writeLines(text = redcapLines, con = "fromREDCap_R_2019-12-05_1111.R")


# Setting working directory
# ======================================================================
filepath <- c("/Users/nkaveti/Documents/Kaggle/AV_Xtreme_ML_Hack/")
setwd(filepath)

# Loading required packages
# ======================================================================
library(forecast)
library(data.table)
library(plyr)
library(dplyr)
library(data.table)
library(lubridate)

# Reading training and test data
# ======================================================================
dt_train_contacts <- fread("Train/Contacts_Pre_2017.csv",header = TRUE,na.strings = c("","-"))
dt_train_resolution <- fread("Train/Resolution_Pre_2017.csv",header = TRUE,na.strings = c("","-"))

dt_test_contacts <- fread("Test/Contacts2017.csv",header = TRUE,na.strings = c("","-"))
dt_test_resolution <- fread("Test/Resolution2017.csv",header = TRUE,na.strings = c("","-"))

# Changing data type
# =================================================================
str(dt_train_contacts)
dt_train_contacts[,START.DATE := as.Date(START.DATE)] 
dt_train_contacts[,END.DATE := as.Date(END.DATE)]

str(dt_test_contacts)
dt_test_contacts[,Date := as.Date(Date)]

str(dt_train_resolution)
dt_train_resolution[,Date := as.Date(Date)]
dt_train_resolution[,End_Date := as.Date(End_Date)]

str(dt_test_resolution)
dt_test_resolution[,Date := as.Date(Date)]

# Rolling training data by date
# ====================================================================
dt_train_contacts <- dt_train_contacts[,.(Contacts = sum(Contacts,na.rm = TRUE)), by = c("START.DATE","CONTACT.TYPE")]
dt_train_resolution <- dt_train_resolution[,.(Resolution = sum(Resolution,na.rm = TRUE)), by = c("Date","Category","Subject")]

# Using only last two years of data
# ====================================================================
dt_train_contacts <- subset(dt_train_contacts,year(START.DATE) >= 2015)
names(dt_train_contacts)[1] <- "Date"
dt_train_resolution <- subset(dt_train_resolution,year(Date) >= 2015)


# Adding missing weeks
# ====================================================================
contact_type <- unique(dt_train_contacts$CONTACT.TYPE)

dt_train_contacts_full <- data.table(NULL)
for(type in contact_type)
{
  dt_type <- subset(dt_train_contacts,CONTACT.TYPE == type)
  min_date <- min(dt_type$Date)
  max_date <- max(dt_type$Date)
  dt_temp <- data.table(Date = seq(min_date,max_date,by = 1),CONTACT.TYPE = type)
  dt_temp <- join(x = dt_temp,y = dt_type,by = c("Date","CONTACT.TYPE"),type = "left")
  dt_train_contacts_full <- rbindlist(list(dt_train_contacts_full,dt_temp))
}
dt_train_contacts_full <- dt_train_contacts_full[order(CONTACT.TYPE,Date),]

category_type <- unique(dt_train_resolution$Category)
subject_type <- unique(dt_train_resolution$Subject)

dt_train_resolution_full <- data.table(NULL)
for(type in category_type)
{
  for( sub in subject_type)
  {
  dt_type <- subset(dt_train_resolution,Category == type & Subject == sub)
  if(nrow(dt_type) == 0)
  {
    next
  }
  min_date <- min(dt_type$Date)
  max_date <- max(dt_type$Date)
  dt_temp <- data.table(Date = seq(min_date,max_date,by = 1),Category = type,Subject = sub)
  dt_temp <- join(x = dt_temp,y = dt_type,by = c("Date","Category","Subject"),type = "left")
  dt_train_resolution_full <- rbindlist(list(dt_train_resolution_full,dt_temp))
  }
}

dt_train_resolution_full <- dt_train_resolution_full[order(Category,Subject,Date),]


# Feature Engineering
# ===================================================================
dt_train_contacts_full[,Year := year(Date)]
dt_train_contacts_full[,Quarter := quarter(Date)]
dt_train_contacts_full[,Month := month(Date)]
dt_train_contacts_full[,DayOfWeek := wday(Date)]
dt_train_contacts_full[,Week := week(Date)]

dt_test_contacts[,Year := year(Date)]
dt_test_contacts[,Quarter := quarter(Date)]
dt_test_contacts[,Month := month(Date)]
dt_test_contacts[,DayOfWeek := wday(Date)]
dt_test_contacts[,Week := week(Date)]


dt_train_resolution_full[,Year := year(Date)]
dt_train_resolution_full[,Quarter := quarter(Date)]
dt_train_resolution_full[,Month := month(Date)]
dt_train_resolution_full[,DayOfWeek := wday(Date)]
dt_train_resolution_full[,Week := week(Date)]


dt_test_resolution[,Year := year(Date)]
dt_test_resolution[,Quarter := quarter(Date)]
dt_test_resolution[,Month := month(Date)]
dt_test_resolution[,DayOfWeek := wday(Date)]
dt_test_resolution[,Week := week(Date)]

# Missing value imputation
# =================================================================
dt_train_contacts_full$Contacts[is.na(dt_train_contacts_full$Contacts)] <- 0
dt_train_resolution_full$Resolution[is.na(dt_train_resolution_full$Resolution)] <- 0


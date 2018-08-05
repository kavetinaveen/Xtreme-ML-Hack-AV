####################################### Title: AV - Xtreme - ML - Hack #######################################

# Setting working directory
###############################
filepath <- c("/Users/nkaveti/Documents/Kaggle/AV_Xtreme_ML_Hack/")
setwd(filepath)

# Function to install/load required libraries
##################################################
inst_pack <- function(req_pack){
  new_pack <- req_pack[!(req_pack %in% installed.packages())]
  if(length(new_pack)){
    cat("Installing the packages: ", new_pack, "\n")
    install.packages(new_pack, dependencies = TRUE)
  }
  sapply(req_pack, require, character.only = TRUE)
}

Required_packages <- c("dplyr", "forecast", "data.table", "ggplot2")

inst_pack(Required_packages)

# Reading data
################
train_contacts <- fread("Train/Contacts_Pre_2017.csv") 
train_reso <- fread("Train/Resolution_Pre_2017.csv")

test_contacts <- fread("Test/Contacts2017.csv")
test_reso <- fread("Test/Resolution2017.csv")

contracts_end <- fread("Train/Contracts_End.csv")
contracts_new <- fread("Train/Contracts_New.csv")

# Preprocessing of data
###########################

# Coverting data variable to as.Date type
train_contacts[, START.DATE := as.Date(START.DATE)]
train_contacts[, END.DATE := as.Date(END.DATE)]

train_reso[, Date := as.Date(Date)]
train_reso[, End_Date := as.Date(End_Date)]

test_contacts[, Date := as.Date(Date)]
test_reso[, Date := as.Date(Date)]

# Rolling train data into day level
train_contacts_day_level <- train_contacts[, .(Contacts = sum(Contacts)), by = .(START.DATE, CONTACT.TYPE)]
train_reso_day_level <- train_reso[, .(Resolution = sum(Resolution)), by = .(Date, Category, Subject)]

# Feature Engineering
#########################
train_contacts_day_level$day <- weekdays(train_contacts_day_level$START.DATE)
train_reso_day_level$day <- weekdays(train_reso_day_level$Date)

train_contacts_day_level <- train_contacts_day_level[year(train_contacts_day_level$START.DATE) > 2015, ]
train_reso_day_level <- train_reso_day_level[year(train_reso_day_level$Date) > 2015, ]

cat_day_contacts <- train_contacts_day_level[, .(Contacts = mean(Contacts)), by = .(CONTACT.TYPE, day)]
cat_day_contacts[is.na(cat_day_contacts)] <- 0

gg_contacts_day_cat <- ggplot(data = cat_day_contacts) + geom_bar(aes(x = CONTACT.TYPE, y = Contacts, fill = day), stat = "identity") + labs(x = "Contact Type", y  = "# Contacts", title = "Contact type, day of week Vs # Contacts") + theme(axis.text.x = element_text(size = 10, angle = 45)) + scale_x_discrete(label=abbreviate)
plot(gg_contacts_day_cat)

cat_day_sub_reso <- train_reso_day_level[, .(Resolution = median(Resolution)), by = .(Category, Subject, day)]
cat_day_sub_reso[is.na(cat_day_sub_reso)] <- 0

# Group by mean solution
#########################################
test_contacts$day <- weekdays(test_contacts$Date)
test_reso$day <- weekdays(test_reso$Date)

# test_contacts$month <- month(test_contacts$Date)
# test_reso$month <- month(test_reso$Date)

test_contacts2 <- left_join(test_contacts, cat_day_contacts, by = c("CONTACT.TYPE", "day"))
test_contacts2$Contacts.x <- NULL
colnames(test_contacts2)[ncol(test_contacts2)] <- "Contacts"
# test_contacts2$Contacts[is.na(test_contacts2$Contacts) & test_contacts2$CONTACT.TYPE %in% c("Installation Report - Input", "Tweet - Input")] <- 1
test_contacts2$Contacts[is.na(test_contacts2$Contacts)] <- 0

test_reso2 <- left_join(test_reso, cat_day_sub_reso, by = c("Category", "Subject", "day"))
test_reso2$Resolution.x <- NULL
colnames(test_reso2)[ncol(test_reso2)] <- "Resolution"
test_reso2$Resolution[is.na(test_reso2$Resolution)] <- 0

contacts_sol <- data.table(ID = test_contacts2$ID, Contacts = round(test_contacts2$Contacts))
reso_sol <- data.table(ID = test_reso2$ID, Resolution = round(test_reso2$Resolution))

write.csv(contacts_sol, file = "Contacts.csv", row.names = FALSE)
write.csv(reso_sol, file = "Resolution.csv", row.names = FALSE)
zip("Submission.zip", c("Contacts.csv", "Resolution.csv"))

# Implementing Arima 
#######################

# Function to apply arima with optimal fourier
########################################################
arima_fun <- function(x, datax, datay, h=4) {
  # Convert the data to a time series object incorporating the non integer seasonal pattern
  train.ts <- ts(x, frequency = 1)
  # Initializing the bestfit AICc value to infinity.
  bestfit <- list(aicc=Inf)
  opt_k <- 1 
  for(i in 1:1) {
    # Fit Arima model with Fourier componets as regressors for particular K.
    
    # fit <- try(auto.arima(train.ts, xreg=cbind(fourier(train.ts, K=i), datax), seasonal=FALSE), 
    #            silent = TRUE)
    
    # fit <- try(auto.arima(train.ts, xreg = fourier(train.ts, K=i), seasonal=FALSE), 
    #            silent = TRUE)  
    
    fit <- try(auto.arima(train.ts, seasonal=TRUE),
               silent = TRUE)
    
    # Compare if AICc value is less than that of the previous model. If so, replace the 
    # previous model with the new model and optimal k to the current i value. Exit otherwise.
    if(getElement(fit,"aicc") < getElement(bestfit,"aicc")) {
      bestfit <- fit
      opt_k <- i
    }
    else 
      break;
  }
  cat("Optimal K value: ", opt_k, "\n")
  # Forecast the model with the best fit model
  # fc <- forecast(bestfit, xreg = cbind(fourier(train.ts, K=opt_k, h), datay))
  
  fc <- forecast(bestfit, h)
  
  # fc <- predict(bestfit, n.ahead = h, xreg= cbind(fourier(train.ts, K=opt_k), datay))
  return(fc)
} # END arima_fun

forecast_category_dow <- function(contact_type, train_dat = train_contacts_day_level, test_dat = test_contacts2, compare_with_best_sol = TRUE){
  dat_type <- train_dat[train_dat$CONTACT.TYPE == contact_type, ]
  dat_type <- dat_type[order(dat_type$START.DATE), ]
  
  temp <- data.table(START.DATE = seq.Date(from = min(dat_type$START.DATE), to = max(dat_type$START.DATE), by = 1))
  dat_type <- left_join(temp, dat_type, by = "START.DATE")
  
  cat("Number of missing dates: ", sum(is.na(dat_type$Contacts)), "\n")
  
  result_train <- data.table(NULL)
  result_test <- data.table(NULL)
  validation <- data.table(NULL)
  dow <- names(table(dat_type$day))
  for(i in dow){
    dat_cont_type_dow <- dat_type[dat_type$day == i, ]
    if(nrow(dat_cont_type_dow) > 20){
      test_cont <- test_dat[test_dat$CONTACT.TYPE == contact_type & test_dat$day == i, ]
      arma <- arima_fun(x = dat_cont_type_dow$Contacts, h = nrow(test_cont))
      test_cont$Contacts <- arma$mean
      result_test <- rbind(result_test, test_cont)
      result_train <- rbind(result_train, data.table(dat_cont_type_dow, contacts_fitted = arma$fitted))
      
      if(compare_with_best_sol){
        arma <- arima_fun(dat_cont_type_dow$Contacts[1:(nrow(dat_cont_type_dow) - 20)], h = 20)
        best_sol_rmse <- sd(dat_cont_type_dow$Contacts[(nrow(dat_cont_type_dow) - 20 + 1):nrow(dat_cont_type_dow)])
        new_sol_rmse <- sqrt(mean((arma$mean - dat_cont_type_dow$Contacts[(nrow(dat_cont_type_dow) - 20 + 1):nrow(dat_cont_type_dow)])^2))
        cat("Best Solution RMSE: ", best_sol_rmse, " | New Solution RMSE: ", new_sol_rmse, "\n")
        validation <- rbind(validation, data.table(contact_type, i, best_sol_rmse, new_sol_rmse))
      }
    }
  }
  return(list(res_tr = result_train, res_te = result_test, val = validation))
}


for(i in unique(train_contacts_day_level$CONTACT.TYPE)){
  temp <- train_contacts_day_level[train_contacts_day_level$CONTACT.TYPE == i, ]
  temp <- temp[order(temp$START.DATE), ]
  temp2 <- data.table(START.DATE = seq.Date(from = min(temp$START.DATE), to = max(temp$START.DATE), by = 1))
  temp2 <- left_join(temp2, temp, by = "START.DATE")
  cat("Number of missing dates: ", sum(is.na(temp2$Contacts)), "\n")
  temp2$Contacts[is.na(temp2$Contacts)] <- 0
  arma <- arima_fun(temp2$Contacts[1:(nrow(temp2) - 20)], h = 20)
  rmse <- sqrt(mean((arma$mean - temp2$Contacts[(nrow(temp2) - 20 + 1): nrow(temp2)])^2))
  cat("RMSE for contact type ", i, " is: ", rmse, "\n")
}


# No satuarday and sunday for visit
# No satuarday and sunday for mail input

# remve_contact_type <- c("Tweet - Input", "Installation Report - Input")
# imp_contact_type <- setdiff(unique(train_contacts_day_level$CONTACT.TYPE), remve_contact_type)

imp_contact_type <- unique(train_contacts_day_level$CONTACT.TYPE)

result_forecast_train <- data.table(NULL)
result_forecast_test <- data.table(NULL)
validaton_forecast <- data.table(NULL)
for(i in imp_contact_type){
  run_forecast <- forecast_category_dow(i, train_contacts_day_level, test_contacts2, compare_with_best_sol = TRUE)
  result_forecast_train <- rbind(result_forecast_train, run_forecast$res_tr)
  result_forecast_test <- rbind(result_forecast_test, run_forecast$res_te)
  validaton_forecast <- rbind(validaton_forecast, run_forecast$val)
}

test_contacts3 <- left_join(test_contacts2, result_forecast_test, by = c("ID"))
test_contacts3$Contacts.y <- ifelse(is.na(test_contacts3$Contacts.y), test_contacts3$Contacts.x, test_contacts3$Contacts.y)

contacts_sol <- data.table(ID = test_contacts3$ID, Contacts = test_contacts3$Contacts.y)
write.csv(contacts_sol, file = "Contacts.csv", row.names = FALSE)
zip("Submission_Forecast.zip", c("Contacts.csv", "Resolution.csv"))





forecast_category_dow_reso <- function(category, subject, train_dat, test_dat, compare_with_best_sol = TRUE){
  dat_type <- train_dat[train_dat$Category == category & train_dat$Subject == subject, ]
  cat(nrow(dat_type), "\n")
  dat_type <- dat_type[order(dat_type$Date), ]
  
  temp <- data.table(Date = seq.Date(from = min(dat_type$Date), to = max(dat_type$Date), by = 1))
  dat_type <- left_join(temp, dat_type, by = "Date")
  
  cat("Number of missing dates: ", sum(is.na(dat_type$Resolution)), "\n")
  
  result <- data.table(NULL)
  validation <- data.table(NULL)
  dow <- names(table(dat_type$day))
  for(i in dow){
    dat_cont_type_dow <- dat_type[dat_type$day == i, ]
    if(nrow(dat_cont_type_dow) > 15){
      test_cont <- test_dat[test_dat$Category == category & test_dat$day == i & test_dat$Subject == subject, ]
      arma <- arima_fun(dat_cont_type_dow$Resolution, h = nrow(test_cont))
      test_cont$Contacts <- arma$mean
      result <- rbind(result, test_cont)
      
      if(compare_with_best_sol){
        arma <- arima_fun(dat_cont_type_dow$Resolution[1:(nrow(dat_cont_type_dow) - 20)], h = 20)
        best_sol_rmse <- sd(dat_cont_type_dow$Resolution[(nrow(dat_cont_type_dow) - 20 + 1):nrow(dat_cont_type_dow)])
        new_sol_rmse <- sqrt(mean((arma$mean - dat_cont_type_dow$Resolution[(nrow(dat_cont_type_dow) - 20 + 1):nrow(dat_cont_type_dow)])^2))
        cat("Best Solution RMSE: ", best_sol_rmse, " | New Solution RMSE: ", new_sol_rmse, "\n")
        validation <- rbind(validation, data.table(category, subject, i, best_sol_rmse, new_sol_rmse))
      }
      
    }
  }
  return(list(res = result, val = validation))
}


result_forecast_reso <- data.table(NULL)
validaton_forecast_reso <- data.table(NULL)
for(i in unique(train_reso_day_level$Category)){
  for(j in unique(train_reso_day_level$Subject)){
    run_forecast <- forecast_category_dow_reso(i, j, train_reso_day_level, test_reso2)
    result_forecast_reso <- rbind(result_forecast_reso, run_forecast$res)
    validaton_forecast_reso <- rbind(validaton_forecast_reso, run_forecast$val)
  }
}






temp1 <-  left_join(train_contacts_day_level ,cat_day_contacts, by = c("CONTACT.TYPE", "day"))
temp1$error <- temp1$Contacts.y - temp1$Contacts.x

temp2 <- temp1[, .(Abs_Error = sum(abs(error))), .(CONTACT.TYPE, day)]
temp2 <- temp2[order(temp2$Abs_Error), ]

for(i in unique(temp1$CONTACT.TYPE)){
  plot(temp1$error[temp1$CONTACT.TYPE == i], main = i)
}


temp3 <- left_join(train_reso_day_level, cat_day_sub_reso, by = c("Category", "Subject", "day"))
temp3$error <- temp3$Resolution.y - temp3$Resolution.x

temp4 <- temp3[, .(Abs_Error = sum(abs(error))), by = .(Category, Subject, day)]
temp4 <- temp4[order(temp4$Abs_Error), ]


write.csv(temp2, "Abs_Error_Contacts.csv", row.names = FALSE)
write.csv(temp4, "Abs_Error_Resolution.csv", row.names = FALSE)


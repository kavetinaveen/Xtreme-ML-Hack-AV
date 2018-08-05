

cat_day_contacts_v2 <- dt_train_contacts_full[, .(Contacts = mean(Contacts)), by = .(CONTACT.TYPE, DayOfWeek)]
cat_day_contacts_v2[is.na(cat_day_contacts_v2)] <- 0


# Function to apply arima with optimal fourier
# =================================================
arima_fun <- function(x, datax = NULL, datay = NULL, h=4) {
  train.ts <- ts(x, frequency=7)
  bestfit <- list(aicc=Inf)
  opt_k <- 1 
  for(i in 1:1) {
    if(is.null(datax)){
      # fit <- try(auto.arima(train.ts, xreg = fourier(train.ts, K=i), seasonal=FALSE), 
      #            silent = TRUE)
      # z <- fourier(ts(x, frequency=7), K=3)
      fit <- ets(x)
    }else{
      # fit <- try(auto.arima(train.ts, xreg = cbind(datax, fourier(train.ts, K=i)), seasonal=FALSE), 
                 # silent = TRUE)
      
      fit <- ets(x)
    }
    
     if(getElement(fit,"aicc") < getElement(bestfit,"aicc")) {
      bestfit <- fit
      opt_k <- i
    }
    else 
      break;
  }
  if(is.null(datay)){
    # fc <- forecast(bestfit, xreg = fourier(train.ts, K=opt_k, h))
    fc <- forecast(bestfit,  h)
  }else{
    # fc <- forecast(bestfit, xreg = cbind(datay, fourier(train.ts, K=opt_k, h)))
    fc <- forecast(bestfit, h)
  }
  return(fc)
}

# Forecast contact type level
# ================================
forecast_category_dow <- function(contact_type, train_dat = dt_train_contacts_full, test_dat = dt_test_contacts, compare_with_best_sol = TRUE, regressors = c("Month", "DayOfWeek")){
  dat_type <- train_dat[train_dat$CONTACT.TYPE == contact_type, ]
  dat_type <- left_join(dat_type, cat_day_contacts_v2, by = c("CONTACT.TYPE", "DayOfWeek"))
  dat_type <- dat_type[order(dat_type$Date), ]
  # temp <- data.table(Date = seq.Date(from = min(dat_type$Date), to = max(dat_type$Date), by = 1))
  # dat_type <- left_join(temp, dat_type, by = "Date")
  
  test_cont <- test_dat[test_dat$CONTACT.TYPE == contact_type, ]
  test_cont <- left_join(test_cont, contacts_sol, by = "ID")
  arma <- arima_fun(x = dat_type$Contacts.x, datax = dat_type[, regressors, with = F], datay = test_cont[, regressors, with = F], h = nrow(test_cont))
  test_cont$Contacts <- arma$mean
  result_test <- test_cont
  
  if(compare_with_best_sol){
    arma_valid <- arima_fun(dat_type$Contacts.x[1:(nrow(dat_type) - 20)], datax =  dat_type[1:(nrow(dat_type) - 20), regressors, with = F], datay = dat_type[(nrow(dat_type) - 20 + 1):nrow(dat_type), regressors, with = F], h = 20)
    
    best_sol_rmse <- sqrt(mean((dat_type$Contacts.x[(nrow(dat_type) - 20 + 1):nrow(dat_type)] - dat_type$Contacts.y[(nrow(dat_type) - 20 + 1):nrow(dat_type)])^2))
    result_valid <- data.table(Actuals = dat_type$Contacts.x[(nrow(dat_type) - 20 + 1):nrow(dat_type)], Forecasted = arma_valid$mean)
    new_sol_rmse <- sqrt(mean((arma_valid$mean - dat_type$Contacts.x[(nrow(dat_type) - 20 + 1):nrow(dat_type)])^2))
    cat("Best Solution RMSE: ", best_sol_rmse, " | New Solution RMSE: ", new_sol_rmse, "\n")
    validation <- data.table(contact_type, best_sol_rmse, new_sol_rmse)
  }
  return(list(res_te = result_test, res_valid = result_valid, val = validation))
}


# remve_contact_type <- c("Tweet - Input", "Installation Report - Input")
# imp_contact_type <- setdiff(unique(train_contacts_day_level$CONTACT.TYPE), remve_contact_type)
imp_contact_type <- unique(train_contacts_day_level$CONTACT.TYPE)

result_forecast_valid <- data.table(NULL)
result_forecast_test <- data.table(NULL)
validaton_forecast <- data.table(NULL)
for(i in imp_contact_type){
  run_forecast <- forecast_category_dow(i)
  result_forecast_valid <- rbind(result_forecast_valid, run_forecast$res_valid)
  result_forecast_test <- rbind(result_forecast_test, run_forecast$res_te)
  validaton_forecast <- rbind(validaton_forecast, run_forecast$val)
}


result_forecast_test$Contacts <- as.numeric(result_forecast_test$Contacts)
result_forecast_test$Contacts[result_forecast_test$Contacts < 0] <- 0 
# result_forecast_test$Contacts <- ifelse(result_forecast_test$Contacts < 0, result_forecast_test$Contacts.y, result_forecast_test$Contacts)



cont_sol <- data.table(ID = result_forecast_test$ID, Contacts = round(result_forecast_test$Contacts))

cont_sol <- left_join(cont_sol, contacts_sol, by = "ID")
cont_sol$Contacts <- ifelse(cont_sol$Contacts.x < 0, cont_sol$Contacts.y, cont_sol$Contacts.x)

cont_sol$Contacts <- round(cont_sol$Contacts)

cont_sol$Contacts[cont_sol$Contacts < 0] <- 0
write.csv(cont_sol[, c("ID", "Contacts")], "Contacts.csv", row.names = FALSE)
zip("Submission_Forecast.zip", c("Contacts.csv", "Resolution.csv"))




############ Resolution

forecast_category_dow_reso <- function(category, subject, train_dat = dt_train_resolution_full, test_dat = dt_test_resolution, compare_with_best_sol = TRUE, regressors = c("CONTACT.TYPE", "Year", "Quarter", "Month", "DayOfWeek", "Week")){
  # dat_type <- train_dat[train_dat$Category == category & train_dat$Subject == subject, ] 
  dat_type <- train_dat[train_dat$Category == category, ]
  # dat_type <- left_join(dat_type, cat_day_sub_reso, by = c("Category", "Subject"))
  dat_type <- dat_type[order(dat_type$Date), ]
  
  cat(nrow(dat_type), "\n")
  
  # temp <- data.table(Date = seq.Date(from = min(dat_type$Date), to = max(dat_type$Date), by = 1))
  # dat_type <- left_join(temp, dat_type, by = "Date")
  
  # test_cont <- test_dat[test_dat$Category == category & test_dat$Subject == subject, ]
  test_cont <- test_dat[test_dat$Category == category, ]
  test_cont <- left_join(test_cont, contacts_sol, by = "ID")
  arma <- arima_fun(dat_type$Resolution, h = nrow(test_cont))
  test_cont$Resolution <- arma$mean
  cat(nrow(test_cont), "\n")
  result_test <- test_cont
  validation <- NULL
  
  if(compare_with_best_sol){
    arma_valid <- arima_fun(dat_type$Resolution.x[1:(nrow(dat_type) - 20)], h = 20)
    
    # , datax =  dat_type[1:(nrow(dat_type) - 20), regressors], datay = dat_type[(nrow(dat_type) - 20 + 1):nrow(dat_type), regressors]
    
    best_sol_rmse <- sqrt(mean((dat_type$Resolution.x[(nrow(dat_type) - 20 + 1):nrow(dat_type)] - dat_type$Resolution.y[(nrow(dat_type) - 20 + 1):nrow(dat_type)])^2))
    result_valid <- data.table(Actuals = dat_type$Resolution.x[(nrow(dat_type) - 20 + 1):nrow(dat_type)], Forecasted = arma_valid$mean)
    new_sol_rmse <- sqrt(mean((arma_valid$mean - dat_type$Resolution.x[(nrow(dat_type) - 20 + 1):nrow(dat_type)])^2))
    cat("Best Solution RMSE: ", best_sol_rmse, " | New Solution RMSE: ", new_sol_rmse, "\n")
    validation <- data.table(contact_type, best_sol_rmse, new_sol_rmse)
  }
  return(list(res_te = result_test, res_valid = result_valid, val = validation))
}

categories <- unique(dt_train_resolution_full$Category)
subjects <- unique(dt_train_resolution_full$Subject)

result_forecast_valid_reso <- data.table(NULL)
result_forecast_test_reso <- data.table(NULL)
validaton_forecast_reso <- data.table(NULL)
tab <- table(dt_train_resolution_full$Category, dt_train_resolution_full$Subject)

for(i in categories){
  for(j in subjects){
    if(tab[i, j] > 20){
      cat("Category: ", i, " | Subject: ", j, "\n")
      run_forecast_reso <- forecast_category_dow_reso(i, j)
      result_forecast_valid_reso <- rbind(result_forecast_valid_reso, run_forecast_reso$res_valid)
      result_forecast_test_reso <- rbind(result_forecast_test_reso, run_forecast_reso$res_te)
      validaton_forecast_reso <- rbind(validaton_forecast_reso, run_forecast_reso$val)
    }
  }
}

for(i in categories){
  cat("Category: ", i, "\n")
  run_forecast_reso <- forecast_category_dow_reso(i, j)
  result_forecast_valid_reso <- rbind(result_forecast_valid_reso, run_forecast_reso$res_valid)
  result_forecast_test_reso <- rbind(result_forecast_test_reso, run_forecast_reso$res_te)
  validaton_forecast_reso <- rbind(validaton_forecast_reso, run_forecast_reso$val)
}


result_forecast_test_reso2 <- left_join(dt_test_resolution, result_forecast_test_reso, by = "ID")
result_forecast_test_reso2$Contacts[is.na(result_forecast_test_reso2$Contacts)] <- 0

result_reso <- data.table(ID = result_forecast_test_reso2$ID, Resolution = round(result_forecast_test_reso2$Contacts))

write.csv(result_reso, "Resolution.csv", row.names = FALSE)




plot_cat_sub <- function(category, subject){
  temp <- dt_train_resolution_full[dt_train_resolution_full$Category == category & dt_train_resolution_full$Subject == subject, ]
  temp$
}





mypackages = c("tidyverse","lubridate","dplyr","forecast")   # required packages
tmp = setdiff(mypackages, rownames(installed.packages()))  # packages need to be installed
if (length(tmp) > 0) install.packages(tmp)
lapply(mypackages, require, character.only = TRUE)
# 
# 
# # experiment with David  Thaler's implementation 
# 
# grouped.forecast <- function(train, test, fname, ...){
#   
#   
#   actuals <- NULL
#   FNAMES <- c('seasonal.naive',
#               'product',
#               'tslm.basic')
#   
#   if(fname %in% FNAMES){
#     f <- get(fname)
#   }else{
#     stop(fname,' not legal forecast option')
#   }
#   if('Weekly_Sales' %in% names(test)){
#     test$Weekly_Sales = actuals
#     test <- subset(test, select=-Weekly_Sales)
#   }
#   
#   test.dates <- unique(test$Date)
#   num.test.dates <- length(test.dates)
#   all.stores <- unique(test$Store)
#   num.stores <- length(all.stores)
#   test.depts <- unique(test$Dept)
#   
#   #reverse the depts so the grungiest data comes first
#   test.depts <- test.depts[length(test.depts):1]
#   forecast.frame <- data.frame(Date=rep(test.dates, num.stores),
#                                Store=rep(all.stores, each=num.test.dates))
#   pred <- test
#   pred$Weekly_Sales <- actuals
#   pred$Weekly_Sales <- 0
#   
#   train.dates <- unique(train$Date)
#   num.train.dates <- length(train.dates)
#   train.frame <- data.frame(Date=rep(train.dates, num.stores),
#                             Store=rep(all.stores, each=num.train.dates))
#   
#   for(d in test.depts){
#     print(paste('dept:', d))
#     tr.d <- train.frame
#     # This joins in Weekly_Sales but generates NA's. Resolve NA's 
#     # in the model because they are resolved differently in different models.
#     tr.d <- full_join(tr.d, train[train$Dept==d, c('Store','Date','Weekly_Sales')])
#     tr.d <- acast(tr.d, Date ~ Store)    
#     fc.d <- forecast.frame
#     fc.d$Weekly_Sales <- 0
#     fc.d <- acast(fc.d, Date ~ Store)
#     result <- f(tr.d, fc.d, ...)
#     # This has all Stores/Dates for this dept, but may have some that
#     # don't go into the submission.
#     result <- melt(result, value.var = 'Weekly_Sales', varnames = c('Date','Store'))
#     result$Date <- as.Date(result$Date)
#     pred.d.idx <- pred$Dept==d
#     #These are the Store-Date pairs in the submission for this dept
#     pred.d <- pred[pred.d.idx, c('Store', 'Date')]
#     pred.d <- left_join(pred.d, result, by = c('Store','Date'))
#     pred$Weekly_Pred[pred.d.idx] <- pred.d$Weekly_Sales
#   }
#   pred
# }
# seasonal.naive <- function(train, test){
#   # Computes seasonal naive forecasts
#   #
#   # args:
#   # train - A matrix of Weekly_Sales values from the training set of dimension
#   #         (number of weeeks in training data) x (number of stores)
#   # test - An all-zeros matrix of dimension:
#   #       (number of weeeks in training data) x (number of stores)
#   #       The forecasts are written in place of the zeros.
#   #
#   # returns:
#   #  the test(forecast) data frame with the forecasts filled in 
#   h <- nrow(test)
#   tr <- train[nrow(train) - (52:1) + 1,]
#   tr[is.na(tr)] <- 0
#   test[,2:ncol(test)]  <- tr[1:h,2:ncol(test)]
#   test
# }
# 
# product <- function(train, test){
#   # Computes forecasts with the product model. This model predicts the mean
#   # value by store times the mean value by week divided by the mean value
#   # over the department.
#   #
#   # args:
#   # train - A matrix of Weekly_Sales values from the training set of dimension
#   #         (number of weeeks in training data) x (number of stores)
#   # test - An all-zeros matrix of dimension:
#   #       (number of weeeks in training data) x (number of stores)
#   #       The forecasts are written in place of the zeros.
#   #
#   # returns:
#   #  the test(forecast) data frame with the forecasts filled in 
#   h <- nrow(test)
#   tr <- train[nrow(train) - (52:1) + 1,]
#   tr[is.na(tr)] <- 0
#   levels <- colMeans(tr[,2:ncol(tr)])
#   profile <- rowMeans(tr[,2:ncol(tr)])
#   overall <- mean(levels)
#   pred <- matrix(profile, ncol=1) %*% matrix(levels, nrow=1)
#   pred <- pred / overall
#   test[,2:ncol(test)] <- pred[1:h,]
#   test
# }
# 
# tslm.basic <- function(train, test){
#   # Computes a forecast using linear regression and seasonal dummy variables
#   #
#   # args:
#   # train - A matrix of Weekly_Sales values from the training set of dimension
#   #         (number of weeeks in training data) x (number of stores)
#   # test - An all-zeros matrix of dimension:
#   #       (number of weeeks in training data) x (number of stores)
#   #       The forecasts are written in place of the zeros.
#   #
#   # returns:
#   #  the test(forecast) data frame with the forecasts filled in 
#   horizon <- nrow(test)
#   train[is.na(train)] <- 0
#   for(j in 2:ncol(train)){
#     s <- ts(train[, j], frequency=52)
#     model <- tslm(s ~ trend + season)
#     fc <- forecast(model, h=horizon)
#     test[, j] <- as.numeric(fc$mean)
#   }
#   test
# }
# 
# naive_opt_2 = function(train, test){
#   
#   tmp_train <- train %>%
#     group_by(Dept, Store) %>%
#     filter(Date == max(Date)) %>%
#     ungroup() %>%
#     rename(Weekly_Pred = Weekly_Sales) %>%
#     select(-Date, -IsHoliday)
#   
#   pred <- test %>%
#     left_join(tmp_train, by = c('Dept', 'Store')) 
#   return(pred)
#   
# }
# 
# naive_opt_1 = function(train, test){
#   
#   most_recent_date <- max(train$Date)
#   tmp_train <- train %>%
#     filter(Date == most_recent_date) %>%
#     rename(Weekly_Pred = Weekly_Sales) %>%
#     select(-Date, -IsHoliday)
#   
#   pred <- test %>%
#     left_join(tmp_train, by = c('Dept', 'Store')) 
#   return(pred)
#   
# }

s_naive = function(train,test){
  
  start_last_year = min(test$Date) - 375
  end_last_year = max(test$Date) - 350
  tmp_train <- train %>%
    filter(Date > start_last_year & Date < end_last_year) %>%
    mutate(Wk = ifelse(year(Date) == 2010, week(Date)-1, week(Date))) %>%
    rename(Weekly_Pred = Weekly_Sales) %>%
    select(-Date, -IsHoliday)
  
  test <- test %>%
    mutate(Wk = week(Date))
  
  pred <- test %>%
    left_join(tmp_train, by = c('Dept', 'Store', 'Wk')) %>%
    select(-Wk)
  return(pred)
}

adjust_pred <- function(pred){
  
  pred <- 6/7*pred + 1/7*c(tail(pred,1),head(pred,-1))
  
  return(pred)
}

postprocess <- function(pred){
  
  adjusted_pred <- pred %>%
    filter(week(Date)%in% c(49:52)) %>%
    group_by(Store,Dept) %>%
    mutate(Weekly_Pred = adjust_pred(Weekly_Pred))
  
  pred <- pred %>%
    left_join(adjusted_pred, by = c('Store','Dept','Date'))%>%
    mutate(Weekly_Pred = coalesce(Weekly_Pred.y, Weekly_Pred.x))%>%
    select(-Weekly_Pred.x, -Weekly_Pred.y)
  
}

mypredict = function(){
  
  # same code as "what we have done III" adjustment by TA
  
  start_date <- ymd("2011-03-01") %m+% months(2 * (t - 1))
  end_date <- ymd("2011-05-01") %m+% months(2 * (t - 1))
  test_current <- test %>% filter(Date >= start_date & Date < end_date) %>% select(-IsHoliday)
  
  if (t > 1){
    train <<- train %>% add_row(new_train)
  }
  
  
  # find the unique pairs of (Store, Dept) combo that appeared in both training and test sets
  train_pairs <- train[, 1:2] %>% count(Store, Dept) %>% filter(n != 0)
  test_pairs <- test_current[, 1:2] %>% count(Store, Dept) %>% filter(n != 0)
  unique_pairs <- intersect(train_pairs[, 1:2], test_pairs[, 1:2])
  
  # pick out the needed training samples, convert to dummy coding, then put them into a list
  train_split <- unique_pairs %>%
    left_join(train, by = c('Store', 'Dept')) %>%
    mutate(Wk = factor(ifelse(year(Date) == 2010, week(Date) - 1, week(Date)), levels = 1:52)) %>%
    mutate(Yr = year(Date))
  train_split = as_tibble(model.matrix(~ Weekly_Sales + Store + Dept + Yr + Wk, train_split)) %>% group_split(Store, Dept)
  
  # do the same for the test set
  test_split <- unique_pairs %>%
    left_join(test_current, by = c('Store', 'Dept')) %>%
    mutate(Wk = factor(ifelse(year(Date) == 2010, week(Date) - 1, week(Date)), levels = 1:52)) %>%
    mutate(Yr = year(Date))
  test_split = as_tibble(model.matrix(~ Store + Dept + Yr + Wk, test_split)) %>% mutate(Date = test_split$Date) %>% group_split(Store, Dept)
  
  # pre-allocate a list to store the predictions
  
  test_pred1 <- vector(mode = "list", length = nrow(unique_pairs))
  
  
  # perform regression for each split, note we used lm.fit instead of lm
  for (i in 1:nrow(unique_pairs)) {
    tmp_train <- train_split[[i]]
    tmp_test <- test_split[[i]]
    
    mycoef <- lm.fit(as.matrix(tmp_train[, -(2:4)]), tmp_train$Weekly_Sales)$coefficients
    mycoef[is.na(mycoef)] <- 0
    tmp_pred <- mycoef[1] + as.matrix(tmp_test[, 4:55]) %*% mycoef[-1]
    
    test_pred1[[i]] <- cbind(tmp_test[, 2:3], Date = tmp_test$Date, Weekly_Pred = tmp_pred[,1])
  }
  
  # turn the list into a table at once, this is much more efficient then keep concatenating small tables
  test_pred1 <- bind_rows(test_pred1)
  
  #postprocessing predictions for folds which have holiday weeks
  test_pred1 <- postprocess(test_pred1)
    

  #averaging operations for other folds
  test_pred1 = test_pred1%>%
    mutate(Weekly_Pred1 = Weekly_Pred)%>%
    select(-Weekly_Pred)
  
  test_pred1$Weekly_Pred1[is.na(test_pred1$Weekly_Pred1)] <- 0
  
  
  test_pred2 <- vector(mode = "list", length = nrow(test_current))
  test_pred2 <- s_naive(train, test_current)
  
  test_pred2 <- postprocess(test_pred2)
  
  test_pred2 = test_pred2%>%
    mutate(Weekly_Pred2 = Weekly_Pred)%>%
    select(-Weekly_Pred)
  
  test_pred2$Weekly_Pred2[is.na(test_pred2$Weekly_Pred2)] <- 0
  
  test_pred <- vector(mode = "list", length = nrow(test_current))
  test_pred = test_pred1%>%
    inner_join(test_pred2, by = c("Dept","Store","Date"))%>%
    mutate(Weekly_Pred = rowMeans(cbind(Weekly_Pred1,Weekly_Pred2)))%>%
    select(-Weekly_Pred1,-Weekly_Pred2)

  
  return(test_pred)
}



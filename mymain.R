# title: "Project1_Ames"
# author: "Saurav Prem Kaushik Chetry"
# date: "10/19/2020"
# output: R Script

mypackages = c("xgboost", "caret","glmnet")   # required packages
tmp = setdiff(mypackages, rownames(installed.packages()))  # packages need to be installed
if (length(tmp) > 0) install.packages(tmp)
lapply(mypackages, require, character.only = TRUE)

train.x <- read.csv("train.csv")
train.y <- train.x$Sale_Price
test.x <- read.csv("test.csv")
test.y <- read.csv("test_y.csv")

remove.var <- c("Low_Qual_Fin_SF", "Kitchen_AbvGr", "Three_season_porch",
                "Pool_Area", "Misc_Val", "Condition_2", "Heating",
                "Pool_QC", "Roof_Matl", "Street", "Utilities", "Longitude",
                "Latitude", "Garage_Yr_Blt")


train.x <- train.x[ , -which(names(train.x) %in% remove.var)]

# Identifying New of the building
train.x$IsNew <- ifelse(train.x$Year_Sold==train.x$Year_Built, 1, 0)

dmy1 <- dummyVars(" ~ .", data = train.x)
train.x <- data.frame(predict(dmy1, newdata = train.x))

test.x <- test.x[ , -which(names(test.x) %in% remove.var)]

# Identifying New of the building
test.x$IsNew <- ifelse(test.x$Year_Sold==test.x$Year_Built, 1, 0)

dmy2 <- dummyVars(" ~ .", data = test.x)
test.x <- data.frame(predict(dmy2, newdata = test.x))

cols_train <- colnames(train.x)
cols_test <- colnames(test.x)

common_vars <- intersect(cols_test, cols_train)
train.x <- train.x[,common_vars]
test.x <- test.x[,common_vars]

X_train <- train.x[,!(colnames(train.x) %in% c("PID"))]
Y_train <- train.y
X_test <- test.x[,!(colnames(test.x) %in% c("PID"))]
X_train <- as.matrix(X_train)
Y_train <- as.matrix(Y_train)
X_test <- as.matrix(X_test)
Y_test <- test.y[,"Sale_Price"]
Y_test <- as.matrix(Y_test)


columns <- c("PID", "Sale_Price")


# Model 1 XGB
set.seed(4726)
xgb.model <- xgboost(data = as.matrix(X_train),
                     label = log(Y_train), max_depth = 6,
                     eta = 0.01, nrounds = 5000,
                     subsample = 0.5,
                     verbose = FALSE)

Ytest.pred = exp(predict(xgb.model, as.matrix(X_test) ))
mysubmission1 <- data.frame(PID=test.x[,"PID"], Sale_Price = Ytest.pred)
colnames(mysubmission1) <- columns
write.table(mysubmission1, file = "mysubmission1.txt", sep = ",", row.names = FALSE)


# Winsorize
winsor.vars <- c("Lot_Frontage", "Lot_Area", "Mas_Vnr_Area", "BsmtFin_SF_2", "Bsmt_Unf_SF", "Total_Bsmt_SF", "Second_Flr_SF", 'First_Flr_SF', "Gr_Liv_Area", "Garage_Area", "Wood_Deck_SF", "Open_Porch_SF", "Enclosed_Porch", "Screen_Porch")

train.x_win <- train.x[ , which(names(train.x) %in% winsor.vars)]
train.x_not_win <- train.x[ , -which(names(train.x) %in% winsor.vars)]

quan.value <- 0.95

for(var in winsor.vars){
  tmp <- train.x_win[, var]
  myquan <- quantile(tmp, probs = quan.value, na.rm = TRUE)
  tmp[tmp > myquan] <- myquan
  train.x_win[, var] <- tmp
}

train.x = cbind(train.x_win,train.x_not_win)


dmy3 <- dummyVars(" ~ .", data = train.x)
train.x <- data.frame(predict(dmy3, newdata = train.x))

test.x_win <- test.x[ , which(names(test.x) %in% winsor.vars)]
test.x_not_win <- test.x[ , -which(names(test.x) %in% winsor.vars)]


quan.value <- 0.95

for(var in winsor.vars){
  tmp <- test.x_win[, var]
  myquan <- quantile(tmp, probs = quan.value, na.rm = TRUE)
  tmp[tmp > myquan] <- myquan
  test.x_win[, var] <- tmp
}

test.x = cbind(test.x_win,test.x_not_win)

dmy4 <- dummyVars(" ~ .", data = test.x)
test.x <- data.frame(predict(dmy4, newdata = test.x))

cols_train <- colnames(train.x)
cols_test <- colnames(test.x)

common_vars <- intersect(cols_test, cols_train)
train.x <- train.x[,common_vars]
test.x <- test.x[,common_vars]

X_train <- train.x[,!(colnames(train.x) %in% c("PID"))]
Y_train <- train.y
X_test <- test.x[,!(colnames(test.x) %in% c("PID"))]
X_train <- as.matrix(X_train)
Y_train <- as.matrix(Y_train)
X_test <- as.matrix(X_test)
Y_test <- test.y[,"Sale_Price"]
Y_test <- as.matrix(Y_test)


columns <- c("PID", "Sale_Price")


#Model 2 LM with Elasticnet penalty

set.seed(4726)
cv.out = cv.glmnet(X_train, log(Y_train), alpha = 0.2)
Ytest.pred = exp(predict(cv.out, s = cv.out$lambda.min, newx = X_test))
mysubmission2 <- data.frame(PID=test.x[,"PID"], Sale_Price = Ytest.pred)
colnames(mysubmission2) <- columns
write.table(mysubmission2, file = "mysubmission2.txt", sep = ",", row.names = FALSE)












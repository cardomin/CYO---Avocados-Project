# Packages that are needed to resolve the Avocados Project

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(MASS)

# Avocado Prices dataset
# https://www.kaggle.com/neuromusic/avocado-prices?select=avocado.csv

av <- tempfile()
download.file("https://www.kaggle.com/neuromusic/avocado-prices/download", av)

avocado_csv <- read.csv("C:/Users/cdomi/Downloads/avocado.csv")

# Due to R 3.6.3 is used, we add sample.kind argument to set.seed function.
set.seed(1, sample.kind="Rounding")

# Development set will be 80% of Avocado data. Train and test sets will be 70% and 30% respectively of Development data.
# Validation set will be 20% of Avocado data.
# These percentages are based on the paper below. It is important to know that depend on the size of the database
# Shahin, M. A., Maier, H. R., and Jaksa, M. B. (2004). "Data division for developing neural networks applied to geotechnical engineering." Journal of Computing in Civil Engineering,ASCE, 18(2), [105-114]

test_index <- createDataPartition(y = avocado_csv$AveragePrice, times = 1, p = 0.8, list = FALSE)

Development <- avocado_csv[test_index,]
Validation <- avocado_csv[-test_index,]

## Data exploration
# 1.- Number of rows & columns

#Development

nrow(Development) #14601
ncol(Development) #14

#Validation

nrow(Validation) #3648
ncol(Validation) #14

# 2.- Name of the variables in both datasets

#Same variables in both cases
colnames(Development) # "X" "Date" "AveragePrice" "Total.Volume" "X4046" "X4225"  "X4770"  "Total.Bags" "Small.Bags"  "Large.Bags"   "XLarge.Bags"  "type" "year" "region"

# 3.- Summary stadistics

#Development

summary(Development)

#Validation

summary(Validation)

# 4.- How many different types, years and regions are in both datasets

#Development

n_distinct(Development$type) #2
n_distinct(Development$year) #4
n_distinct(Development$region) #54

#Validation

n_distinct(Validation$type) #2
n_distinct(Validation$year) #4
n_distinct(Validation$region) #54

## Data cleaning and Influence of variables on Average Price

# 1.- Date & Average Price

# In order to do a complete analysis of the influence of the date of the observation (Date variable) on Average Prices, 2 graphs are plotted: Date rounded to week and Date rounded to month.

# Firstable, we are converting Date variable (factor) to a date.
# Then, we are rounding it by week (date_week) and month (date_month) to see the relation between these new variables and Average Prices
Development <- Development %>% mutate(Datetime = as_datetime(Date)) %>% mutate(date_week = round_date(Datetime, unit = "week"),date_month = round_date(Datetime, unit = "month")) 

Date_week <- Development %>% group_by(date_week) %>% summarize(AvPrice = mean(AveragePrice)) %>% ggplot(aes(date_week,AvPrice)) + geom_point() + geom_smooth() + ggtitle("Date rounded to week & Average Price") + labs(x = "Date rounded to week", y = "Average Price")
Date_week

Date_month <- Development %>% group_by(date_month) %>% summarize(AvPrice = mean(AveragePrice)) %>% ggplot(aes(date_month,AvPrice)) + geom_point() + geom_smooth() + ggtitle("Date rounded to month & Average Price") + labs(x = "Date rounded to month", y = "Average Price")
Date_month

# Conclusion 1.-: There is strong evidence of a date effect on average price.

# 2.- Type & Average Price

# Relation between the type of avocado (conventional or organic) and Average Prices.

Development %>% group_by(type) %>% summarize(AvPrice = mean(AveragePrice)) %>% ggplot(aes(reorder(type,AvPrice),AvPrice)) + geom_bar(stat="identity", width=0.1, color = "black", fill = "aquamarine2") + labs(x = "Type", y = "Average Price") +
  ggtitle("Type & Average Price")

# Conclusion 2.-: There is strong evidence of a type effect on average price.

# 3.- Region & Average Price

# Relation between the city or region of the observation (region variable) and Average Prices. 

Development %>% group_by(region) %>% summarize(AvPrice = mean(AveragePrice)) %>% ggplot(aes(reorder(region,AvPrice),AvPrice, fill = AvPrice)) + geom_bar(stat="identity", width=0.5) + coord_flip() + scale_fill_distiller(palette = "YlOrRd") + labs(x = "Region", y = "Average Price") +
  ggtitle("Region & Average Price")

# Conclusion 3.-: There is strong evidence of a region effect on average price.

# 4.- Total volume & Average Price

# Relation between total number of avocados sold (Total Volume) and Average Prices.
# Firstable, we have to check how many different Average Prices are in the Development dataset to group them and summarize the total number of avocados sold per each price.

n_distinct(Development$AveragePrice) #257

Development %>% group_by(AveragePrice) %>% summarize(T.Volume = mean(Total.Volume)) %>% ggplot(aes(T.Volume, AveragePrice)) + geom_point() + geom_smooth() + ggtitle("Total Volume & Average Price") + labs(x = "Total Volume", y = "Average Price") 

# Conclusion 4.-: There is strong evidence of a Total Volume effect on average price.

## Results

# Training process

# Due to R 3.6.3 is used, we add sample.kind argument to set.seed function.
set.seed(2020,sample.kind = "Rounding")
options(digits = 5)

# Train set will be 70% of Development data
# Test set will be 30% of Development data

Development_test_index <- createDataPartition(Development$AveragePrice,times = 1, p = 0.3, list = FALSE)

train <- Development[-Development_test_index,]
test <- Development[Development_test_index,]

# To train our algotithm, we will calculate first RMSE without regularization technique. 

#Just the average
mu_hat <- mean(train$AveragePrice)

naive_rmse <- RMSE(test$AveragePrice,mu_hat)

options(pillar.sigfig = 5)
rmse_results <- tibble(Model = "Just the average", RMSE = naive_rmse)
rmse_results

#Date effect
date_avgs <- train%>%
  group_by(Date) %>%
  summarize(b_d = mean(AveragePrice-mu_hat))

predicted_average <- mu_hat + test %>%
  left_join(date_avgs, by='Date') %>%
  pull(b_d)
Date_model <- RMSE(predicted_average, test$AveragePrice,na.rm=TRUE)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Date Effect",  
                                     RMSE =Date_model))
rmse_results 

#Type effect
type_avgs <- train %>%
  group_by(type) %>%
  summarize(b_t = mean(AveragePrice-mu_hat))

predicted_average <- mu_hat + test %>%
  left_join(type_avgs, by='type') %>%
  pull(b_t)
type_model <- RMSE(predicted_average, test$AveragePrice,na.rm=TRUE)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Type Effect",  
                                     RMSE =type_model))
rmse_results 

#Region effect
region_avgs <- train %>%
  group_by(region) %>%
  summarize(b_r = mean(AveragePrice-mu_hat))

predicted_average <- mu_hat + test %>%
  left_join(region_avgs, by='region') %>%
  pull(b_r)
region_model <- RMSE(predicted_average, test$AveragePrice,na.rm=TRUE)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Region Effect",  
                                     RMSE =region_model))
rmse_results 

#Total Volume effect
volume_avgs <- train %>%
  group_by(Total.Volume) %>%
  summarize(b_v = mean(AveragePrice-mu_hat))

predicted_average <- mu_hat + test %>%
  left_join(volume_avgs, by='Total.Volume') %>%
  pull(b_v)
volume_model <- RMSE(predicted_average, test$AveragePrice,na.rm=TRUE)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Total Volume Effect",  
                                     RMSE =volume_model))
rmse_results 

# Due to Type and Region variables got the smallest RMSE values, we will combine them in order to check if we can reduce the Root Mean Squared Error.

# Type + Region effect

type_avgs <- train %>%
  left_join(region_avgs, by='region') %>%
  group_by(type) %>%
  summarize(b_t = mean(AveragePrice - mu_hat - b_r))

predicted_ratings <- test %>%
  left_join(region_avgs, by='region') %>%
  left_join(type_avgs, by='type') %>%
  mutate(pred = mu_hat + b_t + b_r) %>%
  pull(pred)

Type_plus_Region_model <- RMSE(predicted_ratings, test$AveragePrice,na.rm=TRUE)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Type + Region Effects",  
                                     RMSE =Type_plus_Region_model))
rmse_results 

#Know, we will calculate RMSE with regularization technique.

#Regularization with Date effect

lambdas_1 <- seq(0, 10, 0.1)
rmses_1 <- sapply(lambdas_1, function(l){
  mu <- mean(train$AveragePrice)
  b_d <- train %>%
    group_by(Date) %>%
    summarize(b_d = sum(AveragePrice - mu)/(n()+l))
  
  predicted_ratings <-
    test %>%
    left_join(b_d, by = "Date") %>%
    mutate(pred = mu + b_d) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test$AveragePrice,na.rm = TRUE))
})
qplot(lambdas_1,rmses_1,main = "Lambda vs RMSE | Regularization with Date effect",xlab = "Lambda",ylab = "RMSE")
Reg_Date_model <- min(rmses_1)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Regularized Date Effect",  
                                     RMSE =Reg_Date_model))
rmse_results 

#Regularization with Type effect

lambdas_2 <- seq(60, 75, 0.1)
rmses_2 <- sapply(lambdas_2, function(l){
  mu <- mean(train$AveragePrice)
  b_t <- train %>%
    group_by(type) %>%
    summarize(b_t = sum(AveragePrice - mu)/(n()+l))
  
  predicted_ratings <-
    test %>%
    left_join(b_t, by = "type") %>%
    mutate(pred = mu + b_t) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test$AveragePrice,na.rm = TRUE))
})
qplot(lambdas_2,rmses_2,main = "Lambda vs RMSE | Regularization with Type effect",xlab = "Lambda",ylab = "RMSE")
Reg_Type_model <- min(rmses_2)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Regularized Type Effect",  
                                     RMSE =Reg_Type_model))
rmse_results 

#Regularization with Region effect

lambdas_3 <- seq(15, 25, 0.1)
rmses_3 <- sapply(lambdas_3, function(l){
  mu <- mean(train$AveragePrice)
  b_r <- train %>%
    group_by(region) %>%
    summarize(b_r = sum(AveragePrice - mu)/(n()+l))
  
  predicted_ratings <-
    test %>%
    left_join(b_r, by = "region") %>%
    mutate(pred = mu + b_r) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test$AveragePrice,na.rm = TRUE))
})
qplot(lambdas_3,rmses_3,main = "Lambda vs RMSE | Regularization with Region effect",xlab = "Lambda",ylab = "RMSE")
Reg_Region_model <- min(rmses_3)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Regularized Region Effect",  
                                     RMSE =Reg_Region_model))
rmse_results 

#Regularization with Total Volume effect

lambdas_4 <- seq(0, 2.5, 0.05)
rmses_4 <- sapply(lambdas_4, function(l){
  mu <- mean(train$AveragePrice)
  b_v <- train %>%
    group_by(Total.Volume) %>%
    summarize(b_v = sum(AveragePrice - mu)/(n()+l))
  
  predicted_ratings <-
    test %>%
    left_join(b_v, by = "Total.Volume") %>%
    mutate(pred = mu + b_v) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test$AveragePrice,na.rm = TRUE))
})
qplot(lambdas_4,rmses_4,main = "Lambda vs RMSE | Regularization with Total Volume effect",xlab = "Lambda",ylab = "RMSE")
Reg_Volume_model <- min(rmses_4)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Regularized Total Volume Effect",  
                                     RMSE =Reg_Volume_model))
rmse_results 

# Due to Type and Region variables got the smallest RMSE values, we will combine them in order to check if we can reduce the Root Mean Squared Error with regularization technique.

#Regularization with Type + Region effect

lambdas_5 <- seq(2.5, 15, 0.1)
rmses_5 <- sapply(lambdas_5, function(l){
  mu <- mean(train$AveragePrice)
  b_r <- train %>%
    group_by(region) %>%
    summarize(b_r = sum(AveragePrice - mu)/(n()+l))
  
  b_t <- train %>%
    left_join(b_r, by="region") %>%
    group_by(type) %>%
    summarize(b_t = sum(AveragePrice - b_r - mu)/(n()+l))
  
  predicted_ratings <-
    test %>%
    left_join(b_r, by = "region") %>%
    left_join(b_t, by = "type") %>%
    mutate(pred = mu + b_r + b_t) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test$AveragePrice,na.rm = TRUE))
})
qplot(lambdas_5,rmses_5,main = "Lambda vs RMSE | Regularization with Type + Region effect",xlab = "Lambda",ylab = "RMSE")
lambda <- lambdas_5[which.min(rmses_5)]
Reg_Type_plus_Region_model <- min(rmses_5)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Regularized Type + Region Effects",  
                                     RMSE =Reg_Type_plus_Region_model))
rmse_results 

# For this project, we have to apply machine learning techniques that go beyond standard linear regression so glm, RandomForest and knn techniques are also tested to try to reduce RMSE value.
# Other techniques such as lda, qda or Naive Bayes have not been finally used because they have generated errors whose solution has not been found.

#Glm model

train_glm <- train(AveragePrice ~ Date + Total.Volume + type + region, method = "glm", data = train)
y_hat_glm <- predict(train_glm, test, type = "raw")

glm_RMSE <- RMSE(test$AveragePrice,y_hat_glm)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Glm",  
                                     RMSE =glm_RMSE))
rmse_results 

#Random forest model

# Because with random forest the fitting is the slowest part of the procedure rather than the predicting (as with kNN), we will use only three ntrees values: 10, 30 and 50. It is recommend it to use more than 100 trees but the time of computation is too hight.

# 10 trees

train_rf_10 <- train(AveragePrice ~ Date + Total.Volume + type + region, method = "rf", data = train, ntree = 10,
                   tuneGrid = data.frame(mtry = 15))

plot(train_rf_10$finalModel,main = "Trees vs Error | Random Forest - 10 trees")

y_hat_rf_10 <- predict(train_rf_10, test, type = "raw")

rf_10_RMSE <- RMSE(test$AveragePrice,y_hat_rf_10)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Random Forest - 10 trees",  
                                     RMSE =rf_10_RMSE))
rmse_results 

#30 trees

train_rf_30 <- train(AveragePrice ~ Date + Total.Volume + type + region, method = "rf", data = train, ntree = 30,
                  tuneGrid = data.frame(mtry = 15))

plot(train_rf_30$finalModel,main = "Trees vs Error | Random Forest - 30 trees")

y_hat_rf_30 <- predict(train_rf_30, test, type = "raw")

rf_30_RMSE <- RMSE(test$AveragePrice,y_hat_rf_30)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Random Forest - 30 trees",  
                                     RMSE =rf_30_RMSE))
rmse_results

#50 trees

train_rf_50 <- train(AveragePrice ~ Date + Total.Volume + type + region, method = "rf", data = train, ntree = 50,
                     tuneGrid = data.frame(mtry = 15))

plot(train_rf_50$finalModel,main = "Trees vs Error | Random Forest - 50 trees")

y_hat_rf_50 <- predict(train_rf_50, test, type = "raw")

rf_50_RMSE <- RMSE(test$AveragePrice,y_hat_rf_50)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Random Forest - 50 trees",  
                                     RMSE =rf_50_RMSE))
rmse_results


#knn model

# As random forest model, in Knn the fitting is the slowest part of the procedure rather than the predicting. 
# We will use only three-fold cross validation: 200, 250 and 300. 
# Other values have been tested (1,7,50,100, etc) but the trend of the error curve was decreasing for higher values of k.

train_knn_1 <- train(AveragePrice ~ Date + Total.Volume + type + region, method = "knn", data = train,
                     tuneGrid = data.frame(k = seq(200,300,50)))

seq_k_1 <- plot(train_knn_1,main = "Neighbors vs RMSE (Bootstrap)")
seq_k_1
y_hat_knn_1 <- predict(train_knn_1, test, type = "raw")

knn_1_RMSE <- RMSE(test$AveragePrice,y_hat_knn_1)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Knn",  
                                     RMSE =knn_1_RMSE))
rmse_results 

Results <- as.data.frame(rmse_results)
Results %>% arrange(RMSE)

#Analyzing the results, we notice that Random Forest with 50 trees model give us the smallest RMSE. 

#Validation process


rf_val <- train(AveragePrice ~ Date + Total.Volume + type + region, method = "rf", data = Development, ntree = 50,
                     tuneGrid = data.frame(mtry = 15))

plot(rf_val$finalModel,main = "Trees vs Error | Random Forest - 50 trees")

y_hat_rf_val <- predict(rf_val, Validation, type = "raw")


#Validation RMSE

rf_val_RMSE <- RMSE(y_hat_rf_val,Validation$AveragePrice)
rf_val_RMSE


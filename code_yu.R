#install.packages("randomForest")
library(randomForest)
library(pROC)
library(ROCR)

rdsdata_one <- readRDS("C:/Users/navee/OneDrive/Desktop/centrifuge_abundance_yu_status.rds")

head(rdsdata_one)

rdsdata_one$status = as.factor(rdsdata_one$status)
names(rdsdata_one)[names(rdsdata_one) == 'status'] <- 'result'
response_col <- which(colnames(rdsdata_one) == "result")
colnames(rdsdata_one)[-response_col] <- paste0( "V", colnames(rdsdata_one)[-response_col])

data_set_size= floor(nrow(rdsdata_one)*0.80)
index <- sample(1:nrow(rdsdata_one), size = data_set_size)
train_ds <- rdsdata_one[index,]
test_ds <- rdsdata_one[-index,]
train_ds$result

rf <- randomForest(result ~ ., data=train_ds, ntree=2001, importance= TRUE)
rf
plot(rf)

# Test the testing data with our model
# Find the AUC score.
# Test the other 5 datasets with our model

testing <- predict(rf, test_ds[,1:2534], type="response")
outcome <- data.frame(test_ds$result, predict(rf, test_ds[,1:2534], type="response"))
outcome

plot(outcome)

rf_p_train <- as.vector(rf$votes[,2])
rf_pr_train <- prediction(rf_p_train, train_ds$result);
r_auc_train2 <- performance(rf_pr_train, measure = "auc")@y.values[[1]]
r_auc_train2
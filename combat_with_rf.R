set.seed(1)

library(purrr)

library(sva)
library(BiocManager)

library("dplyr")
library(randomForest)
library(pROC)
library(ROCR)
library(data.table)
library(readr)
library(metafor)
library(caret)
library(operators)

file_paths <- c("C:/Users/navee/OneDrive/Desktop/centrifuge_abundance_feng_status.rds", "C:/Users/navee/OneDrive/Desktop/centrifuge_abundance_hannigan_status.rds", "C:/Users/navee/OneDrive/Desktop/centrifuge_abundance_thomas_status.rds", "C:/Users/navee/OneDrive/Desktop/centrifuge_abundance_vogtmann_status.rds", "C:/Users/navee/OneDrive/Desktop/centrifuge_abundance_yu_status.rds", "C:/Users/navee/OneDrive/Desktop/centrifuge_abundance_zeller_status.rds")

datasets <- map(file_paths, readRDS)

# Get all unique column names across all datasets
all_columns <- unique(unlist(map(datasets, function(df) setdiff(colnames(df), "status"))))

# Function to align a single dataset to have all columns
align_columns <- function(df, all_columns) {
  missing_cols <- setdiff(all_columns, colnames(df))
  if (length(missing_cols) > 0) {
    df[missing_cols] <- 0
  }
  # Ensure 'status' column is the last column for consistency
  df <- df[, c(all_columns, "status"), drop = FALSE]
  return(df)
}

# Apply the function to all datasets
aligned_datasets <- map(datasets, align_columns, all_columns = all_columns)

# Combine all aligned datasets into one data frame
combined_dataset <- do.call(rbind, aligned_datasets)

process_data <- function(data, threshold = 0.80) {
  zero_proportions <- sapply(data, function(column) sum(column == 0) / length(column))
  filtered_data <- data[, zero_proportions <= threshold, drop = FALSE]
  # adjusted_data <- filtered_data + small_number
  return(filtered_data)
}

get_rel_abun <- function(ds, status){
  ds = ds/rowSums(ds) #change to relative abundance
  min = min(apply(ds[,1:ncol(ds)], 1, function(x) min(x[x>0])))
  ds[ds == 0] = min*0.80
  ds = log(ds)
  ds$status = status
  return(as.data.frame(ds))
}

rdsdata_one <- combined_dataset
rdsdata_two <- rdsdata_one[, !(names(rdsdata_one) %in% "status")]
# rdsdata_two <- process_data(rdsdata_two)
# rdsdata_two <- get_rel_abun(rdsdata_two, rdsdata_one$status)

rdsdata_two <- as.data.frame(apply(rdsdata_two, 2, function(col) ifelse(col == 0, col + 10^-6, col)))
rdsdata_two <- log(rdsdata_two)

rdsdata_two$status <- rdsdata_one$status

values_to_drop <- c('Adenoma', 'advanced adenoma') 
rdsdata_two <- rdsdata_two[!rdsdata_two$status %in% values_to_drop, ]

rdsdata_two$status = as.factor(rdsdata_two$status)
names(rdsdata_two)[names(rdsdata_two) == 'status'] <- 'result'
response_col <- which(colnames(rdsdata_two) == "result")
colnames(rdsdata_two)[-response_col] <- paste0( "V", colnames(rdsdata_two)[-response_col])

data_set_size= floor(nrow(rdsdata_two)*0.80)
index <- sample(1:nrow(rdsdata_two), size = data_set_size)
train_ds <- rdsdata_two[index,]
test_ds <- rdsdata_two[-index,]

training <- train_ds
training_status <- train_ds$result
test <- test_ds
test_status <- test_ds$result

training$result <- as.factor(training$result)
training$result <- as.numeric(training$result)

test$result <- as.factor(test$result)
test$result <- as.numeric(test$result)

##############ComBat normalization is only available for cross-dataset analysi####################
# training = training/rowSums(training) #renormalize
training = training[,order(colnames(training))]

for (j in 1:ncol(training)) {
  if(colnames(training)[j] %!in% colnames(test)){
    test = cbind(test,0)
    colnames(test)[ncol(test)] = colnames(training)[j]
  }
}
# test = test/rowSums(test) #renormalize
test = test[,colnames(test)%in%colnames(training)]
test = test[,order(colnames(test))]
training$batch = 0 #add 0 for training batch
test$batch = 1 #add 1 for testing batch

data = rbind(training, test)
batch = data$batch
data = t(data[,1:(ncol(data)-1)])

data_norm = as.data.frame(t(ComBat(data, batch)))

training_norm = data_norm[1:nrow(training),]
training_norm$result = training_status
test_norm = data_norm[-(1:nrow(training)),]
test_norm$result = test_status

auc_arr <- c()
set.seed(10)

for (x in 1:10) {
  
  rf <- randomForest(result ~ ., data=training_norm, ntree=2001, importance= TRUE, cutoff=c(0.6,0.4))
  
  testing <- predict(rf, test_norm[,2:4331], type="response")
  outcome <- data.frame(test_norm$result, predict(rf, test_norm[,2:4331], type="response"))
  
  rf_p_train <- as.vector(rf$votes[,2])
  rf_pr_train <- prediction(rf_p_train, training_norm$result);
  r_auc_train2 <- performance(rf_pr_train, measure = "auc")@y.values[[1]]
  
  auc_arr[length(auc_arr) + 1] <- r_auc_train2
  
}

auc_arr
mean (auc_arr)

# boxplot(auc_arr)
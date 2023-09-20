rm(list = ls()) 

# 1
data <- read.table("data 14.1\\breast-cancer-wisconsin.data.txt", header=FALSE, sep = ",")

calculate_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

calculate_mode(data[,"V7"])

data[,"V7"]



# 2
rm(list = ls()) 
data <- read.table("data 14.1\\breast-cancer-wisconsin.data.txt", header=FALSE, sep = ",")
#install.packages("scales")
library(scales)
x <- data[,"V7"]
missing_indicies <- which(x == "?"); missing_indicies

data_train <- data[-missing_indicies,]
data_missing <- data[missing_indicies,]

lm_data_train <- lm(V7~V3+V4+V5+V8+V9+V11, 
            data_train[missing_indicies,]); lm_data_train

summary(lm_data_train)

results_list <- vector()
for(i in list(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)){
  data_test = data.frame(V3 = data_missing[i,3], V4 = data_missing[i,4],V5 = data_missing[i,5],
                         V8 = data_missing[i,8],V9=data_missing[i,9],V11=data_missing[i,11])
  predict_test <- predict(lm_data_train, data_test) 
  results_list[i] <- predict_test
  
}

predict_scaled <- rescale(results_list, to=c(1,10))
predict_scaled

predict_scaled_rounded <- vector()
for(i in list(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)){
  predict_scaled_rounded[i] <- round(predict_scaled[i], 0)
}

predict_scaled_rounded

data_missing_filled <- cbind(data_missing, predict_scaled_rounded)



# 3
rm(list = ls()) 
data <- read.table("data 14.1\\breast-cancer-wisconsin.data.txt", header=FALSE, sep = ",")

install.packages("rnorm")
library(rnorm)

x <- data[,"V7"]
missing_indicies <- which(x == "?"); missing_indicies

data_train <- data[-missing_indicies,]
data_missing <- data[missing_indicies,]

lm_data_train <- lm(V7~V3+V4+V5+V8+V9+V11, 
                    data_train[missing_indicies,]); lm_data_train

results_list <- vector()
for(i in list(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)){
  data_test = data.frame(V3 = data_missing[i,3], V4 = data_missing[i,4],V5 = data_missing[i,5],
                         V8 = data_missing[i,8],V9=data_missing[i,9],V11=data_missing[i,11])
  predict_test <- predict(lm_data_train, data_test) 
  
  results_list[i] <- predict_test
  
}

# Add perturbation:
results_list
results_list_perturbed <- results_list + rnorm(results_list, 0, 1)
results_list_perturbed

predict_scaled_perturbed<- rescale(results_list_perturbed, to=c(1,10))
predict_scaled_perturbed

predict_scaled_perturbed_rounded <- vector()
for(i in list(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)){
  predict_scaled_perturbed_rounded[i] <- round(predict_scaled_perturbed[i], 0)
}

predict_scaled_perturbed_rounded

data_missing_filled <- cbind(data_missing, predict_scaled_perturbed_rounded)

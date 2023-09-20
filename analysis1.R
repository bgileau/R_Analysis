#data <- credit_card_data.headers

data <- read.table("data 2.2\\credit_card_data-headers.txt", header=TRUE)

library(kernlab)

# call ksvm.  Vanilladot is a simple linear kernel.
model <- ksvm(as.matrix(data[,1:10]),as.factor(data[,11]),type="C-svc",kernel="vanilladot",C=.01,scaled=TRUE)
# calculate a1.am
a <- colSums(model@xmatrix[[1]] * model@coef[[1]])
a
# calculate a0
a0 <- -model@b
a0
# see what the model predicts
pred <- predict(model,data[,1:10])
pred
# see what fraction of the model's predictions match the actual classification
sum(pred == data[,11]) / nrow(data)

# 2.3
i = 96
library(kknn)
model_knn = kknn(R1~A1+A2+A3+A8+A9+A10+A11+A12+A14+A15,
  data[-i,],
  data[i,],
  k=10,
  distance = 2,
  scale = TRUE)

#results <- as.integer(fitted.values(model_knn)+.5)
results <- fitted.values(model_knn)
#data[i,11]

results

count <- 0
overall_count <- 0

results <- c()
accuracy <- c()

for (x in 1:5){
  results <- c()
  for (i in 1:nrow(data)){
    model_knn = kknn(R1~A1+A2+A3+A8+A9+A10+A11+A12+A14+A15,
                     data[-i,],
                     data[i,],
                     k=x,
                     distance = 2,
                     scale = TRUE)
    
    pred = predict(model_knn)
    
    results <- c(results, as.integer(pred+.5))
    
    accuracy <- c(accuracy, sum(results == data[,11]) / nrow(data))
    
    if (!is.na(data[i,11]) & !is.na(results[i])){
      if (data[i,11] != results[i]){
        count <- count + 1
      }
      
      
    }
    
  }
}

accuracy

results
data[,11]

overall_count

accuracy <- count / overall_count
accuracy

# 3.1
set.seed(1)
model <- train.kknn(V11~.,data,kmax=20,scale=TRUE)

fitted(model)[[4]][1:nrow(data)]

model <- cv.kknn(V11~.,data,kcv=10,k=5,scale=TRUE)

model[[1]][,2]
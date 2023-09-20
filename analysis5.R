# Question 11.1

# 1.
# Clear environment
rm(list = ls()) 

data <- read.table("data 11.1\\uscrime.txt", header=TRUE)

model_stepwise <- lm(Crime~., data=data)

?step
step(model_stepwise, direction = "both")

# 2.
# Clear environment
rm(list = ls()) 

data <- read.table("data 11.1\\uscrime.txt", header=TRUE)

#data_scaled <- scale(data)


#install.packages("glmnet")
library(glmnet)

?glmnet

set.seed(30)
model_lasso <- cv.glmnet(x = as.matrix(data[,-16]),
                         y = as.matrix(data[,16]),
                         alpha=1,
                         nfolds=8,
                         nlambda=20, 
                         type.measure="mse",
                         family="gaussian",
                         standardize=TRUE)
model_lasso
plot(model_lasso)
model_lasso$lambda.min
coef(model_lasso, s=model_lasso$lambda.min)

# 3.
# Clear environment
rm(list = ls()) 

data <- read.table("data 11.1\\uscrime.txt", header=TRUE)
library(glmnet)
?glmnet

set.seed(30)
model_elastic <- cv.glmnet(x = as.matrix(data[,-16]),
                           y = as.matrix(data[,16]),
                           alpha=.33333333,
                           nfolds=8,
                           nlambda=20, 
                           type.measure="mse",
                           family="gaussian",
                           standardize=TRUE)

plot(model_elastic)

model_elastic$lambda.min


# Question 12.2

# Clear environment
rm(list = ls()) 

#install.packages("FrF2")
library(FrF2)
?FrF2

FrF2(16, 10)

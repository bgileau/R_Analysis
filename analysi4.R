# Question 9.1

# Clear environment
rm(list = ls()) 

data <- read.table("data 9.1\\uscrime.txt", header=TRUE)

?prcomp

prcomp_data <- prcomp(data[,1:15], scale. = TRUE)
summary(prcomp_data)

chosen_pcs <- prcomp_data$x[,1:5]

prcomp_datacrime <- as.data.frame(cbind(chosen_pcs, data[,16]))

prcomp_lm <- lm(V6~PC1+PC2+PC4+PC5, data = prcomp_datacrime)
summary(prcomp_lm)

beta0 <- prcomp_lm$coefficients[1]
beta_non0 <- prcomp_lm$coefficients[2:5]

prcomp_data$rotation[,1:4]

alpha_non0 <- prcomp_data$rotation[,1:4] %*% beta_non0
t(alpha_non0)

unscaled_alpha_non0 <- alpha_non0/sapply(data[,1:15], sd)
unscaled_beta0 <- beta0 - sum(alpha_non0 * sapply(data[,1:15], mean)/sapply(data[,1:15], sd))

t(unscaled_alpha_non0)
unscaled_beta0

# Question 10.1

# Clear environment
rm(list = ls()) 

data <- read.table("data 10.1\\uscrime.txt", header=TRUE)

#install.packages("tree")
library(tree)

tree_data <- tree(Crime~., data=data, split="deviance")
summary(tree_data)

plot(tree_data)
text(tree_data)

cv_tree_data = cv.tree(tree_data)
plot(cv_tree_data$size, cv_tree_data$dev, type = "b")

terminal_nodes <- 5
pruned_tree_data <- prune.tree(tree_data, best = terminal_nodes)
pruned_tree_data

plot(pruned_tree_data)
text(pruned_tree_data)

#(b)

#install.packages("randomForest")
library("randomForest")

set.seed(6)

n_pred <- 4
rf_data <- randomForest(Crime~., data=data, mtry = n_pred, importance = TRUE)
rf_data

varImpPlot(rf_data)

# 10.3 

# Clear environment
rm(list = ls()) 

data <- read.table("data 10.3\\germancredit.txt", header=TRUE)

set.seed(1)

data$X1.1[data$X1.1==1] <- 0
data$X1.1[data$X1.1==2] <- 1

m <- nrow(data)
train_data <- sample(1:m, size=round(m * .7), replace = FALSE)

data_learn <- data[train_data,]
data_validate <- data[-train_data,]

logistic_regression_model <- glm(X1.1~., family=binomial(link = "logit"), data=data_learn)

summary(logistic_regression_model)

# Worse model accuracy (69% vs 74%)
#logistic_regression_model <- glm(X1.1~A11 + A43 + A65 + X4 + X67, family=binomial(link = "logit"), data=data_learn)
#summary(logistic_regression_model)

logistic_regression_model <- glm(X1.1~A11 + X6+ A34 + X1169 + A93 + A101 + A43 + A65 + X4 + X67 + A143 + A192 + A201, family=binomial(link = "logit"), data=data_learn)
summary(logistic_regression_model)

y_hat <- predict(logistic_regression_model, data_validate, type="response") ; y_hat
y_hat_round <- as.integer(y_hat > .5)

t <- table(y_hat_round, data_validate$X1.1) ; t
accuracy <- (t[1,1] + t[2,2]) / sum(t) ; accuracy

# (2)
#install.packages("pROC")
library(pROC)

roc_obj <- roc(data_validate$X1.1, y_hat) ; roc_obj$auc

plot.roc(roc_obj, main="ROC curve")

residuals_df = data.frame(Thresholds=roc_obj$thresholds, Sens=roc_obj$sensitivities, spec=roc_obj$specificities)

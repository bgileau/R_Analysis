# 7.2

# Clear environment
rm(list = ls()) 

# Load Data
data <- read.table("data 7.2\\temps.txt", header=TRUE)

library(stats)

# Flatten the data from a matrix to 1 giant vector
?unlist
flat_data_vec <- as.vector(unlist(data[,2:21]))
flat_data_vec
# Plot it
plot(flat_data_vec)

?ts

data_ts <- ts(flat_data_vec, start = 1996, frequency = 123)
data_ts

plot(data_ts)

?decompose

plot(decompose(data_ts))

?HoltWinters
data_ts_hw <- HoltWinters(data_ts, alpha=NULL, beta = NULL, gamma = NULL, seasonal = "multiplicative")
data_ts_hw_fitted <- data_ts_hw$fitted

plot(data_ts_hw_fitted)

#write(as.vector(data_ts_hw_fitted[,"xhat"]), "C:\\Users\\brett\\Documents\\GATech\\ISYE6501 - Intro Analytics Modeling\\week 3 Homework-Summer21\\week 3 data-summer\\data 7.2\\fitted.txt")

data_hw_excel <- matrix(data_ts_hw_fitted[,4], nrow =123)

write.csv(data_hw_excel, file= "data 7.2\\fitted.csv", fileEncoding = "UTF-16LE")



# 8.2

# Clear environment
rm(list = ls()) 

# Load Data
data <- read.table("data 8.2\\uscrime.txt", header=TRUE)

?lm

lm_data <- lm(Crime~., data)

summary(lm_data)

outlier <- boxplot.stats(data$Crime)$out


# Check which rows the outlier values are inside the data set, and filter to remove them
#data <- data[-which(data$Crime %in% outlier),]

#lm_data <- lm(Crime~., data)
summary(lm_data)
# Predict data with this char:
#formula <- as.formula(Crime~14*data$M + 0*So + 10*Ed + 12*Po1 + 15.5*Po2 + .64*LF + 94*M.F + 150*Pop + 1.1*NW + .120*U1 + 3.6*U2 + 3200*Wealth + 20.1*Ineq + .04*Prob + 39*Time)

predict_test_data <- data.frame(M = 14, So = 0, Ed = 10, Po1 = 12,Po2 = 15.5,
                           LF=.64,M.F=94,Pop=150,NW=1.1,U1=.12,U2=3.6,Wealth=3200,Ineq=20.1,
                           Prob=.04, Time = 39)

predict_test <- predict(lm_data,predict_test_data)
predict_test

qqnorm(data$Crime)

predict_test_data2 <- data.frame(M = 14, So = 0, Ed = 10, Po1 = 12,Po2 = 15.5,
                                LF=.64,M.F=94,Pop=150,NW=1.1,U1=.12,U2=3.6,Wealth=3200,Ineq=20.1,
                                Prob=.04, Time = 39, Crime=155)

data2 <- rbind(data,predict_test_data2)

outlier2 <- boxplot.stats(data2$Crime)$out

qqnorm(data2$Crime)

?qqnorm
?predict

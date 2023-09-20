# Question 4.2

# Clear environment
rm(list = ls()) 

# Load Data
data <- read.table("data 4.2\\iris.txt", header=TRUE)

data[,1:4] <- scale(data[,1:4])

# preview data table, column Species (response)
table(data$Species)

# remove the 5th row "species" from the table
#data_no_results <- data[,1:4]

library(stats)

# Investigate the API
?kmeans 

#cols <- list(c(1,2,3,4),c(1,2,3),c(1,2,4),c(1,3,4), c(2,3,4))
cols <- list(c(1),c(2),c(3),c(4))
cols
for (x in cols){
  kmeans_result2 <- kmeans(data[,x], centers = 2, nstart=5)
  kmeans_result3 <- kmeans(data[,x], centers = 3, nstart=5)
  kmeans_result4 <- kmeans(data[,x], centers = 4, nstart=5)
  kmeans_result5 <- kmeans(data[,x], centers = 5, nstart=5)
  
  # Get a within-cluster sum of squares (evaluates error terms for each item within cluster)
  #kmeans_result1$tot.withinss
  #kmeans_result1$betweenss
  
  x
  
  print(kmeans_result2$betweenss/kmeans_result2$totss)
  print(kmeans_result3$betweenss/kmeans_result3$totss)
  print(kmeans_result4$betweenss/kmeans_result4$totss)
  print(kmeans_result5$betweenss/kmeans_result5$totss)
  
  print(kmeans_result2$tot.withinss)
  print(kmeans_result3$tot.withinss)
  print(kmeans_result4$tot.withinss)
  print(kmeans_result5$tot.withinss)
}





# final look to see how it went (AFTER FINDING BEST VALUE ABOVE)
table(kmeans_result3$cluster,data$Species)


# 5.1
# Clear environment
rm(list = ls()) 

data <- read.table("data 5.1\\uscrime.txt", header=TRUE)

#install.packages("outliers")
library(outliers)

?grubbs.test
outlier_result = grubbs.test(data$Crime)
outlier_result$p.value


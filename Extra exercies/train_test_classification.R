library(tsfeatures)
library(caret)
library(datasets)
data("AirPassengers")

# divide data by month and prepare target (summer/winter)
ts <- c()
for(i in 1:12){ts <- cbind(ts , AirPassengers[seq(i,length(AirPassengers),by = 12)])}
target = as.factor(rep(c("w", "s", "w"), c(3,6,3)))

# extract features
feature_matrix <- as.data.frame(tsfeatures(ts, features = c("stl_features")))

# train -test -split
set.seed (1)
train <- createDataPartition(y=target , p=0.75, list=FALSE)

# train a classifier
mod_rf <- caret ::train(x = feature_matrix[train ,], y = target[train], method = "rf") # random forest ("rf")

# predict
pred <- predict(mod_rf, newdata = feature_matrix[-train ,])
confusionMatrix(pred , reference = target[-train])

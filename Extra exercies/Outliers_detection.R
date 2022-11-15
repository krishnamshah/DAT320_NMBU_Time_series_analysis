library(datasets)
data("faithful")

# z-scores & Mahalanobis distance
z <- scale(faithful) %>% as.data.frame ()

mahalanobis(z, center = c(0,0), cov = cov(faithful))

# DBSCAN & LOF
library(dbscan)
dbscan(faithful , eps = 1)$cluster == 0
lof(faithful , minPts = 5)

# Isolation forest
library(isotree)
iso_mod <- isolation.forest(faithful)
predict(iso_mod , newdata = faithful)

# one -class SVM
library(e1071)
svm_mod <- svm(faithful , type = ’one -classification ’)
predict(svm_mod , newdata = faithful)
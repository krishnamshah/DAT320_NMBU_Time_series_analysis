
library(jmotif)
ecg <- ts(ecg0606)

# brute -force discord detection
disc_bf <- find_discords_brute_force(ecg , w_size = 100, discords_num = 1)

# HOT -SAX discord detection
disc_hs <- find_discords_hotsax(ecg , w_size = 100, paa_size= 3, a_size = 3, n_threshold = 0.01, discords_num = 1)

# distance -based approach
window_size = 148
windows <- runner(ecg , at = seq(window_size , length(ecg), by= window_size), k = window_size)
reference <- rowMeans(windows)
dist <- colMeans(abs(sweep(windows , 1, STATS = reference)))

# model -based approach
window_size = 148

pred <- runner(ecg , f = function(x){
x <- ts(x, frequency = window_size)
pred <- snaive(x, h = window_size)$mean
return(pred)},
at = seq(window_size , length(ecg)-window_size , by = window_size), k = window_size)

reference <- runner(ecg , at = seq(2 * window_size , length(ecg), by = window_size), k = window_size)

error <- c()
for(i in 1:ncol(pred)){
  error[i] <- sum(abs(pred[,i] - reference[,i]))
}

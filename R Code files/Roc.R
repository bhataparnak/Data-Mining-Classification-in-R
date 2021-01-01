library(ROCR)
rocr_data <-data(ROCR.simple)
pred <- prediction( ROCR.simple$predictions, ROCR.simple$labels)
perf <- performance(pred,"tpr","fpr")
plot(perf)

library(FSelector)
pred1 <- prediction(as.numeric(p1),test_data$y)
infogain_perf <- performance(pred1,"tpr","fpr")
plot(infogain_perf,add=T, col="blue",lwd=2)

pred2<-  prediction(as.numeric(predict_gini_tree),test_data$y)
infogain_perf1 <- performance(pred2,"tpr","fpr")
plot(infogain_perf1,col="green", lwd=2)

pred3<-  prediction(as.numeric(predict_tree_data),test_data$y)
infogain_perf2 <- performance(pred3,"tpr","fpr")
plot(infogain_perf2,col="red", lwd=2)


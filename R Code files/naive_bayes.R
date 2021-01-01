bank_details <- read.csv(file = "F:/UTA/Courses/Assignments/Data Mining/bank-additional-full.csv", 
                         header = TRUE, sep = ";",stringsAsFactors = FALSE,
                         na.strings = c("NA","N/A","Unknown","unknown",".P"))
sum(is.na(bank_details))
bank_details_new <- droplevels(na.omit(bank_details))
library(dplyr)
bank_details_new %>% select(c(-3,-5,-6,-7,-8)) -> bank_details_new
set.seed(20)
bank_sample_data <- bank_details_new [sample(nrow(bank_details_new),10000),]
data_convert_int <- sample(2,nrow(bank_sample_data),replace = TRUE,prob = c(0.8,0.2))
training_data <- bank_sample_data [data_convert_int == 1,]
test_data <- bank_sample_data [data_convert_int == 2,]

library(e1071)
library(naivebayes)
library(psych)
model<- naive_bayes(y ~ ., data= training_data, usekernel = T)
plot(model)

accuracy_fscore <- function(matrix1){
  true_positive <- matrix1[1,1]
  true_negative <- matrix1[2,2]
  false_positive <- matrix1[1,2]
  false_negative <- matrix1[2,1]
  
  accuracy <- (true_positive+true_negative)/(true_positive+true_negative+false_positive+false_negative)
  precision <- true_positive/(true_positive+false_positive)
  recall  <- true_positive/(true_positive+false_negative)
  fscore <- 2*(precision*recall)/(precision+recall)
  vector <- c(accuracy,precision,recall,fscore)
  return(vector)
}

p1<- predict(model, test_data,type = "prob")
(tab1<- table(p2, test_data$y))
# p1      no  yes
# no  1609  128
# yes  159  126
1- sum(diag(tab1))/sum(tab1)
# [1] 0.1419387


print(accuracy_fscore(tab1))

#[1] 0.8580613 0.9263097 0.9100679 0.9181170


ex<-bank_sample_data
ex$duration<-NULL
ex_convert_int <- sample(2,nrow(ex),replace = TRUE,prob = c(0.8,0.2))
train <- ex [ex_convert_int == 1,]
test <- ex [ex_convert_int == 2,]

model1<- naive_bayes(y ~ ., data= train, usekernel = T)
plot(model1)


p2<- predict(model1, test)
(tab2<- table(p2, test$y))
# p2      no  yes
# no  1608  122
# yes  150  122
1- sum(diag(tab2))/sum(tab2)
# [1] 0.1358641


print(accuracy_fscore(tab2))

#0.8641359 0.9294798 0.9146758 0.9220183



ex$age<-NULL
ex$campaign<-NULL
ex$job<-NULL
ex$day_of_week<-NULL
ex$previous<-NULL
ex$month<-NULL
ex$poutcome<-NULL
ex$education<-NULL
ex$cons.conf.idx<-NULL
ex$euribor3m<-NULL
ex$emp.var.rate<-NULL

ex_convert_int <- sample(2,nrow(ex),replace = TRUE,prob = c(0.8,0.2))
train <- ex [ex_convert_int == 1,]
test <- ex [ex_convert_int == 2,]

model1<- naive_bayes(y ~ ., data= train, usekernel = T)
plot(model1)


p3<- predict(model1, test)
(tab3<- table(p3, test$y))
# p3      no  yes
# no  1754  215
# yes   28   52
1- sum(diag(tab3))/sum(tab3)
# [1] 0.1185944


print(accuracy_fscore(tab3))
# [1] 0.8814056 0.8908075 0.9842873 0.9352173


#50-50 split
bank_details <- read.csv(file = "F:/UTA/Courses/Assignments/Data Mining/bank-additional-full.csv", 
                         header = TRUE, sep = ";",stringsAsFactors = FALSE,
                         na.strings = c("NA","N/A","Unknown","unknown",".P"))
sum(is.na(bank_details))
bank_details_new <- droplevels(na.omit(bank_details))
library(dplyr)
bank_details_new %>% select(c(-3,-5,-6,-7,-8)) -> bank_details_new
set.seed(20)
bank_sample_data <- bank_details_new [sample(nrow(bank_details_new),10000),]
data_convert_int <- sample(2,nrow(bank_sample_data),replace = TRUE,prob = c(0.5,0.5))
training_data <- bank_sample_data [data_convert_int == 1,]
test_data <- bank_sample_data [data_convert_int == 2,]

library(e1071)
library(naivebayes)
library(psych)
model<- naive_bayes(y ~ ., data= training_data, usekernel = T)
plot(model)

p1<- predict(model, test_data)
(tab1<- table(p1, test_data$y))
# p2      no  yes
# no  3988  347
# yes  305  302
1- sum(diag(tab1))/sum(tab1)
# [1] 0.1319304

accuracy_fscore <- function(matrix1){
  true_positive <- matrix1[1,1]
  true_negative <- matrix1[2,2]
  false_positive <- matrix1[1,2]
  false_negative <- matrix1[2,1]
  
  accuracy <- (true_positive+true_negative)/(true_positive+true_negative+false_positive+false_negative)
  precision <- true_positive/(true_positive+false_positive)
  recall  <- true_positive/(true_positive+false_negative)
  fscore <- 2*(precision*recall)/(precision+recall)
  vector <- c(accuracy,precision,recall,fscore)
  return(vector)
}
print(accuracy_fscore(tab1))

# [1] 0.8680696 0.9199539 0.9289541 0.9244321
ex<-bank_sample_data
ex$duration<-NULL
ex_convert_int <- sample(2,nrow(ex),replace = TRUE,prob = c(0.5,0.5))
train <- ex [ex_convert_int == 1,]
test <- ex [ex_convert_int == 2,]

model1<- naive_bayes(y ~ ., data= train, usekernel = T)
plot(model1)

p2<- predict(model1, test)
(tab2<- table(p2, test$y))

1- sum(diag(tab2))/sum(tab2)


print(accuracy_fscore(tab2))
# [1] 0.8640641 0.9185017 0.9267064 0.9225858

ex$age<-NULL
ex$campaign<-NULL
ex$job<-NULL
ex$day_of_week<-NULL
ex$previous<-NULL
ex$month<-NULL
ex$poutcome<-NULL
ex$education<-NULL
ex$cons.conf.idx<-NULL
ex$euribor3m<-NULL
ex$emp.var.rate<-NULL

ex_convert_int <- sample(2,nrow(ex),replace = TRUE,prob = c(0.5,0.5))
train <- ex [ex_convert_int == 1,]
test <- ex [ex_convert_int == 2,]

model1<- naive_bayes(y ~ ., data= train, usekernel = T)
plot(model1)

p3<- predict(model1, test)
(tab3<- table(p3, test$y))

1- sum(diag(tab3))/sum(tab3)


print(accuracy_fscore(tab3))
#  0.8874398 0.8931234 0.9896718 0.9389222
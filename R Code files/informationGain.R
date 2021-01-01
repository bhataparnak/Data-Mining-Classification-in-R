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

library(rpart)
library(rpart.plot)
tree_data<- rpart(y ~ ., data = training_data, parms = list(split='information'), minsplit=2, minbucket=1, cp=-1)
library(FSelector)
library(caret)
info_gain <- information.gain(y ~ ., data=training_data, unit="log2")
print(info_gain)
#                  attr_importance
# age                0.016221717
# job                0.016696056
# education          0.002677582
# month              0.043929312
# day_of_week        0.001747176
# duration           0.103043229
# campaign           0.003338100
# pdays              0.051736432
# previous           0.027587124
# poutcome           0.050655212
# emp.var.rate       0.086202774
# cons.price.idx     0.112392960
# cons.conf.idx      0.109457056
# euribor3m          0.100781344
# nr.employed        0.100074324
summary(tree_data)
print(tree_data)

# n= 7978 
# 
# node), split, n, loss, yval, (yprob)
# * denotes terminal node
# 
# 1) root 7978 1015 no (0.87277513 0.12722487)  
# 2) nr.employed>=5087.65 6802  488 no (0.92825640 0.07174360)  
# 4) duration< 447.5 5784  136 no (0.97648686 0.02351314) *
#   5) duration>=447.5 1018  352 no (0.65422397 0.34577603)  
# 10) duration< 689.5 588  133 no (0.77380952 0.22619048) *
#   11) duration>=689.5 430  211 yes (0.49069767 0.50930233)  
# 22) duration< 953.5 237  102 no (0.56962025 0.43037975)  
# 44) euribor3m>=1.4025 193   73 no (0.62176166 0.37823834) *
#   45) euribor3m< 1.4025 44   15 yes (0.34090909 0.65909091) *
#   23) duration>=953.5 193   76 yes (0.39378238 0.60621762) *
#   3) nr.employed< 5087.65 1176  527 no (0.55187075 0.44812925)  
# 6) duration< 172.5 440   72 no (0.83636364 0.16363636) *
#   7) duration>=172.5 736  281 yes (0.38179348 0.61820652)  
# 14) poutcome=failure,nonexistent 528  246 yes (0.46590909 0.53409091)  
# 28) duration< 208.5 90   28 no (0.68888889 0.31111111) *
#   29) duration>=208.5 438  184 yes (0.42009132 0.57990868)  
# 58) cons.price.idx< 92.681 177   84 no (0.52542373 0.47457627)  
# 116) day_of_week=mon,thu,wed 105   42 no (0.60000000 0.40000000) *
#   117) day_of_week=fri,tue 72   30 yes (0.41666667 0.58333333) *
#     59) cons.price.idx>=92.681 261   91 yes (0.34865900 0.65134100) *
#   15) poutcome=success 208   35 yes (0.16826923 0.83173077) *

rpart.plot(tree_data, extra = 1)

predict_tree_data <-predict(tree_data, test_data, type = "class")
(confusion_matrix_IG<- table(predict_tree_data, test_data$y))
# predict_tree_data   no  yes
# no  1708  129
# yes   60  125

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
print(accuracy_fscore(confusion_matrix_IG))
#[1] 0.9065282 0.9297768 0.9660633 0.9475728

ex<-bank_sample_data
ex$duration<-NULL
set.seed(20)
ex_convert_int <- sample(2,nrow(ex),replace = TRUE,prob = c(0.8,0.2))
train <- ex [ex_convert_int == 1,]
test <- ex [ex_convert_int == 2,]

tree_data1<- rpart(y ~ ., data = train, parms = list(split='information'), minsplit=2, minbucket=1)
info_gain1 <- information.gain(y ~ ., data=train, unit="log2")
print(info_gain1)
summary(tree_data1)
print(tree_data1)


rpart.plot(tree_data1, extra = 1)

predict_tree_data1 <-predict(tree_data1, test, type = "class")
(confusion_matrix_IG1<- table(predict_tree_data1, test$y))
# predict_tree_data1   no  yes
# no  1749  219
# yes   22   51

print(accuracy_fscore(confusion_matrix_IG1))

# [1] 0.8819206 0.8887195 0.9875776 0.9355443


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
set.seed(20)
ex_convert_int <- sample(2,nrow(ex),replace = TRUE,prob = c(0.8,0.2))
train <- ex [ex_convert_int == 1,]
test <- ex [ex_convert_int == 2,]

tree_data1<- rpart(y ~ ., data = train, parms = list(split='information'), minsplit=2, minbucket=1)
summary(tree_data1)
print(tree_data1)


rpart.plot(tree_data1, extra = 1)

predict_tree_data1 <-predict(tree_data1, test, type = "class")
(confusion_matrix_IG1<- table(predict_tree_data1, test$y))
# predict_tree_data1   no  yes
# no  1749  219
# yes   22   51

print(accuracy_fscore(confusion_matrix_IG1))
#[1] 0.8819206 0.8887195 0.9875776 0.9355443


info_gain1 <- information.gain(y ~ ., data=train, unit="log2")
print(info_gain1)
# attr_importance
# pdays               0.05318502
# emp.var.rate        0.08111709
# cons.price.idx      0.10570617
# euribor3m           0.10122340
# nr.employed         0.09630574


#split ratio 50-50


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

library(rpart)
library(rpart.plot)
tree_data<- rpart(y ~ ., data = training_data, parms = list(split='information'), minsplit=2, minbucket=1)
info_gain <- information.gain(y ~ ., data=training_data, unit="log2")
print(info_gain)
# attr_importance
# age                0.018949352
# job                0.018526407
# education          0.002746288
# month              0.041254530
# day_of_week        0.001508146
# duration           0.113825582
# campaign           0.004624565
# pdays              0.052599694
# previous           0.025075471
# poutcome           0.048762021
# emp.var.rate       0.073246028
# cons.price.idx     0.106651410
# cons.conf.idx      0.105323786
# euribor3m          0.094012520
# nr.employed        0.092440029
summary(tree_data)
print(tree_data)

# n= 5058 
# 
# node), split, n, loss, yval, (yprob)
# * denotes terminal node
# 
# 1) root 5058 620 no (0.877421906 0.122578094)  
# 2) nr.employed>=5087.65 4345 310 no (0.928653625 0.071346375)  
# 4) duration< 453.5 3728  87 no (0.976663090 0.023336910)  
# 8) month=aug,dec,jul,jun,may,nov 3392  24 no (0.992924528 0.007075472) *
#   9) month=apr,mar,oct 336  63 no (0.812500000 0.187500000)  
#   18) euribor3m< 1.515 275  32 no (0.883636364 0.116363636) *
#   19) euribor3m>=1.515 61  30 yes (0.491803279 0.508196721)  
# 38) duration< 189 41  11 no (0.731707317 0.268292683) *
#   39) duration>=189 20   0 yes (0.000000000 1.000000000) *
#   5) duration>=453.5 617 223 no (0.638573744 0.361426256)  
# 10) duration< 682.5 333  72 no (0.783783784 0.216216216) *
#   11) duration>=682.5 284 133 yes (0.468309859 0.531690141)  
# 22) campaign>=5.5 29   8 no (0.724137931 0.275862069) *
#   23) campaign< 5.5 255 112 yes (0.439215686 0.560784314) *
#   3) nr.employed< 5087.65 713 310 no (0.565217391 0.434782609)  
# 6) duration< 165.5 270  35 no (0.870370370 0.129629630) *
#   7) duration>=165.5 443 168 yes (0.379232506 0.620767494)  
# 14) pdays>=513 303 144 yes (0.475247525 0.524752475)  
# 28) cons.price.idx< 92.681 124  46 no (0.629032258 0.370967742) *
#   29) cons.price.idx>=92.681 179  66 yes (0.368715084 0.631284916)  
# 58) duration< 385 116  57 yes (0.491379310 0.508620690)  
# 116) month=jun,mar,nov,oct,sep 85  36 no (0.576470588 0.423529412)  
# 232) duration< 253 44  12 no (0.727272727 0.272727273) *
#   233) duration>=253 41  17 yes (0.414634146 0.585365854) *
#   117) month=apr,aug,dec,jul,may 31   8 yes (0.258064516 0.741935484) *
#     59) duration>=385 63   9 yes (0.142857143 0.857142857) *
#   15) pdays< 513 140  24 yes (0.171428571 0.828571429) *

rpart.plot(tree_data, extra = 1)

predict_tree_data <-predict(tree_data, test_data, type = "class")
(confusion_matrix_IG<- table(predict_tree_data, test_data$y))
# predict_tree_data   no  yes
# no  4073  308
# yes  220  341
print(accuracy_fscore(confusion_matrix_IG))
#[1] 0.8931607 0.9296964 0.9487538 0.9391284

ex<-bank_sample_data
ex$duration<-NULL
set.seed(20)
ex_convert_int <- sample(2,nrow(ex),replace = TRUE,prob = c(0.5,0.5))
train <- ex [ex_convert_int == 1,]
test <- ex [ex_convert_int == 2,]

tree_data1<- rpart(y ~ ., data = train, parms = list(split='information'), minsplit=2, minbucket=1)
info_gain1 <- information.gain(y ~ ., data=train, unit="log2")
print(info_gain1)
summary(tree_data1)
print(tree_data1)


rpart.plot(tree_data1, extra = 1)

predict_tree_data1 <-predict(tree_data1, test, type = "class")
(confusion_matrix_IG1<- table(predict_tree_data1, test$y))


print(accuracy_fscore(confusion_matrix_IG1))

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
set.seed(20)
ex_convert_int <- sample(2,nrow(ex),replace = TRUE,prob = c(0.5,0.5))
train <- ex [ex_convert_int == 1,]
test <- ex [ex_convert_int == 2,]

tree_data1<- rpart(y ~ ., data = train, parms = list(split='information'), minsplit=2, minbucket=1)
summary(tree_data1)
print(tree_data1)


rpart.plot(tree_data1, extra = 1)

predict_tree_data1 <-predict(tree_data1, test, type = "class")
(confusion_matrix_IG1<- table(predict_tree_data1, test$y))


print(accuracy_fscore(confusion_matrix_IG1))
#[1] 0.8924088 0.8988415 0.9883985 0.9414951




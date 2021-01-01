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
gini_tree <- rpart(
  y ~ ., 
  data = training_data, 
  parms = list(split = 'gini'), 
  minsplit = 2, 
  minbucket = 1)
printcp(gini_tree)

# Classification tree:
#   rpart(formula = y ~ ., data = training_data, parms = list(split = "gini"), 
#         minsplit = 2, minbucket = 1)
# 
# Variables actually used in tree construction:
#   [1] cons.price.idx day_of_week    duration       nr.employed    poutcome      
# 
# Root node error: 1015/7978 = 0.12722
# 
# n= 7978 
# 
# CP nsplit rel error  xerror     xstd
# 1 0.088177      0   1.00000 1.00000 0.029324
# 2 0.018227      2   0.82365 0.83941 0.027179
# 3 0.013793      4   0.78719 0.83350 0.027094
# 4 0.010345      6   0.75961 0.83744 0.027151
# 5 0.010000      8   0.73892 0.81281 0.026795

gini_tree$variable.importance
# 
# duration    nr.employed      euribor3m   emp.var.rate  cons.conf.idx 
# 342.2062000    287.8817937    251.5620796    165.4442546    153.4260522 
# cons.price.idx          month       poutcome          pdays       previous 
# 125.4424080     81.3123468     25.9425665     23.3612166      6.3243073 
# day_of_week      education            job            age 
# 3.3635500      0.8980557      0.5253249      0.4426080 


rpart.plot(gini_tree, extra = 101)
print(gini_tree)
# n= 7978 
# 
# node), split, n, loss, yval, (yprob)
# * denotes terminal node
# 
# 1) root 7978 1015 no (0.8727751 0.1272249)  
# 2) nr.employed>=5087.65 6802  488 no (0.9282564 0.0717436)  
# 4) duration< 493.5 5932  158 no (0.9733648 0.0266352) *
#   5) duration>=493.5 870  330 no (0.6206897 0.3793103)  
# 10) duration< 801 573  163 no (0.7155323 0.2844677) *
#   11) duration>=801 297  130 yes (0.4377104 0.5622896) *
#   3) nr.employed< 5087.65 1176  527 no (0.5518707 0.4481293)  
# 6) duration< 177.5 463   81 no (0.8250540 0.1749460) *
#   7) duration>=177.5 713  267 yes (0.3744741 0.6255259)  
# 14) poutcome=failure,nonexistent 512  235 yes (0.4589844 0.5410156)  
# 28) duration< 208.5 74   23 no (0.6891892 0.3108108) *
#   29) duration>=208.5 438  184 yes (0.4200913 0.5799087)  
# 58) cons.price.idx< 92.681 177   84 no (0.5254237 0.4745763)  
# 116) day_of_week=mon,thu,wed 105   42 no (0.6000000 0.4000000) *
#   117) day_of_week=fri,tue 72   30 yes (0.4166667 0.5833333) *
#     59) cons.price.idx>=92.681 261   91 yes (0.3486590 0.6513410) *
#   15) poutcome=success 201   32 yes (0.1592040 0.8407960) *

predict_gini_tree <-predict(gini_tree, test_data, type = "class")
(matrix_gini_table<- table(predict_gini_tree, test_data$y))

# predict_gini_tree   no  yes
# no  1698  122
# yes   70  132

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
print(accuracy_fscore(matrix_gini_table))
#[1] 0.9050445 0.9329670 0.9604072 0.9464883

gini_tree_new <- rpart(
  y ~ ., 
  data = training_data, 
  parms = list(split = 'gini'), 
  minsplit = 2, 
  minbucket = 1, cp=0.05)
printcp(gini_tree_new)
# Classification tree:
#   rpart(formula = y ~ ., data = training_data, parms = list(split = "gini"), 
#         minsplit = 2, minbucket = 1, cp = 0.05)
# 
# Variables actually used in tree construction:
#   [1] duration    nr.employed
# 
# Root node error: 1015/7978 = 0.12722
# 
# n= 7978 
# 
# CP nsplit rel error  xerror     xstd
# 1 0.088177      0   1.00000 1.00000 0.029324
# 2 0.050000      2   0.82365 0.83941 0.027179

gini_tree_new$variable.importance
# nr.employed      euribor3m   emp.var.rate  cons.conf.idx cons.price.idx 
# 284.0835068    249.8593475    161.6087296    147.3562408    118.8512630 
# duration          month      education    day_of_week            job 
# 113.9821659     77.7847697      0.7385454      0.4923636      0.2461818

rpart.plot(gini_tree_new, extra = 101)
print(gini_tree_new)
# n= 7978 
# 
# node), split, n, loss, yval, (yprob)
# * denotes terminal node
# 
# 1) root 7978 1015 no (0.8727751 0.1272249)  
# 2) nr.employed>=5087.65 6802  488 no (0.9282564 0.0717436) *
#   3) nr.employed< 5087.65 1176  527 no (0.5518707 0.4481293)  
# 6) duration< 177.5 463   81 no (0.8250540 0.1749460) *
#   7) duration>=177.5 713  267 yes (0.3744741 0.6255259) *

predict_gini_tree_new <-predict(gini_tree_new, test_data, type = "class")
(matrix_gini_new_table<- table(predict_gini_tree_new, test_data$y))

# predict_gini_tree_new   no  yes
# no  1701  150
# yes   67  104


print(accuracy_fscore(matrix_gini_new_table))
#[1] 0.8926805 0.9189627 0.9621041 0.9400387


#dropping duration attribute to see how much accuracy it affects

ex<-bank_sample_data
ex$duration<-NULL

set.seed(20)
ex_convert_int <- sample(2,nrow(ex),replace = TRUE,prob = c(0.8,0.2))
train <- ex [ex_convert_int == 1,]
test <- ex [ex_convert_int == 2,]

library(rpart)
library(rpart.plot)
ex_gini_tree <- rpart(
  y ~ ., 
  data = train, 
  parms = list(split = 'gini'), 
  minsplit = 2, 
  minbucket = 1)
printcp(ex_gini_tree)

ex_gini_tree$variable.importance

rpart.plot(ex_gini_tree, extra = 101)
print(ex_gini_tree)

predict_ex_gini_tree <-predict(ex_gini_tree, test, type = "class")
(matrix_ex_gini_table<- table(predict_ex_gini_tree, test$y))

print(accuracy_fscore(matrix_ex_gini_table))
# [1] 0.8819206 0.8887195 0.9875776 0.9355443

ex_gini_tree_new <- rpart(
  y ~ ., 
  data = train, 
  parms = list(split = 'gini'), 
  minsplit = 2, 
  minbucket = 1, cp=0.05)
printcp(ex_gini_tree_new)

ex_gini_tree_new$variable.importance

rpart.plot(ex_gini_tree_new, extra = 101)
print(ex_gini_tree_new)

predict_ex_gini_new_tree <-predict(ex_gini_tree_new, test, type = "class")
(matrix_ex_gini_new_table<- table(predict_ex_gini_new_tree, test$y))

print(accuracy_fscore(matrix_ex_gini_new_table))

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

library(rpart)
library(rpart.plot)
ex_gini_tree <- rpart(
  y ~ ., 
  data = train, 
  parms = list(split = 'gini'), 
  minsplit = 2, 
  minbucket = 1)
printcp(ex_gini_tree)
# Classification tree:
#   rpart(formula = y ~ ., data = train, parms = list(split = "gini"), 
#         minsplit = 2, minbucket = 1)
# 
# Variables actually used in tree construction:
#   [1] nr.employed pdays      
# 
# Root node error: 999/7959 = 0.12552
# 
# n= 7959 
# 
# CP nsplit rel error  xerror     xstd
# 1 0.065065      0   1.00000 1.00000 0.029586
# 2 0.010000      2   0.86987 0.87487 0.027921

ex_gini_tree$variable.importance
# nr.employed      euribor3m   emp.var.rate          pdays cons.price.idx 
# 271.7548       237.1378       153.5391       118.4771       115.6253 


rpart.plot(ex_gini_tree, extra = 101)
print(ex_gini_tree)
# n= 7959 
# 
# node), split, n, loss, yval, (yprob)
# * denotes terminal node
# 
# 1) root 7959 999 no (0.87448172 0.12551828)  
# 2) nr.employed>=5087.65 6805 488 no (0.92828802 0.07171198) *
#   3) nr.employed< 5087.65 1154 511 no (0.55719237 0.44280763)  
# 6) pdays>=13.5 882 310 no (0.64852608 0.35147392) *
#   7) pdays< 13.5 272  71 yes (0.26102941 0.73897059) *

predict_ex_gini_tree <-predict(ex_gini_tree, test, type = "class")
(matrix_ex_gini_table<- table(predict_ex_gini_tree, test$y))

# predict_ex_gini_tree   no  yes
# no  1749  219
# yes   22   51


print(accuracy_fscore(matrix_ex_gini_table))
#[1] 0.8819206 0.8887195 0.9875776 0.9355443

gini_tree_new_ex <- rpart(
  y ~ ., 
  data = train, 
  parms = list(split = 'gini'), 
  minsplit = 2, 
  minbucket = 1, cp=0.03)
printcp(gini_tree_new_ex)

gini_tree_new_ex$variable.importance

rpart.plot(gini_tree_new_ex, extra = 101)
print(gini_tree_new_ex)

predict_gini_tree_new_ex <-predict(gini_tree_new_ex, test, type = "class")
(matrix_gini_new_table_ex<- table(predict_gini_tree_new_ex, test$y))

# predict_gini_tree_new_ex   no  yes
# no  1749  219
# yes   22   51

print(accuracy_fscore(matrix_gini_new_table_ex))
# [1] 0.8819206 0.8887195 0.9875776 0.9355443

#50-50 split

bank_details <- read.csv(file = "F:/UTA/Courses/Assignments/Data Mining/bank-additional-full.csv", 
                         header = TRUE, sep = ";",stringsAsFactors = FALSE,
                         na.strings = c("NA","N/A","Unknown","unknown",".P"))
sum(is.na(bank_details))
bank_details_new <- droplevels(na.omit(bank_details))
library(dplyr)
bank_details_new %>% select(c(-3,-5,-6,-7,-8)) -> bank_details_new
bank_sample_data <- bank_details_new [sample(nrow(bank_details_new),10000),]
set.seed(20)
data_convert_int <- sample(2,nrow(bank_sample_data),replace = TRUE,prob = c(0.5,0.5))
training_data <- bank_sample_data [data_convert_int == 1,]
test_data <- bank_sample_data [data_convert_int == 2,]

library(rpart)
library(rpart.plot)
gini_tree <- rpart(
  y ~ ., 
  data = training_data, 
  parms = list(split = 'gini'), 
  minsplit = 2, 
  minbucket = 1)
printcp(gini_tree)
# Classification tree:
#   rpart(formula = y ~ ., data = training_data, parms = list(split = "gini"), 
#         minsplit = 2, minbucket = 1)
# 
# Variables actually used in tree construction:
#   [1] campaign       cons.price.idx duration       nr.employed    pdays         
# 
# Root node error: 630/4991 = 0.12623
# 
# n= 4991 
# 
# CP nsplit rel error  xerror     xstd
# 1 0.097619      0   1.00000 1.00000 0.037242
# 2 0.017460      2   0.80476 0.80476 0.033877
# 3 0.011111      5   0.75238 0.80317 0.033847
# 4 0.010000      9   0.70794 0.81587 0.034083

gini_tree$variable.importance
# 
# duration    nr.employed      euribor3m   emp.var.rate  cons.conf.idx 
# 342.2062000    287.8817937    251.5620796    165.4442546    153.4260522 
# cons.price.idx          month       poutcome          pdays       previous 
# 125.4424080     81.3123468     25.9425665     23.3612166      6.3243073 
# day_of_week      education            job            age 
# 3.3635500      0.8980557      0.5253249      0.4426080 


rpart.plot(gini_tree, extra = 101)
print(gini_tree)
# n= 4991 
# 
# node), split, n, loss, yval, (yprob)
# * denotes terminal node
# 
# 1) root 4991 630 no (0.87377279 0.12622721)  
# 2) nr.employed>=5087.65 4253 301 no (0.92922643 0.07077357)  
# 4) duration< 520.5 3771 106 no (0.97189075 0.02810925) *
#   5) duration>=520.5 482 195 no (0.59543568 0.40456432)  
# 10) duration< 685.5 206  58 no (0.71844660 0.28155340) *
#   11) duration>=685.5 276 137 no (0.50362319 0.49637681)  
# 22) duration< 951.5 161  69 no (0.57142857 0.42857143) *
#   23) duration>=951.5 115  47 yes (0.40869565 0.59130435) *
#   3) nr.employed< 5087.65 738 329 no (0.55420054 0.44579946)  
# 6) duration< 172.5 295  46 no (0.84406780 0.15593220) *
#   7) duration>=172.5 443 160 yes (0.36117381 0.63882619)  
# 14) pdays>=513 298 141 yes (0.47315436 0.52684564)  
# 28) duration< 209.5 56  17 no (0.69642857 0.30357143) *
#   29) duration>=209.5 242 102 yes (0.42148760 0.57851240)  
# 58) cons.price.idx< 92.681 99  44 no (0.55555556 0.44444444)  
# 116) campaign< 1.5 62  22 no (0.64516129 0.35483871) *
#   117) campaign>=1.5 37  15 yes (0.40540541 0.59459459) *
#   59) cons.price.idx>=92.681 143  47 yes (0.32867133 0.67132867) *
#   15) pdays< 513 145  19 yes (0.13103448 0.86896552) *
predict_gini_tree <-predict(gini_tree, test_data, type = "class")
(matrix_gini_table<- table(predict_gini_tree, test_data$y))

# predict_gini_tree   no  yes
# no  4206  332
# yes  164  307
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
print(accuracy_fscore(matrix_gini_table))
#[1] 0.9009782 0.9268400 0.9624714 0.9443197

gini_tree_new <- rpart(
  y ~ ., 
  data = training_data, 
  parms = list(split = 'gini'), 
  minsplit = 2, 
  minbucket = 1, cp=-1)
printcp(gini_tree_new)
# Classification tree:
#   rpart(formula = y ~ ., data = training_data, parms = list(split = "gini"), 
#         minsplit = 2, minbucket = 1, cp = 0.05)
# 
# Variables actually used in tree construction:
#   [1] duration    nr.employed
# 
# Root node error: 630/4991 = 0.12623
# 
# n= 4991 
# 
# CP nsplit rel error  xerror     xstd
# 1 0.097619      0   1.00000 1.00000 0.037242
# 2 0.050000      2   0.80476 0.81746 0.034113

gini_tree_new$variable.importance
# nr.employed      euribor3m   emp.var.rate  cons.conf.idx       duration 
# 176.8954502    158.0839145     98.7546415     91.8034653     82.5853401 
# cons.price.idx          month            age            job       campaign 
# 73.3468940     48.6582336      0.8398509      0.8398509      0.5599006 

rpart.plot(gini_tree_new, extra = 101)
print(gini_tree_new)
# n= 4991 
# 
# node), split, n, loss, yval, (yprob)
# * denotes terminal node
# 
# 1) root 4991 630 no (0.87377279 0.12622721)  
# 2) nr.employed>=5087.65 4253 301 no (0.92922643 0.07077357) *
#   3) nr.employed< 5087.65 738 329 no (0.55420054 0.44579946)  
# 6) duration< 172.5 295  46 no (0.84406780 0.15593220) *
#   7) duration>=172.5 443 160 yes (0.36117381 0.63882619) *
  

predict_gini_tree_new <-predict(gini_tree_new, test_data, type = "class")
(matrix_gini_new_table<- table(predict_gini_tree_new, test_data$y))

# predict_gini_tree_new   no  yes
# no  4180  361
# yes  190  278

print(accuracy_fscore(matrix_gini_new_table))
#[1] 0.8899980 0.9205021 0.9565217 0.9381663

ex<-bank_sample_data
ex$duration<-NULL
set.seed(20)
ex_convert_int <- sample(2,nrow(ex),replace = TRUE,prob = c(0.5,0.5))
train <- ex [ex_convert_int == 1,]
test <- ex [ex_convert_int == 2,]

library(rpart)
library(rpart.plot)
ex_gini_tree <- rpart(
  y ~ ., 
  data = train, 
  parms = list(split = 'gini'), 
  minsplit = 2, 
  minbucket = 1)
printcp(ex_gini_tree)
# Classification tree:
#   rpart(formula = y ~ ., data = train, parms = list(split = "gini"), 
#         minsplit = 2, minbucket = 1)
# 
# Variables actually used in tree construction:
#   [1] nr.employed pdays      
# 
# Root node error: 646/4981 = 0.12969
# 
# n= 4981 
# 
# CP nsplit rel error  xerror     xstd
# 1 0.061146      0   1.00000 1.00000 0.036705
# 2 0.010000      2   0.87771 0.88545 0.034832

ex_gini_tree$variable.importance
# nr.employed          pdays cons.price.idx 
# 161.65220       70.43357       68.65537 


rpart.plot(ex_gini_tree, extra = 101)
print(ex_gini_tree)
# n= 4981 
# 
# node), split, n, loss, yval, (yprob)
# * denotes terminal node
# 
# 1) root 4981 646 no (0.87030717 0.12969283)  
# 2) nr.employed>=5087.65 4204 315 no (0.92507136 0.07492864) *
#   3) nr.employed< 5087.65 777 331 no (0.57400257 0.42599743)  
# 6) pdays>=17 604 205 no (0.66059603 0.33940397) *
#   7) pdays< 17 173  47 yes (0.27167630 0.72832370) *

predict_ex_gini_tree <-predict(ex_gini_tree, test, type = "class")
(matrix_ex_gini_table<- table(predict_ex_gini_tree, test$y))

# predict_ex_gini_tree   no  yes
# no  4345  489
# yes   51  134

print(accuracy_fscore(matrix_ex_gini_table))
#[1] 0.8924088 0.8988415 0.9883985 0.9414951

gini_tree_new_ex <- rpart(
  y ~ ., 
  data = train, 
  parms = list(split = 'gini'), 
  minsplit = 2, 
  minbucket = 1, cp=0.03)
printcp(gini_tree_new_ex)

gini_tree_new_ex$variable.importance

rpart.plot(gini_tree_new_ex, extra = 101)
print(gini_tree_new_ex)

predict_gini_tree_new_ex <-predict(gini_tree_new_ex, test, type = "class")
(matrix_gini_new_table_ex<- table(predict_gini_tree_new_ex, test$y))

# predict_gini_tree_new_ex   no  yes
# no  4345  489
# yes   51  134

print(accuracy_fscore(matrix_gini_new_table_ex))
# [1]  0.8924088 0.8988415 0.9883985 0.9414951



ex<-bank_sample_data
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

library(rpart)
library(rpart.plot)
ex_gini_tree <- rpart(
  y ~ ., 
  data = train, 
  parms = list(split = 'gini'), 
  minsplit = 2, 
  minbucket = 1)
printcp(ex_gini_tree)
# Classification tree:
#   rpart(formula = y ~ ., data = train, parms = list(split = "gini"), 
#         minsplit = 2, minbucket = 1)
# 
# Variables actually used in tree construction:
#   [1] nr.employed pdays      
# 
# Root node error: 646/4981 = 0.12969
# 
# n= 4981 
# 
# CP nsplit rel error  xerror     xstd
# 1 0.061146      0   1.00000 1.00000 0.036705
# 2 0.010000      2   0.87771 0.88545 0.034832

ex_gini_tree$variable.importance
# nr.employed          pdays cons.price.idx 
# 161.65220       70.43357       68.65537 


rpart.plot(ex_gini_tree, extra = 101)
print(ex_gini_tree)
# n= 4981 
# 
# node), split, n, loss, yval, (yprob)
# * denotes terminal node
# 
# 1) root 4981 646 no (0.87030717 0.12969283)  
# 2) nr.employed>=5087.65 4204 315 no (0.92507136 0.07492864) *
#   3) nr.employed< 5087.65 777 331 no (0.57400257 0.42599743)  
# 6) pdays>=17 604 205 no (0.66059603 0.33940397) *
#   7) pdays< 17 173  47 yes (0.27167630 0.72832370) *

predict_ex_gini_tree <-predict(ex_gini_tree, test, type = "class")
(matrix_ex_gini_table<- table(predict_ex_gini_tree, test$y))

# predict_ex_gini_tree   no  yes
# no  4345  489
# yes   51  134

print(accuracy_fscore(matrix_ex_gini_table))
#[1] 0.8924088 0.8988415 0.9883985 0.9414951

gini_tree_new_ex <- rpart(
  y ~ ., 
  data = train, 
  parms = list(split = 'gini'), 
  minsplit = 2, 
  minbucket = 1, cp=0.03)
printcp(gini_tree_new_ex)

gini_tree_new_ex$variable.importance

rpart.plot(gini_tree_new_ex, extra = 101)
print(gini_tree_new_ex)

predict_gini_tree_new_ex <-predict(gini_tree_new_ex, test, type = "class")
(matrix_gini_new_table_ex<- table(predict_gini_tree_new_ex, test$y))

# predict_gini_tree_new_ex   no  yes
# no  4345  489
# yes   51  134

print(accuracy_fscore(matrix_gini_new_table_ex))
# [1]  0.8924088 0.8988415 0.9883985 0.9414951

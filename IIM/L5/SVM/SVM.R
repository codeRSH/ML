#Loading Data into R:
bankdata = read.csv(
  "C:\\RecogX_Backup\\Deepak\\IIM\\Dataset\\UniversalBank.csv",
  header = TRUE,
  sep = ","
)

View(bankdata)
#Data preparation
#(a) to remove the columns ID & ZIP
bankdata_RID_RZip = subset(bankdata, select = -c(ID, ZIP.Code))

Independent_var = subset(bankdata_RID_RZip, select = -Personal.Loan) #remove response variable
dependent_var  = as.factor(bankdata_RID_RZip$Personal.Loan)


head(Independent_var)

head(dependent_var)



summary(Independent_var)

#To create dummy variables for the categorical variable .
library("dummies")
Educations = dummy(Independent_var$Education)
str(Educations)
bankdata_Dummy = subset(Independent_var, select = -c(Education))
bankdata_Dummy = cbind(bankdata_Dummy, Educations)


#(c) Standardization of Data
scaled.dat <- scale(bankdata_Dummy)


scaled.dat <- cbind(scaled.dat, dependent_var)




#Separate dataset into train and test
train = sample(1:5000, 3000) # to take a random sample of  60% of the records for train data
train_bankdata = scaled.dat[train, ]

train_bankdata = as.data.frame(train_bankdata)
class(train_bankdata)

nrow(train_bankdata) #no of rows
test = (1:5000) [-train] # to take a random sample of  40% of the records for test data
test_bankdata = scaled.dat[test, ]
nrow(test_bankdata)  #no of rows

test_bankdata = as.data.frame(test_bankdata)
class(test_bankdata)



#Data Summary for the response variable Personal.Loan

table(train_bankdata$dependent_var)
table(test_bankdata$dependent_var)

colnames(train_bankdata)[which(names(train_bankdata) == "dependent_var")] <-
  "PersonalLoan"

colnames(test_bankdata)[which(names(test_bankdata) == "dependent_var")] <-
  "PersonalLoan"


# Classification using SVM
#install.packages("e1071")
str(train_bankdata)
library(e1071)

#Building the model on train data
x = subset(train_bankdata, select = -PersonalLoan) #remove response variable
y  = as.factor(train_bankdata$PersonalLoan)
model  =  svm(
  x,
  y,
  method = "C-classification",
  kernel = "linear",
  cost = 10,
  gamma = 0.1
)
summary(model)



# Applying the model on train data
pred_train  =  predict(model, x)
#tabulate how many have been predicted as loan takers(1) and how many  are predicted as not loan takers(0). Compare actual (i.e. "y") vs. predicted (pred_train)
table(pred_train)
table(y, pred_train)
#Find accuracy & recall
tb_train = table(y, pred_train)
accuracy_train = sum(diag(table(y, pred_train))) / nrow(train_bankdata)

accuracy_train

recall_train = (tb_train[2, 2] / (tb_train[2, 2] + tb_train[2, 1]))
recall_train



# Applying the model on test data
a  = subset(test_bankdata, select = -PersonalLoan) #remove response variable
b  = as.factor(test_bankdata$PersonalLoan)
pred_test = predict(model, a)



table(pred_test)
table(b, pred_test) #actual (i.e.) is on left and predicted shown on top
tb_test <- table(b, pred_test)
accuracy_test = sum(diag(table(b, pred_test))) / nrow(test_bankdata)
(tb_test[1, 1] + tb_test[1, 2] + tb_test[2, 1] + tb_test[2, 2])
recall_test = (tb_test[2, 2] / (tb_test[2, 2] + tb_test[2, 1]))
recall_test


##Tuning the parameters
tuneResult <-
  tune(
    svm,
    train.x = x,
    train.y = y,
    ranges = list(gamma = 10 ^ (-6:-1), cost = 2 ^ (2:3))
  )
print(tuneResult)
tunedModel <- tuneResult$best.model
tunedModelY <- predict(tunedModel, as.matrix(x))
Conf <- table(y, tunedModelY)

Conf

accuracy_test = sum(diag(table(y, tunedModelY))) / nrow(train_bankdata)

accuracy_test

(Conf[1, 1] + Conf[1, 2] + Conf[2, 1] + Conf[2, 2])
recall_test = (Conf[2, 2] / (Conf[2, 2] + Conf[2, 1]))
recall_test

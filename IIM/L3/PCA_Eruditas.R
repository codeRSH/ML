#Loading dataset

train <-
  read.csv("C:\\Users\\ravs\\Development\\ML\\IIM\\L3\\PCA_train.csv")
nrow(train)

test <-
  read.csv("C:\\Users\\ravs\\Development\\ML\\IIM\\L3\\PCA_test.csv")
head(test)
nrow(test)

#add a column
test$Item_Outlet_Sales <- 1
View(test)

#combine the data set
combi <- rbind(train, test)
nrow(combi)

#impute missing values with median
combi$Item_Weight[is.na(combi$Item_Weight)] <-
  median(combi$Item_Weight, na.rm = TRUE)

#impute 0 with median
combi$Item_Visibility <-
  ifelse(combi$Item_Visibility == 0,
         median(combi$Item_Visibility),
         combi$Item_Visibility)

#find mode and impute
table(combi$Outlet_Size, combi$Outlet_Type)
levels(combi$Outlet_Size)[1] <- "Other"

#remove the dependent and identifier variables

my_data <-
  subset(combi,
         select = -c(Item_Outlet_Sales, Item_Identifier, Outlet_Identifier))

#check available variables
colnames(my_data)

#check variable class
str(my_data)

#load library
# install.packages("dummies")

library(dummies)

new_my_data <-
  dummy.data.frame(
    my_data,
    names = c(
      "Item_Fat_Content",
      "Item_Type",
      "Outlet_Establishment_Year",
      "Outlet_Size",
      "Outlet_Location_Type",
      "Outlet_Type"
    )
  )

#check the data set
str(new_my_data)

View(new_my_data)

#divide the new data
pca.train <- new_my_data[1:nrow(train), ]
pca.test <- new_my_data[-(1:nrow(train)), ]

#principal component analysis
prin_comp <- prcomp(pca.train, scale. = T)
#?prcomp

names(prin_comp)

# center and scale refers to respective mean and standard deviation of the variables
# that are used for normalization prior to implementing PCA

prin_comp$center
prin_comp$scale

# The rotation measure provides the principal component loading.
# Each column of rotation matrix contains the principal component loading vector.
# This is the most important measure we should be interested in

prin_comp$rotation


#In order to compute the principal component score vector,
#we don't need to multiply the loading with data.
#Rather, the matrix x has the principal component score vectors in a 8523 ? 44 dimension

dim(prin_comp$x)
head(prin_comp$x)

biplot(prin_comp, scale = 0)


#compute standard deviation of each principal component
std_dev <- prin_comp$sdev

#compute variance
pr_var <- std_dev ^ 2
#check variance of first 10 components

pr_var[1:10]

#proportion of variance explained
prop_varex <- pr_var / sum(pr_var)
prop_varex[1:20]


#scree plot
plot(prop_varex,
     xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")


#cumulative scree plot
plot(cumsum(prop_varex),
     xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#add a training set with principal components

train.data <-
  data.frame(Item_Outlet_Sales = train$Item_Outlet_Sales, prin_comp$x)
head(train.data)

#we are interested in first 30 PCAs
train.data <- train.data[, 1:31]

library(rpart)

rpart.model <-
  rpart(Item_Outlet_Sales ~ ., data = train.data, method = "anova")

rpart.model


#transform test into PCA

test.data <- predict(prin_comp, newdata = pca.test)
test.data <- as.data.frame(test.data)


#select the first 30 components
test.data <- test.data[, 1:30]

#make prediction on test data
rpart.prediction <- predict(rpart.model, test.data)


head(rpart.prediction)

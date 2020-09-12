#Predicting attrition

#Install CHAID package

install.packages("CHAID", repos="http://R-Forge.R-project.org")

#Install and load dependent packages
install.packages("modeldata")
require(modeldata)
require(dplyr)
require(ggplot2)
theme_set(theme_bw()) # set theme
require(CHAID)
require(purrr)
require(caret)

#Load the data
data(attrition)


#. The str command shows we have a bunch of variables which are of type integer. 
str(attrition)

#Checking number of variables as factor
attrition %>%
  select_if(is.factor) %>%
  ncol

#Checking number of variables as numeric
attrition %>%
  select_if(is.numeric) %>%
  ncol

#variables that are integers how many of them have a small number of values
attrition %>%
  select_if(function(col)
    length(unique(col)) <= 5 & is.integer(col)) %>%
  head


attrition %>%
  select_if(function(col)
    length(unique(col)) <= 10 & is.integer(col)) %>%
  head

#Normal Way
attrition %>%
  mutate(
    JobLevel = factor(JobLevel),
    NumCompaniesWorked = factor(NumCompaniesWorked),
    StockOptionLevel = factor(StockOptionLevel),
    TrainingTimesLastYear = factor(TrainingTimesLastYear)
  ) %>% 
  str

####Another way
attrition <- attrition %>% 
  mutate_if(function(col) length(unique(col)) <= 10 & is.integer(col), as.factor)


#Checking number of variables as factor
attrition %>%
  select_if(is.factor) %>%
  ncol

str(attrition)

summary(attrition)

#CHAID is "non parametric" which means that we don't have to worry
#about how the distribution (normality) looks nor make any assumptions
#about the variance.

#We're simply going to build a
#first model using all 1,470 cases, the 18 factors we have available to
#predict with and we are trying to predict attrition

newattrit <- attrition %>% 
  select_if(is.factor)
dim(newattrit)


# demonstrate a full model using chaid with defaults
chaidattrit1 <- chaid(Attrition ~ ., data = newattrit)

#Printing CHAID results
print(chaidattrit1)

plot(chaidattrit1)

#Checking the first split
chisq.test(newattrit$Attrition, newattrit$OverTime)


# digress for plotting
plot(chaidattrit1, type = "simple")

plot(
  chaidattrit1,
  main = "Testing Graphical Options",
  gp = gpar(fontsize = 8),
  type = "simple"
)

plot(
  chaidattrit1,
  main = "Testing More Graphical Options",
  gp = gpar(
    col = "blue",
    lty = "solid",
    lwd = 3,
    fontsize = 10
  )
)


################
# minsplit - Number of observations in splitted response at which no further split is desired.
# 
# minprob - Minimum frequency of observations in terminal nodes.
# maxheight - Maximum height for the tree.


ctrl <- chaid_control(minsplit = 200, minprob = 0.05)
ctrl # notice the rest of the list is there at the default value


chaidattrit2 <- chaid(Attrition ~ ., data = newattrit, control = ctrl)
print(chaidattrit2)

plot(
  chaidattrit2,
  main = "minsplit = 200, minprob = 0.05",
  gp = gpar(
    col = "blue",
    lty = "solid",
    lwd = 3  )
)

ctrl <- chaid_control(maxheight = 3)
chaidattrit3 <- chaid(Attrition ~ ., data = newattrit, control = ctrl)
print(chaidattrit3)


plot(
  chaidattrit3,
  main = "maxheight = 3",
  gp = gpar(
    col = "blue",
    lty = "solid",
    lwd = 3  )
)


ctrl <- chaid_control(alpha2 = .01, alpha4 = .01)
chaidattrit4 <- chaid(Attrition ~ ., data = newattrit, control = ctrl)
print(chaidattrit4)


plot(
  chaidattrit4,
  main = "alpha2 = .01, alpha4 = .01",
  gp = gpar(
    col = "blue",
    lty = "solid",
    lwd = 3  )
)

# digress how accurate were we
pmodel1 <- predict(chaidattrit1)
head(pmodel1)

pmodel2 <- predict(chaidattrit2)
pmodel3 <- predict(chaidattrit3)
pmodel4 <- predict(chaidattrit4)
confusionMatrix(pmodel1, newattrit$Attrition)


confusionMatrix(pmodel2, newattrit$Attrition)


confusionMatrix(pmodel3, newattrit$Attrition)


confusionMatrix(pmodel4, newattrit$Attrition)

install.packages("kableExtra")

library(kableExtra)
modellist <- list(Model1 = chaidattrit1, Model2 = chaidattrit2, Model3 = chaidattrit3, Model4 = chaidattrit4)

CHAIDResults <- map(modellist, ~ predict(.x)) %>% 
  map(~ confusionMatrix(newattrit$Attrition, .x)) %>%
  map_dfr(~ cbind(as.data.frame(t(.x$overall)),as.data.frame(t(.x$byClass))), .id = "ModelNumb")

kable(CHAIDResults, "html") %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                font_size = 9)


sort(varimp(chaidattrit1), decreasing = TRUE)

plot(sort(varimp(chaidattrit1), decreasing = TRUE))



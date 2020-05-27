#---------------------------------------------------------------------------------------------------------------------------------------------------#
###### R Environment - Load Data ######
#---------------------------------------------------------------------------------------------------------------------------------------------------#

#### ENVIRONMENT ####

### Clean current environment and set the working directory
rm(list = ls())
setwd('~/UTS/STDS/AT2/AT2B/Modelling')

### Install and/or load libraries to be used
#install.packages(c('tidyverse', 'ggplot2','caret','Matrix','"xgboost",'lreshape2','parallel','doParallel','vip','ROCR')) 
#>Uncomment to install all required libraries
library('tidyverse')
library('ggplot2')
library('caret')
library('Matrix')
library("xgboost")
library('reshape2')
library('parallel')
library('doParallel')
library('vip')
library('ROCR')

#### Data 
load("C:/Users/61402/Documents/UTS/STDS/AT2/AT2B/Modelling/master_data_tablebuilder.RData")

# Select 1 SEIFA
dataset <- master_social_good %>% 
  select(-c(IRSAD_Decile,IRSD_Decile,IER_Decile))

# Data quality
### Check structure 
str(dataset)

### Data type
dataset$age <- as.factor(dataset$age)
dataset$sex <- as.factor(dataset$sex)
dataset$education <- as.factor(dataset$education)
dataset$ingp <- as.factor(dataset$ingp)
str(dataset)

#### VARIABLE TYPES ####

cols_num <- names(select_if(dataset,is.numeric))
cols_fac <- names(select_if(dataset,is.factor))
cols_char <- names(select_if(dataset,is.character))

### Check initial records
head(dataset)

### Duplicates
sum(duplicated(dataset))
dataset[which(duplicated(dataset)),]
"There are duplicates"
################################################################################################### DROP DUPLICATES
dataset <- dataset %>% unique()
sum(duplicated(dataset))
##################################################################################################

### Variables summary
summary(dataset[,!(names(dataset) %in% cols_char)])
sapply(dataset[,!(names(dataset) %in% cols_char)], function(x) round(prop.table(table(x))*100,2))
################################################################################################### DROP RARE IN REMOTENESS
dataset <- dataset %>% filter(remoteness_index <= 5)
summary(dataset[,!(names(dataset) %in% cols_char)])
##################################################################################################


### Missing values
sapply(dataset, function(x) sum(is.na(x)))

### Check low cell counts
nrow(dataset)
nrow(dataset[which(dataset$labour_force<20),])
nrow(dataset[which(dataset$labour_force<20 & dataset$age %in% c('100 years and over','60-69 years','70-79 years',
                                                                '80-89 years','90-99 years')),])

### Drop small counts
cut_dataset <- dataset[-which(dataset$age %in% c('100 years and over','60-69 years','70-79 years',
                                                            '80-89 years','90-99 years')),]
### Fix noise inconsistencies #Assume unemployment = 1
check_inc <- which(cut_dataset$unemployment>cut_dataset$labour_force)
cut_dataset[check_inc,]
cut_dataset[check_inc,'labour_force'] <- cut_dataset[check_inc,'unemployment']
cut_dataset[check_inc,]  

### Standardization
str(cut_dataset)
cut_dataset <- cut_dataset %>% mutate_at(c("IEO_Decile", "remoteness_index"), ~(scale(.) %>% as.vector))
summary(cut_dataset)

### Feature engineering
cut_dataset <- cut_dataset %>% 
  mutate(city = sub(" .*", "", sa4), 
         capital = as.factor(if_else(city %in% c("Adelaide","Capital","Australian","Brisbane","Darwin","Greater","Melbourne","Perth","Sydney"),"T","F")))
head(cut_dataset[which(cut_dataset$capital == 'T'),])
head(cut_dataset[which(cut_dataset$capital == 'F'),])


##### Data split ######
# get sa4 names
s_names <- unique(cut_dataset$sa4)
set.seed(2007)
random_names <- sample(1:length(s_names),0.75*length(s_names),replace = FALSE)
train_names <- s_names[random_names]

use_dataset <- cut_dataset 

#### Divide dataset
trainset <- use_dataset %>% filter(sa4 %in% train_names)
testset  <- use_dataset %>% filter(!(sa4 %in% train_names))
nrow(use_dataset)
nrow(trainset) + nrow(testset)

### Check the class distributions
prop.table(table(trainset$unemployment/trainset$labour_force))
prop.table(table(testset$unemployment/testset$labour_force))

### Train - test prop visualisations
barplot(prop.table(table(use_dataset$unemployment/use_dataset$labour_force)), col = c("blue","red"), main = 'Whole dataset')
barplot(prop.table(table(trainset$unemployment/trainset$labour_force)), col = c("blue","red"), main = 'Train dataset')
barplot(prop.table(table(testset$unemployment/testset$labour_force)), col = c("blue","red"), main = 'Test dataset')
use_dataset$ratio <- use_dataset$unemployment/use_dataset$labour_force
trainset$ratio <- trainset$unemployment/trainset$labour_force
testset$ratio <- testset$unemployment/testset$labour_force
summary(use_dataset)
summary(trainset)
summary(testset)

ggplot(use_dataset, aes(ratio)) + geom_histogram(aes( fill = ratio), alpha =0.6, position = "identity") + xlim(c(0,1)) + ggtitle("Whole")
ggplot(trainset, aes(ratio)) + geom_histogram(aes( fill = ratio), alpha =0.6, position = "identity") + xlim(c(0,1)) + ggtitle("Train")
ggplot(testset, aes(ratio)) + geom_histogram(aes( fill = ratio), alpha =0.6, position = "identity") + xlim(c(0,1)) + ggtitle("Test")
"Confirmed that proportion is maintained"


### Caret

# Cross_validation
folds <- groupKFold(trainset$sa4, k = 5) 
# get sa4 names
# set.seed(2007)
# get_names_folds <- sample.int(5,length(s_names), replace = TRUE)
# pool <- as.data.frame(cbind(s_names,get_names_folds))
# f1 <- as.character(pool[which(pool$get_names_folds == "1"),"s_names"])
# f2 <- as.character(pool[which(pool$get_names_folds == "2"),"s_names"])
# f3 <- as.character(pool[which(pool$get_names_folds == "3"),"s_names"])
# f4 <- as.character(pool[which(pool$get_names_folds == "4"),"s_names"])
# f5 <- as.character(pool[which(pool$get_names_folds == "5"),"s_names"])
# folds_ini <- as.data.frame(trainset$sa4)
# folds_ini <- folds_ini %>% 
#   rename(as4='trainset$sa4') %>% 
#   mutate(fold=ifelse( as4 %in% f1,1,
#                                          ifelse(as4 %in% f2, 2,
#                                                 ifelse(as4 %in% f3,3,
#                                                        ifelse(as4 %in% f4,4,5)))))
# 
# folds <- list(folds_ini$fold)
# "Use Stratified k-fold cross validation to maintain levels proportions within folds as there is significant class imbalance"
# unique(trainset$sa4[which(folds == 1)])
# unique(trainset$sa4[which(folds == 2)])
# unique(trainset$sa4[which(folds == 3)])
# unique(trainset$sa4[which(folds == 4)])
# unique(trainset$sa4[which(folds == 5)])

### Drop columns
trainset <- trainset %>% select(-c(sa4, population, ratio, city))
testset <- testset %>% select(-c(sa4, population, ratio, city))
use_dataset <- use_dataset %>%  select(-c(sa4, population, ratio, city))

glm.model <- train(unemployment/labour_force ~ age + sex + education + ingp + IEO_Decile +
                     remoteness_index + capital + age*sex +
                     age*education + age*ingp + 
                     sex*education + sex*ingp + 
                     education*ingp +
                     age*capital + sex*capital +
                     ingp*capital ,
                   data = trainset,
                   method = "glmStepAIC",
                   family = "binomial",
                   trace = F,
                   weights = labour_force,
                   verbose = TRUE,
                   na.action = na.omit) #to handle division by zero (labour force = 0)
summary(glm.model)

testset$prediction <-  round(predict(glm.model, newdata = testset) *  testset$labour_force)


testset$employed <-  testset$labour_force-testset$unemployment
testset$prediction_employed <-  testset$labour_force-testset$prediction
testset$true_positives <- ifelse(testset$prediction < testset$unemployment, testset$prediction, testset$unemployment)
testset$true_negatives <- ifelse(testset$prediction_employed < testset$employed, testset$prediction_employed, testset$employed)   
testset$false_positives <- ifelse(testset$prediction > testset$unemployment, testset$prediction - testset$unemployment, 0)
testset$false_negatives <- ifelse(testset$prediction_employed > testset$employed, testset$prediction_employed - testset$employed, 0)

testset$control_positives <- (testset$prediction == testset$true_positives + testset$false_positives)
testset$control_negatives <- (testset$prediction_employed == testset$true_negatives + testset$false_negatives)


recall <- sum(testset$true_positives)/(sum(testset$true_positives)+sum(testset$false_negatives))
precision <- sum(testset$true_positives)/(sum(testset$true_positives)+sum(testset$false_positives))
f_1 <- (( 2 * precision * recall) / (precision + recall))

recall
precision
f_1

#run the "best" model manually using the result of stepwise selection
best <- glm(unemployment/labour_force ~ age + sex + education + ingp + IEO_Decile +
             remoteness_index + capital + age*sex +
             age*education + age*ingp + 
             sex*education + sex*ingp + 
             education*ingp +
             age*capital + sex*capital +
             ingp*capital,
           data = use_dataset, 
           family = "binomial",
           weights = labour_force)

summary(best)

saveRDS(best,"best.rds")

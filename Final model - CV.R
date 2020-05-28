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

### Plot for the report
library('dlookr')
dataset %>%
  plot_outlier(diagnose_outlier(dataset) %>% 
                 filter(outliers_ratio >= 0.5) %>% 
                 filter(variables == 'age') %>% 
                 select(variables) %>% 
                 unlist())

### Missing values
sapply(dataset, function(x) sum(is.na(x)))

library(gridExtra)
library(grid)
grid.table(data.frame(Variable = c('Unemployed','Labour force','SA4','Age','Sex','Education','Indigenous','IEO Decile','Remoteness'),
                      Missing.Values = 0), rows=NULL)
            
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


### Caret

# Cross_validation
#get sa4 names
set.seed(2007)
get_names_folds <- sample.int(5,length(s_names), replace = TRUE)
pool <- as.data.frame(cbind(s_names,get_names_folds))
f1 <- as.character(pool[which(pool$get_names_folds == "1"),"s_names"])
f2 <- as.character(pool[which(pool$get_names_folds == "2"),"s_names"])
f3 <- as.character(pool[which(pool$get_names_folds == "3"),"s_names"])
f4 <- as.character(pool[which(pool$get_names_folds == "4"),"s_names"])
f5 <- as.character(pool[which(pool$get_names_folds == "5"),"s_names"])
folds_ini <- as.data.frame(use_dataset$sa4)
folds_ini <- folds_ini %>%
  rename(as4='use_dataset$sa4') %>%
  mutate(fold=ifelse( as4 %in% f1,1,
                                         ifelse(as4 %in% f2, 2,
                                                ifelse(as4 %in% f3,3,
                                                       ifelse(as4 %in% f4,4,5)))))

folds <- folds_ini$fold
"Use Stratified k-fold cross validation to maintain levels proportions within folds as there is significant class imbalance"
unique(use_dataset$sa4[which(folds == 1)])
unique(use_dataset$sa4[which(folds == 2)])
unique(use_dataset$sa4[which(folds == 3)])
unique(use_dataset$sa4[which(folds == 4)])
unique(use_dataset$sa4[which(folds == 5)])

### Drop columns
use_dataset <- use_dataset %>%  select(-c(sa4, population, city))


### Feature Selection
glm.model <- train(unemployment/labour_force ~ age + sex + education + ingp + IEO_Decile +
                     remoteness_index + capital + age*sex +
                     age*education + age*ingp + 
                     sex*education + sex*ingp + 
                     education*ingp +
                     age*capital + sex*capital +
                     ingp*capital ,
                   data = use_dataset,
                   method = "glmStepAIC",
                   family = "binomial",
                   trace = F,
                   weights = labour_force,
                   verbose = TRUE,
                   na.action = na.omit) #to handle division by zero (labour force = 0)
summary(glm.model)
# No variales discarded
grid.table(data.frame(Selected.Predictors = c('Age','Sex','Education','Indigenous','IEO Decile','Remoteness',
                                   'Capital','Age:Sex','Age:Education','Age:Indigenous','Sex:Education',
                                   'Sex:Indigenous','Education:Indigenous','Age:Capital','Sex:Capital',
                                   'Indigenous:Capital')), rows=NULL)
results_cv <- data.frame(fold = c(1,2,3,4,5),
                         recall = NA,
                         precision = NA,
                         f_1 = NA)
for (i in 1:5){
  
  trainset_temp <- use_dataset[-which(folds == i),]
  testset_temp <- use_dataset[which(folds == i),]
  nrow(testset_temp)/(nrow(trainset_temp)+nrow(testset_temp))
  
glm.model.cv <-  glm(unemployment/labour_force ~ age + sex + education + ingp + IEO_Decile +
                    remoteness_index + capital + age*sex +
                    age*education + age*ingp + 
                    sex*education + sex*ingp + 
                    education*ingp +
                    age*capital + sex*capital +
                    ingp*capital,
                  data = trainset_temp, 
                  family = "binomial",
                  weights = labour_force)

summary(glm.model.cv)

testset_temp$prediction <-  round(predict.glm(glm.model.cv, type='response', newdata = testset_temp) *  testset_temp$labour_force)

print(nrow(use_dataset))
print(nrow(use_dataset[which(folds == i),]) + nrow(use_dataset[-which(folds == i),]))
#### Divide dataset

### Train - test prop visualisations
use_dataset$ratio <- use_dataset$unemployment/use_dataset$labour_force
trainset_temp$ratio <- trainset_temp$unemployment/trainset_temp$labour_force
testset_temp$ratio <- testset_temp$unemployment/testset_temp$labour_force

a <- ggplot(use_dataset, aes(ratio)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="grey") +
  geom_density(aes( fill = ratio), alpha =0.8, position = "identity") + 
  xlim(c(0,1)) + ggtitle("All Data") +
  scale_fill_grey() +theme(axis.title.x = element_blank(),
                           axis.title.y = element_blank())
b<- ggplot(trainset_temp, aes(ratio)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="grey") +
  geom_density(aes( fill = ratio), alpha =0.6, position = "identity") + 
  xlim(c(0,1)) + ggtitle("Train") +
  scale_fill_grey() +theme(axis.title.x = element_blank(),
                           axis.title.y = element_blank())
c <- ggplot(testset_temp, aes(ratio)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="grey") +
  geom_density(aes( fill = ratio), alpha =0.6, position = "identity") + 
  xlim(c(0,1)) + ggtitle("Test") +
  scale_fill_grey() +theme(axis.title.x = element_blank(),
                           axis.title.y = element_blank())
"Confirmed that proportion is maintained"
d <- grid.arrange(a, b, c, 
             ncol = 3, nrow = 1, top = textGrob(paste0('Stratified K-Fold CV, k=',i), gp=gpar(fontface="bold", fontsize=14)), 
             left = 'Density', bottom = 'Unemployment Ratio')


testset_temp$employed <-  testset_temp$labour_force-testset_temp$unemployment
testset_temp$prediction_employed <-  testset_temp$labour_force-testset_temp$prediction

testset_temp$true_positives <- pmin(testset_temp$unemployment,testset_temp$prediction)
testset_temp$true_negatives <- pmin(testset_temp$employed,testset_temp$prediction_employed)   

testset_temp$false_positives <- testset_temp$prediction - testset_temp$true_positives
testset_temp$false_negatives <- testset_temp$prediction_employed - testset_temp$true_negatives
  
sum(testset_temp$labour_force) == (sum(testset_temp$true_positives) + sum(testset_temp$true_negatives) + sum(testset_temp$false_negatives) + sum(testset_temp$false_positives))

recall <- sum(testset_temp$true_positives)/(sum(testset_temp$true_positives)+sum(testset_temp$false_negatives))
precision <- sum(testset_temp$true_positives)/(sum(testset_temp$true_positives)+sum(testset_temp$false_positives))
f_1 <- (( 2 * precision * recall) / (precision + recall))

results_cv[i,2] <- recall
results_cv[i,3] <- precision
results_cv[i,4] <-f_1
}

avg_recall <- round(mean(results_cv$recall),4)
avg_precision <- round(mean(results_cv$precision),4)
avg_f1 <- round(mean(results_cv$f_1),4)

results_cv <- results_cv %>% 
  mutate(Fold = fold,
         Recall = round(recall,4),
         Precision = round(precision,4),
         F1 = round(f_1,4)) %>% 
  select(Fold, Recall, Precision, F1)

results_cv[6,1] <- 'Average'
results_cv[6,2] <- avg_recall
results_cv[6,3] <- avg_precision
results_cv[6,4] <- avg_f1
grid.table(results_cv, rows=NULL)

confusion <- data.frame(a=c('','Actual Employed','Actual Unemployed'),
                        b=c('Predicted Employed', format(sum(testset_temp$true_negatives),big.mark=','),format(sum(testset_temp$false_negatives),big.mark=',')),
                        c=c('Predicted Unemployed',format(sum(testset_temp$false_positives),big.mark=','),format(sum(testset_temp$true_positives),big.mark=','))
                        )
grid.table(confusion, rows=NULL)


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

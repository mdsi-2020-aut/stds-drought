'DATA'
'One thing that becomes a bit more complex about logistic regression is model diagnostics. The interpetation of the coefficients
is somewhat more difficult, and so it can be harder to check models using residuals (although you can!) Here, we will use a different
procedure to validate our model. We will split our dataset into two bits (a training and a test set) and then evaluate the model that 
we build on the training set with reference to the test set.'
titanicData$id <- 1:nrow(titanicData)   # adding an id number to each row for tracking
titanic.train <- titanicData %>% dplyr::sample_frac(.75)
titanic.test  <- dplyr::anti_join(titanicData, titanic.train, by = 'id')
'Do not forget about cleansing - especially missing values'
#> Are we interested in this kind of validation anyway??? 
#> (Our focus is features significance as well as coefficients interpretability -i.e. insights, features importance-
#> Summary with significance testing should be enough, no?


'TRAIN'
'Generalized linear models weaken the assumptions associated with general linear models.'
'To use glm() we in essence just need to specify the link function that we want to use in addition to constructing our equation.'
glm1 = glm(survived ~ pclass + sex, family=binomial(logit), data = titanic.train)

'EVALUATION'
'Is this model any good? To work that out you will have to validate it in some way… but first you should go and have a look up AIC.
AIC penalises a model for adding extra terms… so you want to minimise the AIC, which makes it a good measure of model performance when
compared to RMSE or R2 (which will always just get better if you add more terms to them.)'
summary(glm1)

'VALIDATION'
pred<-predict(glm1,newdata = titanic.test)
head(pred)
'Those are not probabilities! predict is returning a number that has to be substituted back into an equation like this:'
prob<- 1/(1 + exp(-pred))
head(prob)
'Or if you want to deal with all that a bit more easily then you could just use this short cut in R:'
probability<-predict(glm1,newdata = titanic.test,type="response")
head(probability)
'How can we get a bit more rigorous about demonstrating model validity? We turn to that problem next.
We still need to work out a measure for the “correctness of our model”.
One way to proceed consists of using a confusion matrix. To do this however, we first need to define a threshold value.'
#setting a threshold value of 0.5 for survived/not survived... you may want to see if there are better settings that you could 
#use here (Hint: do a search for "ROC curve")
prediction <- ifelse(probability > 0.5, 1, 0) 
# building a contingency table of the counts at each combination of factor levels
confusion  <- table(titanic.test$survived, prediction) 
confusion 

'INTERACTIONS'
'Interaction terms can be very important. So it might be the case that the probability of survival is affected by what 
sex you are in the class you are in…'
glm2 = glm(survived ~ pclass*sex + age,family = binomial(logit), data = titanic.train)
summary(glm2)

'AGGREGATED DATA MODELLING'
'In R, you can specify a logistic regression model in 3 different ways, depending on whether your data is grouped or ungrouped (individual-level).'
library(magrittr)   
library(tidyverse)
'the Poisson log-linear regression model could also be used, since the Binomial distribution can be approximated well by Poisson in the case
of rare events. However, we will stick with the logistic regression'
'Turned the smoke variable into factors, so that the regression coefficients can be interpreted with respect to a baseline (i.e. in linear 
regression terms: what is the expected change in death rate of male pensioners who smoked cigars or pipe only - coded 2, vs those who never 
smoked - baseline, coded 1), rather than a linear increase in smoking status which does not make sense (e.g. in linear regression terms: what
is the expected change in death rate of male pensioners when smoking status increases by one unit)'
#> Clarify the later please
cancerData = read_delim("http://data.princeton.edu/wws509/datasets/smoking.raw",
                        delim="\t",
                        col_names = c("age","smoke","pop","dead"))
cancerData_grouped <- cancerData %<>% 
  select(smoke, pop, dead) %>%
  mutate(smoke=as.factor(smoke)) %>%
  group_by(smoke) %>%
  summarize(pop=sum(pop), dead=sum(dead))
cancerData_grouped
'Specify the response as the observed proportions, and supply the sample size as the weights. Supplying the sample size is essential, as 50% 
success with a sample size of 2 is not the same as 50% success with a sample size of 100.'
#> why do we have to set the y as ratio if we enter the pop as weight
glm(dead/pop~smoke, family=binomial, data=cancerData_grouped, weights=pop) %>%
  summary()
#> Do not understand summary. How to read coefficient for smoke1
#> Smoking not significant?
#> Any benefit of using way 1 or 2? If output is exactly the same
#> Still do not understand how to interpret gender and age 'Individual-level data format' only possible if just one predictor, no?
#> What is overdispersion?
#> Why intercept changes between way-2 and 3?
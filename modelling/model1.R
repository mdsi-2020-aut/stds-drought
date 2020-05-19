library(tidyverse)
library(corrplot)
library(car)
library(ResourceSelection)

load("./data/master_data_tablebuilder.RData")

#edit row that has unemployment > labour_force
#glm somehow only accept target value between 0 and 1
master_social_good %>% filter(unemployment > labour_force)
master_social_good_fix <- master_social_good
master_social_good_fix$unemployment[master_social_good_fix$unemployment > master_social_good_fix$labour_force] <- master_social_good_fix$labour_force[master_social_good_fix$unemployment > master_social_good_fix$labour_force]
master_social_good_fix %>% filter(unemployment > labour_force)

#model per each SA4
models <- list()
sa4_list <- unique(master_social_good_fix$sa4)

for(idx in sa4_list){
  print(idx)
  master_per_sa4 <- master_social_good_fix %>% filter(sa4 == idx)
  models[[idx]] <- glm(unemployment/labour_force ~ education + ingp + IRSAD_Decile + age + sex + remoteness_index,
                       master_per_sa4, family = binomial, weights = labour_force)
}

master_social_good_fix %>% filter(remoteness_index > 3) %>% select(sa4) %>% distinct()
summary(models[["Cairns"]]) #sex, indigenous, age, and indigenous significant (education > sex > age & indigenous)
summary(models[["Queensland - Outback"]]) #sex not significant, indigenous, sex, education significant ()
summary(models[["Townsville"]]) #sex, indigenous, age, and indigenous significant (education > sex > age & indigenous)


master_social_good_fix %>% filter(remoteness_index > 2 & remoteness_index <= 3) %>% select(sa4) %>% distinct()
summary(models[["Darwin"]]) #sex not significant, education > indigenous
summary(models[["Ballarat"]]) #sex, education significant, age & indigenous not significant
summary(models[["Capital Region"]]) #sex, education, age, indigenous significant

master_social_good_fix %>% filter(remoteness_index > 1 & remoteness_index <= 2) %>% select(sa4) %>% distinct()
summary(models[["Sydney - Inner West"]]) #sex not significant, indigenous > education, age significant
summary(models[["Sydney - Northern Beaches"]]) #sex not significant, indigenous > education, age significant
summary(models[["Central Coast"]]) #sex not significant, 
summary(models[["Melbourne - Inner"]]) #sex, education significant,
summary(models[["Brisbane Inner City"]])


#sex as predictor
model_gender <- glm(unemployment/labour_force ~ sex,
                    master_social_good_fix, family = binomial, weights = labour_force)
summary(model_gender)
hoslem.test(model_gender$y, fitted(model_gender), g=20)

#age as predictor
model_age <- glm(unemployment/labour_force ~ age,
                 master_social_good_fix, family = binomial, weights = labour_force)
summary(model_age)

#education as predictor
model_education <- glm(unemployment/labour_force ~ education,
                       master_social_good_fix, family = binomial, weights = labour_force)
summary(model_education)

hoslem.test(model_education$y, fitted(model_education), g=10)


#indigenous as predictor
model_indigenous <- glm(unemployment/labour_force ~ ingp,
                        master_social_good_fix, family = binomial, weights = labour_force)
summary(model_indigenous)

#indigenous as predictor
model_indigenous_sex <- glm(unemployment/labour_force ~ ingp + sex,
                            master_social_good_fix, family = binomial, weights = labour_force)
summary(model_indigenous_sex)

#education and indigenous as predictor
model_education_ingp <- glm(unemployment/labour_force ~ education + ingp,
                            master_social_good_fix, family = binomial, weights = labour_force)
summary(model_education_ingp)

#education, indigenous, and sex as predictor
model_education_ingp_sex <- glm(unemployment/labour_force ~ education + ingp + sex,
                                master_social_good_fix, family = binomial, weights = labour_force)
summary(model_education_ingp_sex)

#education, indigenous, and age as predictor
model_education_ingp_age <- glm(unemployment/labour_force ~ education + ingp + age,
                                master_social_good_fix, family = binomial, weights = labour_force)
summary(model_education_ingp_age)

#education, indigenous, and IRSAD as predictor
model_education_ingp_irsad <- glm(unemployment/labour_force ~ education + ingp + IRSAD_Decile,
                                  master_social_good_fix, family = binomial, weights = labour_force)
summary(model_education_ingp_irsad)

hoslem.test(model_education_ingp_irsad$y, fitted(model_education_ingp_irsad), g=10)

#education, indigenous, and IRSD as predictor
model_education_ingp_irsd <- glm(unemployment/labour_force ~ education + ingp + IRSD_Decile,
                                 master_social_good_fix, family = binomial, weights = labour_force)
summary(model_education_ingp_irsd)

#education, indigenous, and IER as predictor
model_education_ingp_ier <- glm(unemployment/labour_force ~ education + ingp + IER_Decile,
                                master_social_good_fix, family = binomial, weights = labour_force)
summary(model_education_ingp_ier)

#education, indigenous, and IEO as predictor
model_education_ingp_ieo <- glm(unemployment/labour_force ~ education + ingp + IEO_Decile,
                                master_social_good_fix, family = binomial, weights = labour_force)
summary(model_education_ingp_ieo)



#education, indigenous, and IRSAD + IRSD as predictor
model_education_ingp_irsad_irsd <- glm(unemployment/labour_force ~ education + ingp + IRSAD_Decile + IRSD_Decile,
                                       master_social_good_fix, family = binomial, weights = labour_force)
summary(model_education_ingp_irsad_irsd)
vif(model_education_ingp_irsad_irsd)

master_social_good_fix %>%
  ggplot(mapping = aes(x = IRSAD_Decile, y = IRSD_Decile)) +
  geom_point()

#education, indigenous, and all seifa as predictor
model_education_ingp_seifa <- glm(unemployment/labour_force ~ education + ingp + IRSAD_Decile + IRSD_Decile + IEO_Decile + IER_Decile,
                                  master_social_good_fix, family = binomial, weights = labour_force)
summary(model_education_ingp_seifa)
vif(model_education_ingp_seifa)

corr.matrix <- cor(master_social_good_fix %>% select(IRSAD_Decile, IRSD_Decile, IEO_Decile, IER_Decile))
corrplot.mixed(corr.matrix, upper = "circle", lower = "number", tl.pos = "lt")

#education, indigenous, and IRSAD + IRSD as predictor
model_education_ingp_irsad <- glm(unemployment/labour_force ~ education + ingp + IRSAD_Decile + IRSD_Decile,
                                  master_social_good_fix, family = binomial)
summary(model_education_ingp_irsad)

#education, indigenous, and remoteness as predictor
model_education_ingp_remote <- glm(unemployment/labour_force ~ education + ingp + remoteness_index,
                                   master_social_good_fix, family = binomial, weights = labour_force)
summary(model_education_ingp_remote)

#education, indigenous, and remoteness as predictor
model_education_ingp_remote_irsad <- glm(unemployment/labour_force ~ education + ingp + IRSAD_Decile + remoteness_index,
                                         master_social_good_fix, family = binomial)
summary(model_education_ingp_remote_irsad)


master_social_good_fix$probability <- predict(model_education_ingp_ier, newdata = master_social_good_fix, type="response")

master_social_good_fix %>% mutate(unemployment_rate = unemployment/labour_force) %>%
  select(education, ingp, IER_Decile, unemployment_rate, probability)

master_social_good_fix %>% mutate(unemployment_rate = unemployment/labour_force) %>%
  select(education, ingp, IER_Decile, unemployment_rate, probability) %>% filter(abs(unemployment_rate - probability) > 0.3 )


model_allsa4 <- glm(unemployment/labour_force ~ sa4 + education + ingp,
                    master_social_good_fix, family = binomial)
summary(model_allsa4)



#Limitations
#1. Multicolinearity among SEIFA indicators --> is it a limitation?
#2. If data sliced per SA4, the predictors (education, indigenous, etc) are not significant --> 
#   probably try to split between capital vs non capital
#3. 

master_social_good_fix %>% filter(labour_force == 0) %>% select(probability)

master_social_good_fix %>%
  ggplot(mapping = aes(x = IRSAD_Decile, y = IRSD_Decile)) +
  geom_point()


glm(unemployment/labour_force ~ sex+ age + sex + education + ingp,
    master_social_good_temp %>% filter(sa4 == "Adelaide - North"), family = binomial) %>% anova()
summary(model)

master_social_good %>% mutate(ratio = unemployment/labour_force) %>%
  ggplot(aes(x = IRSD_Decile, y = ratio)) +
  geom_point()

master_social_good %>% mutate(ratio = unemployment/labour_force) %>%
  ggplot(aes(x = remoteness_index, y = ratio)) +
  geom_point()

x <- master_social_good_temp %>% filter(unemployment > labour_force)
master_social_good_temp <- master_social_good
master_social_good_temp$unemployment[master_social_good_temp$unemployment > master_social_good_temp$labour_force] <- master_social_good_temp$labour_force[master_social_good_temp$unemployment > master_social_good_temp$labour_force]

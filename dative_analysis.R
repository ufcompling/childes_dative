rm(list=ls(all=TRUE))
library(lme4)
library(DHARMa)  # residual diagnostics
library(effects) # effects plots of (final) model predictions
library(MuMIn)   # AIC
library(party)   # conditional inference trees, here used as a factorization tool
library(ggrepel)
library(ggeffects)
library(brms)
library(dplyr)
library(nlme)
library(car)

## load data
original_data <- read.csv('CHILDES_dative_2025.csv', header = T, sep = ',')

original_data <- subset(original_data, Include == 'yes')

data <- subset(original_data, !(Sentence %in% c("give Mommy Mommy", 
                                       "Doggie Woggie top doggy give Mommy Mommy",
                                       "eh give me s uh",
                                       "I show you El")))

data <- subset(data, as.numeric(Age)>=12)

## Recoding Role
data$Role[data$Role == 'Father'] <- 'Parent'
data$Role[data$Role == 'Mother'] <- 'Parent'
data$Role[data$Role == 'Target_Child'] <- 'Child'
data$Role <- as.factor(data$Role)

## Recoding Structure
data$Structure[data$Structure == 'v_np_pp'] <- 'V-NP-PP'
data$Structure[data$Structure == 'v_np_np'] <- 'V-NP-NP'
data$Structure <- as.factor(data$Structure)
data$Structure <- relevel(data$Structure, ref = "V-NP-NP") 

## Deviation coding
data$Obj_givenness <- as.factor(data$Obj_givenness)
contrasts(data$Obj_givenness) <- cbind(no_yes=c(-.5, .5))
contrasts(data$Obj_givenness)

data$Other_givenness <- as.factor(data$Other_givenness)
contrasts(data$Other_givenness) <- cbind(no_yes=c(-.5, .5))
contrasts(data$Other_givenness)

data$Obj_pro <- as.factor(data$Obj_pro)
contrasts(data$Obj_pro) <- cbind(nonpro_pro=c(-.5, .5))
contrasts(data$Obj_pro)

data$Other_pro <- as.factor(data$Other_pro)
contrasts(data$Other_pro) <- cbind(nonpro_pro=c(-.5, .5))
contrasts(data$Other_pro)

data$Obj_animacy_final <- as.factor(data$Obj_animacy_final)
contrasts(data$Obj_animacy_final) <- cbind(no_yes=c(-.5, .5))
contrasts(data$Obj_animacy_final)

data$Other_animacy_final <- as.factor(data$Other_animacy_final)
contrasts(data$Other_animacy_final) <- cbind(no_yes=c(-.5, .5))
contrasts(data$Other_animacy_final)

data$Obj_toyhood_final <- as.factor(data$Obj_toyhood_final)
contrasts(data$Obj_toyhood_final) <- cbind(no_yes=c(-.5, .5))
contrasts(data$Obj_toyhood_final)

data$Other_toyhood_final <- as.factor(data$Other_toyhood_final)
contrasts(data$Other_toyhood_final) <- cbind(no_yes=c(-.5, .5))
contrasts(data$Other_toyhood_final)

## Controlling for
data$Obj_type <- as.factor(data$Obj_type)
data$Other_type <- as.factor(data$Other_type)

data$Verb_class_final <- as.factor(data$Verb_class_final)

data$Syntactic_persistence <- as.factor(data$Syntactic_persistence)

data <- data %>% 
  mutate(Exact_repetition_binary = case_when
         (Exact_repetition_binary != '0' ~ 'Yes',
           TRUE ~ 'No'))

## Centering numerical variables
data$Age_int <- as.numeric(data$Age)
data <- subset(data, Age_int >= 12)
data$Age_int <- data$Age_int - mean(data$Age_int)

data$Sent_len <- as.numeric(data$Sent_len)
data$Sent_len <- data$Sent_len - mean(data$Sent_len)

data$Obj_len <- as.numeric(data$Obj_len)
data$Obj_len <- data$Obj_len - mean(data$Obj_len)

data$Other_len <- as.numeric(data$Other_len)
data$Other_len <- data$Other_len - mean(data$Other_len)

new_other_len <- rep(0, nrow(data))

for (i in 1:nrow(data)){
  if (data$Structure[i] == 'V-NP-PP'){
    new_other_len[i] = data$Other_len[i] -1
  }
  else {
    new_other_len[i] = data$Other_len[i]
  }
}
data$New_other_len <- new_other_len
data$New_other_len <- data$New_other_len - mean(data$New_other_len)

data <- data %>% 
  mutate(Previous_double_d_binary = case_when
         (Previous_double_d != "NONE" ~ 'Yes',
           TRUE ~ 'No'))

data <- data %>% 
  mutate(Previous_prepositional_d_binary = case_when
         (Previous_prepositional_d != "NONE" ~ 'Yes',
           TRUE ~ 'No'))

data$Exact_repetition <- as.numeric(data$Exact_repetition)


### Organizing by age bin
organize_bin <- data.frame(matrix(ncol = 5, nrow = 0))

for (age in c(12, 18, 24, 30, 36, 42, 48, 54, 60, 66)){
  for (role in c('Child', 'Parent')){
    select <- subset(data, Role == role  & as.numeric(Age) >= age & as.numeric(Age) < age + 6)
    total = nrow(select)
    for (structure in c('V-NP-NP', 'V-NP-PP')){
      freq = nrow(subset(select, Structure == structure))
      proportion = round(freq * 100 / total, 2)
      info <- c(role, structure, paste(as.character(age),'_', as.character(age + 6), sep=''), freq, proportion)
      organize_bin[nrow(organize_bin) + 1, ] <- info
    }
  }
}

for (role in c('Child', 'Parent')){
  select = subset(data, Role == role & as.numeric(Age) >= 72)
  total = nrow(select)
  for (structure in c('V-NP-NP', 'V-NP-PP')){
    freq = nrow(subset(select, Structure == structure))
    proportion = round(freq * 100 / total, 2)
    info <- c(role, structure, paste(as.character(72),'_', sep=''), freq, proportion)
    organize_bin[nrow(organize_bin) + 1, ] <- info
  }
}

names(organize_bin) <- c('Role', 'Structure', 'Age_range', 'Freq', 'Proportion')
organize_bin$Structure[organize_bin$Structure == 'V-NP-NP'] = 'double object'
organize_bin$Structure[organize_bin$Structure == 'V-NP-PP'] = 'prepositional object'

organize_bin %>% 
  ggplot(aes(Age_range, as.numeric(as.character(Freq)), group = Structure, fill = Structure, pattern = Structure)) +
  geom_bar(stat = 'identity') +
  geom_col_pattern(pattern_size = .25) +
   scale_fill_manual(values = c("steelblue",  "mediumpurple4")) + 
  facet_wrap( ~ Role) +
  theme_classic() + 
  theme(text = element_text(size=16.5, family="Times")) + 
  theme(legend.position="top") +
  xlab("child age (months)") + 
  ylab("production frequency") + 
  guides(color = guide_legend(nrow = 2)) + 
  theme(legend.title = element_blank()) 


### Check the proportion of utterances with give in each age bin
for (age in c(12, 18, 24, 30, 36, 42, 48, 54, 60, 66)){
  for (role in c('Child', 'Parent')){
    select <- subset(data, Role == role & Structure == 'V-NP-NP' & as.numeric(Age) >= age & as.numeric(Age) < age + 6)
    total = nrow(select)
    give<-subset(select, Verb_lemma == 'give')
    give_total = nrow(give)
    ratio = give_total / total
    print(c(role, age, age + 6, give_total, total, round(ratio, 2)))
  }
}

### Growth curve modeling
cumulative <- function(data, descriptive){
  
  data$Age <- as.numeric(as.vector(data$Age))
  
  temp <- data.frame()
  for (structure in c('V-NP-NP', 'V-NP-PP')){
    age_list <- as.vector(unique(as.integer(data$Age)))
    freq_list <- rep(0, length(age_list))
    n_utterances_list <- rep(0, length(age_list))
    structure_list <- rep(0, length(age_list))
    
    for (i in 1:length(age_list)){
      age = as.numeric(age_list[i])
      select <- data.frame(subset(data, Age <= age & Structure == structure)) 
      freq = nrow(select)
      n_utterances = sum(subset(descriptive, as.numeric(Age) <= age)$N_utterance)
      freq_list[i] = freq
      n_utterances_list[i] = n_utterances
      structure_list[i] = structure
    }
    
    cumulative_data <- data.frame(as.character(age_list))
    names(cumulative_data) <- c('Age')
    cumulative_data$Freq <- freq_list
    cumulative_data$N_utterance <- n_utterances_list
    cumulative_data$Total_ratio <- cumulative_data$Freq / cumulative_data$N_utterance
    cumulative_data$Structure <- structure_list
    temp <- rbind(temp, cumulative_data)
  }
  
  return(temp)
  
}

child_data <- subset(data, Role == 'Child')

child_descriptive <- subset(read.csv('../data/child_descriptive.txt', header = T, sep = '\t'), Age >= 12)
child_data <- subset(data,Role == 'Child')
child_production <- cumulative(child_data, child_descriptive)
child_production$ppt <- child_production$Total_ratio * 1000
child_production$Age <- as.numeric(child_production$Age)
child_production$Structure[child_production$Structure == 'V-NP-NP'] = 'double object'
child_production$Structure[child_production$Structure == 'V-NP-PP'] = 'prepositional object'

# Real cumulative ratios
child_production %>% 
  ggplot(aes(as.numeric(as.character(Age)), ppt, group = Structure, color = Structure)) +
  geom_line(aes(linetype=Structure), alpha = 1) +
  geom_point(aes(color = Structure, shape=Structure), size = 1.5) +
  geom_label_repel(aes(label=Structure), data=filter(child_production, as.numeric(Age) == 60)) +
  scale_x_continuous(breaks=seq(6, 150, 6)) +
  theme_classic() + 
  theme(legend.position="none") +
  xlab("child age (months)") + 
  ylab("Cumulative Production Ratio (Per Thousand)") + 
  theme(legend.title = element_blank())


# Building Growth Curve Models
child_growth_double <- brm(
  bf(ppt  ~ upperAsymptote * exp(-exp(-growthRate*(Age-inflection))),
     upperAsymptote ~ 1,
     growthRate ~ 1,
     inflection ~ 1,
     nl = TRUE),
  data = subset(child_production, Structure == 'V-NP-NP'),
  prior = c(
    prior(uniform(0, 10), nlpar = "upperAsymptote", lb = 0, ub = 10),
    prior(uniform(0, 3), nlpar = "growthRate", lb = 0, ub = 3),
    prior(uniform(12, 144), nlpar = "inflection", lb = 12, ub = 72)
  ),
  file = "child_growth_double",
  iter = 4000)

bayes_R2(child_growth_double, subset(child_production, Structure == 'V-NP-NP'))

child_growth_prepositional <- brm(
  bf(ppt  ~ upperAsymptote * exp(-exp(-growthRate*(Age-inflection))),
     upperAsymptote ~ 1,
     growthRate ~ 1,
     inflection ~ 1,
     nl = TRUE),
  data = subset(child_production, Structure == 'V-NP-PP'),
  prior = c(
    prior(uniform(0, 10), nlpar = "upperAsymptote", lb = 0, ub = 10),
    prior(uniform(0, 3), nlpar = "growthRate", lb = 0, ub = 3),
    prior(uniform(12, 72), nlpar = "inflection", lb = 12, ub = 72)
  ),
  file = "child_growth_prepositional",
  iter = 4000)

bayes_R2(child_growth_prepositional, subset(child_production, Structure == 'V-NP-PP'))

double_predictions <- as.data.frame(ggpredict(child_growth_double))
double_predictions$Structure <- "double object"

prepositional_predictions <- as.data.frame(ggpredict(child_growth_prepositional))
prepositional_predictions$Structure <- "prepositional object"

predictions <- rbind(double_predictions, prepositional_predictions)
saveRDS(predictions, "predictions")

predictions <- readRDS("predictions")

predictions %>%
  ggplot(aes(Age.x, Age.predicted, color=Structure)) +
  geom_line(aes(color=Structure)) +
  geom_label_repel(aes(label=Structure), data=filter(predictions, Age.x == 60)) +
  geom_ribbon(aes(ymin = Age.conf.low, ymax = Age.conf.high, fill = Structure), alpha=0.3, linetype=0) +
  xlab("child age (months)") + ylab("Cumulative Production Ratio (Per Thousand)")+
  theme_linedraw() + theme(legend.position="none") +
  scale_x_continuous(breaks = seq(12, 150, by = 6))

aoa_estimates <- data.frame(
  structures = c("V-NP-NP", "V-NP-NP", "V-NP-NP", "V-NP-PP", "V-NP-PP", "V-NP-PP"),
  parameters = c('Upper asymptote', 'Growth rate', 'Inflection point', 'Upper asymptote', 'Growth rate', 'Inflection point'),
  mean = c(fixef(child_growth_double)[1,1], fixef(child_growth_double)[2,1], fixef(child_growth_double)[3,1], fixef(child_growth_prepositional)[1,1], fixef(child_growth_prepositional)[2,1], fixef(child_growth_prepositional)[3,1]),
  lower = c(fixef(child_growth_double)[1,3], fixef(child_growth_double)[2,3], fixef(child_growth_double)[3,3], fixef(child_growth_prepositional)[1,3], fixef(child_growth_prepositional)[2,3], fixef(child_growth_prepositional)[3,3]),
  upper = c(fixef(child_growth_double)[1,4], fixef(child_growth_double)[2,4], fixef(child_growth_double)[3,4], fixef(child_growth_prepositional)[1,4], fixef(child_growth_prepositional)[2,4], fixef(child_growth_prepositional)[3,4])
)

aoa_estimates %>%
  filter(!(parameters %in% c('Growth rate', 'Upper asymptote'))) %>%
  ggplot(aes(mean, structures, color=structures)) +
  #  geom_point(position=position_dodge(0.4)) +
  geom_point() +
  #  geom_linerange(aes(xmin=lower, xmax=upper), position=position_dodge(0.4)) +
  geom_linerange(aes(xmin=lower, xmax=upper)) +
  scale_x_continuous(breaks = seq(12, 150, by = 6)) +
  theme_bw() 


### Regression modeling
model <- glm(Structure ~ Obj_len * Obj_givenness + Obj_type + Obj_animacy_final + Obj_toyhood_final +
               New_other_len * Other_givenness + Other_type + Other_animacy_final + Other_toyhood_final +
               Verb_class_final + 
               Previous_double_d_binary + Previous_prepositional_d_binary + Exact_repetition_binary + 
               Role *  Age_int + Sent_len*  Age_int,
             data = data,
             family = 'binomial') 


logit_model = model
deviance <- summary(logit_model)$deviance
null_deviance <- summary(logit_model)$null.deviance
rsquared <- 1 - (deviance / null_deviance)

# Print the R-squared value
print(rsquared)

saveRDS(model, 'full_model_new.rds')

temp_data <- data
temp_data$MLU <- as.numeric(temp_data$MLU)
temp_data <- subset(temp_data, !is.na(temp_data$MLU))
temp_data$MLU <- temp_data$MLU - mean(temp_data$MLU)

child_data <- subset(temp_data, Role == 'Child')
child_model <- glm(Structure ~ Obj_len * Obj_givenness + Obj_type + Obj_animacy_final + Obj_toyhood_final +
                     New_other_len * Other_givenness + Other_type + Other_animacy_final + Other_toyhood_final +
                     Verb_class_final + 
                     Previous_double_d_binary + Previous_prepositional_d_binary + Exact_repetition_binary + 
                     Sent_len + Age + MLU,
                   data = child_data,
                   family = 'binomial') 

logit_model = child_model
deviance <- summary(logit_model)$deviance
null_deviance <- summary(logit_model)$null.deviance
rsquared <- 1 - (deviance / null_deviance)

# Print the R-squared value
print(rsquared)

saveRDS(child_model, 'child_model_new.rds')


### Applying child model to parents' data
### Calculating logloss

parent_data <- subset(data, Role == 'Parent')
parent_data$MLU <- as.numeric(parent_data$MLU)
parent_data <- subset(data, !is.na(parent_data$MLU))

parent_model <- glm(Structure ~ Obj_len * Obj_givenness + Obj_type + Obj_animacy_final + Obj_toyhood_final +
                     New_other_len * Other_givenness + Other_type + Other_animacy_final + Other_toyhood_final +
                     Verb_class_final + 
                     Previous_double_d_binary + Previous_prepositional_d_binary + Exact_repetition_binary + 
                     Sent_len + Age,
                   data = parent_data,
                   family = 'binomial') 

vif(parent_model)

saveRDS(parent_model, 'parent_model_new.rds')

BIC(parent_model)

logit_model = parent_model
deviance <- summary(logit_model)$deviance
null_deviance <- summary(logit_model)$null.deviance
rsquared <- 1 - (deviance / null_deviance)

# Print the R-squared value
print(rsquared)

child_data <- subset(data, Role == 'Child')

select <- subset(child_data, Age >= 12 & Age < 24)
nrow(select)
mupdar_pred <- data.frame(predict(parent_model, newdata = select, type='response'))
names(mupdar_pred) <- c('Estimate')
mupdar_logloss <- rep(0, nrow(select))

mupdar_acc = 0
mupdar_acc_list = rep(0, nrow(mupdar_pred))

for (i in 1:nrow(mupdar_pred)){
  prob <- as.numeric(mupdar_pred$Estimate[i])
  if (select$Order[i] == '1'){
    if (prob > 0.5){
      mupdar_acc = mupdar_acc + 1
      mupdar_acc_list[i] = 'yes'
      mupdar_logloss[i] = -1 * log(prob, 2)
    }}
  if (select$Order[i] == '0'){
    if (prob < 0.5){
      mupdar_acc = mupdar_acc + 1
    }}}

round(mupdar_acc / nrow(select) * 100, 2)
mean(mupdar_logloss) 


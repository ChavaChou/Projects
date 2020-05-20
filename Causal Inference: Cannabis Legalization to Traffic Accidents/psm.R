library(tidyverse)
library("Hmisc")
library(MatchIt)
library(readxl)
setwd("~/MSBA/spring/Econometrics/project")
# load data
master <- read.csv('clean_data/normalized_m.csv', header = TRUE)
# perform necessary transformations
master <- master %>% 
  select(-X, -X.1, -legalization, -PScore) %>%
  filter(State != 'Colorado' & State != 'Washington' &  State != 'Alaska' & State != 'Oregon') %>%
  mutate(legality = ifelse(State %in% c('Nevada', 'California', 'Massachusetts', 'Maine'), 1, 0)) 

master_pre <- master %>% select(-State)
master$legality <- as.factor(master$legality)
master_pre$legality <- as.factor(master_pre$legality) 
# correlation matrix
controls <- master_pre %>% select(-legality, -pp)
rcorr(as.matrix(controls))
# psm
ps_model <- glm(legality ~ age + pp + prop_male + ps + rev_ratio + beverage + population_density,
                           data = master_pre, family = 'binomial')
summary(ps_model)
# check results
master$PS <- ps_model$fitted.values
ggplot(master) + geom_density(aes(x = PS, color = legality))
master <- master %>% select(State, legality, PS, age:population_density)

# run MatchIt algo
match_output <- matchit(legality ~ age + pp + prop_male + ps + rev_ratio + beverage + population_density, 
                        data = master_pre, method = "nearest", distance = "logit", 
                        caliper = 0.01, replace = FALSE, ratio = 1)
summary(match_output)

data_matched <- match.data(match_output)
data_matched$state <- c('Maine', 'Minnesota', 'Nevada', 'Texas')
data_matched <- data_matched %>% select(state, legality, age:population_density)

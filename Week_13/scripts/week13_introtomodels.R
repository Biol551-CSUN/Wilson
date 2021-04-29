### This is a script for Spring 2021 Computer Modeling covering models in R
### Created by: Emily Wilson
### Created on: 04/28/21
############################################################################################

# Load libraries ###########################################################################
library(tidyverse)
library(here)
library(palmerpenguins)
library(broom)
library(performance)
library(modelsummary)
library(tidymodels)
library(wesanderson)


# Get modeling ##############################################################################

# Simple linear model: mod <- lm(y~x, data = df)
# Multiple regression: mod <- lm(y~x1 + x2, data = df)
# Interaction term: mod <- lm(y~x1*x2, data = df)


# Modeling the penguin dataset:

penguins %>%
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm, color = species))+
  geom_point()+
  geom_smooth(method = "lm") + #
  labs(color = "Species",
       x = "Bill Length (mm)",
       y = "Bill Depth (mm)")+
  theme_bw()

# Linear model of bill depth by bill length, by species:
peng_mod <- lm(bill_length_mm ~ bill_depth_mm*species, data = penguins)

# Checking model assumptions with performance:
check_model(peng_mod) # check assumptions of an lm model

# viewing results with base R:
anova(peng_mod)

# Coefficients (effect size) with error:
summary(peng_mod)


# Viewing results with broom (b/c these are untidy and not user-friendly):

# Tidying coefficients
coeffs <- tidy(peng_mod) # just put tidy() around it
coeffs

# Glance extracts r^2, AICs, etc. of model:
results <- glance(peng_mod) 
results

# Augment add residuals and predicted values to og data, requires you put the model and data
resid_fitted<-augment(peng_mod)
resid_fitted

# New model
Peng_mod_noX<-lm(bill_length_mm ~ bill_depth_mm, data = penguins)

#Make a list of models and name them
models<-list("Model with interaction" = Peng_mod,
             "Model with no interaction" = Peng_mod_noX)


# Results in {modelsummary}:

# model w/out interaction
peng_mod_noX <- lm(bill_length_mm ~ bill_depth_mm, data = penguins)

#Make a list of models and name them
models<-list("Model with interaction" = peng_mod,
             "Model with no interaction" = peng_mod_noX)

#Save the results as a .docx
modelsummary(models, output = here("Week_13","outputs","table.docx"))

# Modelplot: canned coefficient modelplots

modelplot(models) +
  labs(x = 'Coefficients', 
       y = 'Term names') +
  scale_color_manual(values = wes_palette('Darjeeling1'))

# Compare a bunch of models at the same time with purrr, dplyr, and broom

models2 <- penguins %>%
  ungroup() %>% # the penguin data are grouped so we need to ungroup them
  nest(-species) %>% # nest data by species 
  mutate(fit = map(data, ~lm(bill_length_mm~body_mass_g, data = .))) # fit models

models2

models2$fit # shows all three models

# Mutate models list so we have a tidy coefficient dataframe
results <- models %>%
  mutate(coeffs = map(fit, tidy), # look at the coefficients #<<
         modelresults = map(fit, glance))  # R2 and others #<<

results

results <- models %>%
  mutate(coeffs = map(fit, tidy), # look at coefficients
         modelresults = map(fit, glance)) %>% # R2, etc 
  select(species, coeffs, modelresults) %>% # only keep results
  unnest() # put it back in dataframe, specify which columns to unnest 

view(results)


# Other very common stats packages

#- `stats`: General (`lm`)and generalized (`glm`) linear models (already loaded with base R)   
#- `lmer` : mixed effects models  
#- `lmerTest`' : getting results from lmer  
#- `nlme` : non-linear mixed effects models  
#- `mgcv`, `gam` : generalized additive models  
#- `brms`, `rstan`, and many more  : Bayesian modeling  
#- `lavaan`, `peicewiseSEM` : Structural Equation Models  
#- `rpart`, `randomForest`, `xgboost`, and more : Machine learning models  


# Tidymodels:

lm_mod<-linear_reg() %>% # set functional form
  set_engine("lm") %>% # set engine (OLS regression in this case)
  fit(bill_length_mm ~ bill_depth_mm*species, data = penguins) %>% # set model fit
  tidy() # tidy
  ggplot()+ # pipe to plot
  geom_point(aes(x = term, y = estimate)) + # set axes
  geom_errorbar(aes(x = term, ymin = estimate-std.error, # add error bars
                    ymax = estimate+std.error), width = 0.1 ) +
  coord_flip()
  
lm_mod









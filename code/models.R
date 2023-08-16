## this script contains the code for the 3 models used in the manuscript.
# These bayesians models are extensions from the methods published by Patrick Taffé 
# and incorporated in the MethodCompare-package
# Each model determines the precision for each distinct method of handling PPmax and PPmin 
# for 5 distinct observation windows
# for more details see the manuscript.

library(tidyverse)
library(brms)
library(shinystan)
library(MethodCompare)

# import master_dataset
master_dataset <- read_csv("data/master_dataset.csv") 

# reference method and BLUP

mc <- measure_compare(master_dataset, new='iPPV_2', Ref='iPPV_1', ID='ID')

## adding the reference value for eacht measurement

master_dataset2 <- master_dataset %>% 
  left_join(mc$New %>% group_by(id) %>% summarise (PPVref = mean(y2_hat, na.rm=T)) %>% rename(ID= id),
            by ='ID')

# Data sets per family for analysis
master_dataset_i <- master_dataset2 %>% select(ID, iPPV_1:iPPV_5, PPVref)%>% 
  pivot_longer(cols=iPPV_1:iPPV_5, names_to = 'method', values_to = 'PPV') %>% na.omit()
master_dataset_p <- master_dataset2 %>% select(ID, pPPV_1:pPPV_5, PPVref)%>% 
  pivot_longer(cols=pPPV_1:pPPV_5, names_to = 'method', values_to = 'PPV') %>% na.omit()
master_dataset_t <- master_dataset2 %>% select(ID, tPPV_12:tPPV_60, PPVref)%>% 
  pivot_longer(cols=tPPV_12:tPPV_60, names_to = 'method', values_to = 'PPV') %>% na.omit()

# model_formule
model_fm_int <- bf(
  PPV ~ PPVref*method,
  sigma ~ PPVref*method
)

# priors
## remark: brms transforms data to centered data... Intercepts are intercepts for centered data.

#automatic 
get_prior(model_fm_int, master_dataset_i)

# MCMC

## i-family (individual RC based models)
# Compiling and Sampling may take several hours -> 'Adjust your expectations accordingly!' ; )
model_i <- brm(
  formula = model_fm_int,
  data = master_dataset_i,
  family=gaussian(),
  warmup=2000,
  iter = 10000,
  seed = 134,
  cores=8,
  chains=4,
  # refresh=0,
  # moment_match = TRUE,
  file = "fits/model_i",
  # control = list(
  #   max_treedepth = 12 ),
  sample_prior = T
)
# underlyingstan code 
# make_stancode(model_fm_int, master_dataset_i)

model_i                     # report sampling of coefficients of the model 
plot(model_i)               # density and trace plots
launch_shinystan(model_i)   # MCMC diagnostics

## p-family (pooled RC based models)
model_p  <- brm(
  formula = model_fm_int,
  data = master_dataset_p,
  family=gaussian(),
  warmup=2000,
  iter = 10000,
  seed = 134,
  cores=8,
  chains=4,
  # refresh=0,
  # moment_match = TRUE,
  file = "fits/model_p",
  # control = list(
  #   max_treedepth = 12 ),
  sample_prior = T
)

model_p                     # report sampling of coefficients of the model 
plot(model_p)               # density and trace plots
launch_shinystan(model_p)   # MCMC diagnostics


## t-family (time based models)

model_t  <- brm(
  formula = model_fm_int,
  data = master_dataset_t,
  family=gaussian(),
  warmup=2000,
  iter = 10000,
  seed = 1234,
  cores=8,
  chains=4,
  # refresh=0,
  # moment_match = TRUE,
  file = "fits/model_t",
  control = list(
    max_treedepth = 12 ),
  sample_prior = T
)

model_t                     # report sampling of coefficients of the model 
plot(model_t)               # density and trace plots
launch_shinystan(model_t)   # MCMC diagnostics

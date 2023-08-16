## models to estimate the impact of measurement error on measured values and measured changes in measured values of PPV
# we choose to model this for the iPPV3 method and tPPV15
# missing values are added to the dataset and their values imputed using the model.

library(tidyverse)
library(MethodCompare)
library(brms)
library(shinystan)


# import master_dataset, calculating BLUP and selecting iPPV_3 subgroup (see: models.R)

master_dataset_def <- read_csv("data/master_dataset.csv") 


mc <- measure_compare(master_dataset_def, new='iPPV_2', Ref='iPPV_1', ID='ID')

master_dataset2 <- master_dataset_def %>% 
  left_join(mc$New %>% group_by(id) %>% summarise (PPVref = mean(y2_hat, na.rm=T)) %>% rename(ID= id),
            by ='ID')


# data set selection

master_dataset_iPPV3 <- master_dataset2 %>% select(ID, iPPV_1:iPPV_5, PPVref)%>% 
  pivot_longer(cols=iPPV_1:iPPV_5, names_to = 'method', values_to = 'PPV') %>% na.omit() %>%
  filter(method == 'iPPV_3')

master_dataset_tPPV15 <- master_dataset2 %>% select(ID, tPPV_12:tPPV_60, PPVref)%>% 
  pivot_longer(cols=tPPV_12:tPPV_60, names_to = 'method', values_to = 'PPV') %>% na.omit()%>% 
  filter(method == 'tPPV_15')


## adding missing values
mi_tibble <- tibble(
  ID= 'X',
  PPVref= NA,
  method = c(rep('iPPV_3',14),rep('tPPV_15',14)),
  PPV = rep(c(9, 9.5, 10, 10.5, 11, 11.5, 12, 12.5,
          13, 13.5, 14, 14.5, 15, 15.5),2)
)

master_dataset_iPPV3_mi <- rbind(
  master_dataset_iPPV3,
  mi_tibble %>% filter(method == 'iPPV_3')
)
master_dataset_tPPV15_mi <- rbind(
  master_dataset_tPPV15,
  mi_tibble %>% filter(method == 'tPPV_15')
)

# model_formule
model_fm_mi <- bf(PPV ~ mi(PPVref),
                  sigma ~ mi(PPVref))+
  bf(PPVref | mi()~1)+ set_rescor(FALSE)

# priors
## remark: brms transforms data to centered data... Intercepts are intercepts for centered data.

#automatic 
get_prior(model_fm_mi, master_dataset_iPPV3_mi)
get_prior(model_fm_mi, master_dataset_tPPV15_mi)


# MCMC

## iPPV3- method
# Compiling and Sampling may take several hours -> 'Adjust your expectations accordingly!' ; )

model_iPPV3_imp <- brm(
  formula = model_fm_mi,
  data = master_dataset_iPPV3_mi,
  family=gaussian(),
  warmup=2000,
  iter = 10000,
  seed = 134,
  cores=8,
  chains=4,
  # refresh=0,
  # moment_match = TRUE,
  file = "fits/model_iPPV3_imp",
  # control = list(
  #   max_treedepth = 12 ),
  sample_prior = T
)
# underlyingstan code 
# make_stancode(model_fm_mi, master_dataset_iPPV3_mi)

model_iPPV3_imp                     # report sampling of coefficients of the model 
plot(model_iPPV3_imp)               # density and trace plots
launch_shinystan(model_iPPV3_imp)   # MCMC diagnostics

## tPPV15-method
# Compiling and Sampling may take several hours -> 'Adjust your expectations accordingly!' ; )

model_tPPV15_imp <- brm(
  formula = model_fm_mi,
  data = master_dataset_tPPV15_mi,
  family=gaussian(),
  warmup=2000,
  iter = 10000,
  seed = 134,
  cores=8,
  chains=4,
  # refresh=0,
  # moment_match = TRUE,
  file = "fits/model_tPPV15_imp",
  # control = list(
  #   max_treedepth = 12 ),
  sample_prior = T
)
# underlyingstan code 
# make_stancode(model_fm_int, master_dataset_i)

model_tPPV15_imp                     # report sampling of coefficients of the model 
plot(model_tPPV15_imp)               # density and trace plots
launch_shinystan(model_tPPV15_imp)   # MCMC diagnostics

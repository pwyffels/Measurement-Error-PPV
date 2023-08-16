## This is a wrapper for visualising Bias and Precision for each PPV-family faceted into its different methods
# the output of the function contains
## B_bias:  The model matrix for Bias
## mu_bias: a list containing Design-matrix %% model_matrix for each method's bias
## plot_data_bias_draws: all draws of the model for bias for each method
## plot_data_bias: summarized predictions for bias for each method

## B_sigma:  The model matrix for Sigma/SD
## mu_sigma: a list containing Design-matrix %% model_matrix for each method's sigma/SD
## plot_data_sigma_draws: all draws of the model for sigma/SD for each method
## plot_data_sigma: summarized predictions for sigma/SD for each method

bias_precision <- function(model){
  # extracting correct parameter names
  p_names <- c(colnames(as_tibble(model)))
  
  # Design_matrix  (same for the separate analyses Bias/precision)
  X_1 <- tibble(!!p_names[1]:=1, 
                !!p_names[3]:= c(4:40), 
                !!p_names[4]:= 0, !!p_names[5]:=0, !!p_names[6]:=0, !!p_names[7]:=0,
                !!p_names[8]:= 0, !!p_names[9]:= 0, !!p_names[10]:= 0, !!p_names[11]:= 0)
  X_2 <- tibble(!!p_names[1]:=1, 
                !!p_names[3]:= c(4:40), 
                !!p_names[4]:= 1, !!p_names[5]:=0, !!p_names[6]:=0, !!p_names[7]:=0,
                !!p_names[8]:= c(4:40), !!p_names[9]:= 0, !!p_names[10]:= 0, !!p_names[11]:= 0)
  X_3 <- tibble(!!p_names[1]:=1, 
                !!p_names[3]:= c(4:40), 
                !!p_names[4]:= 0, !!p_names[5]:=1, !!p_names[6]:=0, !!p_names[7]:=0,
                !!p_names[8]:= 0, !!p_names[9]:= c(4:40), !!p_names[10]:= 0, !!p_names[11]:= 0)
  X_4 <- tibble(!!p_names[1]:=1, 
                !!p_names[3]:= c(4:40), 
                !!p_names[4]:= 0, !!p_names[5]:=0, !!p_names[6]:=1, !!p_names[7]:=0,
                !!p_names[8]:= 0, !!p_names[9]:= 0, !!p_names[10]:= c(4:40), !!p_names[11]:= 0)
  X_5 <- tibble(!!p_names[1]:=1, 
                !!p_names[3]:= c(4:40), 
                !!p_names[4]:= 0, !!p_names[5]:=0, !!p_names[6]:=0, !!p_names[7]:=1,
                !!p_names[8]:= 0, !!p_names[9]:= 0, !!p_names[10]:= 0, !!p_names[11]:= c(4:40))
  # Model matrix
  B_bias <- as_tibble(model)[,c(1,3:11)]
  B_sigma <- as_tibble(model)[,c(2,12:20)]
  
  
  # bias plot data
  mu_bias_1 <- as.matrix(X_1) %*% t(B_bias)
  mu_bias_2 <- as.matrix(X_2) %*% t(B_bias)
  mu_bias_3 <- as.matrix(X_3) %*% t(B_bias)
  mu_bias_4 <- as.matrix(X_4) %*% t(B_bias)
  mu_bias_5 <- as.matrix(X_5) %*% t(B_bias)
  
  
  plot_data_bias_draws <- rbind(
    as_tibble (cbind(X_1 %>% select(b_PPVref), mu_bias_1)) %>% 
      pivot_longer(cols=c(-b_PPVref), names_to = 'draw', values_to = 'predicted') %>% 
      mutate(draw = as.numeric(draw))%>%
      mutate(RC='1'),
    as_tibble (cbind(X_2 %>% select(b_PPVref), mu_bias_2)) %>% 
      pivot_longer(cols=c(-b_PPVref), names_to = 'draw', values_to = 'predicted') %>% 
      mutate(draw = as.numeric(draw))%>%
      mutate(RC='2'),
    as_tibble (cbind(X_3 %>% select(b_PPVref), mu_bias_3)) %>% 
      pivot_longer(cols=c(-b_PPVref), names_to = 'draw', values_to = 'predicted') %>% 
      mutate(draw = as.numeric(draw))%>%
      mutate(RC='3'),
    as_tibble (cbind(X_4%>% select(b_PPVref), mu_bias_4)) %>% 
      pivot_longer(cols=c(-b_PPVref), names_to = 'draw', values_to = 'predicted') %>% 
      mutate(draw = as.numeric(draw))%>%
      mutate(RC='4'),
    as_tibble (cbind(X_5 %>% select(b_PPVref), mu_bias_5)) %>% 
      pivot_longer(cols=c(-b_PPVref), names_to = 'draw', values_to = 'predicted') %>% 
      mutate(draw = as.numeric(draw))%>%
      mutate(RC='5'))
  
  plot_data_bias <- plot_data_bias_draws %>%
    group_by(b_PPVref, RC) %>%
    summarise(LL = quantile(predicted, 0.025),
              Pred = quantile(predicted, 0.5),
              UL= quantile(predicted, 0.975),.groups='drop')
  
  # precision data
  mu_sigma_1 <- as.matrix(X_1) %*% t(B_sigma)
  mu_sigma_2 <- as.matrix(X_2) %*% t(B_sigma)
  mu_sigma_3 <- as.matrix(X_3) %*% t(B_sigma)
  mu_sigma_4 <- as.matrix(X_4) %*% t(B_sigma)
  mu_sigma_5 <- as.matrix(X_5) %*% t(B_sigma)
  
  plot_data_sigma_draws <- rbind(
    as_tibble (cbind(X_1%>% select(b_PPVref), mu_sigma_1)) %>% 
      pivot_longer(cols=c(-b_PPVref), names_to = 'draw', values_to = 'predicted') %>% 
      mutate(draw = as.numeric(draw))%>%
      mutate(RC='1'),
    as_tibble (cbind(X_2%>% select(b_PPVref), mu_sigma_2)) %>% 
      pivot_longer(cols=c(-b_PPVref), names_to = 'draw', values_to = 'predicted') %>% 
      mutate(draw = as.numeric(draw))%>%
      mutate(RC='2'),
    as_tibble (cbind(X_3%>% select(b_PPVref), mu_sigma_3)) %>% 
      pivot_longer(cols=c(-b_PPVref), names_to = 'draw', values_to = 'predicted') %>% 
      mutate(draw = as.numeric(draw))%>%
      mutate(RC='3'),
    as_tibble (cbind(X_4%>% select(b_PPVref), mu_sigma_4)) %>% 
      pivot_longer(cols=c(-b_PPVref), names_to = 'draw', values_to = 'predicted') %>% 
      mutate(draw = as.numeric(draw))%>%
      mutate(RC='4'),
    as_tibble (cbind(X_5%>% select(b_PPVref), mu_sigma_5)) %>% 
      pivot_longer(cols=c(-b_PPVref), names_to = 'draw', values_to = 'predicted') %>% 
      mutate(draw = as.numeric(draw))%>%
      mutate(RC='5')) 
  
  plot_data_sigma <- plot_data_sigma_draws %>%
    group_by(b_PPVref, RC) %>%
    summarise(LL = quantile(predicted, 0.025),
              Pred = quantile(predicted, 0.5),
              UL= quantile(predicted, 0.975),.groups='drop') 
  
  output <- list(B_bias =B_bias,
                 mu_bias = list(mu_bias_1,mu_bias_2,mu_bias_3,mu_bias_4,mu_bias_5),
                 plot_data_bias_draws = plot_data_bias_draws,
                 plot_data_bias = plot_data_bias,
                 B_sigma = B_sigma,
                 mu_sigma = list(mu_sigma_1,mu_sigma_2,mu_sigma_3,mu_sigma_4,mu_sigma_5),
                 plot_data_sigma_draws = plot_data_sigma_draws,
                 plot_data_sigma = plot_data_sigma)
  return(output)
}

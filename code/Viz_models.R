## with this function the plots that can be found in the supplementary section of the article were made.
# note to run this code you need to first run two other scripts:
#   - models.R for the specific models you want to visualize (or import the stored fits in fits/)
#   - bias_precision.R for the wrapper to build precision- and bias-plots in line with the method proposed by Patrick Taffé

library(tidyverse)
library(ggpubr)
library(bayesplot)
library(modelr)
library(tidybayes)


## function for plot

combined_plots <- function(model, master_dataset){
  # settings
  Viz_colors <- tibble(
    RC = c(1:5),
    i = c('black','#005824', '#238b45', '#41ae76', '#66c2a4'),
    p = c('black','#084594', '#2171b5', '#4292c6', '#6baed6' ),
    t = c('#8c2d04', '#d94801', '#f16913', '#fd8d3c', '#fdae6b')
  )
  
  # generating the data
  family <- str_sub(deparse(substitute(model)),7,7)
  post <- tibble(
    i = c(1,2,3,4,5),
    p = c(1,2,3,4,5),
    t = c(12,15,20,30,60)
  )
  
  model_viz_data <- bias_precision(model)
  
  PPV_labels = c(`1` = paste0(family, 'PPV_', (post %>% select(!!family))[1,1]), 
                 `2` = paste0(family, 'PPV_', (post %>% select(!!family))[2,1]), 
                 `3` = paste0(family, 'PPV_', (post %>% select(!!family))[3,1]), 
                 `4` = paste0(family, 'PPV_', (post %>% select(!!family))[4,1]), 
                 `5` = paste0(family, 'PPV_', (post %>% select(!!family))[5,1]))
  
  # Prediction Plot
  
  prediction_PPV <- master_dataset %>%
    data_grid(PPVref = rep(seq_range(PPVref, 51),5), method= c(rep(PPV_labels[1],51),rep(PPV_labels[2],51),rep(PPV_labels[3],51),
                                                               rep(PPV_labels[4],51),rep(PPV_labels[5],51))) %>%
    add_predicted_draws(model) %>%
    ggplot(aes(x= PPVref, y= PPV,col=method))+
    stat_lineribbon(aes(y= .prediction), .width = c(.99, .95, .5), alpha=0.25)+
    geom_point(data = master_dataset, aes(x= PPVref, y= PPV), alpha=0.04)+
    geom_abline(linetype=2, col='red')+
    coord_cartesian(ylim = c(0,60), xlim= c(4,40))+
    #scale_y_continuous(limits = c(0,80))+
    scale_fill_brewer(palette = 'Greys')+
    labs(
      x= '',
      y= 'Measured PPV (%)',
      tag ='A') +
    theme_classic()+
    facet_wrap(~method,nrow=1
               #labeller = labeller(method = method.labels)
    )+
    scale_color_manual( values= (Viz_colors %>% select(!!family))[[1]])+
    theme(legend.position='none') 
  prediction_PPV
  
  # Bias Plot
  bias_PPV <- model_viz_data$plot_data_bias %>%
    ggplot(aes(x= b_PPVref, y=Pred-b_PPVref))+
    geom_hline(yintercept = 0, linetype=2, col='grey80')+
    geom_ribbon(aes(x= b_PPVref, ymin=LL-b_PPVref, ymax=UL-b_PPVref),fill='grey90')+
    geom_line(aes(x= b_PPVref, y= Pred-b_PPVref,col=RC))+
    theme_classic()+
    coord_cartesian(xlim= c(4,40), ylim = c(-5,10))+
    labs(x= '', y= 'Measured PPV - BLUP (%)',
         tag = 'B')+
    scale_color_manual( values= (Viz_colors %>% select(!!family))[[1]])+
    theme(legend.position = 'none',
          strip.text.x = element_blank())+
    facet_wrap(~RC, nrow=1)
  
  # Precision Plot
  
  precision_PPV <- model_viz_data$plot_data_sigma %>%
    ggplot(aes(x= b_PPVref, y=exp(Pred)))+
    geom_hline(yintercept = 0, linetype=2, col='grey80')+
    geom_ribbon(aes(x= b_PPVref, ymin=exp(LL), ymax=exp(UL)),fill='grey90')+
    geom_line(aes(x= b_PPVref, y= exp(Pred),col=RC))+
    theme_classic()+
    coord_cartesian(ylim = c(-1,15), xlim= c(4,40))+
    labs(x= 'BLUP (Best Unbiased Linear Predictor) (%)', y= 'SD of the error (%)',
         tag='C')+
    scale_color_manual(values= (Viz_colors %>% select(!!family))[[1]])+
    theme(legend.position = 'none',
          strip.text.x = element_blank())+
    facet_wrap(~RC, nrow=1)
  
  # combination
  PPV_plots <- ggarrange(
    prediction_PPV, bias_PPV, precision_PPV , ncol=1, align = 'v'
  )
  PPV_plots
  
  
}


combined_plots(model_p,master_dataset_p)
combined_plots(model_i,master_dataset_i)
combined_plots(model_t,master_dataset_t)


## remark running this function gives a warning for each subplot: Unknown or uninitialised column: `linewidth`.
# this warning doesn't seem to be critical... a solution for this warning is being searched for, but is lacking for the moment.

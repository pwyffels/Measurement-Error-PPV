# this script is the code for the visualization of the different model as can be found in fig 3.
# note to run this code you need to first run two other scripts:
#   - models.R for the specific models you want to visualize (or import the stored fits in fits/)
#   - bias_precision.R for the wrapper to build precision- and bias-plots in line with the method proposed by Patrick Taffé

library(tidyverse)
library(ggpubr)


model_i_viz_data <- bias_precision(model_i)
model_p_viz_data <- bias_precision(model_p)
model_t_viz_data <- bias_precision(model_t)

Viz_group <- function(data){
  # settings
  Viz_colors <- tibble(
    RC = c(1:5),
    i = c('black','#005824', '#238b45', '#41ae76', '#66c2a4'),
    p = c('black','#084594', '#2171b5', '#4292c6', '#6baed6' ),
    t = c('#8c2d04', '#d94801', '#f16913', '#fd8d3c', '#fdae6b')
    )
  Viz_labels <- tibble(
    RC = c(1:5),
    i = c(expression('iPPV'[1]),expression('iPPV'[2]), expression('iPPV'[3]), expression('iPPV'[4]), expression('iPPV'[5])),
    p = c(expression('pPPV'[1]),expression('pPPV'[2]), expression('pPPV'[3]), expression('pPPV'[4]), expression('pPPV'[5])),
    t = c(expression('tPPV'[12]),expression('tPPV'[15]), expression('tPPV'[20]), expression('tPPV'[30]), expression('tPPV'[60]))
    
  )  
  family <- str_sub(deparse(substitute(data)),7,7)
  post <- tibble(
    i = c(1,2,3,4,5),
    p = c(1,2,3,4,5),
    t = c(12,15,20,30,60)
    
  )
  bias_plot <- data$plot_data_bias  %>%
    ggplot(aes(x= b_PPVref, y=Pred-b_PPVref))+
    geom_hline(yintercept = 0, linetype=2, col='grey80')+
    geom_ribbon(aes(x= b_PPVref, ymin=LL-b_PPVref, ymax=UL-b_PPVref, group=RC),fill='grey90')+
    geom_line(aes(x= b_PPVref, y= Pred-b_PPVref,col=RC))+
    theme_classic()+
    coord_cartesian(xlim= c(4,40), ylim = c(-5,10))+
    labs(x= '', y= 'Measured PPV - BLUP (%)')+
    scale_color_manual( values= (Viz_colors %>% select(!!family))[[1]],
                        labels=(Viz_labels %>% select(!!family))[[1]])+
    theme(legend.position = 'bottom', legend.title = element_blank())
  precision_plot <- data$plot_data_sigma %>%
    ggplot(aes(x= b_PPVref, y=exp(Pred)))+
    geom_hline(yintercept = 0, linetype=2, col='grey80')+
    geom_ribbon(aes(x= b_PPVref, ymin=exp(LL), ymax=exp(UL), group=RC),fill='grey90')+
    geom_line(aes(x= b_PPVref, y= exp(Pred),col=RC))+
    theme_classic()+
    coord_cartesian(xlim= c(4,40), ylim = c(-5,10))+
    labs(x= 'BLUP (%)', y= 'SD of the Error (%)')+
    scale_color_manual( values= (Viz_colors %>% select(!!family))[[1]],
                        labels=(Viz_labels %>% select(!!family))[[1]])+
    theme(legend.position = 'bottom', legend.title = element_blank())
  #precision_plot
  ggarrange(bias_plot,precision_plot, ncol=1, common.legend = T, legend='bottom')
}

plot_i<-Viz_group(model_i_viz_data)
plot_p<-Viz_group(model_p_viz_data)
plot_t<-Viz_group(model_t_viz_data)

# figure 3

ggarrange(
  annotate_figure(plot_i,top = text_grob('Individual RC PPV-class: iPPV', face='bold')),
  annotate_figure(plot_p, top =text_grob('Pooled RC PPV-class: pPPV', face='bold')),
  annotate_figure(plot_t, top =text_grob('Time window based PPV-class: tPPV', face='bold')),
  ncol=3
)
# format save: pdf landscape: 12,25 x 7,69 inch

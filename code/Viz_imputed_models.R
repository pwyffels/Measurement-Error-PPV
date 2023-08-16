# In this script contains the code for the visualization of the simulations of the imputed models (fig 4 in the manuscript)

library(tidyverse)
library(ggpubr)
library(HDInterval)
library(ggridges) 

plot_data_iPPV3 <- as_tibble(model_iPPV3_imp)
plot_data_tPPV15 <- as_tibble(model_tPPV15_imp)

## fig 4a

m_labels <- c('maesured PPV = 9 %','maesured PPV = 10 %','maesured PPV = 11 %',
              'maesured PPV = 12 %','maesured PPV = 13 %','measured PPV = 14 %', 'measured PPV = 15%')
names(m_labels) <- c('PPVm_09','PPVm_10','PPVm_11',
                     'PPVm_12','PPVm_13','PPVm_14', 'PPVm_15')

gg_compare <-  rbind(
  plot_data_iPPV3 %>% 
    select(`Ymi_PPVref[2197]`,`Ymi_PPVref[2199]`,`Ymi_PPVref[2201]`,
           `Ymi_PPVref[2203]`,`Ymi_PPVref[2205]`,`Ymi_PPVref[2207]`,`Ymi_PPVref[2209]`) %>% 
    rename (PPVm_09 =`Ymi_PPVref[2197]`, PPVm_10 =`Ymi_PPVref[2199]`,PPVm_11=`Ymi_PPVref[2201]`,
            PPVm_12 =`Ymi_PPVref[2203]`, PPVm_13 = `Ymi_PPVref[2205]`, PPVm_14= `Ymi_PPVref[2207]`, PPVm_15 = `Ymi_PPVref[2209]`) %>% 
    pivot_longer(cols=PPVm_09:PPVm_15, names_to = 'PPV_m', values_to = 'value') %>% mutate(method='iPPV3'),
  
  plot_data_tPPV15 %>% 
    select(`Ymi_PPVref[2121]`,`Ymi_PPVref[2123]`,`Ymi_PPVref[2125]`,
           `Ymi_PPVref[2127]`,`Ymi_PPVref[2129]`,`Ymi_PPVref[2131]`,`Ymi_PPVref[2133]`) %>%
    rename (PPVm_09 =`Ymi_PPVref[2121]`, PPVm_10 =`Ymi_PPVref[2123]`,PPVm_11=`Ymi_PPVref[2125]`,
            PPVm_12 =`Ymi_PPVref[2127]`, PPVm_13 = `Ymi_PPVref[2129]`, PPVm_14= `Ymi_PPVref[2131]`, PPVm_15= `Ymi_PPVref[2133]`) %>%
    pivot_longer(cols=PPVm_09:PPVm_15, names_to = 'PPV_m', values_to = 'value') %>% mutate(method='tPPV15'))%>%
  ggplot(aes(x= value , col = method))+
  geom_density(size=1)+
  facet_wrap(~ PPV_m,ncol=1, labeller = labeller(PPV_m= m_labels))+
  theme_classic()+
  scale_x_continuous(breaks=c(7:19), limits = c(6,20),minor_breaks =c(7,19))+
  scale_y_continuous(breaks = c(0,0.1,0.2,0.3,0.4,0.5),
                     labels= c(rep('',6)))+
  labs(y='density', x='PPV (%)')+
  #theme(legend.position = 'bottom',
  #      legend.title = element_blank(),
  #      strip.text.x = element_blank(),
  #      axis.ticks.y = element_blank(),
  #      axis.text.y = element_blank())+
  scale_color_manual(values = c('#d94801','#238b45'))+
  labs(y='')


table_vlines <- tibble(
  PPV_m = c('PPVm_09', 'PPVm_10', 'PPVm_11',
            'PPVm_12' ,'PPVm_13', 'PPVm_14' , 'PPVm_15'),
  xinter = c(9:15)
) 

gg_compare_abs <- gg_compare + geom_vline(data= table_vlines, aes(xintercept = xinter),size=1.1)+
  theme_minimal()+
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        strip.text = element_blank())

gg_compare_abs


## fig 4b:
plot_data_dif_iPPV3 <- 
  plot_data_iPPV3 %>%
  mutate(d_0.5 = `Ymi_PPVref[2198]`-`Ymi_PPVref[2197]`,
         d_1.0 = `Ymi_PPVref[2199]`-`Ymi_PPVref[2197]`,
         d_1.5 = `Ymi_PPVref[2200]`-`Ymi_PPVref[2197]`,
         d_2.0 = `Ymi_PPVref[2201]`-`Ymi_PPVref[2197]`,
         d_2.5 = `Ymi_PPVref[2202]`-`Ymi_PPVref[2197]`,
         d_3.0 = `Ymi_PPVref[2203]`-`Ymi_PPVref[2197]`,
         d_3.5 = `Ymi_PPVref[2204]`-`Ymi_PPVref[2197]`,) %>%
  select(d_0.5:d_3.5)

plot_data_dif_tPPV15 <- 
  plot_data_tPPV15 %>%
  mutate(d_0.5 = `Ymi_PPVref[2122]`-`Ymi_PPVref[2121]`,
         d_1.0 = `Ymi_PPVref[2123]`-`Ymi_PPVref[2121]`,
         d_1.5 = `Ymi_PPVref[2124]`-`Ymi_PPVref[2121]`,
         d_2.0 = `Ymi_PPVref[2125]`-`Ymi_PPVref[2121]`,
         d_2.5 = `Ymi_PPVref[2126]`-`Ymi_PPVref[2121]`,
         d_3.0 = `Ymi_PPVref[2127]`-`Ymi_PPVref[2121]`,
         d_3.5 = `Ymi_PPVref[2128]`-`Ymi_PPVref[2121]`,) %>%
  select(d_0.5:d_3.5)

diff_m_labels <- c('PPV change = 0.5 %','PPV change = 1.0 %','PPV change = 1.5 %','PPV change = 2.0 %','PPV change = 2.5 %','PPV change = 3.0 %',
                   'PPV change = 3.5 %')
names(diff_m_labels) <- c('d_0.5','d_1.0','d_1.5',
                          'd_2.0','d_2.5','d_3.0', 'd_3.5')

vline_data_delta <- tibble(
  dif_m = c('d_0.5', 'd_1.0', 'd_1.5',
            'd_2.0', 'd_2.5', 'd_3.0', 'd_3.5'),
  xinter = c(0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5)
  
)

gg_compare2 <- rbind(
  plot_data_dif_tPPV15 %>%
    pivot_longer(cols='d_0.5':'d_3.5', names_to = 'dif_m', values_to = 'value') %>% mutate(method = 'tPPV15'),
  plot_data_dif_iPPV3 %>%
    pivot_longer(cols='d_0.5':'d_3.5', names_to = 'dif_m', values_to = 'value') %>% mutate(method = 'iPPV3')
)
gg_compare_delta_a <- gg_compare2 %>% filter(dif_m %in% c('d_0.5', 'd_1.0', 'd_1.5',
                                                          'd_2.0', 'd_2.5', 'd_3.0', 'd_3.5')) %>%
  ggplot(aes(x=value, col=method))+
  geom_density(size=1)+
  geom_vline(xintercept = 0)+
  facet_wrap(~ dif_m,ncol=1, labeller = labeller(dif_m= diff_m_labels))+
  theme_minimal()+
  labs (x= 'Absolute change in PPV (%)',y='')+
  scale_color_manual(values = c('#d94801','#238b45'))+
  scale_x_continuous(breaks = c(-4,-2,0,2,4,6,8,10,12))+
  scale_y_continuous(breaks = c(0,0.1,0.2,0.3), 
                     labels = c('','','',''))

gg_compare_delta <-gg_compare_delta_a+ geom_vline(data= vline_data_delta, aes(xintercept = xinter),size=1.1)+
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        strip.text = element_blank())
gg_compare_delta

# fig4
fig_4 <- ggarrange (gg_compare_abs, gg_compare_delta, ncol=2, common.legend = T)
fig_4

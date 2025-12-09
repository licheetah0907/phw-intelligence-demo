# installing tidyverse (ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats, lubridate)
install.packages('tidyverse')
library(tidyverse)

# importing dataset
data <- read.csv('wales_cancer.csv')
View(data)

# separating data to different objects (for easier analysis and visualisation)
population <- subset(data, Indicator == 'Population resident in Wales', 
                     select = c(Date, Measure, Value))

rate_2019 <- subset(data, Measure == 'Age specific rate per 100,000',
                    select = c(Date, Characteristic, Value))

cancer_women <- subset(data, Characteristic == 'Women',
                       select = c(Date, Indicator, Measure, Value))

cancer_men <- subset(data, Characteristic == 'Men',
                     select = c(Date, Indicator, Measure, Value))

# transforming cancer data frames
cancer_women <- cancer_women %>% 
  pivot_wider(names_from = 'Indicator', values_from = 'Value') %>% 
  colnames(cancer_women) <- c('Year', 'Measure', 'Breast',
                                    'Colorectal', 'Lung', 'All_Non-NMSC')

cancer_men <- cancer_men %>% 
  pivot_wider(names_from = 'Indicator', values_from = 'Value') %>% 
  colnames(cancer_men) <- c('Year', 'Measure','Colorectal', 'Lung', 'All_Non-NMSC', 'Prostate')
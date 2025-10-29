# installing tidyverse (ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats, lubridate)
install.packages('tidyverse')
library(tidyverse)

# importing dataset
raw <- read.csv('wales_cancer.csv')
View(raw)

# separating data to different objects (for easier analysis and visualisation)
population <- subset(raw, Indicator == 'Population resident in Wales', 
                     select = c(Date, Measure, Value))

rate_2019 <- subset(raw, Measure == 'Age specific rate per 100,000',
                    select = c(Date, Characteristic, Value))

cancer_women <- subset(raw, Characteristic == 'Women',
                       select = c(Date, Indicator, Measure, Value))

cancer_men <- subset(raw, Characteristic == 'Men',
                     select = c(Date, Indicator, Measure, Value))

# transforming dataset
cancer_women <- cancer_women %>% 
  pivot_wider(names_from = 'Indicator', values_from = 'Value')

cancer_men <- cancer_men %>% 
  pivot_wider(names_from = 'Indicator', values_from = 'Value')
# installing tidyverse (ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats, lubridate)
install.packages('tidyverse')
library(tidyverse)

install.packages('scales')
library(scales)


# replacing 'to' with '-' and specifying the data order
rate_2019 <- rate_2019 %>% 
  mutate(Characteristic = str_replace_all(Characteristic, ' to ', '-')) %>% 
  mutate(Characteristic = fct_inorder(Characteristic))

# plotting the age-specific cancer rate for new cancer cases in 2019 (excl. NMSC)
p2 <- ggplot(data = rate_2019, aes(x = Characteristic, y = Value)) +
               geom_col(fill = 'navy') +
  labs(title = 'Number of New Cancer Cases by Age in 2019 -- excluding Non-Melanoma Skin Cancer',
                 x = 'Age group',
                 y = 'Annual new cases per 100,000')

# modifying y axis
p2 <- p2 + scale_y_continuous(limits = c(0, 2800),
                              expand = c(0,0),
                              breaks = seq(0, 2500, by=500), 
                              labels = label_comma(big.mark = ','))

# rotating x axis
p2 <- p2 + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# cleaning the plot
p2 <- p2 + theme(axis.line = element_line(colour = 'grey'),
           axis.ticks = element_blank(),
           panel.background = element_blank())

# saving the plot
ggsave(filename = "outputs/Wales Age-Specific Cancer Rate 2019.png", plot = p2)
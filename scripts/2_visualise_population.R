# installing tidyverse (ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats, lubridate)
install.packages('tidyverse')
library(tidyverse)

install.packages('scales')
library(scales)


# changing measures to "Historical" and "Projected"
population <- population %>% 
  mutate(Measure = str_replace(Measure, "Historical population", "Historical"), 
         Measure = str_replace(Measure, "Projected population", "Projected"))

# plotting population data
p1 <- ggplot(data = population, aes(x = Date, y = Value, linetype = Measure)) + 
  geom_line(colour = "blue") +
  scale_linetype_manual(values = c("Historical" = 1, "Projected" = 2)) +
      labs(title = "Wales Population Trend and Projection 2002-2035",
       x = 'Year',
       y = 'Population aged 65+',
       linetype = NULL)
  
# modifying x and y axis
p1 <- p1 +
  scale_x_continuous(breaks = seq(2000, 2035, by=5)) +
  scale_y_continuous(limits = c(0, 850000),
                     breaks = seq(0, 8000000, by=200000), 
                     labels = label_comma(big.mark = ","))

# cleaning the plot
p1 <- p1 + theme(axis.line = element_line(colour = "grey"),
           panel.background = element_blank(),
           legend.position = "top")

# saving the plot
ggsave(filename = "outputs/Wales Population Growth Projection.png", plot = p1)
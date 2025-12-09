# installing tidyverse (ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats, lubridate)
install.packages('tidyverse')
library(tidyverse)

install.packages('scales')
library(scales)


# plotting the lung cancer data
women_lung <- ggplot(data = cancer_women, aes(x = Year, y = Lung, colour = 'Historical data')) + 
  geom_line() +
  scale_colour_manual(values = c("Historical data" = "navy")) +
  labs(title = "Lung Cancer Incidence in Women",
       x = 'Year',
       y = 'Cases',
       colour = '') + # removes the legend title

# modifying x and y axis
women_lung <- women_lung +
  scale_x_continuous(limits = c(1990, 2020),
                     breaks = seq(1990, 2020, by=5)) +
  scale_y_continuous(limits = c(0, 1700),
                     expand = c(0,0),
                     breaks = seq(0, 1600, by=200), 
                     labels = label_comma(big.mark = ","))

# cleaning the plot
women_lung <- women_lung + 
  theme(
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.line = element_line(colour = 'grey'),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(color = "grey85"),
    axis.ticks = element_blank(),
    legend.position = "top"
    )


# saving the lung cancer plot
ggsave(filename = 'outputs/Women - Lung Cancer Incidence.png', plot = women_lung)



# creating type of measures (to group projections)

cancer_women <- cancer_women %>% 
  mutate(Type = case_when(
    Measure == "Count" ~ "Historical",
    Measure == "Projected count (mean of all methods)" ~ "Average Projection",
    .default = "Projections"
  ))

# creating the plot: all cancer (exc NMSC) trends and projections in women
women_all <- ggplot() + 
  geom_line(data = cancer_women %>% filter(Type == "Projections"),
            aes(x = Year, y = `All_Non-NMSC`, group = Measure, colour = "Projections"),
            linewidth = 0.5, alpha = 0.5) +
  
  geom_line(data = cancer_women %>% filter(Type == "Average Projection"),
            aes(x = Year, y = `All_Non-NMSC`, colour = "Average projection"),
            linewidth = 0.8) + 
  
  geom_line(data = cancer_women %>% filter(Type == "Historical"), 
            aes(x = Year, y = `All_Non-NMSC`, colour = "Historical data"),
            linewidth = 0.8) + 
  
  scale_colour_manual(
    name = NULL,
    values = c(
      "Projections" = "grey80",
      "Average projection" = "grey40",
      "Historical data" = "navy"
    )
  ) +
  
  scale_x_continuous(limits = c(1990, 2035),
                     breaks = seq(1990, 2035, by=5)) +
  scale_y_continuous(limits = c(0, 14000),
                     expand = c(0,0),
                     breaks = seq(0, 14000, by=2000), 
                     labels = scales::label_comma()) +
  
  labs(
    title = "All Cancers (exc NMSC) Trends and Projections - Women",
    x = "Year",
    y = "Cases"
  ) +

  theme(
    legend.position = "top",
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5, size = 14, face = 'bold'),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(color = "grey85"),
    axis.line = element_line(colour = "grey40", size = 0.8),
    axis.ticks = element_blank()
  )
# saving the plot
ggsave(filename = "outputs/Women - All Cancer (excl. NMSC) Trends and Projections.png", plot = women_all)



# creating the plot: colorectal cancer trends and projections in women
women_colorectal <- ggplot() + 
  geom_line(data = cancer_women %>% filter(Type == "Projections"),
            aes(x = Year, y = Colorectal, group = Measure, colour = "Projections"),
            linewidth = 0.5, alpha = 0.5) +
  
  geom_line(data = cancer_women %>% filter(Type == "Average Projection"),
            aes(x = Year, y = Colorectal, colour = "Average projection"),
            linewidth = 0.8) + 
  
  geom_line(data = cancer_women %>% filter(Type == "Historical"), 
            aes(x = Year, y = Colorectal, colour = "Historical data"),
            linewidth = 0.8) + 
  
  scale_colour_manual(
    name = NULL,
    values = c(
      "Projections" = "grey80",
      "Average projection" = "grey40",
      "Historical data" = "navy"
    )
  ) +
  
  scale_x_continuous(limits = c(1990, 2035),
                     breaks = seq(1990, 2035, by=5)) +
  scale_y_continuous(limits = c(0, 1800),
                     expand = c(0,0),
                     breaks = seq(0, 1800, by=200), 
                     labels = scales::label_comma()) +
  
  labs(
    title = "Colorectal Cancer Trends and Projections - Women",
    x = "Year",
    y = "Cases"
  ) +
  
  theme(
    legend.position = "top",
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5, size = 14, face = 'bold'),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(color = "grey85"),
    axis.line = element_line(colour = "grey40", size = 0.8),
    axis.ticks = element_blank()
  )
# saving the plot
ggsave(filename = "outputs/Women - Colorectal Cancer Trends and Projections.png", plot = women_colorectal)



# creating the plot: breast cancer trends and projections in women
women_breast <- ggplot() + 
  geom_line(data = cancer_women %>% filter(Type == "Projections"),
            aes(x = Year, y = Breast, group = Measure, colour = "Projections"),
            linewidth = 0.5, alpha = 0.5) +
  
  geom_line(data = cancer_women %>% filter(Type == "Average Projection"),
            aes(x = Year, y = Breast, colour = "Average projection"),
            linewidth = 0.8) + 
  
  geom_line(data = cancer_women %>% filter(Type == "Historical"), 
            aes(x = Year, y = Breast, colour = "Historical data"),
            linewidth = 0.8) + 
  
  scale_colour_manual(
    name = NULL,
    values = c(
      "Projections" = "grey80",
      "Average projection" = "grey40",
      "Historical data" = "navy"
    )
  ) +
  
  scale_x_continuous(limits = c(1990, 2035),
                     breaks = seq(1990, 2035, by=5)) +
  scale_y_continuous(limits = c(0, 4000),
                     expand = c(0,0),
                     breaks = seq(0, 4000, by=500), 
                     labels = scales::label_comma()) +
  
  labs(
    title = "Breast Cancer Trends and Projections - Women",
    x = "Year",
    y = "Cases"
  ) +
  
  theme(
    legend.position = "top",
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5, size = 14, face = 'bold'),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(color = "grey85"),
    axis.line = element_line(colour = "grey40", size = 0.8),
    axis.ticks = element_blank()
  )
# saving the plot
ggsave(filename = "outputs/Women - Breast Cancer Trends and Projections.png", plot = women_breast)
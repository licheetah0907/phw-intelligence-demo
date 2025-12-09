# installing tidyverse (ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats, lubridate)
install.packages('tidyverse')
library(tidyverse)

install.packages('scales')
library(scales)


# plotting the lung cancer data
men_lung <- ggplot(data = cancer_men, aes(x = Year, y = Lung, colour = 'Historical data')) + 
  geom_line() +
  scale_colour_manual(values = c("Historical data" = "navy")) +
  labs(title = "Lung Cancer Incidence in Men",
       x = 'Year',
       y = 'Cases',
       colour = '') +
  scale_x_continuous(limits = c(1990, 2020),
                     breaks = seq(1990, 2020, by=5)) +
  scale_y_continuous(limits = c(0, 1700),
                     expand = c(0,0),
                     breaks = seq(0, 1600, by=200), 
                     labels = label_comma(big.mark = ",")) +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5, size = 14, face = 'bold'),
        axis.line = element_line(colour = "grey"),
        axis.ticks = element_blank(),
        panel.background = element_blank())

# saving the lung cancer plot
ggsave(filename = 'outputs/Men Lung Cancer Plot.png', plot = men_lung)


# creating type of measures (to group projections)

cancer_men <- cancer_men %>% 
  mutate(Type = case_when(
    Measure == "Count" ~ "Historical",
    Measure == "Projected count (mean of all methods)" ~ "Average Projection",
    .default = "Projections"
  ))


# creating the plot: all cancer (exc NMSC) trends and projections in men
men_all <- ggplot() + 
  geom_line(data = cancer_men %>% filter(Type == "Projections"),
            aes(x = Year, y = `All_Non-NMSC`, group = Measure, colour = "Projections"),
            linewidth = 0.5, alpha = 0.5) +
  
  geom_line(data = cancer_men %>% filter(Type == "Average Projection"),
            aes(x = Year, y = `All_Non-NMSC`, colour = "Average projection"),
            linewidth = 0.8) + 
  
  geom_line(data = cancer_men %>% filter(Type == "Historical"), 
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
    title = "All Cancers (exc NMSC) Trends and Projections - Men",
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
ggsave(filename = "outputs/Men - All Cancer (excl. NMSC) Trends and Projections.png", plot = men_all)



# creating the plot: colorectal cancer trends and projections in men
men_colorectal <- ggplot() + 
  geom_line(data = cancer_men %>% filter(Type == "Projections"),
            aes(x = Year, y = Colorectal, group = Measure, colour = "Projections"),
            linewidth = 0.5, alpha = 0.5) +
  
  geom_line(data = cancer_men %>% filter(Type == "Average Projection"),
            aes(x = Year, y = Colorectal, colour = "Average projection"),
            linewidth = 0.8) + 
  
  geom_line(data = cancer_men %>% filter(Type == "Historical"), 
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
    title = "Colorectal Cancer Trends and Projections - Men",
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
ggsave(filename = "outputs/Men - Colorectal Cancer Trends and Projections.png", plot = men_colorectal)


# creating the plot: prostate cancer trends and projections in men
men_prostate <- ggplot() + 
  geom_line(data = cancer_men %>% filter(Type == "Projections"),
            aes(x = Year, y = Prostate, group = Measure, colour = "Projections"),
            linewidth = 0.5, alpha = 0.5) +
  
  geom_line(data = cancer_men %>% filter(Type == "Average Projection"),
            aes(x = Year, y = Prostate, colour = "Average projection"),
            linewidth = 0.8) + 
  
  geom_line(data = cancer_men %>% filter(Type == "Historical"), 
            aes(x = Year, y = Prostate, colour = "Historical data"),
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
  scale_y_continuous(limits = c(0, 5000),
                     expand = c(0,0),
                     breaks = seq(0, 5000, by=500), 
                     labels = scales::label_comma()) +
  
  labs(
    title = "Prostate Cancer Trends and Projections - Men",
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
ggsave(filename = "outputs/Men - Prostate Cancer Trends and Projections.png", plot = men_prostate)
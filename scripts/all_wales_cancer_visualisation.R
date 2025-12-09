# installing relevant packages
install.packages('tidyverse')
library(tidyverse)
install.packages('scales')
library(scales)


# import dataset
data <- read.csv('wales_cancer.csv')
View(data)


# visualising aged 65+ population growth projection

population_plot <- ggplot(data = data %>% filter(Indicator == "Population resident in Wales"), 
                          aes(x = Date, y = Value, linetype = Measure)) + 
  geom_line(linewidth = 0.8, colour = "navy") +
  labs(title = "Wales Population Trend and Projection 2002-2035",
       x = 'Year', 
       y = 'Population aged 65+',
       linetype = NULL) +
  scale_x_continuous(breaks = seq(2000, 2035, by=5)) +
  scale_y_continuous(limits = c(0, 1000000),
                     expand = c(0,0),
                     breaks = seq(0, 1000000, by=200000), 
                     labels = label_comma(big.mark = ",")) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(color = "grey85"),
    axis.line = element_line(colour = "grey"),
    legend.position = "top",
    )
# saving population plot
ggsave(filename = "outputs/Wales Aged 65+ Population Projection.png", plot = population_plot)

--------------------------------------------------------------------------------

# visualising age-specific cancer rate

# replacing 'to' with '-' and specifying the data order
data <- data %>% 
  mutate(Characteristic = str_replace_all(Characteristic, ' to ', '-')) %>% 
  mutate(Characteristic = fct_inorder(Characteristic))

# creating the plot
rate_plot <- ggplot(
  data = data %>% filter(Measure == "Age specific rate per 100,000"),
  aes(x = Characteristic, y = Value)) +
  geom_col(fill = 'navy') +
  labs(title = 'Number of New Cancer Cases by Age in 2019 (exc NMSC)',
       x = 'Age group',
       y = 'Annual new cases per 100,000') +
  scale_y_continuous(limits = c(0, 2800),
                     expand = c(0,0),
                     breaks = seq(0, 2500, by=500), 
                     labels = label_comma(big.mark = ',')) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +  #rotate x axis
  theme(
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.line = element_line(colour = 'grey'),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(color = "grey85"),
    axis.ticks = element_blank()
    )
# saving rate plot
ggsave(filename = "outputs/Wales Age-Specific Cancer Rate 2019.png", plot = rate_plot)

--------------------------------------------------------------------------------

# visualising lung cancer trend for both women and men

# creating the plot: lung cancer in women
lung_women <- ggplot(
  data = data %>% filter(Indicator == "Lung cancer incidence", Characteristic == "Women"),
  aes(x = Date, y = Value, colour = "Historical Data")) +
    geom_line(linewidth = 0.8) +
    scale_colour_manual(values = c("Historical Data" = "navy")) +
    labs(title = "Lung Cancer Incidence in Women",
         x = 'Year',
         y = 'Cases',
         colour = '') +                                   # removes the legend title
  scale_x_continuous(limits = c(1990, 2020),
                     breaks = seq(1990, 2020, by=5)) +
  scale_y_continuous(limits = c(0, 1700),
                     expand = c(0,0),
                     breaks = seq(0, 1600, by=200), 
                     labels = label_comma(big.mark = ",")) +
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
# saving the plot
ggsave(filename = 'outputs/Lung Cancer Incidence - Women.png', plot = lung_women)


# creating the plot: lung cancer in men
lung_men <- ggplot(
  data = data %>% filter(Indicator == "Lung cancer incidence", Characteristic == "Men"),
  aes(x = Date, y = Value, colour = "Historical Data")) +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(values = c("Historical Data" = "navy")) +
  labs(title = "Lung Cancer Incidence in Men",
       x = 'Year',
       y = 'Cases',
       colour = '') +                                   # removes the legend title
  scale_x_continuous(limits = c(1990, 2020),
                     breaks = seq(1990, 2020, by=5)) +
  scale_y_continuous(limits = c(0, 1700),
                     expand = c(0,0),
                     breaks = seq(0, 1600, by=200), 
                     labels = label_comma(big.mark = ",")) +
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
# saving the plot
ggsave(filename = 'outputs/Lung Cancer Incidence - Men.png', plot = lung_men)

--------------------------------------------------------------------------------

# creating measure type to aggregate and visualise cancer trends and projections
data <- data %>% 
  mutate(Type = case_when(
    Measure == "Count" ~ "Historical",
    Measure == "Projected count (mean of all methods)" ~ "Average Projection",
    Measure == "Age specific rate per 100,000" ~ "Rate Data",
    Measure == "Historical population" ~ "Population Data",
    Measure == "Projected population" ~ "Population Data",
    .default = "Projections"
  ))

--------------------------------------------------------------------------------

# creating the plot: all cancer (excl. NMSC) trends projections for both women and men

# all cancer in women (all_women)
all_women <- ggplot() +
  geom_line(data = data %>% filter(
    Indicator == "All cancers excluding NMSC incidence",
    Characteristic == "Women",
    Type == "Historical"),
    aes(x = Date, y = Value, colour = "Historical data"),
    linewidth = 0.8) +
  
  geom_line(data = data %>% filter(
    Indicator == "All cancers excluding NMSC incidence",
    Characteristic == "Women",
    Type == "Projections"),
    aes(x = Date, y = Value, group = Measure, colour = "Projections"),
    linewidth = 0.5, alpha = 0.5) + 
  
  geom_line(data = data %>% filter(
    Indicator == "All cancers excluding NMSC incidence",
    Characteristic == "Women",
    Type == "Average Projection"),
    aes(x = Date, y = Value, colour = "Average projection"),
    linewidth = 0.8) +
  
  scale_colour_manual(
    name = NULL,
    values = c(
      "Historical data" = "navy",
      "Projections" = "grey80",
      "Average projection" = "grey40"
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
ggsave(filename = "outputs/All cancers (exc NMSC) - Women.png", plot = all_women)


# all cancer in men (all_men)
all_men <- ggplot() +
  geom_line(data = data %>% filter(
    Indicator == "All cancers excluding NMSC incidence",
    Characteristic == "Men",
    Type == "Historical"),
    aes(x = Date, y = Value, colour = "Historical data"),
    linewidth = 0.8) +
  
  geom_line(data = data %>% filter(
    Indicator == "All cancers excluding NMSC incidence",
    Characteristic == "Men",
    Type == "Projections"),
    aes(x = Date, y = Value, group = Measure, colour = "Projections"),
    linewidth = 0.5, alpha = 0.5) + 
  
  geom_line(data = data %>% filter(
    Indicator == "All cancers excluding NMSC incidence",
    Characteristic == "Men",
    Type == "Average Projection"),
    aes(x = Date, y = Value, colour = "Average projection"),
    linewidth = 0.8) +
  
  scale_colour_manual(
    name = NULL,
    values = c(
      "Historical data" = "navy",
      "Projections" = "grey80",
      "Average projection" = "grey40"
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
ggsave(filename = "outputs/All cancers (exc NMSC) - Men.png", plot = all_men)

-------------------------------------------------------------------------------
  
# creating the plot: colorectal cancer trends and projections for both women and men
  
# colorectal cancer in women
colorectal_women <- ggplot() +
  geom_line(data = data %>% filter(
    Indicator == "Colorectal cancer incidence",
    Characteristic == "Women",
    Type == "Historical"),
    aes(x = Date, y = Value, colour = "Historical data"),
    linewidth = 0.8) +
  
  geom_line(data = data %>% filter(
    Indicator == "Colorectal cancer incidence",
    Characteristic == "Women",
    Type == "Projections"),
    aes(x = Date, y = Value, group = Measure, colour = "Projections"),
    linewidth = 0.5, alpha = 0.5) + 
  
  geom_line(data = data %>% filter(
    Indicator == "Colorectal cancer incidence",
    Characteristic == "Women",
    Type == "Average Projection"),
    aes(x = Date, y = Value, colour = "Average projection"),
    linewidth = 0.8) +
  
  scale_colour_manual(
    name = NULL,
    values = c(
      "Historical data" = "navy",
      "Projections" = "grey80",
      "Average projection" = "grey40")
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
ggsave(filename = "outputs/Colorectal Cancer - Women.png", plot = colorectal_women)


# colorectal cancer in men
colorectal_men <- ggplot() +
  geom_line(data = data %>% filter(
    Indicator == "Colorectal cancer incidence",
    Characteristic == "Men",
    Type == "Historical"),
    aes(x = Date, y = Value, colour = "Historical data"),
    linewidth = 0.8) +
  
  geom_line(data = data %>% filter(
    Indicator == "Colorectal cancer incidence",
    Characteristic == "Men",
    Type == "Projections"),
    aes(x = Date, y = Value, group = Measure, colour = "Projections"),
    linewidth = 0.5, alpha = 0.5) + 
  
  geom_line(data = data %>% filter(
    Indicator == "Colorectal cancer incidence",
    Characteristic == "Men",
    Type == "Average Projection"),
    aes(x = Date, y = Value, colour = "Average projection"),
    linewidth = 0.8) +
  
  scale_colour_manual(
    name = NULL,
    values = c(
      "Historical data" = "navy",
      "Projections" = "grey80",
      "Average projection" = "grey40")
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
ggsave(filename = "outputs/Colorectal Cancer - Men.png", plot = colorectal_men)

--------------------------------------------------------------------------------
  
# creating the plot: breast cancer trends and projections for women

breast_women <- ggplot() +
  geom_line(data = data %>% filter(
    Indicator == "Breast cancer incidence",
    Characteristic == "Women", 
    Type == "Historical"),
    aes(x = Date, y = Value, colour = "Historical data"),
    linewidth = 0.8) +
  
  geom_line(data = data %>% filter(
    Indicator == "Breast cancer incidence",
    Characteristic == "Women", 
    Type == "Projections"),
    aes(x = Date, y = Value, group = Measure, colour = "Projections"),
    linewidth = 0.5, alpha = 0.5) +
  
  geom_line(data = data %>% filter(
    Indicator == "Breast cancer incidence",
    Characteristic == "Women", 
    Type == "Average Projection"),
    aes(x = Date, y = Value, colour = "Average projection"),
    linewidth = 0.8) + 
  
  scale_colour_manual(
    name = NULL,
    values = c("Historical data" = "navy",
              "Projections" = "grey80",
              "Average projection" = "grey40")
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
ggsave(filename = "outputs/Breast Cancer - Women.png", plot = breast_women)

--------------------------------------------------------------------------------

# creating the plot: prostate cancer trends and projections for men

prostate_men <- ggplot() +
  geom_line(data = data %>% filter(
    Indicator == "Prostate cancer incidence",
    Characteristic == "Men", 
    Type == "Historical"),
    aes(x = Date, y = Value, colour = "Historical data"),
    linewidth = 0.8) +
  
  geom_line(data = data %>% filter(
    Indicator == "Prostate cancer incidence",
    Characteristic == "Men", 
    Type == "Projections"),
    aes(x = Date, y = Value, group = Measure, colour = "Projections"),
    linewidth = 0.5, alpha = 0.5) +
  
  geom_line(data = data %>% filter(
    Indicator == "Prostate cancer incidence",
    Characteristic == "Men", 
    Type == "Average Projection"),
    aes(x = Date, y = Value, colour = "Average projection"),
    linewidth = 0.8) + 
  
  scale_colour_manual(
    name = NULL,
    values = c("Historical data" = "navy",
               "Projections" = "grey80",
               "Average projection" = "grey40")
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
ggsave(filename = "outputs/Prostate Cancer - Men.png", plot = prostate_men)
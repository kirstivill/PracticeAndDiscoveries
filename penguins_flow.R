
# Creating geom_alluvium and geom_flow charts on sample data

# R version 4.0.5

library(ggplot2)
library(ggalluvial)
library(readxl)
library(tidyverse)
library(dplyr)
library(ggtext)
library(shadowtext)
library(ggrepel)


set.seed(123)  # Setting seed for reproducibility

# Data -----

# Sample data for the penguins' habitat status over three years with random numbers

habitat_2020 <- c("Inland habitat", "Inland habitat", "Inland habitat", "Inland habitat", "Coastal habitat", "Coastal habitat", "Coastal habitat", "Coastal habitat",
                  "Migratory", "Migratory", "Migratory", "Migratory", "Juvenile", "Juvenile", "Juvenile", "Juvenile")
habitat_2021 <- c("Coastal habitat", "Inland habitat", "Migratory", "Migratory", "Coastal habitat", "Inland habitat", "Coastal habitat", "Coastal habitat",
                  "Inland habitat", "Coastal habitat", "Inland habitat", "Coastal habitat", "Adult", "Adult", "Adult", "Adult")
habitat_2022 <- c("Inland habitat", "Coastal habitat", "Migratory", "Inland habitat", "Inland habitat", "Coastal habitat", "Migratory", "Migratory",
                  "Inland habitat", "Coastal habitat", "Migratory", "Inland habitat", "Inland habitat", "Coastal habitat", "Migratory", "Coastal habitat")
count <- sample(1:100, length(habitat_2020), replace = TRUE)


data <- data.frame(habitat_2020, habitat_2021, habitat_2022, count) 



# Alluvium -----

ggplot(data = data,
       aes(axis1 = habitat_2020,   # First variable on the X-axis
           axis2 = habitat_2021, # Second variable on the X-axis
           axis3 = habitat_2022,   # Third variable on the X-axis
           y = count)) +
  geom_alluvium(aes(fill = habitat_2022), width = 1/10) +
  geom_stratum(width = 1/5, fill = "#FFFFFF", color = "#DDDDDD") +  
  geom_text(stat = "stratum", # specifies that the statistical transformation is based on the "stratum" variable
            aes(label = after_stat(stratum))) + # labels according to stratum, after_stat() ensures the labels are correctly placed within the relevant statistical transformation
  scale_x_discrete(limits = c("habitat_2020", "habitat_2021", "habitat_2022"),
                   labels = c("habitat_2020" = "Status in 2020", "habitat_2021" = "Status in 2021", "habitat_2022" = "Status in 2022"),
                   expand = c(0, 0)) + #0.15, 0.05
  scale_fill_manual(values = c("#04009A",  "#77ACF1",
                               "#3EDBF0", "#C0FEFC"))+ 
  theme_minimal() +
  labs(title = "Penguins habitat status from 2020 to 2022",
       y = "Number of penguins")+
  theme(legend.position = "none",
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 18))




# Flow -----

# wrap text
data$habitat_2020 <- str_wrap(data$habitat_2020, width = 13)
data$habitat_2021 <- str_wrap(data$habitat_2021, width = 13)
data$habitat_2022 <- str_wrap(data$habitat_2022, width = 13)

all_levels <- unique(c(unique(data$habitat_2020), unique(data$habitat_2021), unique(data$habitat_2022)))

# Generate a named color vector for these levels, # color for adult
color_map <- setNames(rep("#CAF0F8", length(all_levels)), all_levels)  # Set the left side colors 
color_map[names(color_map) %in% unique(data$habitat_2020)] <- c("#0077B6", "#77ACF1","#3EDBF0", "#C0FEFC")  # Set the right side colors


# Modify the ggplot code
ggplot(as.data.frame(data),
       aes(y = count,
           axis1 = habitat_2020, axis2 = habitat_2021, axis3 = habitat_2022,
           fill = after_stat(stratum))
       ) + 
  theme_minimal() +
  # geom_alluvium() +
  geom_flow(width = 1/10)+
  geom_stratum(width = 1/5) + 
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum)),
            # curve_type = "quintic",
            size = 5, 
            color= "black") +
  scale_fill_manual(values = color_map) +
  scale_x_discrete(limits = c("habitat_2020", "habitat_2021", "habitat_2022"),
                   labels = c("habitat_2020" = "Status in 2020", "habitat_2021" = "Status in 2021", "habitat_2022" = "Status in 2022"), # x teljel väärtuste nimed teiseks
                   expand = c(0, 0)) + 
  labs(title = "Penguins habitat status from 2020 to 2022",
       y = "Number of Penguins") +
  theme(legend.position="none",
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 18))  





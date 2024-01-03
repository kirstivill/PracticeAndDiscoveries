

# Quarterly calculating how many products have been sold during the past two years


library(dplyr)
library(tidyr)
library(ggplot2)

set.seed(123)

# creating sample data
data <- data.frame(
  Category = sample(c("Electronics", "Clothing", "Home Appliances", "Books", "Furniture",
                      "Sports Equipment", "Beauty Products", "Toys", "Kitchen Appliances",
                      "Music Instruments", "Outdoor Gear", "Pet Supplies", "Art and Crafts",
                      "Office Supplies", "Health and Fitness"), 10000, replace = TRUE), 
  Year = sample(2018:2023, 10000, replace = TRUE),
  Q1 = sample(50:500, 10000, replace = TRUE),
  Q2 = sample(50:500, 10000, replace = TRUE),
  Q3 = sample(50:500, 10000, replace = TRUE),
  Q4 = sample(50:500, 10000, replace = TRUE)
) 

# summing up sales by quarters
data <- data %>% group_by(Category, Year) %>%
  summarise(Q1 = sum(Q1),
            Q2 = sum(Q2),
            Q3 = sum(Q3),
            Q4 = sum(Q4)) %>%
  ungroup()



for (i in c(2020:2023)) {  
  
  for (j in c("Q1", "Q2", "Q3", "Q4")) { 
    
    Year <- i 
    Quarter <- j  
    
    current_status <- paste0(Quarter,"_",Year)
    
    year_1 = Year - 1
    year_2 = Year - 2
    
    starting_point <- case_when(Quarter == "Q4" ~ paste0("Q1","_",year_1),
                                Quarter == "Q3" ~ paste0("Q4","_",year_2),
                                Quarter == "Q2" ~ paste0("Q3","_",year_2),
                                Quarter == "Q1" ~ paste0("Q2","_",year_2))
    
    
    df <- data %>% 
      pivot_wider(names_from="Year", values_from=c("Q1", "Q2", "Q3", "Q4")) %>% 
      mutate_at(vars(2:last_col()), ~ifelse(is.na(.), 0, .)) #  from second to last column, replacing missing values with 0

    # order for years and quarters
    years <- as.numeric(gsub(".*_(\\d+)$", "\\1", colnames(df)))
    quarters <- as.numeric(gsub("Q(\\d).*", "\\1", colnames(df)))
    
    # Order by year, then by quarter
    order_indices <- order(years, quarters)
    
    # Rearrange columns
    df <- df %>% select(Category, all_of(order_indices))
    
    start_index <- which(colnames(df) == starting_point)
    end_index <- which(colnames(df) == current_status)
    
    # Check if starting_point exists in the dataframe
    if(!(starting_point %in% colnames(df))) { # if there is no data for starting point, start from the second column
      start_index <- 2 
    } else {
      start_index <- which(colnames(df) == starting_point) # if starting_point exists, start from starting point
    }
    
    df <- df %>%
      mutate(Sales = rowSums(select(., all_of(start_index):all_of(end_index)), na.rm = TRUE)) %>% 
      mutate(Quarter = Quarter,
             Year = Year)
    
    
    assign(paste0("sales_",i, j), df)
    
  }
  
}

Sales <- bind_rows(
  sales_2020Q1, sales_2020Q2, sales_2020Q3, sales_2020Q4,
  sales_2021Q1, sales_2021Q2, sales_2021Q3, sales_2021Q4,
  sales_2022Q1, sales_2022Q2, sales_2022Q3, sales_2022Q4,
  sales_2023Q1, sales_2023Q2, sales_2023Q3, sales_2023Q4
) %>% 
  select(Category, Year, Quarter, Sales) # everything()

rm(sales_2020Q1, sales_2020Q2, sales_2020Q3, sales_2020Q4,
   sales_2021Q1, sales_2021Q2, sales_2021Q3, sales_2021Q4,
   sales_2022Q1, sales_2022Q2, sales_2022Q3, sales_2022Q4,
   sales_2023Q1, sales_2023Q2, sales_2023Q3, sales_2023Q4)



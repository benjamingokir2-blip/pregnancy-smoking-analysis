# Load packages
  library (tidyverse)
  library (janitor)
  library (naniar)

# Import data
  library(readr)
  raw_data <- read_csv("smok-time-del-msds-eng-q4-2526.csv")
  View(raw_data)


# Inspect dataset
  dim(raw_data)                  # view dimension of dataset

  raw_data <- raw_data |> 
    clean_names()                #standardise column names
    names (raw_data)             # view names column names
  
  glimpse(raw_data)              # view data structure and types
  
  unique_counts <- raw_data |> 
    summarise ( across ( everything(), n_distinct))       #view number of unique records across all columns
  
  raw_data |> 
    distinct ( year,reporting_period, quarter,)           # shows 4 possible combinations
  
  raw_data |> 
    distinct ( org_type)                    # shows 7 unique observations
  
  raw_data |> 
    distinct ( measure)                     # shows 9 unique observations


# convert data type
  raw_data <- raw_data |> 
    mutate ( quarter = as.character(quarter),
           value = readr::parse_number(value))

# Extract regional data
  regional_data <- raw_data |> 
    filter ( org_type == "NHS England (Region)") |> 
    select( -c (org_code, org_type)) |>                 #Drops these two columns which may not be useful
    
    filter ( measure %in% c ( 
      "Number of maternities (with a known smoking status)",
      "Women known to be smokers at time of delivery",
      "Percentage of women known to be smokers at time of delivery")) |>      #filter out relevant measures
    
 # Reshape dataframe
   pivot_wider ( names_from = "measure",
                values_from = "value") 
  
# Check missing / duplicate records

  miss_var_summary ( regional_data)
  sum ( duplicated ( regional_data))

# Drop unknown regional identifier
  regional_data <- regional_data |> 
    filter ( org_name != "Unknown") |> 
    arrange ( quarter) |> 
    
# Calculate prevalence and create a new column for the figures  
  mutate ( calculated_prevalence = 
             `Women known to be smokers at time of delivery` / 
             `Number of maternities (with a known smoking status)` * 100) |> 

   
# Round calculated prevalence to one decimal place, for consistency
  mutate ( calculated_prevalence = 
             round ( calculated_prevalence, digits = 1 ) ) |> 

# Reorder columns to place prevalence columns side-by-side for comparison 
  select ( 1, 2, 3, 4, 5, 7, 6, 8) 


# Create line graph

  regional_data |> 
    ggplot ( 
      aes ( 
        x = quarter,
        y = calculated_prevalence,
        colour = org_name,
        group = org_name)) +
    
      geom_point ( size = 2) +
    
      geom_line ( linewidth = 0.7 ) +
    
    labs ( title = "Smoking prevalence at delivery across English regions", 
           subtitle = "Quarterly data, 2025/26",
           x = "Quarter", 
           y = "Smoking prevalence (%)",
             colour = "Region") +
    
    scale_x_discrete ( 
      labels = 
        c ( "1" = "Apr–Jun 2025",
            "2" = "Jul–Sep 2025",
            "3" = "Oct–Dec 2025",
            "4" = "Jan–Mar 2026" )) +
    
      scale_colour_manual ( 
        values = c (
          "East of England" = "#023BE6",
          "London" = "#FA5019",
          "Midlands" = "#009E73",
          "North East and Yorkshire" = "#CC79A7",
          "North West" = "#E69F00",
          "South East" = "#95C11F",
          "South West" = "#FA0080" ) ) +
    
      theme_minimal ( )


# Create bar plot for the latest Quarter (Q4)
  regional_data |> 
    filter ( quarter == "4") |> 
    
    ggplot ( 
      aes ( 
        x = calculated_prevalence,
        y = forcats:: fct_reorder ( org_name, calculated_prevalence),
        )) +
    
      geom_col ( fill = "#3D737A",  width = 0.7, show.legend = NULL ) +
    
      labs ( title = "Smoking prevalence at delivery by English region", 
             subtitle = "January–March 2026 (Q4)",
             x = "Smoking prevalence (%)", 
             y = NULL,) +
    
      theme_minimal ()  

  
  
  



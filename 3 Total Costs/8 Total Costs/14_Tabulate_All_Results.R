library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(writexl)

# 1. Base directory and discount-rate folders
base_dir <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/3 Total Costs/10 Total Costs Results Final"
rates    <- c("1.5", "2", "2.5")
folders  <- paste0("discount rate ", rates, " percent")

# 2. Helper: read, recode, format, pivot, and write one folder
process_one <- function(folder, rate) {
  in_csv   <- file.path(base_dir, folder, "All_Costs.csv")
  out_xlsx <- file.path(
    base_dir, folder,
    paste0("Formatted_Cost_Matrix_", gsub("\\.", "_", rate), "pct.xlsx")
  )
  
  # Read & round, then recode Cost_Type values
  df <- read_csv(in_csv, show_col_types = FALSE) %>%
    mutate(
      Cost_bUSD = round(Cost_bUSD, 2),
      Cost_Type = recode(
        Cost_Type,
        Total_Costs            = "Sum",
        `Unmet Demand Penalty` = "Unmet demand penalty",
        `Air Emissions`        = "Air emissions",
        `GHG Emissions`        = "GHG emissions"
      )
    )
  
  # Split out min / mean / max
  min_df  <- df %>% filter(Statistic == "min")  %>% select(Pathway, Cost_Type, min  = Cost_bUSD)
  mean_df <- df %>% filter(Statistic == "mean") %>% select(Pathway, Cost_Type, mean = Cost_bUSD)
  max_df  <- df %>% filter(Statistic == "max")  %>% select(Pathway, Cost_Type, max  = Cost_bUSD)
  
  # Join, build "mean\n(min-max)", pivot to wide, order rows
  formatted <- list(mean_df, min_df, max_df) %>%
    reduce(full_join, by = c("Pathway", "Cost_Type")) %>%
    mutate(
      Formatted = paste0(
        mean, "\n(",
        min, "-", max, ")"
      )
    ) %>%
    select(Pathway, Cost_Type, Formatted) %>%
    pivot_wider(
      names_from = Pathway,
      values_from = Formatted
    ) %>%
    arrange(factor(
      Cost_Type,
      levels = c(
        "CAPEX",
        "Fixed O&M",
        "Variable O&M",
        "Fuel",
        "Imports",
        "GHG emissions",
        "Air emissions",
        "Unmet demand penalty",
        "Sum"
      )
    ))
  
  # Write to Excel
  write_xlsx(formatted, path = out_xlsx)
  message("✔️ Written: ", out_xlsx)
  
  invisible(formatted)
}

# 3. Process all discount-rate folders and keep results in R
results_list <- map2(folders, rates, process_one)
names(results_list) <- paste0("data_", gsub("\\.", "_", rates), "pct")

# install.packages(c("readr","dplyr","tidyr","writexl"))  # if not already installed

library(readr)
library(dplyr)
library(tidyr)
library(writexl)

# 1. Base path and folder names
base_dir_2 <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/3 Total Costs/9 Total Costs Results"
rates    <- c("1.5", "2", "2.5")
folders  <- paste0("discount rate ", rates, " percent")

# 2. Loop through each folder
for (i in seq_along(folders)) {
  folder   <- folders[i]
  rate     <- rates[i]
  in_csv   <- file.path(base_dir_2, folder, "County_Level_Emissions_Summary_Total.csv")
  out_xlsx <- file.path(
    base_dir, folder,
    paste0("Formatted_Emissions_Matrix_", gsub("\\.", "_", rate), "pct.xlsx")
  )
  
  # 3. Read the CSV
  df <- read_csv(in_csv, show_col_types = FALSE)
  
  # 4. Build Location and Formatted columns, then pivot wide
  formatted_df <- df %>%
    mutate(
      Location            = paste0(County, ", ", State),
      total_mean_emission = round(total_mean_emission, 2),
      total_min_emission  = round(total_min_emission,  2),
      total_max_emission  = round(total_max_emission,  2),
      Formatted = paste0(
        total_mean_emission, "\n(",
        total_min_emission, "-", total_max_emission, ")"
      )
    ) %>%
    select(Location, Pathway, Formatted) %>%
    pivot_wider(
      names_from = Pathway,
      values_from = Formatted
    ) %>%
    arrange(Location)
  
  # 5. Write out the Excel file
  write_xlsx(formatted_df, path = out_xlsx)
  message("✔️ Written: ", out_xlsx)
}





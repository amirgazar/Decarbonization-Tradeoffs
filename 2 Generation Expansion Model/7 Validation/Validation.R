## ===========================
## Validation: Deterministic Trajectory (Total Load & Renewables)
## ===========================

library(data.table)
library(readxl)
library(ggplot2)
library(gridExtra) # For combining plots

# --- 1. USER SETTINGS & PATHS (GENERIC) ---
target_scenario <- "High Electrification"
target_pathway  <- "B1"
years_to_validate <- seq(2025, 2050, by = 5)

# >>> UPDATE THIS ROOT DIRECTORY ONLY <<<
# Example: "C:/Users/Name/Documents/GitHub/Decarbonization-Tradeoffs"
proj_dir <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs"

# Construct File Paths Relative to Root
model_file <- file.path(proj_dir, "2 Generation Expansion Model", "5 Dispatch Curve", 
                        "4 Final Results", "1 Comprehensive Days Summary Results", "Yearly_Results.csv")

excel_file <- file.path(proj_dir, "4 External Data", 
                        "Massachusetts 2050 Decarbonization Roadmap Study", 
                        "Massachusetts Workbook of Energy Modeling Results 2024.xlsx")

# --- 2. LOAD MODEL RESULTS ---
if (!file.exists(model_file)) stop(paste("Model file not found at:", model_file))
cat("Loading Model Results...\n")
model_data <- fread(model_file)

# Filter
model_data <- model_data[Pathway == target_pathway & Year %in% years_to_validate]

# Calculate Metrics (Per Simulation)
# Total Load = Sum of all Generation (No curtailment subtraction, matching ISO-NE method)
model_sims <- model_data[, .(
  Renewables = Solar_TWh + Onshore_TWh + Offshore_TWh + Hydro_TWh + Biomass_TWh,
  Total_Load = (Old_Fossil_Fuels_adj_TWh + New_Fossil_Fuel_TWh) + 
    Nuclear_TWh + 
    (Solar_TWh + Onshore_TWh + Offshore_TWh + Hydro_TWh + Biomass_TWh) + 
    Calibrated_Total_import_net_TWh
), by = .(Simulation, Year)]

# Calculate Median Trajectories (The "Model Line")
model_stats <- model_sims[, .(
  Model_Renewables = median(Renewables),
  Model_TotalLoad  = median(Total_Load)
), by = Year]

# --- 3. LOAD BENCHMARK (ISO-NE) ---
if (!file.exists(excel_file)) stop(paste("Benchmark file not found at:", excel_file))
cat("Loading Benchmark Data...\n")
raw_gen <- as.data.table(read_excel(excel_file, sheet = "10. Electricity Generation", col_names = FALSE))

get_benchmark <- function(yr) {
  # Find Column
  scenarios <- as.character(raw_gen[2, ])
  years_row <- as.character(raw_gen[3, ])
  col_idx <- which(grepl(target_scenario, scenarios, ignore.case = TRUE) & 
                     grepl(as.character(yr), years_row))[1]
  
  if (is.na(col_idx)) return(NULL)
  
  # 1. Summary Table (Clean Gen)
  start_row <- which(raw_gen[[1]] == "New England Clean Electricity Generation (TWh)")
  summary_data <- raw_gen[(start_row + 3):nrow(raw_gen), c(1, col_idx), with = FALSE]
  colnames(summary_data) <- c("Type", "Value")
  summary_data[, Value := as.numeric(Value)]
  
  # Sum Renewables
  renew_val <- sum(summary_data[grepl("solar|wind|hydro", Type, ignore.case=TRUE)]$Value, na.rm=TRUE)
  
  # 2. Fossil (Top Section)
  top_section <- raw_gen[1:(start_row-1)]
  col_top <- which(grepl(target_scenario, as.character(top_section[2,])) & 
                     grepl(as.character(yr), as.character(top_section[3,])))[1]
  fossil_data <- top_section[4:nrow(top_section), c(2, col_top), with = FALSE]
  fossil_val <- sum(as.numeric(fossil_data[grepl("gas|coal|oil", ...2, ignore.case=TRUE)][[2]]), na.rm=TRUE)
  
  # 3. Total Gen (Grand Total Clean + Fossil)
  # Note: Summary table "Grand Total" is just clean gen. Need to add Fossil.
  # Or sum components manually to be safe.
  nuc_val <- sum(summary_data[grepl("nuclear", Type, ignore.case=TRUE)]$Value, na.rm=TRUE)
  imp_val <- sum(summary_data[grepl("transmission", Type, ignore.case=TRUE)]$Value, na.rm=TRUE)
  total_val <- renew_val + nuc_val + imp_val + fossil_val
  
  return(data.table(Year = yr, Bench_Renewables = renew_val, Bench_TotalLoad = total_val))
}

bench_results <- rbindlist(lapply(years_to_validate, get_benchmark))

# --- 4. CALCULATE R-SQUARED ---
validation_df <- merge(model_stats, bench_results, by = "Year")

calc_r2 <- function(obs, pred) {
  ss_res <- sum((obs - pred)^2)
  ss_tot <- sum((obs - mean(obs))^2)
  return(1 - (ss_res / ss_tot))
}

r2_load <- calc_r2(validation_df$Bench_TotalLoad, validation_df$Model_TotalLoad)
r2_renew <- calc_r2(validation_df$Bench_Renewables, validation_df$Model_Renewables)

print("--- VALIDATION STATISTICS ---")
print(validation_df)
cat(sprintf("\nR-squared (Total Load): %.4f", r2_load))
cat(sprintf("\nR-squared (Renewables): %.4f\n", r2_renew))

# --- 5. PLOTTING ---
# Reshape for faceted plot
plot_load <- melt(validation_df[, .(Year, Model = Model_TotalLoad, Benchmark = Bench_TotalLoad)], 
                  id.vars = "Year", variable.name = "Source", value.name = "TWh")
plot_load[, Metric := "Total Annual Load"]

plot_renew <- melt(validation_df[, .(Year, Model = Model_Renewables, Benchmark = Bench_Renewables)], 
                   id.vars = "Year", variable.name = "Source", value.name = "TWh")
plot_renew[, Metric := "Renewable Generation"]

plot_data <- rbind(plot_load, plot_renew)

# Annotation Data for R2
ann_text <- data.table(
  Year = 2025, TWh = 300, 
  Metric = c("Total Annual Load", "Renewable Generation"),
  Label = c(paste0("R² = ", round(r2_load, 3)), paste0("R² = ", round(r2_renew, 3)))
)

p <- ggplot(plot_data, aes(x = Year, y = TWh, color = Source, linetype = Source)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  
  facet_wrap(~Metric, scales = "free_y") +
  
  # Add R2 Labels
  geom_text(data = ann_text, aes(label = Label), 
            x = 2030, y = Inf, vjust = 2, color = "black", size = 5, fontface = "bold", inherit.aes = FALSE) +
  
  scale_color_manual(values = c("Model" = "#377eb8", "Benchmark" = "#e41a1c")) +
  
  labs(
       subtitle = paste("Scenario:", target_scenario, "| Comparisons of Median Model Trajectory vs ISO-NE Target"),
       y = "Annual Energy (TWh)",
       x = "Year") +
  
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold", size = 14),
        plot.title = element_text(face = "bold", size = 16),
        axis.title = element_text(face = "bold", size = 12))

print(p)
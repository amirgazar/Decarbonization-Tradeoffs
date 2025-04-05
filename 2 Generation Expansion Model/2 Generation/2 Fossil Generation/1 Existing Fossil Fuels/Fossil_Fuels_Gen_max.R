# Fossil Fuels Maximums with Retirement Integration
library(data.table)
library(ggplot2)

# Load facility (NPC) data with retirement info and generation data
npc_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/2 Generation/2 Fossil Generation/1 Existing Fossil Fuels/1 Fossil Fuels Facilities Data/Fossil_Fuel_Facilities_Data.csv"
Fossil_Fuels_NPC <- fread(npc_path)

gen_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/2 Generation/2 Fossil Generation/1 Existing Fossil Fuels/2 Fossil Fuels Generation and Emissions/Fossil_Fuel_Generation_Emissions.csv"
Fossil_Fuels_Gen <- fread(gen_path)

# Create simulation grid: Dates (2025-2050) x Hours (1-24)
Dates <- seq.Date(as.Date("2025-01-01"), as.Date("2050-12-31"), by = "day")
full_grid <- CJ(Date = Dates, Hour = 1:24)
full_grid[, `:=`(Year = as.integer(format(Date, "%Y")),
                 DayLabel = as.integer(format(Date, "%j")))]

# Compute hourly maximum generation per facility using columns that start with "Gen_"
hourly_max <- Fossil_Fuels_Gen[, .(
  max_gen_MW_hr = do.call(pmax, c(.SD, na.rm = TRUE))
), by = .(Facility_Unit.ID, DayLabel, Hour), .SDcols = patterns("^Gen_")]

# Merge retirement info into the generation data
hourly_max <- merge(
  hourly_max, 
  Fossil_Fuels_NPC[, .(Facility_Unit.ID, Retirement_year)], 
  by = "Facility_Unit.ID", 
  all.x = TRUE
)

# Merge the simulation grid with hourly generation data.
# The merge by DayLabel and Hour "broadcasts" each facility's daily profile across all Dates sharing that DayLabel.
merged_grid <- merge(
  full_grid, 
  hourly_max, 
  by = c("DayLabel", "Hour"), 
  allow.cartesian = TRUE
)

# Flag records as retired if the simulation Year is greater than or equal to the facility's Retirement_year.
# Facilities with NA in Retirement_year are assumed active.
merged_grid[, is_retired := !is.na(Retirement_year) & Year >= Retirement_year]

# Aggregate generation by Date, Hour, and DayLabel.
hourly_totals <- merged_grid[, .(
  max_gen_hr_retired_MW = sum(max_gen_MW_hr[is_retired == TRUE], na.rm = TRUE),
  max_gen_hr_active_MW  = sum(max_gen_MW_hr[is_retired == FALSE], na.rm = TRUE)
), by = .(Date, Hour, DayLabel)]

# Check for NA and zero values in the aggregated columns
if (nrow(hourly_totals[is.na(max_gen_hr_retired_MW)]) > 0) {
  print("NA in retired generation")
} else {
  print("No NA in retired generation")
}

if (nrow(hourly_totals[max_gen_hr_retired_MW == 0]) > 0) {
  print("Zero retired generation")
} else {
  print("No zero retired generation")
}

if (nrow(hourly_totals[is.na(max_gen_hr_active_MW)]) > 0) {
  print("NA in active generation")
} else {
  print("No NA in active generation")
}

if (nrow(hourly_totals[max_gen_hr_active_MW == 0]) > 0) {
  print("Zero active generation")
} else {
  print("No zero active generation")
}

# Reshape to long format for plotting
hourly_long <- melt(hourly_totals, 
                    id.vars = c("Date", "DayLabel", "Hour"), 
                    measure.vars = c("max_gen_hr_retired_MW", "max_gen_hr_active_MW"),
                    variable.name = "Retirement_Status", 
                    value.name = "Total_Gen")

# Rename the retirement status labels for clarity
hourly_long[, Retirement_Status := fifelse(Retirement_Status == "max_gen_hr_retired_MW", 
                                           "Retired", "Non-Retired")]

# Plot by DayLabel, faceted by Retirement_Status and simulation year (extracted from Date)
ggplot(hourly_long, aes(x = DayLabel, y = Total_Gen, color = factor(Hour))) +
  geom_line() +
  facet_grid(Retirement_Status ~ format(Date, "%Y"), scales = "free_x") +
  labs(x = "Day Label (Day of Year)",
       y = "Total Max Generation (MW/hr)",
       color = "Hour",
       title = "Hourly Total Generation by Retirement Status") +
  theme_minimal()

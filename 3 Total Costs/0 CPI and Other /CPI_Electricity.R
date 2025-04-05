#library(devtools)
#install_github("mikeasilva/blsAPI")
# Load necessary libraries
library(blsAPI)
library(rjson)

# Your API key (store it securely)
api_key <- "a2c1e702f81947118e83cb8d029d6623"

# Define the series IDs you want to query
series_ids <- c("APU011072610", "APUS11A72610","APU012072610", "APUS12A72610", "APUS12B72610")
regions <- c("New England", "Boston", "Middle Atlantic", "New York", "Philadelphia")

# Create the payload for the API request
payload <- list(
  'seriesid' = series_ids,
  'regions' = regions,
  'startyear' = '2022',
  'endyear' = '2024',
  'registrationkey' = api_key
)

# Request data from the BLS API
response <- blsAPI(payload)
json_data <- fromJSON(response)

# Extract and organize the data into a data frame
cpi_data <- do.call(rbind, lapply(json_data$Results$series, function(series) {
  data.frame(
    seriesID = series$seriesID,
    Region = series$regions,
    year = sapply(series$data, function(x) x$year),
    period = sapply(series$data, function(x) x$period),
    periodName = sapply(series$data, function(x) x$periodName),
    value = sapply(series$data, function(x) as.numeric(x$value))
  )
}))

# View the resulting data frame
print(cpi_data)

file_path <-  "/Users/amirgazar/Downloads/Fossil_Fuels.rds"
test <- readRDS(file_path)

test <- test[test$Facility_Unit.ID == "542_13", ]

Fossil_Fuels <- Fossil_Fuels[Fossil_Fuels$Facility_Unit.ID == "542_13", ]
# Assuming df1 and df2 are your data frames

result <- all.equal(test, Fossil_Fuels)

# Print the result
print(result)

z <- test[ , .N, by = .(Facility_Unit.ID)]
y <- EPADecarb_data[ , .N, by = .(Facility_Unit.ID)]

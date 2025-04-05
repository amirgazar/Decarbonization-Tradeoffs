library(randtoolbox)

# Function to generate pseudo-random sequences
generate_pseudo_random_sequences <- function(num_sequences, num_numbers) {
  random_sequences <- replicate(num_sequences, sample(1:99, num_numbers, replace = TRUE), simplify = FALSE)
  return(random_sequences)
}

# Parameters
num_sequences <- 1e6
num_numbers <- 250

# Generate Pseudo-random sequences
Percentile_sequences_random <- generate_pseudo_random_sequences(num_sequences, num_numbers)

# Convert the list to a matrix and transpose it
Percentile_sequences_matrix <- t(do.call(rbind, Percentile_sequences_random))

# Save Pseudo-random sequences as a CSV
file_path_random_csv <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/4 Randomization/1 Randomized Data/Random_Sequence.csv"
write.csv(Percentile_sequences_matrix, file_path_random_csv, row.names = FALSE)

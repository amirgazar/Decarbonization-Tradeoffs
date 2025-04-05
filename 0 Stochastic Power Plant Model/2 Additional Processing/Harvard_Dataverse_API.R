# Load the necessary libraries
library(httr)
library(jsonlite)

# Set up your Dataverse API key and server
api_token <- "dcde5cbe-2043-4d84-8cb4-248716537659"  # Replace with your API key
server_url <- "https://dataverse.harvard.edu"  # Replace with your Dataverse server URL
dataverse_id <- "amirgazar_test"  # Replace with your Dataverse alias (the collection you're creating the dataset in)

# Create the metadata JSON for the dataset with corrected field types
metadata <- list(
  datasetVersion = list(
    protocol = "doi",
    authority = "10.502",
    identifier = "ZZ7/MOSEISLEYDB94",
    metadataBlocks = list(
      citation = list(
        fields = list(
          list(
            typeName = "title",
            multiple = FALSE,
            value = "Imported dataset with package files No. 3",
            typeClass = "primitive"
          ),
          list(
            typeName = "productionDate",
            multiple = FALSE,
            value = "2011-02-23",
            typeClass = "primitive"
          ),
          list(
            typeName = "dsDescription",
            multiple = TRUE,
            value = list(
              list(
                dsDescriptionValue = list(
                  typeName = "dsDescriptionValue",
                  multiple = FALSE,
                  value = "Native Dataset",
                  typeClass = "primitive"
                )
              )
            ),
            typeClass = "compound"
          ),
          list(
            typeName = "subject",
            multiple = TRUE,
            value = list("Medicine, Health and Life Sciences"),
            typeClass = "controlledVocabulary"
          ),
          list(
            typeName = "author",
            multiple = TRUE,
            value = list(
              list(
                authorAffiliation = list(
                  typeName = "authorAffiliation",
                  multiple = FALSE,
                  value = "LibraScholar Medical School",
                  typeClass = "primitive"
                ),
                authorName = list(
                  typeName = "authorName",
                  multiple = FALSE,
                  value = "Mortazavigazar, Amir",
                  typeClass = "primitive"
                )
              ),
              list(
                authorAffiliation = list(
                  typeName = "authorAffiliation",
                  multiple = FALSE,
                  value = "LibraScholar Medical School",
                  typeClass = "primitive"
                ),
                authorName = list(
                  typeName = "authorName",
                  multiple = FALSE,
                  value = "Prof, Arthur",
                  typeClass = "primitive"
                )
              )
            ),
            typeClass = "compound"
          ),
          list(
            typeName = "depositor",
            multiple = FALSE,
            value = "Prof, Arthur",
            typeClass = "primitive"
          ),
          list(
            typeName = "datasetContact",
            multiple = TRUE,
            value = list(
              list(
                datasetContactEmail = list(
                  typeName = "datasetContactEmail",
                  multiple = FALSE,
                  value = "amirgazar@vt.edu",
                  typeClass = "primitive"
                )
              )
            ),
            typeClass = "compound"
          )
        ),
        displayName = "Citation Metadata"
      )
    )
  )
)

# Convert the metadata to JSON format
metadata_json <- toJSON(metadata, pretty = TRUE)

# Print the JSON to verify it matches the desired format
cat(metadata_json)

# Define the API endpoint for creating a dataset
create_dataset_url <- paste0(server_url, "/api/dataverses/", dataverse_id, "/datasets")

# Upload the dataset
response <- httr::POST(
  create_dataset_url,
  add_headers("X-Dataverse-key" = api_token),
  body = metadata_json,
  encode = "json"
)

# Check the response
if (response$status_code == 200) {
  print("Dataset created successfully!")
} else {
  print(paste("Failed to create dataset:", response$status_code))
  print(content(response, "text"))
}

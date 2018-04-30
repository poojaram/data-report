#install.packages("httr")
#install.packages("jsonlite")
#install.packages("dplyr")
#install.packages("markdown")
#install.packages("knitr")

# Loading all the required libraries
library("httr")
library("jsonlite")
library("dplyr")
library("markdown")
library("knitr")

# sourcing Access key
source("api_key.R")

# base URI
base_uri <- "https://api.propublica.org/congress/v1"

# A function that receives the resource uri and returns the parsed data
parse_the_data <- function(resource_uri) {
  get_uri <- GET(paste0(base_uri,resource_uri), add_headers('X-API-Key' = api_key))
  parsed_data <- content(get_uri, "text") %>%
    fromJSON()
  parsed_data
}

# Bills data
resource_uri <- "/115/both/bills/updated.json"

# Call for the function that takes the resource uri and parses the data
# and returns the content body
parsed_data <- parse_the_data(resource_uri)

# Selects the required data frame from deeper level
bills_data <- parsed_data$results$bills

# Flattening the data and arranging them from most recent bills on top
bills_data_frame <- flatten(data.frame(bills_data)) %>%
  arrange(desc(introduced_date))

# Creating a new column with 'Sponsor name, Sponsor party, Sponsor State'
# for the bills
bills_data_frame <- mutate(bills_data_frame, legislation_info = paste(bills_data_frame$sponsor_name, bills_data_frame$sponsor_party, bills_data_frame$sponsor_state, sep = ", "))

# Filtering out the data with only the required columns and informations
bills_data_frame <- select(bills_data_frame, 4, 6, 35, 18, 31, 16)

# Filtering out the most recent 10 bills
bills_data_frame <- head(bills_data_frame, 10)

############################################
################## PART 2 ##################
############################################



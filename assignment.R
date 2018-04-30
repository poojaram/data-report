#install.packages("httr")
#install.packages("jsonlite")
#install.packages("dplyr")
#install.packages("markdown")
#install.packages("knitr")
#install.packages("ggplot2")

# Loading all the required libraries
library("httr")
library("jsonlite")
library("dplyr")
library("markdown")
library("knitr")
library("ggplot2")

# sourcing Access key
source("api_key.R")

############################################
################## PART 1 ##################
############################################

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
bills_all <- flatten(data.frame(bills_data)) %>%
  arrange(desc(introduced_date))

# Creating a new column with 'Sponsor name, Sponsor party, Sponsor State'
# for the bills
bills_all <- mutate(bills_all, legislation_info = paste(bills_all$sponsor_name, bills_all$sponsor_party, bills_all$sponsor_state, sep = ", "))

# Filtering out the data with only the required columns and informations
bills_data_frame <- select(bills_all, 4, 6, 35, 18, 31, 16)

# Filtering out the most recent 10 bills
bills_data_frame <- head(bills_data_frame, 10)

############################################
################## PART 2 ##################
############################################

# One selected bill based on the bill that has maximum cosponsors
one_bill <- filter(bills_all, bills_all$cosponsors == max(bills_all$cosponsors))

# Filtering out the important information
chosen_bill <- select_(one_bill, 'bill_id', 'bill_uri', 'title', 'sponsor_uri', 'govtrack_url',
                    'active', 'introduced_date','cosponsors', 'committees','latest_major_action',
                    'cosponsors_by_party.D','cosponsors_by_party.R', 'legislation_info')

# Getting the data
chosen_bill_id <- one_bill$bill_id
chosen_bill_slug <- one_bill$bill_slug
chosen_bill_name <- chosen_bill$title
chosen_bill_rep <- chosen_bill$legislation_info
chosen_bill_url <- chosen_bill$govtrack_url

related_uri <- paste0("/115/bills/", chosen_bill_slug, "/related.json")
related_response <- GET(paste0(base_uri, related_uri), add_headers('X-API-Key' = api_key))

parsed_chosen_data <- parse_the_data(related_response)

############################################
################## PART 3 ##################
############################################





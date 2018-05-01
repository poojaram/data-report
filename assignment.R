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
resource_uri <- "/bills/search.json?query=immigration"

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
bills_data_frame <- select(bills_all, 4, 6, 35, 31, 32, 16)

values <- c("Bill ID","Name","Legislator Info", "Latest action date", "Status", "More Info")

names(bills_data_frame) <- values

# Filtering out the most recent 10 bills
bills_data_frame <- head(bills_data_frame, 10)

############################################
################## PART 2 ##################
############################################

# One selected bill based on the bill that has maximum cosponsors
one_bill <- filter(bills_all, bills_all$cosponsors == max(bills_all$cosponsors))

# Pasting the uri
related_uri <- paste0("/115/bills/", one_bill$bill_slug, ".json")

# Calling the function to extract and pass the data content
related_response <- parse_the_data(related_uri)

# Getting the required data frame
related_data <- related_response$results

# Filtering out the important information
chosen_bill <- select_(related_data, 'number', 'title', 'introduced_date', 'sponsor', 'sponsor_party',
                       'sponsor_state', 'latest_major_action_date', c)

# Combing the Sponsor name, Sponsor party and sponsor state as one value
chosen_bill <- mutate(chosen_bill, legislation_info = paste(chosen_bill$sponsor, chosen_bill$sponsor_party, chosen_bill$sponsor_state, sep = ", "))

# Selecting the required details to display as output
chosen_bill <- select(chosen_bill, 'number', 'title', 'introduced_date', 'legislation_info','latest_major_action_date',
                      'latest_major_action', 'govtrack_url')

# Checking if the bill was passed and other voting details
results <- related_data$votes[[1]]

############################################
################## PART 3 ##################
############################################

# Uri for current WA state members of the house
members_uri <- "/members/house/WA/current.json"

# passing the uri to obtain the member data
parsed_member_data <- parse_the_data(members_uri)

# Getting the required data frame from the member data
member_data <- parsed_member_data$results

# Chosen reprensentative ID
representative_id <- "S000510"

# uri for the chosen representative
member_data_uri <- paste0("/members/", representative_id, ".json")

# passing the chosen representative uri to obtain the representative data
rep_parsed_data <- parse_the_data(member_data_uri)

# uri for the bills passed by the chosen representative
rep_bills_uri <- paste0("/members/", representative_id, "/bills/introduced.json")

#passing the uri to obtain the chosen representative's bill data
bills_results <- parse_the_data(rep_bills_uri)

# Getting the required data frame about the bills passed by this representative
rep_bills_data <- bills_results$results
representative_bills_data <- data.frame(rep_bills_data$bills) %>%
  flatten()

# Representative's contact information
twitter <- member_data$twitter_id
more_info <- member_data$times_topics_url
name <- paste(member_data$first_name, member_data$last_name)

# Selecting the required informations to display
rep_billdata <- select(representative_bills_data, number, title, latest_major_action, govtrack_url)

resource.uri <- paste0("/members/", representative_id, "/votes.json")

parsed_resource_data <- parse_the_data(resource.uri)

resource_data <- as.data.frame(parsed_resource_data$results[["votes"]])

my_senator_votes <- data.frame(resource_data$votes) %>%
  flatten()
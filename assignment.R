# Loading all the required libraries
library("httr")
library("jsonlite")
library("dplyr")
library("markdown")
library("knitr")

# sourcing Access key
source("api_key.R")

# topic for the recent 10 bills
resource_uri <- "/bills/search.json?query=immigration"

# Representative ID
representative_id <- "S000510"

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


# One selected bill based on the bill that has maximum cosponsors
one_bill <- filter(bills_all, bills_all$cosponsors == max(bills_all$cosponsors))

# Pasting the uri
related_uri <- paste0("/115/bills/", one_bill$bill_slug, ".json")

# Calling the function to extract and pass the data content
related_response <- parse_the_data(related_uri)

related_data <- related_response$results

# Filtering out the important information
chosen_bill <- select_(related_data, 'number', 'title', 'introduced_date', 'sponsor', 'sponsor_party', 'sponsor_state', 'latest_major_action_date', 'latest_major_action', 'govtrack_url')

chosen_bill <- mutate(chosen_bill, legislation_info = paste(chosen_bill$sponsor, chosen_bill$sponsor_party, chosen_bill$sponsor_state, sep = ", "))

chosen_bill <- select(chosen_bill, 'number', 'title', 'introduced_date', 'legislation_info','latest_major_action_date', 'latest_major_action', 'govtrack_url')

#kable(chosen_bill, format = "markdown")

# Uri for current WA state members of the house
members_uri <- "/members/house/WA/current.json"

# passing the uri to obtain the member data
parsed_member_data <- parse_the_data(members_uri)

# Getting the required data frame from the member data
member_data <- parsed_member_data$results

# filtering the data based on chosen reprensentative ID
member_data <- filter(member_data, id == representative_id)

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
name <- paste(member_data$first_name, member_data$last_name)
party <- member_data$party
twitter <- member_data$twitter_id
facebook <- member_data$facebook_account
more_info <- member_data$times_topics_url

# Selecting the required informations to display
rep_billdata <- select(representative_bills_data, number, title)

# Selecting the required informations to display
rep_billdata <- select(representative_bills_data, number, title, latest_major_action, govtrack_url)

# For calculating the percentage the legislator represented the majority-opposing party

# uri to get the votes of the chosen representative
resource.uri <- paste0("/members/", representative_id, "/votes.json")

# Parsing to obtain the required data
parsed_resource_data <- parse_the_data(resource.uri) 

# Getting the required data frame
resource_data <- flatten(data.frame(parsed_resource_data$results$votes))

# Creating a common column to join two data frame
resource_data <- mutate(resource_data, roll_call = as.character(roll_call))

# Votes data

# uri to get the votes data
votes_uri <- "/house/votes/recent.json"

# Parsing to obtain the required data 
parsed_votes_data <- parse_the_data(votes_uri)

# Getting the required data
votes_data <- parsed_votes_data$results$votes %>% flatten()

# Creating a common column to join the two data frames
votes_data <- mutate(votes_data, roll_call = as.character(roll_call))

# Joining the two dataframes and arranging them from most recent
votes_joined_data <- left_join(votes_data, resource_data, by = "roll_call") %>%
arrange(desc(date.x), desc(time.x))

# Obtaining the 20 most recent file
votes_joined_data <- head(votes_joined_data, 20)


# Filtering the bills voted where the chosen representative took position with the
# majority of the opposing party
democratic_with_republicans <- filter(votes_joined_data, votes_joined_data$republican.majority_position == votes_joined_data$position)

# Count of times the democratic representative agreed with the republicans 
count_democratic_with_republicans <- as.numeric(nrow(democratic_with_republicans))

# Total number of bills we are considering
total_count <- as.numeric(nrow(votes_joined_data))

# Percentage of the times the democratic representative agreed with the republicans
percent <- count_democratic_with_republicans*100/total_count

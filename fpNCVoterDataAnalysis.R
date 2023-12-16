library(tidycensus)
library(tidyverse)
library(ggplot2)
library(maps)
library(sf)
library(scales)

ncvoter <- read_tsv("C:/Users/jsali/Downloads/ncvoter_Statewide/ncvoter_Statewide.txt", 
                    col_types = cols(
                      voter_reg_num = col_character(),
                      birth_year = col_integer(),
                      race_code = col_character(),
                      gender_code = col_character(),
                      party_cd = col_character(),
                      registr_dt = col_date(format = "%m/%d/%Y")
                    ))

ncvhis <- read_tsv("C:/Users/jsali/Downloads/ncvhis_Statewide.zip", 
                   col_types = cols(
                     voter_reg_num = col_character(),
                     election_lbl = col_character(),
                     voting_method = col_character()
                   ))

# One-Stop and By-Mail Voting Data for 2020 and 2022
# Create the Republican Voters 2020 dataset
republican_voters_2020 <- ncvhis %>%
  filter(grepl("2020", election_lbl)) %>%
  inner_join(ncvoter %>% filter(party_cd == "REP"), by = "voter_reg_num") %>%
  distinct(voter_reg_num, .keep_all = TRUE) # Keep all columns and ensure unique voters

# Total number of Republican voters in 2020
total_republican_voters_2020 <- republican_voters_2020 %>%
  summarise(total = n())

# Filter for Republicans who voted by mail in 2020
mail_voters_2020_distinct <- republican_voters_2020 %>%
  filter(voting_method == "ABSENTEE BY MAIL") %>%
  distinct(voter_reg_num) # Ensure unique voters

percentage_mail_voters_2020_distinct <- nrow(mail_voters_2020_distinct) / total_republican_voters_2020$total * 100

print(paste("Percentage of Republicans who voted by mail in 2020:", round(percentage_mail_voters_2020_distinct, 2)))

# Creating the Republican Voters 2022 dataset
republican_voters_2022 <- ncvhis %>%
  filter(grepl("2022", election_lbl)) %>%
  inner_join(ncvoter %>% filter(party_cd == "REP"), by = "voter_reg_num") %>%
  distinct(voter_reg_num, .keep_all = TRUE) 

total_republican_voters_2022 <- republican_voters_2022 %>%
  summarise(total = n())

# Filter for Republicans who voted by mail in 2022
mail_voters_2022 <- republican_voters_2022 %>%
  filter(voting_method == "ABSENTEE BY MAIL") %>%
  distinct(voter_reg_num) # Ensure unique voters

percentage_mail_voters_2022 <- nrow(mail_voters_2022) / total_republican_voters_2022$total * 100

print(paste("Percentage of Republicans who voted by mail in 2022:", round(percentage_mail_voters_2022, 2)))

# Create the Democratic Voters 2020 dataset
democratic_voters_2020 <- ncvhis %>%
  filter(grepl("2020", election_lbl)) %>%
  inner_join(ncvoter %>% filter(party_cd == "DEM"), by = "voter_reg_num") %>%
  distinct(voter_reg_num, .keep_all = TRUE) # Keep all columns and ensure unique voters

# Total number of Democratic voters in 2020
total_democratic_voters_2020 <- democratic_voters_2020 %>%
  summarise(total = n())

# Filter for Democrats who voted by mail in 2020
mail_voters_2020_democrats <- democratic_voters_2020 %>%
  filter(voting_method == "ABSENTEE BY MAIL") %>%
  distinct(voter_reg_num) # Ensure unique voters

percentage_mail_voters_2020_democrats <- nrow(mail_voters_2020_democrats) / total_democratic_voters_2020$total * 100

print(paste("Percentage of Democrats who voted by mail in 2020:", round(percentage_mail_voters_2020_democrats, 2)))

# Recreating the Democratic Voters 2022 dataset
democratic_voters_2022 <- ncvhis %>%
  filter(grepl("2022", election_lbl)) %>%
  inner_join(ncvoter %>% filter(party_cd == "DEM"), by = "voter_reg_num") %>%
  distinct(voter_reg_num, .keep_all = TRUE) # Keep all columns and ensure unique voters

total_democratic_voters_2022 <- democratic_voters_2022 %>%
  summarise(total = n())

# Filter for Democrats who voted by mail in 2022
mail_voters_2022_democrats <- democratic_voters_2022 %>%
  filter(voting_method == "ABSENTEE BY MAIL") %>%
  distinct(voter_reg_num) # Ensure unique voters

percentage_mail_voters_2022_democrats <- nrow(mail_voters_2022_democrats) / total_democratic_voters_2022$total * 100

print(paste("Percentage of Democrats who voted by mail in 2022:", round(percentage_mail_voters_2022_democrats, 2)))

#ONESTOP VOTING 
# Filter for Republicans who voted via one-stop in 2020
one_stop_voters_2020_republicans <- republican_voters_2020 %>%
  filter(voting_method == "ABSENTEE ONESTOP") %>%
  distinct(voter_reg_num)

percentage_one_stop_voters_2020_republicans <- nrow(one_stop_voters_2020_republicans) / total_republican_voters_2020$total * 100

print(paste("Percentage of Republicans who voted via one-stop in 2020:", round(percentage_one_stop_voters_2020_republicans, 2)))

# Filter for Republicans who voted via one-stop in 2022
one_stop_voters_2022_republicans <- republican_voters_2022 %>%
  filter(voting_method == "ABSENTEE ONESTOP") %>%
  distinct(voter_reg_num)

percentage_one_stop_voters_2022_republicans <- nrow(one_stop_voters_2022_republicans) / total_republican_voters_2022$total * 100

print(paste("Percentage of Republicans who voted via one-stop in 2022:", round(percentage_one_stop_voters_2022_republicans, 2)))

# Filter for Democrats who voted via one-stop in 2020
one_stop_voters_2020_democrats <- democratic_voters_2020 %>%
  filter(voting_method == "ABSENTEE ONESTOP") %>%
  distinct(voter_reg_num)

percentage_one_stop_voters_2020_democrats <- nrow(one_stop_voters_2020_democrats) / total_democratic_voters_2020$total * 100

print(paste("Percentage of Democrats who voted via one-stop in 2020:", round(percentage_one_stop_voters_2020_democrats, 2)))

# Filter for Democrats who voted via one-stop in 2022
one_stop_voters_2022_democrats <- democratic_voters_2022 %>%
  filter(voting_method == "ABSENTEE ONESTOP") %>%
  distinct(voter_reg_num)

percentage_one_stop_voters_2022_democrats <- nrow(one_stop_voters_2022_democrats) / total_democratic_voters_2022$total * 100

print(paste("Percentage of Democrats who voted via one-stop in 2022:", round(percentage_one_stop_voters_2022_democrats, 2)))

# Combine the percentage data into a data frame
voting_data <- data.frame(
  Party = rep(c("Republican", "Democrat"), each = 4),
  Year = rep(c("2020 Mail", "2022 Mail", "2020 One-Stop", "2022 One-Stop"), 2),
  Percentage = c(percentage_mail_voters_2020_distinct, 
                 percentage_mail_voters_2022, 
                 percentage_one_stop_voters_2020_republicans, 
                 percentage_one_stop_voters_2022_republicans,
                 percentage_mail_voters_2020_democrats, 
                 percentage_mail_voters_2022_democrats,
                 percentage_one_stop_voters_2020_democrats, 
                 percentage_one_stop_voters_2022_democrats)
)

ggplot(voting_data, aes(x = Year, y = Percentage, fill = Party)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Voting Methods by Party in 2020 and 2022",
       x = "Year and Method",
       y = "Percentage (%)") +
  scale_fill_manual(values = c("Republican" = "red", "Democrat" = "blue")) +
  theme_minimal()

# People who did not vote
# 2020 Election Day: Nov 3
cut_off_date <- as.Date("2020-11-03")

# Filter all registered Republicans as of the cut-off date
all_registered_republicans_2020 <- ncvoter %>%
  filter(party_cd == "REP" & registr_dt <= cut_off_date)

# Get the Republicans who did not vote in 2020
non_voting_republicans_2020 <- anti_join(all_registered_republicans_2020, republican_voters_2020, by = "voter_reg_num")

total_registered_republicans_2020 <- nrow(all_registered_republicans_2020)
total_non_voting_republicans_2020 <- nrow(non_voting_republicans_2020)

percentage_non_voting_republicans_2020 <- (total_non_voting_republicans_2020 / total_registered_republicans_2020) * 100

print(paste("Percentage of Registered Republicans Who Did Not Vote in 2020:", round(percentage_non_voting_republicans_2020, 2)))

# 2022 Election Day
cut_off_date_2022 <- as.Date("2022-11-08")

# Filter all registered Republicans in 2022 as of the cut-off date
all_registered_republicans_2022 <- ncvoter %>%
  filter(party_cd == "REP" & registr_dt <= cut_off_date_2022)

non_voting_republicans_2022 <- anti_join(all_registered_republicans_2022, republican_voters_2022, by = "voter_reg_num")

total_registered_republicans_2022 <- nrow(all_registered_republicans_2022)
total_non_voting_republicans_2022 <- nrow(non_voting_republicans_2022)

percentage_non_voting_republicans_2022 <- (total_non_voting_republicans_2022 / total_registered_republicans_2022) * 100

print(paste("Percentage of Registered Republicans Who Did Not Vote in 2022:", round(percentage_non_voting_republicans_2022, 2)))

cut_off_date_2020 <- as.Date("2020-11-03")

# Filter all registered Democrats as of the cut-off date in 2020
all_registered_democrats_2020 <- ncvoter %>%
  filter(party_cd == "DEM" & registr_dt <= cut_off_date_2020)

non_voting_democrats_2020 <- anti_join(all_registered_democrats_2020, democratic_voters_2020, by = "voter_reg_num")

total_registered_democrats_2020 <- nrow(all_registered_democrats_2020)
total_non_voting_democrats_2020 <- nrow(non_voting_democrats_2020)

percentage_non_voting_democrats_2020 <- (total_non_voting_democrats_2020 / total_registered_democrats_2020) * 100

print(paste("Percentage of Registered Democrats Who Did Not Vote in 2020:", round(percentage_non_voting_democrats_2020, 2)))

# Filter all registered Democrats as of the cut-off date in 2020
all_registered_democrats_2020 <- ncvoter %>%
  filter(party_cd == "DEM" & registr_dt <= cut_off_date_2020)

non_voting_democrats_2020 <- anti_join(all_registered_democrats_2020, democratic_voters_2020, by = "voter_reg_num")

total_registered_democrats_2020 <- nrow(all_registered_democrats_2020)
total_non_voting_democrats_2020 <- nrow(non_voting_democrats_2020)

percentage_non_voting_democrats_2020 <- (total_non_voting_democrats_2020 / total_registered_democrats_2020) * 100

print(paste("Percentage of Registered Democrats Who Did Not Vote in 2020:", round(percentage_non_voting_democrats_2020, 2)))

# Filter all registered Democrats as of the cut-off date in 2022
all_registered_democrats_2022 <- ncvoter %>%
  filter(party_cd == "DEM" & registr_dt <= cut_off_date_2022)

non_voting_democrats_2022 <- anti_join(all_registered_democrats_2022, democratic_voters_2022, by = "voter_reg_num")

total_registered_democrats_2022 <- nrow(all_registered_democrats_2022)
total_non_voting_democrats_2022 <- nrow(non_voting_democrats_2022)

percentage_non_voting_democrats_2022 <- (total_non_voting_democrats_2022 / total_registered_democrats_2022) * 100

print(paste("Percentage of Registered Democrats Who Did Not Vote in 2022:", round(percentage_non_voting_democrats_2022, 2)))

voter_turnout_data <- data.frame(
  Party = c("Republican", "Republican", "Democrat", "Democrat"),
  Year = c("2020", "2022", "2020", "2022"),
  Percentage_Not_Voted = c(percentage_non_voting_republicans_2020, 
                           percentage_non_voting_republicans_2022, 
                           percentage_non_voting_democrats_2020, 
                           percentage_non_voting_democrats_2022)
)

ggplot(voter_turnout_data, aes(x = Year, y = Percentage_Not_Voted, fill = Party)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Percentage of Registered Voters Who Did Not Vote (2020 vs 2022)",
       x = "Year",
       y = "Percentage Not Voted (%)") +
  scale_fill_manual(values = c("Republican" = "red", "Democrat" = "blue")) +
  theme_minimal()

# IN PERSON VOTING DATA
# Filter for Republicans who voted in-person on Election Day in 2020
election_day_voters_2020_republicans <- republican_voters_2020 %>%
  filter(voting_method == "IN-PERSON") %>%
  distinct(voter_reg_num)

percentage_election_day_voters_2020_republicans <- nrow(election_day_voters_2020_republicans) / total_republican_voters_2020$total * 100

print(paste("Percentage of Republicans who voted in-person on Election Day in 2020:", round(percentage_election_day_voters_2020_republicans, 2)))

# Calculate total in-person voting percentage (One-Stop + Election Day)
total_in_person_voting_2020_republicans <- percentage_one_stop_voters_2020_republicans + percentage_election_day_voters_2020_republicans

print(paste("Total Percentage of Republicans who voted in-person in 2020:", round(total_in_person_voting_2020_republicans, 2)))

# Filter for Republicans who voted in-person on Election Day in 2022
election_day_voters_2022_republicans <- republican_voters_2022 %>%
  filter(voting_method == "IN-PERSON") %>%
  distinct(voter_reg_num)

percentage_election_day_voters_2022_republicans <- nrow(election_day_voters_2022_republicans) / total_republican_voters_2022$total * 100

print(paste("Percentage of Republicans who voted in-person on Election Day in 2022:", round(percentage_election_day_voters_2022_republicans, 2)))

total_in_person_voting_2022_republicans <- percentage_one_stop_voters_2022_republicans + percentage_election_day_voters_2022_republicans

print(paste("Total Percentage of Republicans who voted in-person in 2022:", round(total_in_person_voting_2022_republicans, 2)))

# Filter for Democrats who voted in-person on Election Day in 2020
election_day_voters_2020_democrats <- democratic_voters_2020 %>%
  filter(voting_method == "IN-PERSON") %>%
  distinct(voter_reg_num)

percentage_election_day_voters_2020_democrats <- nrow(election_day_voters_2020_democrats) / total_democratic_voters_2020$total * 100

print(paste("Percentage of Democrats who voted in-person on Election Day in 2020:", round(percentage_election_day_voters_2020_democrats, 2)))

total_in_person_voting_2020_democrats <- percentage_one_stop_voters_2020_democrats + percentage_election_day_voters_2020_democrats

print(paste("Total Percentage of Democrats who voted in-person in 2020:", round(total_in_person_voting_2020_democrats, 2)))

# Filter for Democrats who voted in-person on Election Day in 2022
election_day_voters_2022_democrats <- democratic_voters_2022 %>%
  filter(voting_method == "IN-PERSON") %>%
  distinct(voter_reg_num)

percentage_election_day_voters_2022_democrats <- nrow(election_day_voters_2022_democrats) / total_democratic_voters_2022$total * 100

print(paste("Percentage of Democrats who voted in-person on Election Day in 2022:", round(percentage_election_day_voters_2022_democrats, 2)))

total_in_person_voting_2022_democrats <- percentage_one_stop_voters_2022_democrats + percentage_election_day_voters_2022_democrats

print(paste("Total Percentage of Democrats who voted in-person in 2022:", round(total_in_person_voting_2022_democrats, 2)))

voting_data <- data.frame(
  Party = c("Republican", "Republican", "Democrat", "Democrat"),
  Year = c("2020", "2022", "2020", "2022"),
  Percentage = c(total_in_person_voting_2020_republicans, 
                 total_in_person_voting_2022_republicans, 
                 total_in_person_voting_2020_democrats, 
                 total_in_person_voting_2022_democrats)
)


ggplot(voting_data, aes(x = interaction(Party, Year), y = Percentage, fill = Party)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "In-Person Voting Percentages by Party and Year",
       x = "Party and Year",
       y = "Percentage of In-Person Voters (%)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

# Track Voting Behavior: Republican One-Stop Voters in 2020
# Get the voter registration numbers of Republicans who voted one-stop in 2020
one_stop_voters_2020_republicans <- republican_voters_2020 %>%
  filter(voting_method == "ABSENTEE ONESTOP") %>%
  select(voter_reg_num)

voting_behavior_2022 <- ncvhis %>%
  filter(voter_reg_num %in% one_stop_voters_2020_republicans$voter_reg_num, grepl("2022", election_lbl)) %>%
  inner_join(ncvoter %>% filter(party_cd == "REP"), by = "voter_reg_num")

voting_method_distribution_2022 <- voting_behavior_2022 %>%
  group_by(voting_method) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

print(voting_method_distribution_2022)

# Get the voter registration numbers of Democrats who voted one-stop in 2020
one_stop_voters_2020_democrats <- democratic_voters_2020 %>%
  filter(voting_method == "ABSENTEE ONESTOP") %>%
  select(voter_reg_num)

voting_behavior_2022_democrats <- ncvhis %>%
  filter(voter_reg_num %in% one_stop_voters_2020_democrats$voter_reg_num, grepl("2022", election_lbl)) %>%
  inner_join(ncvoter %>% filter(party_cd == "DEM"), by = "voter_reg_num")

voting_method_distribution_2022_democrats <- voting_behavior_2022_democrats %>%
  group_by(voting_method) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

print(voting_method_distribution_2022_democrats)

# IN PERSON VOTING IN 2020 to 2022 
in_person_voters_2020_republicans <- republican_voters_2020 %>%
  filter(voting_method == "IN-PERSON") %>%
  select(voter_reg_num)

voting_behavior_2022_republicans <- ncvhis %>%
  filter(voter_reg_num %in% in_person_voters_2020_republicans$voter_reg_num, grepl("2022", election_lbl)) %>%
  inner_join(ncvoter %>% filter(party_cd == "REP"), by = "voter_reg_num")

voting_method_distribution_2022_republicans <- voting_behavior_2022_republicans %>%
  group_by(voting_method) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

in_person_voters_2020_democrats <- democratic_voters_2020 %>%
  filter(voting_method == "IN-PERSON") %>%
  select(voter_reg_num)

voting_behavior_2022_democrats <- ncvhis %>%
  filter(voter_reg_num %in% in_person_voters_2020_democrats$voter_reg_num, grepl("2022", election_lbl)) %>%
  inner_join(ncvoter %>% filter(party_cd == "DEM"), by = "voter_reg_num")

voting_method_distribution_2022_democrats <- voting_behavior_2022_democrats %>%
  group_by(voting_method) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

ggplot(voting_method_distribution_2022_republicans, aes(x = voting_method, y = percentage, fill = voting_method)) +
  geom_bar(stat = "identity") +
  labs(title = "2022 Voting Methods of Republicans Who Voted In Person in 2020",
       x = "Voting Method",
       y = "Percentage (%)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

ggplot(voting_method_distribution_2022_democrats, aes(x = voting_method, y = percentage, fill = voting_method)) +
  geom_bar(stat = "identity") +
  labs(title = "2022 Voting Methods of Democrats Who Voted In Person in 2020",
       x = "Voting Method",
       y = "Percentage (%)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

# BY MAIL VOTING IN 2020 to 2022
mail_voters_2020_republicans <- republican_voters_2020 %>%
  filter(voting_method == "ABSENTEE BY MAIL") %>%
  select(voter_reg_num)

voting_behavior_2022_mail_republicans <- ncvhis %>%
  filter(voter_reg_num %in% mail_voters_2020_republicans$voter_reg_num, grepl("2022", election_lbl)) %>%
  inner_join(ncvoter %>% filter(party_cd == "REP"), by = "voter_reg_num")

voting_method_distribution_2022_mail_republicans <- voting_behavior_2022_mail_republicans %>%
  group_by(voting_method) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

mail_voters_2020_democrats <- democratic_voters_2020 %>%
  filter(voting_method == "ABSENTEE BY MAIL") %>%
  select(voter_reg_num)

voting_behavior_2022_mail_democrats <- ncvhis %>%
  filter(voter_reg_num %in% mail_voters_2020_democrats$voter_reg_num, grepl("2022", election_lbl)) %>%
  inner_join(ncvoter %>% filter(party_cd == "DEM"), by = "voter_reg_num")

voting_method_distribution_2022_mail_democrats <- voting_behavior_2022_mail_democrats %>%
  group_by(voting_method) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

ggplot(voting_method_distribution_2022_mail_republicans, aes(x = voting_method, y = percentage, fill = voting_method)) +
  geom_bar(stat = "identity") +
  labs(title = "2022 Voting Methods of Republicans Who Voted By Mail in 2020",
       x = "Voting Method",
       y = "Percentage (%)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

ggplot(voting_method_distribution_2022_mail_democrats, aes(x = voting_method, y = percentage, fill = voting_method)) +
  geom_bar(stat = "identity") +
  labs(title = "2022 Voting Methods of Democrats Who Voted By Mail in 2020",
       x = "Voting Method",
       y = "Percentage (%)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

# Filter for Republicans who voted by mail in 2020
mail_voters_2020_republicans <- republican_voters_2020 %>%
  filter(voting_method == "ABSENTEE BY MAIL") %>%
  select(voter_reg_num)

voting_behavior_2022_mail_republicans <- ncvhis %>%
  filter(voter_reg_num %in% mail_voters_2020_republicans$voter_reg_num, grepl("2022", election_lbl)) %>%
  inner_join(ncvoter %>% filter(party_cd == "REP"), by = "voter_reg_num")

voting_method_distribution_2022_mail_republicans <- voting_behavior_2022_mail_republicans %>%
  group_by(voting_method) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

print(voting_method_distribution_2022_mail_republicans)

# FOR REPUBLICANS WHO VOTED IN PERSON 2020
# Join with ncvoter to get race demographics
voting_behavior_2022_republicans_race <- voting_behavior_2022_republicans %>%
  inner_join(ncvoter, by = "voter_reg_num")

race_changes_2022_republicans_in_person <- voting_behavior_2022_republicans_race %>%
  group_by(race_code, voting_method) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

print(race_changes_2022_republicans_in_person)

# ONE STOP VOTING in 2020 to 2022
one_stop_voters_2020_republicans <- republican_voters_2020 %>%
  filter(voting_method == "ABSENTEE ONESTOP") %>%
  select(voter_reg_num)

voting_behavior_2022_one_stop_republicans <- ncvhis %>%
  filter(voter_reg_num %in% one_stop_voters_2020_republicans$voter_reg_num, grepl("2022", election_lbl)) %>%
  inner_join(ncvoter %>% filter(party_cd == "REP"), by = "voter_reg_num")

voting_method_distribution_2022_one_stop_republicans <- voting_behavior_2022_one_stop_republicans %>%
  group_by(voting_method) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

one_stop_voters_2020_democrats <- democratic_voters_2020 %>%
  filter(voting_method == "ABSENTEE ONESTOP") %>%
  select(voter_reg_num)

voting_behavior_2022_one_stop_democrats <- ncvhis %>%
  filter(voter_reg_num %in% one_stop_voters_2020_democrats$voter_reg_num, grepl("2022", election_lbl)) %>%
  inner_join(ncvoter %>% filter(party_cd == "DEM"), by = "voter_reg_num")

voting_method_distribution_2022_one_stop_democrats <- voting_behavior_2022_one_stop_democrats %>%
  group_by(voting_method) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

ggplot(voting_method_distribution_2022_one_stop_republicans, aes(x = voting_method, y = percentage, fill = voting_method)) +
  geom_bar(stat = "identity") +
  labs(title = "2022 Voting Methods of Republicans Who Voted One-Stop in 2020",
       x = "Voting Method",
       y = "Percentage (%)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

ggplot(voting_method_distribution_2022_one_stop_democrats, aes(x = voting_method, y = percentage, fill = voting_method)) +
  geom_bar(stat = "identity") +
  labs(title = "2022 Voting Methods of Democrats Who Voted One-Stop in 2020",
       x = "Voting Method",
       y = "Percentage (%)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

# RACE VOTER DEMOGRAPHICS
# Filter for Republicans who voted via one-stop in 2020
one_stop_voters_2020_republicans <- republican_voters_2020 %>%
  filter(voting_method == "ABSENTEE ONESTOP") %>%
  distinct(voter_reg_num)

# Join with ncvoter to get race demographics
one_stop_voters_2020_republicans_race <- one_stop_voters_2020_republicans %>%
  inner_join(ncvoter, by = "voter_reg_num")

race_demographics_2020_republicans <- one_stop_voters_2020_republicans_race %>%
  group_by(race_code) %>%
  summarise(count = n())

race_demographics_2020_republicans <- race_demographics_2020_republicans %>%
  mutate(percentage = (count / sum(count)) * 100)

print(race_demographics_2020_republicans)

ggplot(race_demographics_2020_republicans, aes(x = race_code, y = percentage, fill = race_code)) +
  geom_bar(stat = "identity") +
  labs(title = "Race Demographics for Republican One-Stop Voters in 2020",
       x = "Race Code",
       y = "Percentage of Voters (%)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

# Filter for Republicans who voted via one-stop in 2022
one_stop_voters_2022_republicans <- republican_voters_2022 %>%
  filter(voting_method == "ABSENTEE ONESTOP") %>%
  distinct(voter_reg_num)

one_stop_voters_2022_republicans_race <- one_stop_voters_2022_republicans %>%
  inner_join(ncvoter, by = "voter_reg_num")

race_demographics_2022_republicans <- one_stop_voters_2022_republicans_race %>%
  group_by(race_code) %>%
  summarise(count = n())

race_demographics_2022_republicans <- race_demographics_2022_republicans %>%
  mutate(percentage = (count / sum(count)) * 100)

print(race_demographics_2022_republicans)

ggplot(race_demographics_2022_republicans, aes(x = race_code, y = percentage, fill = race_code)) +
  geom_bar(stat = "identity") +
  labs(title = "Race Demographics for Republican One-Stop Voters in 2022",
       x = "Race Code",
       y = "Percentage of Voters (%)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

# Filter for Democrats who voted via one-stop in 2020
one_stop_voters_2020_democrats <- democratic_voters_2020 %>%
  filter(voting_method == "ABSENTEE ONESTOP") %>%
  distinct(voter_reg_num)

one_stop_voters_2020_democrats_race <- one_stop_voters_2020_democrats %>%
  inner_join(ncvoter, by = "voter_reg_num")

race_demographics_2020_democrats <- one_stop_voters_2020_democrats_race %>%
  group_by(race_code) %>%
  summarise(count = n())

race_demographics_2020_democrats <- race_demographics_2020_democrats %>%
  mutate(percentage = (count / sum(count)) * 100)

print(race_demographics_2020_democrats)

ggplot(race_demographics_2020_democrats, aes(x = race_code, y = percentage, fill = race_code)) +
  geom_bar(stat = "identity") +
  labs(title = "Race Demographics for Democrat One-Stop Voters in 2020",
       x = "Race Code",
       y = "Percentage of Voters (%)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

# Filter for Democrats who voted via one-stop in 2022
one_stop_voters_2022_democrats <- democratic_voters_2022 %>%
  filter(voting_method == "ABSENTEE ONESTOP") %>%
  distinct(voter_reg_num)

# Join with ncvoter to get race demographics
one_stop_voters_2022_democrats_race <- one_stop_voters_2022_democrats %>%
  inner_join(ncvoter, by = "voter_reg_num")

race_demographics_2022_democrats <- one_stop_voters_2022_democrats_race %>%
  group_by(race_code) %>%
  summarise(count = n())

race_demographics_2022_democrats <- race_demographics_2022_democrats %>%
  mutate(percentage = (count / sum(count)) * 100)

print(race_demographics_2022_democrats)

ggplot(race_demographics_2022_democrats, aes(x = race_code, y = percentage, fill = race_code)) +
  geom_bar(stat = "identity") +
  labs(title = "Race Demographics for Democrat One-Stop Voters in 2022",
       x = "Race Code",
       y = "Percentage of Voters (%)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

# Filter for Democrats who voted by mail in 2020
mail_voters_2020_democrats <- democratic_voters_2020 %>%
  filter(voting_method == "ABSENTEE BY MAIL") %>%
  distinct(voter_reg_num)

mail_voters_2020_democrats_race <- mail_voters_2020_democrats %>%
  inner_join(ncvoter, by = "voter_reg_num")

race_demographics_2020_mail_democrats <- mail_voters_2020_democrats_race %>%
  group_by(race_code) %>%
  summarise(count = n())

race_demographics_2020_mail_democrats <- race_demographics_2020_mail_democrats %>%
  mutate(percentage = (count / sum(count)) * 100)

print(race_demographics_2020_mail_democrats)

ggplot(race_demographics_2020_mail_democrats, aes(x = race_code, y = percentage, fill = race_code)) +
  geom_bar(stat = "identity") +
  labs(title = "Race Demographics for Democrat Mail Voters in 2020",
       x = "Race Code",
       y = "Percentage of Voters (%)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

# Filter for Democrats who voted by mail in 2022
mail_voters_2022_democrats <- democratic_voters_2022 %>%
  filter(voting_method == "ABSENTEE BY MAIL") %>%
  distinct(voter_reg_num)

mail_voters_2022_democrats_race <- mail_voters_2022_democrats %>%
  inner_join(ncvoter, by = "voter_reg_num")

race_demographics_2022_mail_democrats <- mail_voters_2022_democrats_race %>%
  group_by(race_code) %>%
  summarise(count = n())

race_demographics_2022_mail_democrats <- race_demographics_2022_mail_democrats %>%
  mutate(percentage = (count / sum(count)) * 100)

race_demographics_2022_mail_democrats <- race_demographics_2022_mail_democrats %>%
  mutate(percentage = (count / sum(count)) * 100)

print(race_demographics_2022_mail_democrats)

ggplot(race_demographics_2022_mail_democrats, aes(x = race_code, y = percentage, fill = race_code)) +
  geom_bar(stat = "identity") +
  labs(title = "Race Demographics for Democrat Mail Voters in 2022",
       x = "Race Code",
       y = "Percentage of Voters (%)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

# Filter for Republicans who voted by mail in 2022
mail_voters_2022_republicans <- republican_voters_2022 %>%
  filter(voting_method == "ABSENTEE BY MAIL") %>%
  distinct(voter_reg_num)

mail_voters_2022_republicans_race <- mail_voters_2022_republicans %>%
  inner_join(ncvoter, by = "voter_reg_num")

race_demographics_2022_mail_republicans <- mail_voters_2022_republicans_race %>%
  group_by(race_code) %>%
  summarise(count = n())

race_demographics_2022_mail_republicans <- race_demographics_2022_mail_republicans %>%
  mutate(percentage = (count / sum(count)) * 100)

print(race_demographics_2022_mail_republicans)

race_demographics_2022_mail_republicans <- race_demographics_2022_mail_republicans %>%
  mutate(percentage = (count / sum(count)) * 100)

ggplot(race_demographics_2022_mail_republicans, aes(x = race_code, y = percentage, fill = race_code)) +
  geom_bar(stat = "identity") +
  labs(title = "Race Demographics for Republican Mail Voters in 2022",
       x = "Race Code",
       y = "Percentage of Voters (%)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") 

# Filter for Republicans who voted by mail in 2020
mail_voters_2020_republicans <- republican_voters_2020 %>%
  filter(voting_method == "ABSENTEE BY MAIL") %>%
  distinct(voter_reg_num)

mail_voters_2020_republicans_race <- mail_voters_2020_republicans %>%
  inner_join(ncvoter, by = "voter_reg_num")

race_demographics_2020_mail_republicans <- mail_voters_2020_republicans_race %>%
  group_by(race_code) %>%
  summarise(count = n())

race_demographics_2020_mail_republicans <- race_demographics_2020_mail_republicans %>%
  mutate(percentage = (count / sum(count)) * 100)

print(race_demographics_2020_mail_republicans)

ggplot(race_demographics_2020_mail_republicans, aes(x = race_code, y = percentage, fill = race_code)) +
  geom_bar(stat = "identity") +
  labs(title = "Race Demographics for Republican Mail Voters in 2020",
       x = "Race Code",
       y = "Percentage of Voters (%)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

library(tidycensus)
library(tidyverse)
library(ggplot2)
library(maps)
library(sf)


ncvoter1 <- read_tsv("C:/Users/jsali/Documents/ncvoter1.txt", 
                     col_types = cols(
                       voter_reg_num = col_character(),
                       birth_year = col_integer(),
                       race_code = col_character(),
                       gender_code = col_character(),
                       party_cd = col_character(),
                       registr_dt = col_date(format = "%m/%d/%Y")
                     ))

ncvhis1 <- read_tsv("C:/Users/jsali/Downloads/ncvhis1.zip", 
                    col_types = cols_only(
                      voter_reg_num = "c",
                      election_lbl = "c",
                      voting_method = "c"
                    ))

voter_data_combined <- ncvoter1 %>%
  inner_join(ncvhis1, by = "voter_reg_num")

# Create subsets for mail and one-stop voters for 2016 and 2020
mail_voters_2016 <- voter_data_combined %>% 
  filter(election_lbl %in% c("11/08/2016") & 
           voting_method == "ABSENTEE BY MAIL")

one_stop_voters_2016 <- voter_data_combined %>% 
  filter(election_lbl %in% c("11/08/2016") & 
           voting_method == "ABSENTEE ONESTOP")

mail_voters_2020 <- voter_data_combined %>% 
  filter(election_lbl %in% c("11/03/2020") & 
           voting_method == "ABSENTEE BY MAIL")

one_stop_voters_2020 <- voter_data_combined %>% 
  filter(election_lbl %in% c("11/03/2020") & 
           voting_method == "ABSENTEE ONESTOP")

# Contains all voters from 2016 along with a column indicating whether they voted by mail or one-stop.
voters_2016 <- mail_voters_2016 %>%
  mutate(method_2016 = "Mail") %>%
  select(voter_reg_num, party_cd, method_2016) %>%
  bind_rows(one_stop_voters_2016 %>%
              mutate(method_2016 = "One-Stop") %>%
              select(voter_reg_num, party_cd, method_2016))
#Contains all voters from 2020 along with a column indicating whether they voted by mail or one-stop.
voters_2020 <- mail_voters_2020 %>%
  mutate(method_2020 = "Mail") %>%
  select(voter_reg_num, party_cd, method_2020) %>%
  bind_rows(one_stop_voters_2020 %>%
              mutate(method_2020 = "One-Stop") %>%
              select(voter_reg_num, party_cd, method_2020))

# Combine the two years and find voters who changed methods
voting_changes <- voters_2016 %>%
  inner_join(voters_2020, by = "voter_reg_num") %>%
  # Make sure to compare voters with the same party registration in both years
  filter(method_2016 != method_2020 & party_cd.x == party_cd.y)

# Find the number of voters who changed from Mail to One-Stop and vice versa, with their party registration
mail_to_one_stop <- voting_changes %>% 
  filter(method_2016 == "Mail" & method_2020 == "One-Stop")

one_stop_to_mail <- voting_changes %>% 
  filter(method_2016 == "One-Stop" & method_2020 == "Mail")

mail_to_one_stop_party <- mail_to_one_stop %>%
  count(party_cd.x, name = "Number_of_Voters")

one_stop_to_mail_party <- one_stop_to_mail %>%
  count(party_cd.x, name = "Number_of_Voters")

# Output the results
cat("Changes from Mail to One-Stop by party:\n")
print(mail_to_one_stop_party)
cat("\nChanges from One-Stop to Mail by party:\n")
print(one_stop_to_mail_party)

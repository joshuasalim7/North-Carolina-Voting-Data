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

voting_changes <- voters_2016 %>%
  inner_join(voters_2020, by = "voter_reg_num") %>%
  select(voter_reg_num, method_2016, method_2020)
voting_method_changes_summary <- voting_changes %>%
  group_by(method_2016, method_2020) %>%
  summarise(Count = n())
# Or get a table of individuals who changed their voting method.
voters_changed_method <- voting_changes %>%
  filter(method_2016 != method_2020)
# View the results of people who changed their voting method 
print(voting_method_changes_summary)
# View the voters registration number and how they changed their vote
print(voters_changed_method)

# For people who voted one-stop in 2016 but did not vote at all in 2020
one_stop_2016_no_vote_2020 <- voters_2016 %>%
  filter(method_2016 == "One-Stop") %>%
  anti_join(voters_2020, by = "voter_reg_num")
one_stop_2016_no_vote_2020_count <- one_stop_2016_no_vote_2020 %>%
  summarise(Count = n())
print(one_stop_2016_no_vote_2020_count)

# For people who did not vote in 2016 but voted one-stop in 2020
no_vote_2016_one_stop_2020 <- all_voters_2016 %>%
  anti_join(voters_2016, by = "voter_reg_num") %>%
  inner_join(voters_2020 %>% filter(method_2020 == "One-Stop"), by = "voter_reg_num")
no_vote_2016_one_stop_2020_count <- no_vote_2016_one_stop_2020 %>%
  summarise(Count = n())
print(no_vote_2016_one_stop_2020_count)

# For people who voted by mail in 2016 but did not vote at all in 2020
mail_2016_no_vote_2020 <- voters_2016 %>%
  filter(method_2016 == "Mail") %>%
  anti_join(voters_2020, by = "voter_reg_num")
mail_2016_no_vote_2020_count <- mail_2016_no_vote_2020 %>%
  summarise(Count = n())
print(mail_2016_no_vote_2020_count)

# For people who did not vote in 2016 but voted by mail in 2020
no_vote_2016_mail_2020 <- all_voters_2016 %>%
  anti_join(voters_2016, by = "voter_reg_num") %>%
  inner_join(voters_2020 %>% filter(method_2020 == "Mail"), by = "voter_reg_num")
no_vote_2016_mail_2020_count <- no_vote_2016_mail_2020 %>%
  summarise(Count = n())
print(no_vote_2016_mail_2020_count)


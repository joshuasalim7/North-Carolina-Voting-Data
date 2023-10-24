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

#ONESTOP VOTERS
early_voters_2020_one_stop <- ncvhis1 %>% 
  filter(election_lbl %in% c("11/03/2020", "06/23/2020", "03/03/2020") & 
           voting_method == "ABSENTEE ONESTOP")
early_voters_2016_one_stop <- ncvhis1 %>% 
  filter(election_lbl %in% c("11/08/2016","06/07/2016","03/15/2016") & 
           voting_method == "ABSENTEE ONESTOP")

voter_data_combined <- ncvoter1 %>%
  inner_join(ncvhis1, by = "voter_reg_num")

#Republican ONESTOP early voters 
republican_early_voters_2020 <- voter_data_combined %>% 
  filter(party_cd == "REP" & 
           election_lbl %in% c("11/03/2020", "06/23/2020", "03/03/2020") & 
           (voting_method %in% c("ABSENTEE ONESTOP")))
republican_early_voters_2016 <- voter_data_combined %>% 
  filter(party_cd == "REP" & 
           election_lbl %in% c("11/08/2016", "06/07/2016", "03/15/2016") & 
           (voting_method %in% c("ABSENTEE ONESTOP")))

cat("Number of Republicans who voted early in 2020:", nrow(republican_early_voters_2020), "\n")
cat("Number of Republicans who voted early in 2016:", nrow(republican_early_voters_2016), "\n")

#Democrats ONESTOP early voters 
democratic_early_voters_2020 <- voter_data_combined %>% 
  filter(party_cd == "DEM" & 
           election_lbl %in% c("11/03/2020", "06/23/2020", "03/03/2020") & 
           (voting_method %in% c("ABSENTEE BY MAIL", "ABSENTEE ONESTOP")))
democratic_early_voters_2016 <- voter_data_combined %>% 
  filter(party_cd == "DEM" & 
           election_lbl %in% c("11/08/2016", "06/07/2016", "03/15/2016") & 
           (voting_method %in% c("ABSENTEE BY MAIL", "ABSENTEE ONESTOP")))

cat("Number of Democrats who voted early in 2020:", nrow(democratic_early_voters_2020), "\n")
cat("Number of Democrats who voted early in 2016:", nrow(democratic_early_voters_2016), "\n")

#Republican early voters (By Mail)
republican_mail_voters_2020 <- voter_data_combined %>% 
  filter(party_cd == "REP" & 
           election_lbl %in% c("11/03/2020", "06/23/2020", "03/03/2020") & 
           voting_method == "ABSENTEE BY MAIL")
republican_mail_voters_2016 <- voter_data_combined %>% 
  filter(party_cd == "REP" & 
           election_lbl %in% c("11/08/2016", "06/07/2016", "03/15/2016") & 
           voting_method == "ABSENTEE BY MAIL")

cat("Number of Republicans who voted by mail in 2020:", nrow(republican_mail_voters_2020), "\n")
cat("Number of Republicans who voted by mail in 2016:", nrow(republican_mail_voters_2016), "\n")

#Democratic early voters (By Mail)
democratic_mail_voters_2020 <- voter_data_combined %>% 
  filter(party_cd == "DEM" & 
           election_lbl %in% c("11/03/2020", "06/23/2020", "03/03/2020") & 
           voting_method == "ABSENTEE BY MAIL")
democratic_mail_voters_2016 <- voter_data_combined %>% 
  filter(party_cd == "DEM" & 
           election_lbl %in% c("11/08/2016", "06/07/2016", "03/15/2016") & 
           voting_method == "ABSENTEE BY MAIL")

cat("Number of Democrats who voted by mail in 2020:", nrow(democratic_mail_voters_2020), "\n")
cat("Number of Democrats who voted by mail in 2016:", nrow(democratic_mail_voters_2016), "\n")


# Create a summarized data frame for plotting
plot_data <- data.frame(
  Party = c("Republican", "Republican", "Democratic", "Democratic", "Republican", "Republican", "Democratic", "Democratic"),
  Year = c(2016, 2020, 2016, 2020, 2016, 2020, 2016, 2020),
  Method = c("By Mail", "By Mail", "By Mail", "By Mail", "One-Stop", "One-Stop", "One-Stop", "One-Stop"),
  Voters = c(nrow(republican_mail_voters_2016), nrow(republican_mail_voters_2020),
             nrow(democratic_mail_voters_2016), nrow(democratic_mail_voters_2020),
             nrow(republican_early_voters_2016), nrow(republican_early_voters_2020),
             nrow(democratic_early_voters_2016), nrow(democratic_early_voters_2020))
)

# Plot the data using ggplot
ggplot(plot_data, aes(x = Year, y = Voters, fill = Party, group = Party)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  facet_wrap(~Method, scales = "free_y", ncol = 1) +
  labs(title = "Number of Voters by Party and Voting Method for 2016 and 2020",
       y = "Number of Voters",
       x = "Year",
       fill = "Party") +
  theme_minimal()


# important data: birthyear/birthdate, race, gender, party registration, reg_date, date of election
# by mail difference between 2016 and 2020 
# one stop difference between 2016 and 2020
# count(2016, 2020, party)

# "11/03/2020", "06/23/2020", "03/03/2020"



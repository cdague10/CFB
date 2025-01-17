library(tidyverse)
library(ggimage)
library(gt)
library(ggplot2)
library(dplyr)

#NCAA FBS COLORS and passing_pressure are both excel sheets I formed and edited
passing_pressure <- passing_pressure %>%
  left_join(NCAA_FBS_COLORS, by = c("team_name" = "team_ncaa"))

passacc_adot_blitz <- passing_pressure |>
  filter(base_dropbacks >= 400) %>%
  separate(player, into = c("first_name", "last_name"), sep = " ")

passacc_adot_blitz %>%
  ggplot(aes(x=blitz_accuracy_percent, y= blitz_ypa)) +
  geom_hline(yintercept = mean(mtr_cov_per_rec$blitz_ypa),linetype= "dashed") +
  geom_vline(xintercept = mean(mtr_cov_per_rec$blitz_accuracy_percent),linetype= "dashed")+
  #geom_text(aes(label = last_name), vjust = -1.0, hjust = 0.5, size = 2, color = "black") +
  geom_image(aes(image = ncaa_wiki), size=0.04, asp=16/9) +
  #geom_smooth(method="glm") +
  theme_minimal() +
  labs(x = "Accuracy Percentage",
       y = "Yards per Attempt",
       title= "Passing efficiency vs the blitz in 2024 (min. 400 dropbacks)",
       caption= "By Connor Dague | @cdague10 ")
################################################################
#Joins the ncaa sheet to the data sheet (DO THIS ONCE)
rushing_summary_1_ <- rushing_summary_1_ %>%
  left_join(NCAA_FBS_COLORS, by = c("team_name" = "team_ncaa"))

#rushyac_elusive <- rushing_summary_1_ |>
  #filter(attempts >= 100) |>
  #filter(player %in% c("Rahsul Faison", "Makhi Hughes","TJ Harden","Cam Cook","Jaden Nixon","Star Thomas","Keyvone Lee","Frank Peasant","Rocko Griffin","Coleman Bennett","Donald Chaney Jr","AJ Green","Rayshon Luke","Ashaad Clayton","Caleb Hood","Emeka Megwa","John Randle Jr","Kenan Christon","Deshun Murrell","Ky Thomas","Ajay Allen"))
rushyac_elusive <- rushing_summary_1_ %>%
  filter(attempts >= 150) %>%
  #filter(conference == "SB") %>%
  separate(player, into = c("first_name", "last_name"), sep = " ")

rushyac_elusive %>%
  ggplot(aes(x=yco_attempt, y=elusive_rating)) +
  geom_hline(yintercept = mean(rushyac_elusive$elusive_rating),linetype= "dashed") +
  geom_vline(xintercept = mean(rushyac_elusive$yco_attempt),linetype= "dashed")+
  expand_limits(x = c(min(rushyac_elusive$yco_attempt), max(rushyac_elusive$yco_attempt)),
                y = c(min(rushyac_elusive$elusive_rating), max(rushyac_elusive$elusive_rating)+2)) +
  #geom_text(aes(label = last_name), vjust = -1.2, hjust = 0.5, size = 2.75, color = "black") +
  geom_image(aes(image = ncaa_wiki), size=0.04, asp=16/9) +
  #geom_smooth(method="glm") +
  theme_minimal() +
  labs(x = "Yards after contact/attempt",
       y = "Elusive rating (by PFF)",
       title= "Rushing Efficiency in 2024 (min. 150 rush attempts)",
       caption= "By Connor Dague | @cdague10 ")

################################################################
defense_coverage_summary <- defense_coverage_summary %>%
  left_join(NCAA_FBS_COLORS, by = c("team_name" = "team_ncaa"))

mtr_cov_per_rec <- defense_coverage_summary%>%
  filter(snap_counts_pass_play >= 335) %>%
  filter(position == "LB") %>%
  #filter(conference == "MAC") %>%
  group_by(position) %>%
  separate(player, into = c("first_name", "last_name"), sep = " ")

mtr_cov_per_rec %>%
  ggplot(aes(x= missed_tackle_rate, y= coverage_snaps_per_reception)) +
  geom_hline(yintercept = mean(mtr_cov_per_rec$coverage_snaps_per_reception),linetype= "dashed") +
  geom_vline(xintercept = mean(mtr_cov_per_rec$missed_tackle_rate),linetype= "dashed")+
  expand_limits(x = c(min(mtr_cov_per_rec$missed_tackle_rate), max(mtr_cov_per_rec$missed_tackle_rate)),
                y = c(min(mtr_cov_per_rec$coverage_snaps_per_reception), max(mtr_cov_per_rec$coverage_snaps_per_reception)+.15)) +
  #geom_text(aes(label = last_name), vjust = -1.2, hjust = 0.5, size = 3, color = "black") +
  geom_image(aes(image = ncaa_wiki.x), size=0.035, asp=16/9) +
  #geom_smooth(method="lm") +
  theme_minimal() +
  labs(x = "Missed Tackle Rate Percentage",
       y = "Coverage Snaps/Reception",
       title= "FBS LB - Passing game efficiency 2024 (min. 350 cov. snaps)",
       caption= "By Connor Dague | @cdague10 ")

###############################################################################
rushing_summary_2023 <- rushing_summary_2023 %>%
  left_join(NCAA_FBS_COLORS, by = c("team_name" = "team_ncaa"))

rushyac_elusive2023 <- rushing_summary_2023 %>%
  filter(attempts >= 200)

rushyac_elusive2023 %>%
  ggplot(aes(x=yco_attempt, y=elusive_rating)) +
  geom_hline(yintercept = mean(rushyac_elusive2023$elusive_rating),linetype= "dashed") +
  geom_vline(xintercept = mean(rushyac_elusive2023$yco_attempt),linetype= "dashed")+
  #geom_text(aes(label = player), vjust = -1.0, hjust = 0.5, size = 3, color = "black") +
  geom_image(aes(image = ncaa_wiki), size=0.05, asp=16/9) +
  geom_smooth(method="glm") +
  theme_minimal() +
  labs(x = "Yards after contact/attempt",
       y = "Elusive rating (by PFF)",
       title= "Rushing Efficiency in 2023 (min. 140 rush attempts)",
       caption= "By Connor Dague | @cdague10 ")

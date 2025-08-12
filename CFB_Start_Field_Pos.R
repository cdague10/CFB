library(tidyverse)
library(ggimage)
library(gt)
library(ggplot2)
library(dplyr)
library(tidymodels)

fbs_conf <- c("FBS Independents", "Big Ten", "SEC", "Big 12", "ACC", "American Athletic", "Conference USA", "Mid-American","Mountain West", "Sun Belt")

start_field_pos <- drives_2024 %>%
  filter(driveResult == "Uncategorized") %>%
  filter(offenseConference %in% fbs_conf,
         defenseConference %in% fbs_conf)

block1 <- start_field_pos %>%
  filter(startYardline >=95) %>%
  filter(startYardline <= 99)
block2 <- start_field_pos %>%
  filter(startYardline >=90) %>%
  filter(startYardline <= 94)
block3 <- start_field_pos %>%
  filter(startYardline >=85) %>%
  filter(startYardline <= 89)
block4 <- start_field_pos %>%
  filter(startYardline >=80) %>%
  filter(startYardline <= 84)
block5 <- start_field_pos %>%
  filter(startYardline >=75) %>%
  filter(startYardline <= 79)
block6 <- start_field_pos %>%
  filter(startYardline >=70) %>%
  filter(startYardline <= 74)
block7 <- start_field_pos %>%
  filter(startYardline >=65) %>%
  filter(startYardline <= 69)
block8 <- start_field_pos %>%
  filter(startYardline >=60) %>%
  filter(startYardline <= 64)
block9 <- start_field_pos %>%
  filter(startYardline >=55) %>%
  filter(startYardline <= 59)
block10 <- start_field_pos %>%
  filter(startYardline >=50) %>%
  filter(startYardline <= 54)
block11 <- start_field_pos %>%
  filter(startYardline >=45) %>%
  filter(startYardline <= 49)
block12 <- start_field_pos %>%
  filter(startYardline >=40) %>%
  filter(startYardline <= 44)
block13 <- start_field_pos %>%
  filter(startYardline >=35) %>%
  filter(startYardline <= 39)
block14 <- start_field_pos %>%
  filter(startYardline >=30) %>%
  filter(startYardline <= 34)
block15 <- start_field_pos %>%
  filter(startYardline >=25) %>%
  filter(startYardline <= 29)
block16 <- start_field_pos %>%
  filter(startYardline >=20) %>%
  filter(startYardline <= 24)
block17 <- start_field_pos %>%
  filter(startYardline >=15) %>%
  filter(startYardline <= 19)
block18 <- start_field_pos %>%
  filter(startYardline >=10) %>%
  filter(startYardline <= 14)
block19 <- start_field_pos %>%
  filter(startYardline >=5) %>%
  filter(startYardline <= 9)
block20 <- start_field_pos %>%
  filter(startYardline >=1) %>%
  filter(startYardline <= 4)

numdrives1 <- nrow(block1)
td1 <- (sum(block1$driveResult == "TD")/numdrives1)
fg1 <- (sum(block1$driveResult == "FG")/numdrives1)
play1 <- mean(block1$plays)
eckel1 <- (sum(block1$startYardline - block1$yards <= 35)/numdrives1)
expoint1 <- (td1*6.98)+(fg1*3)

numdrives2 <- nrow(block2)
td2 <- (sum(block2$driveResult == "TD")/numdrives2)
fg2 <- (sum(block2$driveResult == "FG")/numdrives2)
play2 <- mean(block2$plays)
eckel2 <- (sum(block2$startYardline - block2$yards <= 35)/numdrives2)
expoint2 <- (td2*6.98)+(fg2*3)

numdrives3 <- nrow(block3)
td3 <- (sum(block3$driveResult == "TD")/numdrives3)
fg3 <- (sum(block3$driveResult == "FG")/numdrives3)
play3 <- mean(block3$plays)
eckel3 <- (sum(block3$startYardline - block3$yards <= 35)/numdrives3)
expoint3 <- (td3*6.98)+(fg3*3)

numdrives4 <- nrow(block4)
td4 <- (sum(block4$driveResult == "TD")/numdrives4)
fg4 <- (sum(block4$driveResult == "FG")/numdrives4)
play4 <- mean(block4$plays)
eckel4 <- (sum(block4$startYardline - block4$yards <= 35)/numdrives4)
expoint4 <- (td4*6.98)+(fg4*3)

numdrives5 <- nrow(block5)
td5 <- (sum(block5$driveResult == "TD")/numdrives5)
fg5 <- (sum(block5$driveResult == "FG")/numdrives5)
play5 <- mean(block5$plays)
eckel5 <- (sum(block5$startYardline - block5$yards <= 35)/numdrives5)
expoint5 <- (td5*6.98)+(fg5*3)

numdrives6 <- nrow(block6)
td6 <- (sum(block6$driveResult == "TD")/numdrives6)
fg6 <- (sum(block6$driveResult == "FG")/numdrives6)
play6 <- mean(block6$plays)
eckel6 <- (sum(block6$startYardline - block6$yards <= 35)/numdrives6)
expoint6 <- (td6*6.98)+(fg6*3)

numdrives7 <- nrow(block7)
td7 <- (sum(block7$driveResult == "TD")/numdrives7)
fg7 <- (sum(block7$driveResult == "FG")/numdrives7)
play7 <- mean(block7$plays)
eckel7 <- (sum(block7$startYardline - block7$yards <= 35)/numdrives7)
expoint7 <- (td7*6.98)+(fg7*3)

numdrives8 <- nrow(block8)
td8 <- (sum(block8$driveResult == "TD")/numdrives8)
fg8 <- (sum(block8$driveResult == "FG")/numdrives8)
play8 <- mean(block8$plays)
eckel8 <- (sum(block8$startYardline - block8$yards <= 35)/numdrives8)
expoint8 <- (td8*6.98)+(fg8*3)

numdrives9 <- nrow(block9)
td9 <- (sum(block9$driveResult == "TD")/numdrives9)
fg9 <- (sum(block9$driveResult == "FG")/numdrives9)
play9 <- mean(block9$plays)
eckel9 <- (sum(block9$startYardline - block9$yards <= 35)/numdrives9)
expoint9 <- (td9*6.98)+(fg9*3)

numdrives10 <- nrow(block10)
td10 <- (sum(block10$driveResult == "TD")/numdrives10)
fg10 <- (sum(block10$driveResult == "FG")/numdrives10)
play10 <- mean(block10$plays)
eckel10 <- (sum(block10$startYardline - block10$yards <= 35)/numdrives10)
expoint10 <- (td10*6.98)+(fg10*3)

numdrives11 <- nrow(block11)
td11 <- (sum(block11$driveResult == "TD")/numdrives11)
fg11 <- (sum(block11$driveResult == "FG")/numdrives11)
play11 <- mean(block11$plays)
eckel11 <- (sum(block11$startYardline - block11$yards <= 35)/numdrives11)
expoint11 <- (td11*6.98)+(fg11*3)

numdrives12 <- nrow(block12)
td12 <- (sum(block12$driveResult == "TD")/numdrives12)
fg12 <- (sum(block12$driveResult == "FG")/numdrives12)
play12 <- mean(block12$plays)
eckel12 <- (sum(block12$startYardline - block12$yards <= 33)/numdrives12)
expoint12 <- (td12*6.98)+(fg12*3)

numdrives13 <- nrow(block13)
td13 <- (sum(block13$driveResult == "TD")/numdrives13)
fg13 <- (sum(block13$driveResult == "FG")/numdrives13)
play13 <- mean(block13$plays)
eckel13 <- (sum(block13$startYardline - block13$yards <= 28)/numdrives13)
expoint13 <- (td13*6.98)+(fg13*3)

numdrives14 <- nrow(block14)
td14 <- (sum(block14$driveResult == "TD")/numdrives14)
fg14 <- (sum(block14$driveResult == "FG")/numdrives14)
play14 <- mean(block14$plays)
eckel14 <- (sum(block14$startYardline - block14$yards <= 23)/numdrives14)
expoint14 <- (td14*6.98)+(fg14*3)

numdrives15 <- nrow(block15)
td15 <- (sum(block15$driveResult == "TD")/numdrives15)
fg15 <- (sum(block15$driveResult == "FG")/numdrives15)
play15 <- mean(block15$plays)
eckel15 <- (sum(block15$startYardline - block15$yards <= 18)/numdrives15)
expoint15 <- (td15*6.98)+(fg15*3)

numdrives16 <- nrow(block16)
td16 <- (sum(block16$driveResult == "TD")/numdrives16)
fg16 <- (sum(block16$driveResult == "FG")/numdrives16)
play16 <- mean(block16$plays)
eckel16 <- (sum(block16$startYardline - block16$yards <= 13)/numdrives16)
expoint16 <- (td16*6.98)+(fg16*3)

numdrives17 <- nrow(block17)
td17 <- (sum(block17$driveResult == "TD")/numdrives17)
fg17 <- (sum(block17$driveResult == "FG")/numdrives17)
play17 <- mean(block17$plays)
eckel17 <- (sum(block17$startYardline - block17$yards <= 8)/numdrives17)
expoint17 <- (td17*6.98)+(fg17*3)

numdrives18 <- nrow(block18)
td18 <- (sum(block18$driveResult == "TD")/numdrives18)
fg18 <- (sum(block18$driveResult == "FG")/numdrives18)
play18 <- mean(block18$plays)
eckel18 <- (sum(block18$startYardline - block18$yards <= 3)/numdrives18)
expoint18 <- (td18*6.98)+(fg18*3)

numdrives19 <- nrow(block19)
td19 <- (sum(block19$driveResult == "TD")/numdrives19)
fg19 <- (sum(block19$driveResult == "FG")/numdrives19)
play19 <- mean(block19$plays)
eckel19 <- (sum(block19$driveResult == "TD")/numdrives19)
expoint19 <- (td19*6.98)+(fg19*3)

numdrives20 <- nrow(block20)
td20 <- (sum(block20$driveResult == "TD")/numdrives20)
fg20 <- (sum(block20$driveResult == "FG")/numdrives20)
play20 <- mean(block20$plays)
eckel20 <- (sum(block20$driveResult == "TD")/numdrives20)
expoint20 <- (td20*6.98)+(fg20*3)


###############################################
fg = c(fg1,fg2,fg3,fg4,fg5,fg6,fg7,fg8,fg9,fg10,fg11,fg12,fg13,fg14,fg15,fg16,fg17,fg18,fg19,fg20)
fg_percent <- paste0(round(fg * 100, 2), "%")
td = c(td1,td2,td3,td4,td5,td6,td7,td8,td9,td10,td11,td12,td13,td14,td15,td16,td17,td18,td19,td20)
td_percent <- paste0(round(td*100,2), "%")
eckel = c(eckel1,eckel2,eckel3,eckel4,eckel5,eckel6,eckel7,eckel8,eckel9,eckel10,eckel11,eckel12,eckel13,eckel14,eckel15,eckel16,eckel7,eckel8,eckel9,eckel20)
eckel_percent <- paste0(round(eckel*100,2),"%")
expected_points2 = c(expoint1,expoint2,expoint3,expoint4,expoint5,expoint6,expoint7,expoint8,expoint9,expoint10,expoint11,expoint12,expoint13,expoint14,expoint15,expoint16,expoint17,expoint18,expoint19,expoint20)
expected_points <- paste0(round(expected_points2,2))
num_plays2 = c(play1,play2,play3,play4,play5,play6,play7,play8,play9,play10,play11,play12,play13,play14,play15,play16,play17,play18,play19,play20)
num_plays <- paste0(round(num_plays2,2))
###################################################################

data <- data.frame(
  RowLabel = c("Inside own 5", "Inside own 6-10", "Inside own 11-15", "Inside own 16-20",
               "Inside own 21-25", "Inside own 26-30", "Inside own 31-35", "Inside own 36-40",
               "Inside own 41-45", "Inside own 46-50", "Opposing 49-45", "Opposing 44-40",
               "Opposing 39-35", "Opposing 34-30", "Opposing 29-25", "Opposing 24-20",
               "Opposing 19-15", "Opposing 14-10", "Opposing 9-5", "Opposing 4-1"),
  block1 = expected_points,
  block2 = num_drives,
  block2 = td_rate,
  block3 = fg_rate,
  block5 = avg_plays
)

# Create the gt table and apply custom options
gt_table <- gt(data) %>%
  cols_label(
    RowLabel = "Starting Field Position",
    block1 = "Expected Points",
    block2 = "Number of Drives",
    block3 = "TD Percent",
    block4 = "FG Percent",
    block5 = "Number of Plays"
  ) %>%
  cols_width(
    block1 ~ px(120),
    block2 ~ px(100),  # Make "Number of Drives" narrower
    block3 ~ px(80)    # Make "TD Percent" even narrower
  ) %>%
  tab_options(
    table.background.color = "#f9f9f9",  # Set the background color for the entire table
    table.border.top.style = "solid",    # Border style for the top of the table
    table.border.top.width = 2,          # Border width for the top of the table
    table.border.bottom.style = "solid", # Border style for the bottom of the table
    table.border.bottom.width = 2,       # Border width for the bottom of the table
    column_labels.background.color = "lightblue",  # Column header background color
    column_labels.font.weight = "bold",  # Make column labels bold
    column_labels.font.size = 14,       # Column labels font size
    row.striping.include_table_body = TRUE,  # Add striping to table rows
    row.striping.background_color = "#f2f2f2",  # Row striping color
  )

# Display the table
gt_table





#####################################################################3

#Second Way to complete

yard_bins <- start_field_pos %>%
  filter(startYardsToGoal >= 1 & startYardsToGoal <= 99) %>%
  filter(!(driveResult %in% c("Uncategorized", "END OF GAME", "END OF HALF"))) %>%
  filter(offenseConference %in% fbs_conf,
         defenseConference %in% fbs_conf) %>%
  mutate(bin = floor(startYardsToGoal / 5) * 5) %>%
  group_by(bin) %>%
  summarise(
    num_drives = n(),
    td_rate = mean(driveResult == "TD"),
    fg_rate = mean(driveResult == "FG"),
    avg_plays = mean(plays, na.rm = TRUE),
    expected_points = (td_rate*6.98)+(fg_rate*3),
    .groups = "drop"
  )


labels <- c(
  "Inside own 1-4", "Inside own 5-9", "Inside own 10-14", "Inside own 15-19",
  "Inside own 20-24", "Inside own 25-29", "Inside own 30-34", "Inside own 35-39",
  "Inside own 40-44", "Inside own 45-49", "Opp. 49-45",
  "Opp. 44-40", "Opp. 39-35", "Opp. 34-30", "Opp. 29-25",
  "Opp. 24-20", "Opp. 19-15", "Opp. 14-10", "Opp. 9-5", "Opp. 4-1"
)

# Fix bin mapping so that 95 → "Own 5", 5 → "Opp 95", etc.
yard_bins <- yard_bins %>%
  arrange(desc(bin)) %>%  # From own end zone to opponent's
  mutate(RowLabel = labels[1:n()])



gt_table <- yard_bins %>%
  select(RowLabel, expected_points, num_drives, td_rate, fg_rate, avg_plays) %>%
  gt() %>%
  tab_header(
    title = "Results by Starting Field Position (Every FBS Game)",
    subtitle = "Data aggregated from all drives in 2024 season | By: Connor Dague"
  ) %>%
  cols_label(
    RowLabel = "Starting Field Position",
    expected_points = "Expected Points",
    num_drives = "Number of Drives",
    td_rate = "TD Rate",
    fg_rate = "FG Rate",
    avg_plays = "Avg Plays"
  ) %>%
  cols_width(
    RowLabel ~ px(160),
    expected_points ~ px(70),
    num_drives ~ px(70),
    td_rate ~ px(80),
    fg_rate ~ px(80),
    avg_plays ~ px(80)
  ) %>%
  cols_align(
    align = "center",
    columns = c(expected_points, num_drives, avg_plays)
  ) %>%
  fmt_percent(columns = c(td_rate, fg_rate), decimals = 1) %>%
  fmt_number(columns = c(expected_points, avg_plays), decimals = 2) %>%
  tab_style(
    style = cell_borders(
      sides = "right",
      color = "gray60",
      weight = px(1)
    ),
    locations = cells_body(columns = c("RowLabel", "expected_points"))
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = "RowLabel")
  ) %>%
  tab_options(
    table.background.color = "#f9f9f9",
    table.border.top.style = "solid",
    table.border.top.width = 2,
    table.border.bottom.style = "solid",
    table.border.bottom.width = 2,
    column_labels.background.color = "lightblue",
    column_labels.font.weight = "bold",
    column_labels.font.size = 14,
    row.striping.include_table_body = TRUE,
    row.striping.background_color = "#f2f2f2"
  )

gt_table

library(tidyverse)
library(readxl)
# Hello world

data_path <- "datasets/survivability.xlsx"

weightloss_raw_df <- read_excel(data_path, 
                                sheet="simplified weight data",
                                range = "A1:E20"
                                )

weightloss_long_df <- weightloss_raw_df %>%
  mutate_at(vars(tape_type), factor) %>%
  pivot_longer(
    cols = weight_day_1:weight_day_4,
    names_to = "day_str",
    values_to = "weight"
    ) %>%
  mutate(
    day = str_split_i(day_str, "_", 3)
  ) %>%
  mutate_at(vars(day), parse_number) %>%
  select(tape_type, day, weight)

day_1_weight_df <- weightloss_long_df %>%
  group_by(tape_type) %>%
  filter(day == 1) %>%
  mutate(
    day_1_weight = mean(weight)
  ) %>%
  select(tape_type, day_1_weight) %>%
  unique()

weight_loss_df <- weightloss_long_df %>%
  group_by(tape_type, day) %>%
  mutate(
    avg_weight = mean(weight, na.rm = TRUE)
  ) %>%
  select(tape_type, day, avg_weight) %>%
  unique() %>%
  left_join(day_1_weight_df) %>%
  group_by(tape_type) %>%
  mutate(
    percent_diff = 100 - ((day_1_weight - avg_weight)/avg_weight * 100)
  )


weight_loss_df %>%
  ggplot(
    aes(x=day, y=percent_diff,
        group=tape_type,
        colour=tape_type
    )
  ) +
  geom_line() +
  labs(
    title="Weight loss",
    x = "Day of observation",
    y = "Percent of original weight (%)",
    colour = "Tape types"
  )

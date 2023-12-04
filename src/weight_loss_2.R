library(googlesheets4)
library(readxl)
library(tidyverse)
sheet_url = "https://docs.google.com/spreadsheets/d/1bcj83_IeYjC3aGNcZdNVh8MFgFJbSqJEdrvB0ZNb7bc/edit?usp=sharing"

data_df_raw <- read_sheet(sheet_url,
                      sheet="simplified weight data",
                      range="A1:E20"
                      ) %>%
  mutate_at(vars(tape_type), factor) %>%
  mutate(
    weight_loss_0_1 = 100.0,
    weight_loss_1_2 = (weight_day_2/weight_day_1) * 100,
    weight_loss_2_3 = (weight_day_3/weight_day_1) * 100,
    weight_loss_3_4 = (weight_day_4/weight_day_1) * 100
  ) %>%
  filter(
    weight_loss_1_2 > 97
  )

ordered_loss <- c("0_1", "1_2", "2_3", "3_4")
loss_label <- c("Original weight", "Loss on day 2", "Loss on day 3", "Loss on day 4")

loss_by_tape_sd_df <- data_df_raw %>%
  group_by(tape_type) %>%
  summarize(
    sd_0_1 = 0,
    sd_1_2 = sd(weight_loss_1_2, na.rm = TRUE),
    sd_2_3 = sd(weight_loss_2_3, na.rm = TRUE),
    sd_3_4 = sd(weight_loss_3_4, na.rm = TRUE),
  ) %>%
  pivot_longer(cols=sd_0_1:sd_3_4,
               names_to = "loss_day",
               values_to = "sd",
               names_prefix = "sd_"
  ) %>%
  mutate(loss_day = factor(loss_day, levels=ordered_loss))

loss_by_tape_mean_df <- data_df_raw %>%
  group_by(tape_type) %>%
  summarize(
    avg_loss_0_1 = 100.0,
    avg_loss_1_2 = mean(weight_loss_1_2, na.rm = TRUE),
    avg_loss_2_3 = mean(weight_loss_2_3, na.rm = TRUE),
    avg_loss_3_4 = mean(weight_loss_3_4, na.rm = TRUE),
  )%>%
  pivot_longer(cols=avg_loss_0_1:avg_loss_3_4,
               names_to = "loss_day",
               values_to = "percent_loss",
               names_prefix = "avg_loss_"
               ) %>%
  mutate(loss_day = factor(loss_day, levels=ordered_loss))

loss_by_tape_df <- left_join(loss_by_tape_mean_df, loss_by_tape_sd_df,
                             by=c("loss_day", "tape_type"))

loss_all_df <- data_df_raw %>%
  pivot_longer(cols=weight_loss_0_1:weight_loss_3_4,
               names_to = "loss_day",
               values_to = "percent_loss",
               names_prefix = "weight_loss_"
  ) %>%
  mutate(loss_day = factor(loss_day, levels=ordered_loss)) %>%
  select(tape_type,loss_day, percent_loss)

weight_loss_plot <- loss_by_tape_df %>%
  # ggplot(
  #  aes(x=loss_day,
  #      y=percent_loss,
  #      fill=tape_type,
  #  )
  # ) +
  # geom_bar(stat="identity",position="dodge",) + 
  ggplot(
    aes(x=loss_day,
        y=percent_loss,
        group=tape_type,
        colour=tape_type
        )
  ) +
  geom_line() + 
  geom_errorbar(data=loss_by_tape_df, aes(
    ymin=percent_loss-sd,
    ymax=percent_loss+sd),
    position="dodge",
    width=0.2
    ) +
  labs(
    title="Weight loss",
    x = "Day of observation",
    y = "Average of original weight (%)",
    colour = "Tape types"
  ) +
   #geom_point(data=loss_all_df) +
  scale_x_discrete(labels=loss_label)

weight_loss_plot

png_path <- file.path("outputs", "weight_loss_plot.png")
ggsave(png_path,
  weight_loss_plot,
  device="png",
  units="cm",
  width=15,
  height=9,
  dpi = 200,
  )

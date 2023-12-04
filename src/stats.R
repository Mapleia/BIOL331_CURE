library(googlesheets4)
library(readxl)
library(tidyverse)
library(rstatix)
library(ggpubr)

sheet_url = "https://docs.google.com/spreadsheets/d/1bcj83_IeYjC3aGNcZdNVh8MFgFJbSqJEdrvB0ZNb7bc/edit?usp=sharing"

data_df_raw <- read_sheet(sheet_url,
                          sheet="simplified weight data",
                          range="A1:E20"
  ) %>%
  mutate(tape_type = factor(tape_type,
                            levels=c("scotch", "masking", "packing"),
                            ordered=TRUE)
         ) %>%
  mutate(
    weight_loss_0_1 = 100.0,
    weight_loss_1_2 = (weight_day_2/weight_day_1) * 100,
    weight_loss_2_3 = (weight_day_3/weight_day_1) * 100,
    weight_loss_3_4 = (weight_day_4/weight_day_1) * 100
  ) %>%
  select(tape_type, 
         weight_loss_0_1,weight_loss_1_2,
         weight_loss_2_3,weight_loss_3_4
         ) %>%
  mutate(
    id = seq.int(nrow(data_df_raw))
  )

levels(data_df_raw$tape_type)

loss_df <- data_df_raw %>%
  pivot_longer(cols=weight_loss_0_1:weight_loss_3_4,
                        names_to = "loss_day",
                        values_to = "percent_loss",
                        names_prefix = "weight_loss_"
  ) %>%
  mutate(loss_day = factor(loss_day,
                           levels=c("0_1", "1_2", "2_3", "3_4"),
                           ordered = TRUE)
         )
levels(loss_df$loss_day)

without_outliers <- loss_df %>%
  filter(loss_day != "0_1") %>%
  group_by(tape_type, loss_day) %>%
  filter(!is_outlier(percent_loss)) %>%
  ungroup()

# get summary statistics (sd and mean)
without_outliers %>%
  group_by(tape_type, loss_day) %>%
  get_summary_stats(percent_loss, type = "mean_sd")

loss_df %>%
  group_by(tape_type, loss_day) %>%
  identify_outliers(percent_loss)

# Two-way mixed ANOVA test
res.aov <- without_outliers %>% 
  anova_test(dv = percent_loss, wid = id,
             between = tape_type, within = loss_day)
get_anova_table(res.aov)

# Effect of group at each time point
one.way <- without_outliers %>%
  group_by(loss_day) %>%
  anova_test(dv = percent_loss, wid = id, between = tape_type) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

# Effect of time at each level of exercises group
one.way2 <- without_outliers %>%
  group_by(tape_type) %>%
  anova_test(dv = percent_loss, wid = id, within = loss_day) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way2




create_plot <- function(no_outliers_df, full_df) {
  # Pairwise comparisons between group levels
  pwc <- no_outliers_df %>%
    group_by(loss_day) %>%
    pairwise_t_test(percent_loss ~ tape_type, p.adjust.method = "bonferroni")
  pwc
  
  pwc <- pwc %>% add_xy_position(x = "loss_day")
  loss_label <- c("Original weight", "Loss on day 2", "Loss on day 3", "Loss on day 4")
  bxp <- ggboxplot(
    full_df, x = "loss_day", y = "percent_loss",
    color = "tape_type", palette = "jco"
  ) + 
    stat_pvalue_manual(pwc, tip.length = 0, hide.ns = TRUE) +
    labs(
      subtitle = get_test_label(res.aov, detailed = TRUE),
      caption = get_pwc_label(pwc),
      title="Weight loss",
      x = "Day of observation",
      y = "Average of original weight (%)",
      colour = "Tape types"
    ) +
    scale_x_discrete(labels=loss_label)
  png_path <- file.path("outputs", "weight_loss_boxplot.png")
  ggsave(png_path,
         bxp,
         device="png",
         units="cm",
         width=15,
         height=9,
         dpi = 200,
  )
  return(bxp)
}


bxp <- create_plot(without_outliers, loss_df)

library(tidyverse)
library(readxl)
library(ggplot2)

data_path <- "datasets/survivability.xlsx"
survivability_raw_df <- read_excel(data_path, sheet="hh data")%>%
  mutate_at(vars(TAPE), factor)


percent_surv_df <- survivability_raw_df %>%
  group_by(TAPE, DAY) %>%
  summarise(count_survived = sum(SURVIVED),
            count_total = length(SURVIVED)) %>%
  mutate(percent_survived = count_survived/count_total * 100)

glimpse(percent_surv_df)
percent_surv_df %>%
  ggplot(
    aes(x=DAY, y=percent_survived,
        group=TAPE,
        colour=TAPE
        )
    ) +
  geom_line() +
  labs(
    title="Survivability of different tapes",
    x = "Day of observation",
    y = "Embryos that survived (%)"
  )

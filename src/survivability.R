library(tidyverse)
library(googlesheets4)
library(readxl)
library(tidyverse)
library(ggpubr)

sheet_url = "https://docs.google.com/spreadsheets/d/1bcj83_IeYjC3aGNcZdNVh8MFgFJbSqJEdrvB0ZNb7bc/edit?usp=sharing"
survivability_raw_df <- read_sheet(sheet_url,
                          sheet="hh_data")%>%
  mutate(TAPE = factor(TAPE, levels=c("scotch", "masking", "packing")))

percent_surv_df <- survivability_raw_df %>%
  group_by(TAPE, DAY) %>%
  summarise(count_survived = sum(SURVIVED),
            count_total = length(SURVIVED)) %>%
  mutate(percent_survived = count_survived/count_total * 100)

create_survivability_plot <- function(dat_df) {
  lnp <- ggline(
    data=percent_surv_df,
    x="DAY",
    y="percent_survived",
    color="TAPE",
    palette = "jco"
  ) +
  labs(
    title="Figure 2b. Survivability of different tapes",
    x = "Day of observation",
    y = "Embryos that survived (%)",
    color="Tape types"
  )
  png_path <- file.path("outputs", "survivability_plot.png")
  ggsave(png_path,
         lnp,
         device="png",
         units="cm",
         width=15,
         height=9,
         dpi = 200,
  )
  return(lnp)
}

lnp <- create_survivability_plot(percent_surv_df)

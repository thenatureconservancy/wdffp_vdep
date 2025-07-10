
## Calculate acres surplus closed per PA


library(tidyverse)

bps_atts <- read.csv('inputs/LF20_BPS_220.csv')

data_2023_dry <- read_csv("outputs/full_vdep_dry_forests_2023.csv") 


data_2023_dry <- data_2023_dry %>%
  left_join(select(bps_atts, GROUPVEG, BPS_MODEL), by = c("StratumID" = "BPS_MODEL"))


closed_rows_2023 <- data_2023_dry |>
  filter(grepl("CLS", StateClassID)) |>
  filter(GROUPVEG %in% c("Hardwood", "Conifer", "Hardwood-Conifer")) |>
  mutate(ref_count = (ref_percent * total_count_pa_bps)/100) |>
  mutate(ref_hectares = (ref_count * 0.09)) |>
  mutate(current_hectares = (count_pa_bps_scls_2023 * 0.09)) |>
  mutate(surplus_deficit_hectares = (current_hectares - ref_hectares))



cls_hectares_difference_per_pa_2023 <- closed_rows_2023 |>
  group_by(pas) |>
  summarize(
    ref_closed_hectares = sum(ref_hectares),
    current_closed_hectares = sum(current_hectares)) |>
  mutate(surplus_deficit_hectares = (current_closed_hectares - ref_closed_hectares)) |>
  mutate(percent_change = ((current_closed_hectares - ref_closed_hectares) / ref_closed_hectares) * 100)






write_csv(closed_rows_2023, file = "outputs/dry_forests_closed_rows_2023.csv")

write_csv(cls_hectares_difference_per_pa_2023, file = "outputs/dry_forests_cls_acres_difference_per_pa_2023.csv" )  



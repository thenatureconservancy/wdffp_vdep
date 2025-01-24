


base_data_2020 <- read.csv("data/combine2020.csv") %>%
  clean_names() 

bps_models_base <- unique(base_data_2020$bps_model)

project_areas <- unique(base_data_2020$wdffp_pas_r)

grouped_2020 <- base_data_2020 %>%
  group_by(wdffp_pas_r, bps_model, label) %>%
  summarize(count = sum(count))

bps_models_grouped <- unique(grouped_2020$bps_model)

project_areas_grouped <- unique(grouped_2020$wdffp_pas_r)


grouped_2020 <- grouped_2020 %>%
  filter(!bps_model %in% c(
    "10010",
    "10020",
    "10030",
    "10040",
    "10060",
    "0", # snow/ice, water, barren/sparse
    "-1111" )) # fill not mapped


bps_models_grouped_minus_sparse_otherscls <- unique(grouped_2020$bps_model)

project_areas_grouped_minus_removedBpSs <- unique(grouped_2020$wdffp_pas_r)









bps_models_grouped <- unique(grouped_2020$bps_model)

# Find elements in list1 but not in list2
diff1 <- setdiff(bps_models_base, bps_models_grouped )
print(diff1)  # Output: 1 2 3

# Find elements in list2 but not in list1
diff2 <- setdiff(bps_models_grouped , bps_models_base)
print(diff2)  # Output: 6 7 8

# Find common elements
common <- intersect(list1, list2)
print(common)  # Output: 4 5




ref_con <- read.csv("data/ref_con_long.csv")




library(janitor)
library(tidyverse)
library(data.table)

# Read and clean data
base_data_2023_all_dry_raw <- fread("inputs/all_pas_bps_scls_2023_dry.csv") %>%
  clean_names() 

# Check for values in dry_forest column
print(unique(base_data_2023_all_dry_raw$dry_forest))

# Ensure dry_forest is numeric (if needed)
base_data_2023_all_dry_raw$dry_forest <- as.numeric(base_data_2023_all_dry_raw$dry_forest)

# Filter for dry_forest == 1
base_data_2023_all_dry <- base_data_2023_all_dry_raw %>%
  filter(dry_forest == 1)

# Inspect the filtered data
print(head(base_data_2023_all_dry))

ref_con <- fread("inputs/ref_con_long.csv")
scls_descrpts <- fread("inputs/scls_descriptions.csv")

# Process scls_descrpts
scls_descrpts[, model_label := paste(StratumID, ClassLabelID, sep = "_")]

# Process base_data_2023_all_dry
data_2023_all_dry <- base_data_2023_all_dry[, .(count = sum(count)), by = .(pa_id, bps_model, label)]


data_2023_all_dry  <- data_2023_all_dry[!bps_model %in% c("10010", "10020", "10030", "10040", "10060", "0", "-1111")]
data_2023_all_dry  <- data_2023_all_dry[!label %in% c("Barren or Sparse", "Agriculture", "Developed")]
data_2023_all_dry[, pa_bps := paste(pa_id, bps_model, sep = "_")]
data_2023_all_dry[, pa_bps_scl := paste(pa_id, bps_model, label, sep = "_")]

# Get list of PAs and BpSs for filtering ref_con
pas <- unique(data_2023_all_dry$pa_id)
wdffp_bpss <- unique(data_2023_all_dry$bps_model)

# Filter ref_con to relevant BpSs
filtered_ref_con <- ref_con[model_code %in% wdffp_bpss]

# Build Monster Ref Con
wdffp_ref_con <- rbindlist(lapply(pas, function(pas) {
  temp_ref_con <- copy(filtered_ref_con)
  temp_ref_con[, pas := pas]
  return(temp_ref_con)
}))

# Filter monster ref_con by pa_bps combo
wdffp_ref_con[, pa_bps := paste(pas, model_code, sep = "_")]
pa_bps_unique <- unique(data_2023_all_dry$pa_bps)
wdffp_ref_con <- wdffp_ref_con[pa_bps %in% pa_bps_unique]

# Add in pa_bps_scl column
wdffp_ref_con[, pa_bps_scl := paste(pas, model_code, ref_label, sep = "_")]

# Final filtering
wdffp_ref_con <- wdffp_ref_con[!ref_label %in% c("Agriculture", "Developed", "Water")]

# Remove extra columns before joining
wdffp_ref_con_join <- wdffp_ref_con[, !("bps_name"), with = FALSE]
#data_2023_all <- data_2023_all[, !("pa_bps"), with = FALSE]

# Join data
vdep_2023_all <- data_2023_all_dry[wdffp_ref_con_join, on = "pa_bps_scl"]  # this was wrong in copilot code

# Process vdep_2023
vdep_2023 <- vdep_2023_all[, !c("pa_id", "bps_model", "label"), with = FALSE]
vdep_2023[, count := fifelse(is.na(count), 0, count)]
vdep_2023[, ref_percent := fifelse(is.na(ref_percent), 0, ref_percent)]

# Add column with total count per pa
vdep_2023[, total_count_pa := sum(count), by = pas]

# Add in total count of BpS per PA
vdep_2023[, total_count_pa_bps := sum(count), by = .(pas, model_code)]

# Add in count of each scls 2023 within each PA per BpS
vdep_2023[, count_pa_bps_scls_2023 := sum(count), by = .(pas, model_code, ref_label)]

# Add in scls 2023 percents per BpS within each PA
vdep_2023[, percent_2023 := round((count_pa_bps_scls_2023 / total_count_pa_bps) * 100, 1), by = .(pas, model_code)]

# Add 'similarity' column
vdep_2023[, similarity := pmin(ref_percent, percent_2023)]

# Calculate VDEP per pa_bps
vdep_2023[, pa_bps_vdep := 100 - sum(similarity), by = .(pas, model_code)]

# Calculate VDEP per PA
vdep_2023[, pa_vdep := weighted.mean(pa_bps_vdep, total_count_pa_bps), by = pas]

# Add in sclass descriptions to final full VDEP dataframe for 2023
vdep_2023 <- vdep_2023[scls_descrpts, on = "model_label"]

# Write to CSV
fwrite(vdep_2023, "outputs/full_vdep_dry_forests_2023.csv")

# Summarize VDEP per PA for joining back to PA shapefile
pa_vdep_2023 <- vdep_2023[, .(pa_vdep = mean(pa_vdep)), by = pas]

# write for joining

write.csv(pa_vdep_2023, "outputs/all_pas_vdep_dry_forests_2023.csv",row.names = FALSE)

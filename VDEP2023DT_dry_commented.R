

## Set up ----

library(janitor)  # Load janitor package for data cleaning functions
library(tidyverse)  # Load tidyverse package for data manipulation and visualization
library(data.table)  # Load data.table package for fast data manipulation

# Read and clean raw data column names
base_data_2023_all_dry_raw <- fread("inputs/all_pas_bps_scls_2023_dry.csv") %>%  # Read CSV file into a data.table and clean column names
  clean_names() 

# Read reference condition and sclass descriptions data
ref_con <- fread("inputs/ref_con_long.csv")  # Read reference condition data from CSV file
scls_descrpts <- fread("inputs/scls_descriptions.csv")  # Read sclass descriptions data from CSV file

## Filter for only Dry Forest mask (provided by Kor Blankenship) ----

# Check for values in dry_forest column
print(unique(base_data_2023_all_dry_raw$dry_forest))  # Print unique values in the dry_forest column to check its contents

# Ensure dry_forest is numeric (if needed)
base_data_2023_all_dry_raw$dry_forest <- as.numeric(base_data_2023_all_dry_raw$dry_forest)  # Convert dry_forest column to numeric type

# Filter for dry_forest == 1
base_data_2023_all_dry <- base_data_2023_all_dry_raw %>%  # Filter rows where dry_forest equals 1
  filter(dry_forest == 1)

# Inspect the filtered data
print(head(base_data_2023_all_dry))  # Print the first few rows of the filtered data

## Prep sclass dataframe for joining ----

# Process sclass descriptions to create a model label
scls_descrpts[, model_label := paste(StratumID, ClassLabelID, sep = "_")]  # Create a new column model_label by concatenating StratumID and ClassLabelID


# Remove Descriptions column (super long!) from scls_descrpts dataframe
scls_descrpts[, Description := NULL]

# Quick inspect
print(head(scls_descrpts))


## Wrangle input bps-pa-scls data ----

# Summarize base data by pa_id, bps_model, and label
data_2023_all_dry <- base_data_2023_all_dry[, .(count = sum(count)), by = .(pa_id, bps_model, label)]  # Aggregate data by pa_id, bps_model, and label, summing the count

# Filter out unwanted bps_model and label values
data_2023_all_dry <- data_2023_all_dry[!bps_model %in% c("10010", "10020", "10030", "10040", "10060", "0", "-1111")]  # Remove rows with specific bps_model values
data_2023_all_dry <- data_2023_all_dry[!label %in% c("Barren or Sparse", "Agriculture", "Developed")]  # Remove rows with specific label values

# Create combined columns for pa_bps and pa_bps_scl
data_2023_all_dry[, pa_bps := paste(pa_id, bps_model, sep = "_")]  # Create a new column pa_bps by concatenating pa_id and bps_model
data_2023_all_dry[, pa_bps_scl := paste(pa_id, bps_model, label, sep = "_")]  # Create a new column pa_bps_scl by concatenating pa_id, bps_model, and label

## Prep refcon ----

# Get unique PAs and BpSs for filtering ref_con
pas <- unique(data_2023_all_dry$pa_id)  # Get unique values of pa_id
wdffp_bpss <- unique(data_2023_all_dry$bps_model)  # Get unique values of bps_model

# Filter ref_con to relevant BpSs
filtered_ref_con <- ref_con[model_code %in% wdffp_bpss]  # Filter ref_con to include only rows with model_code in wdffp_bpss

# Build Monster (solid skeleton) Ref Con by replicating filtered_ref_con for each PA
wdffp_ref_con <- rbindlist(lapply(pas, function(pas) {  # For each pa_id, create a copy of filtered_ref_con and add a column for pa_id
  temp_ref_con <- copy(filtered_ref_con)
  temp_ref_con[, pas := pas]
  return(temp_ref_con)
}))

# should be 2270*24210 = 54956700 observations


# Filter monster ref_con by pa_bps combo
wdffp_ref_con[, pa_bps := paste(pas, model_code, sep = "_")]  # Create a new column pa_bps by concatenating pas and model_code for later joining
pa_bps_unique <- unique(data_2023_all_dry$pa_bps)  # Get unique values of pa_bps from data_2023_all_dry
wdffp_ref_con <- wdffp_ref_con[pa_bps %in% pa_bps_unique]  # Filter wdffp_ref_con to include only rows with pa_bps in pa_bps_unique, i.e. get rid of BpSs that are not in each PA


# Add in pa_bps_scl column
wdffp_ref_con[, pa_bps_scl := paste(pas, model_code, ref_label, sep = "_")]  # Create a new column pa_bps_scl by concatenating pas, model_code, and ref_label

# Final filtering to remove unwanted ref_label values
wdffp_ref_con <- wdffp_ref_con[!ref_label %in% c("Agriculture", "Developed", "Water")]  # Remove rows with specific ref_label values

# Remove extra columns before joining
wdffp_ref_con_join <- wdffp_ref_con[, !("bps_name"), with = FALSE]  # Remove the bps_name column from wdffp_ref_con


## Calculate VDEP ----

# Join data_2023_all_dry with wdffp_ref_con_join on pa_bps_scl
vdep_2023_all <- data_2023_all_dry[wdffp_ref_con_join, on = "pa_bps_scl"]  # Perform a join on pa_bps_scl

# Process vdep_2023_all to remove unnecessary columns and handle missing values
vdep_2023 <- vdep_2023_all[, !c("pa_id", "bps_model", "label"), with = FALSE]  # Remove pa_id, bps_model, and label columns
vdep_2023[, count := fifelse(is.na(count), 0, count)]  # Replace NA values in count column with 0
vdep_2023[, ref_percent := fifelse(is.na(ref_percent), 0, ref_percent)]  # Replace NA values in ref_percent column with 0

# Add column with total count per PA
vdep_2023[, total_count_pa := sum(count), by = pas]  # Calculate total count per PA
# Add in total count of BpS per PA
vdep_2023[, total_count_pa_bps := sum(count), by = .(pas, model_code)]  # Calculate total count of BpS per PA

# Add in count of each scls 2023 within each PA per BpS
vdep_2023[, count_pa_bps_scls_2023 := sum(count), by = .(pas, model_code, ref_label)]  # Calculate count of each scls 2023 within each PA per BpS
# Add in scls 2023 percents per BpS within each PA
vdep_2023[, percent_2023 := round((count_pa_bps_scls_2023 / total_count_pa_bps) * 100, 1), by = .(pas, model_code)]  # Calculate scls 2023 percents per BpS within each PA
# Add 'similarity' column to calculate similarity between ref_percent and percent_2023
vdep_2023[, similarity := pmin(ref_percent, percent_2023)]  # Calculate similarity as the minimum of ref_percent and percent_2023

# Calculate VDEP per pa_bps
vdep_2023[, pa_bps_vdep := 100 - sum(similarity), by = .(pas, model_code)]  # Calculate VDEP per pa_bps

# Calculate VDEP per PA
vdep_2023[, pa_vdep := weighted.mean(pa_bps_vdep, total_count_pa_bps), by = pas]  # Calculate VDEP per PA

## Add in sclass descriptions to final full VDEP dataframe for 2023 ----

vdep_2023 <- scls_descrpts[vdep_2023, on = "model_label"] # Join sclass descriptions to vdep_2023 on model_label for later work on CLS classes

# Write the full VDEP dataframe to CSV
fwrite(vdep_2023, "outputs/full_vdep_dry_forests_2023.csv")  # Write vdep_2023 to a CSV file


## Summarize VDEP per PA for joining back to PA shapefile ----

pa_vdep_2023 <- vdep_2023[, .(pa_vdep = mean(pa_vdep)), by = pas]  # Summarize VDEP per PA  
# Write the summarized VDEP per PA to CSV
write.csv(pa_vdep_2023, "outputs/all_pas_vdep_dry_forests_2023.csv", row.names = FALSE)  # Write pa_vdep_2023 to a CSV file












library(dplyr)
library(readxl)
library(readr)
library(EnvStats)
## Import Data Sets ##
Creatinine <- read_csv("All creatinine 5-7-2025.csv")
Mapping <- read_excel("2022-01432_SID-PID_mapping_202410091047_Combined.xlsx")
Neonic_Data <- read_excel("2022-01432_UPEST2_DATA.xlsx")

## Merge on ptid_f ##
Mapping$ptid_f <- Mapping$Original_subject_ID %/% 10

Mapping_small <- Mapping %>% select(ptid_f, PID)
Mapping_unique <- Mapping_small %>%
  group_by(ptid_f) %>%
  summarise(PID = first(PID), .groups = "drop")

Creatinine_full <- Creatinine %>% left_join(Mapping_unique, Creatinine, by = 'ptid_f')

## New Neonic Trimester var ##
Neonic_Data$TRIMESTER <- substr(Neonic_Data$Sample_collection_timepoint, 1, nchar(Neonic_Data$Sample_collection_timepoint) - 3)

## no repeated creatinine values + matching trimester vars ##
Creatinine_full %>% count(PID, TRIMESTER) %>% filter(n>1)
Creatinine_full$TRIMESTER <- as.numeric(Creatinine_full$TRIMESTER)
Neonic_Data$TRIMESTER <- as.numeric(Neonic_Data$TRIMESTER)

## New Neonic Dataset w/ Creatinine for each Trimester ##
Neonic_full <- Neonic_Data %>%
  left_join(
    Creatinine_full %>% select(PID, TRIMESTER, creatinine),
    by = c("PID", "TRIMESTER")
  )
## Overall Summary Stats ##
summary_table <- Neonic_full %>%
  group_by(Analyte_Code) %>%
  summarise(
    LOD = first(LOD),                                         # LOD
    n = sum(!is.na(Concentration)),                           # Sample Size
    geo_mean = round(geoMean(Concentration + 0.0001, na.rm = TRUE),2), # GM
    p50 = round(quantile(Concentration, 0.50, na.rm = TRUE),2),        # 50th %
    p75 = round(quantile(Concentration, 0.75, na.rm = TRUE),2),        # 75th %
    p90 = round(quantile(Concentration, 0.90, na.rm = TRUE),2),        # 90th %
    p95 = round(quantile(Concentration, 0.95, na.rm = TRUE),2),        # 95th %
    IQR = round(IQR(Concentration, na.rm = TRUE),2)                    # IQR
  )
## Trimester Data ##
Tri_table <- Neonic_full %>%
  group_by(TRIMESTER, Analyte_Code) %>%
  summarise(
    LOD = first(LOD),                                         # LOD
    n = sum(!is.na(Concentration)),                           # Sample Size
    geo_mean = round(geoMean(Concentration + 0.0001, na.rm = TRUE),2), # GM
    p50 = round(quantile(Concentration, 0.50, na.rm = TRUE),2),        # 50th %
    p75 = round(quantile(Concentration, 0.75, na.rm = TRUE),2),        # 75th %
    p90 = round(quantile(Concentration, 0.90, na.rm = TRUE),2),        # 90th %
    p95 = round(quantile(Concentration, 0.95, na.rm = TRUE),2),        # 95th %
    IQR = round(IQR(Concentration, na.rm = TRUE),2),                   # IQR
    .groups = 'drop') 

summary_T1 <- Tri_table %>% filter(Tri_table$TRIMESTER == 1)
summary_T2 <- Tri_table %>% filter(Tri_table$TRIMESTER == 2)
summary_T3 <-Tri_table %>% filter(Tri_table$TRIMESTER == 3)

## FOD is an outlier thing? ##

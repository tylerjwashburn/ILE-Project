library(readxl)
library(dplyr)

## Import Data Sets ##
Creatinine <- read_excel("C:/Users/tyler/Downloads/All creatinine 5-7-2025.xlsx")
Mapping <- read_excel("C:/Users/tyler/Downloads/2022-01432_SID-PID_mapping_202410091047_Combined.xlsx")
Neonic_Data <- read_excel("C:/Users/tyler/Downloads/2022-01432_UPEST2_DATA.xlsx")

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

##New Neonic Dataset w/ Creatinine for each Trimester ##
Neonic_full <- Neonic_Data %>%
  left_join(
    Creatinine_full %>% select(PID, TRIMESTER, creatinine),
    by = c("PID", "TRIMESTER")
  )




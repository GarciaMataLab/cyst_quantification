# Script name: Lumen_Phenotypes.R
# Filters and compiles quantification data from "Lumen_Quantification.ijm" Fiji/ImageJ macro
# Outputs 2 CSV files grouped by Condition, IMS timestamp, and ROI
  # Combined_Lumen_CystVolData.csv : Sum of individual lumen volumes, overall cyst volume, percent lumen occupancy, and lumen number (n)
  # Individual_Lumen_Data.csv : individual lumen volume, surface area, and sphericity index
# Author: Dr. Madeline Lovejoy
# Date: 2026-08-13
# R version 4.1.2 (2021-11-01)

# dplyr version 1.1.2
# tidyr version 1.3.0
# stringr version 1.5.0

# load packages
# if you have never used these packages in your R console, you will need to install them first with the code: install packages("package_name")
library(dplyr)
library(tidyr)
library(stringr)

# ***BEFORE RUNNING THIS LINE, MANUALLY SET THE WORKING DIRECTORY TO YOUR QUANTIFICATION FOLDER BY GOING TO SESSION -> SET WORKING DIRECTORY -> CHOOSE DIRECTORY***
# defines working directory as "dir" data frame
dir<-getwd()

# Places Excel sheets ending in "LumenQuant.csv" in the "FilesL" data frame
FilesL <- list.files(pattern = "LumenQuant.csv", recursive = TRUE)
CSVlistL <- lapply(FilesL, read.csv)
names(CSVlistL) <- FilesL
# connects the data between columns
dataLumens <-  bind_rows(CSVlistL, .id = "filename")

# Lumen number quantification
# separates the file name into different variables based on the placement of underscores ( _ )
# depending on how your files are named, you may need to change the number and placement of NAs
Lumen_Tally <- dataLumens %>% separate(filename, c(NA, NA, "Condition", NA, NA, NA, NA, NA, NA, "IMS", "ROI"), sep="_") %>%
  # renames the CSV label for the volume column "Volume..micron.3." to "volume", and "Surface..micron.2." to "surface"
  rename(volume = Volume..micron.3., surface = Surface..micron.2.)%>%
  # discards lumen volume values that are smaller than 7 microns^3 to eliminate artifacts counted in the Fiji/ImageJ code
  filter(volume > 7) %>%
  # only displays the following columns
  group_by(Condition, IMS, ROI) %>%
  # counts the number of lumen measurements that remain after filtering
  tally() %>%
  # eliminates "LumenQuant.csv" attached to the ROI value so multiple quantification files can be grouped together based on the ROI value
  mutate(ROI =  str_replace(ROI, "LumenQuant.csv", ""))

# Lumen volume, surface area, and sphericity index quantification  
dataLumens <- dataLumens %>% separate(filename, c(NA, NA, "Condition", NA, NA, NA, NA, NA, NA, "IMS", "ROI"), sep="_") %>%
  rename(volume = Volume..micron.3., surface = Surface..micron.2.)%>%
  # equation for sphericity index using lumen volume and surface area measurements
  mutate(sphericity = (nthroot((36 * pi * volume^2), 3)) / surface) %>%
  filter(volume > 7) %>%
  select(Condition, IMS, ROI, volume, surface, sphericity) %>% 
  mutate(ROI =  str_replace(ROI, "LumenQuant.csv", ""))
 
# Sum of lumen volumes quantification
  LumenSum <- dataLumens %>% group_by(Condition, IMS, ROI) %>%
  summarise(VolumeSum = sum(volume))%>%
  arrange(ROI, .by_group = TRUE)

#Quantification for Excel sheets from Cyst Volume macro
FilesC <- list.files(pattern = "CystVol.csv", recursive = TRUE)
CSVlistC <- lapply(FilesC, read.csv)
names(CSVlistC) <- FilesC
dataCyst <-  bind_rows(CSVlistC, .id = "filename")

dataCyst <- dataCyst %>% separate(filename, c(NA, NA, "Condition", NA, NA, NA, NA, NA, NA, "IMS", "ROI"), sep="_") %>%
  rename(CystVolume = Volume..micron.3.) %>%
  select(Condition, IMS, ROI, CystVolume) %>%
  mutate(ROI =  str_replace(ROI, "CystVol.csv", ""))%>%
  filter(CystVolume > 8000)%>%
  arrange(Condition, IMS, ROI, .by_group = TRUE)

# Lumen occupancy calculation
# Combines cyst volume quantification and lumen quantification into the same data frame
# ***YOU MUST HAVE AN EQUAL NUMBER OF CSV FILES FOR CYST VOLUME QUANTIFICATION AND LUMEN QUANTIFICATION OR THIS PART WILL NOT WORK***
  # If these two parameters are quantified at the same time in Fiji/ImageJ, this should not be an issue
Combined <- LumenSum %>% bind_cols(dataCyst) %>%
  select(Condition, IMS, ROI, VolumeSum, CystVolume )%>%
  mutate(LumenOccupancy = (VolumeSum/CystVolume)*100)

# Adds lumen number quantification as a column in the Combined_Lumen_CystVol_Data data frame
Combined_Lumen_CystVol_Data <- left_join(Combined, Lumen_Tally)

# Saves data frames with lumen quantification
write.csv(dataLumens,file = "Individual_Lumen_Data.csv")
write.csv(Combined_Lumen_CystVol_Data, file = "Combined_Lumen_CystVolData.csv")

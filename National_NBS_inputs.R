####################################################
### === INPUT DATA FOR NBS NATIONAL ANALYSIS === ###
####################################################

library(tidyverse)
library(rgdal)
library(leaflet)
library(shiny)

# Initial data import from SharePoint folder "Scenarios"
SPDir = "https://eu001-sp.shell.com//sites//AAAAB3387//Nature%20Based%20Solutions"
TopDir = "https://eu001-sp.shell.com//sites//AAAAB3387//Nature%20Based%20Solutions//Scenarios"
DataDir = "https://eu001-sp.shell.com//sites//AAAAB3387//Nature%20Based%20Solutions//Scenarios//National%20Potentials//Standardized%20country%20data//Scripts%20and%20input%20data"
Local_DataDir = file.path(getwd(), "data")

source("R_functions.R") # Load core functions used in many scripts - includes SharePoint CSV read function

### Import the original dataset used in the BD national analysis
country_lookups = SP_readCSV(file.path(DataDir, "FAO_countrycodes.csv"), has_header=T) # Lookup dataset for all country naming conventions
base_country_data = SP_readCSV(file.path(DataDir, "country_data.csv"), has_header=T) # Initial dataset of country data - includes land area, population, coastline length and mean annual temperature
nat_map = SP_readCSV(file.path(DataDir, "nat_map.csv"), has_header=T) # Precipitation in mm/yr
live_graz = SP_readCSV(file.path(DataDir, "FAOSTAT_Data_Livestock_Grazing_Area.csv"), has_header=T) # Livestock grazing areas in 000s hectares
livestock = SP_readCSV(file.path(DataDir, "FAOSTAT_Data_Livestock_Management.csv"), has_header=T) # Livestock numbers in head of cattle
cropland = SP_readCSV(file.path(DataDir, "FAOSTAT_Data_Cropland_Area.csv"), has_header=T) # Cropland areas in 000s hectares
graz = SP_readCSV(file.path(DataDir, "FAOSTAT_all_Grazing_Area.csv"), has_header=T) # Grassland area in 000s hectares (FAO category - permanent meadows and pastures)
seagrass = SP_readCSV(file.path(DataDir, "seagrass_area.csv"), has_header=T) # Seagrass areas in hectares (note, protection is averaged over 10 years)
saltmarsh = SP_readCSV(file.path(DataDir, "saltmarsh_area.csv"), has_header=T) # Salt marsh areas in hectares (note, protection is averaged over 10 years)
plantation = SP_readCSV(file.path(DataDir, "plantation_data.csv"), has_header=T) # Plantation areas in 000s hectares
natfor = SP_readCSV(file.path(DataDir, "natural_forest_data.csv"), has_header=T) # Natural forest areas in 000s hectares
fertilizer = SP_readCSV(file.path(DataDir, "FAOSTAT_Data_Nadditions.csv"), has_header=T) # N fertilizer use in tonnes Nitrogen 
biochar = SP_readCSV(file.path(DataDir, "FAOSTAT_Data_Biochar.csv"), has_header=T) # Biochar feedstock data - Cereals, Rice and Sugarcane in tonnes of 'production' and wood residues in m3
wood_residues = biochar[biochar$Item == "Wood residues",] # Subset biochar to wood residues in m3
rice = biochar[biochar$Item == "Rice, paddy",] # Subset biochar to rice production in tonnes
sugarcane = biochar[biochar$Item == "Sugar cane",] # Subset biochar to sugarcane production in tonnes
cereals = biochar[!biochar$Item %in% c("Wood residues", "Rice, paddy"),] # Subset biochar to all cereal productions in tonnes (uses FAO definition of cereal crops)

### Wrangle data to get into a consistent format and combine
# Set up initial dataset to build off
base_country_data$worldshapename = base_country_data$Country # Use a column name common to the country lookups dataset to allow a merge
base_country_data = merge(country_lookups[c("ISO3", "worldshapename")], base_country_data) # Merge

### Various data-wrangling processes
source("National_NBS_reformatting.R")

### If needed, clean environment
CleanEnvir()

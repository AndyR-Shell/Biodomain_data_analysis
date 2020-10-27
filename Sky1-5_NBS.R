###############################################################
###############################################################
### CALCULATING ANNUAL EMISSION REDUCTIONS FOR NBS PATHWAYS ###
### WORK FOR SHELL SKY SCENARIO 1.5 IN 2020                 ###
###############################################################
# SIMPLE MODEL-ONLY VERSION #
#############################

rm(list=ls()) # Start with a blank slate if needed

#####
### INPUTS AND SET UP
#####
TopDir = getwd() # Assumes project is run from same location that this script is stored. This is denoted ShinyDir and contains two folders (data and figures) as well as this script
setwd(TopDir) # Set the working directory to be explicit
DataDir = file.path(TopDir, "data") # Essentially the input folder (where input data is stored)

# Load functions
source(file.path(TopDir, "Sky1-5_functions.R")) # Load functions used to calculate proportional adoption over time and derive ERs over time

# Load necessary datafiles - Note: newer files have uncertainty columns as well
adopt_in = read.csv(file.path(DataDir, "adoption_uncert.csv"), header=T) # Read in the inputs required to calculate annual adoption rates of each NBS type
extent_in = read.csv(file.path(DataDir, "extent_uncert.csv"), header=T) # Read in the inputs required to convert adoption rates to absolute values of hectares and emissions reductions

NBS_in = merge(adopt_in, extent_in) # Create one dataframe that can be used as inputs to the function

#####
### RUN BASIC MODEL
#####

# Run the model and output
Global_AllNBS_Base = Annual_NBS_potentials(run_name = "Global - All Pathways - Base Adopt - Max extent",
                                           inputs = NBS_in,
                                           output_area_data = F)

# Quick plotcheck
ggplot(Global_AllNBS_Base, aes(x=year, y=TERs_MtCO2, colour=NBS_short_name)) +
  geom_line() +
  facet_wrap(~run_ID)

#####
### SIMPLE PLOT CHECKS
#####

# Simple metric to track change in parameters - Remove-to-Avoid ratio of total emissions
Summary_Data = merge(Global_AllNBS_Base, extent_in[c("NBS_short_name", "NBS_desc", "active_remove")], by=c("NBS_short_name")) %>% 
  group_by(year, active_remove) %>%
  summarise(totals = sum(TERs_MtCO2, na.rm=T)) %>%
  spread(key = active_remove, value = totals) %>%
  rename("Avoid" = "0", "Remove" = "1") %>%
  gather(key, value, -year)

ggplot(Summary_Data, aes(x=year, y=value, colour=key)) +
  geom_line()

#####
### CLEAN UP AND SAVE AS NECESSARY
#####

# Remove all unnecessary objects
rm(list=ls(pattern = "^tmp"))

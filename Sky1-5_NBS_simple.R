###############################################################
###############################################################
### CALCULATING ANNUAL EMISSION REDUCTIONS FOR NBS PATHWAYS ###
### WORK FOR SHELL SKY SCENARIO 1.5 IN 2020                 ###
###############################################################
# SIMPLE MODEL-ONLY VERSION #
#############################

rm(list=ls()) # Start with a blank slate if needed

# Libraries
library(tidyverse)
library(plyr)

# General user-inputs
force_average_ERprofile = T # Do you want to force the proportional averages to equal 1 overall (T=Yes, F=No)
adoption_input_selection = "default" # Choose which adoption input scenarios you want to use - options are "old", "accelerated", or "default" (spelling for old/accelerated must be exact)
timeframe_start = 2000 # Default setting to start the scenario from 2000 but could choose current year if you wish
timeframe_end = 2200 # By default the scenario runs from year 2000 but this is a user-defined input to assign the ending year (i.e. 2100 or 2200, or whatever!)
# Note - Of course end year must be after the start year for the scenario to work

#####
### INPUTS AND SET UP
#####
TopDir = getwd() # Assumes project is run from same location that this script is stored. This is denoted ShinyDir and contains two folders (data and figures) as well as this script
setwd(TopDir) # Set the working directory to be explicit
DataDir = file.path(TopDir, "data") # Essentially the input folder (where input data is stored)

# Load functions
source(file.path(TopDir, "Sky1-5_functions.R")) # Load functions used to calculate proportional adoption over time and derive ERs over time

# Load necessary datafiles
adopt_in = read.csv(file.path(DataDir, "adoption.csv"), header=T) # Read in the inputs required to calculate annual adoption rates of each NBS type
extent_in = read.csv(file.path(DataDir, "extent.csv"), header=T) # Read in the inputs required to convert adoption rates to absolute values of hectares and emissions reductions

###############
### === GLOBAL NBS ADOPTION OVER TIME === ###
###############
#####
### CALCULATING ADOPTION RATES
#####

# Get the full list of NBS pathways that you want to analyse (should be complete in input file)
TNCfull_NBSlist = as.character(adopt_in[["NBS_short_name"]]) 

# Loop over pathways to calculate adoption for each one using the provided input data for S-curves
adoption_perc = data.frame() # Create empty dataframe to put the adoption data into

for(i in TNCfull_NBSlist) { # Start of adoption calculation loop
  timeframe_df = data.frame(year=timeframe_start:timeframe_end) # Create simple dataframe with column of continuous years running from start to finish
  tmp_df = adopt_in[adopt_in$NBS_short_name==i,] # Limit adoption input data to the pathway of interest (in the loop)
  
  # Use proportional_adoption function (user-defined) to calculate adoption rates for the provided years
  tmp = proportional_adoption(timeframe_df = timeframe_df, 
                              year_start = tmp_df$year_start,
                              year_50 = tmp_df$year_50,
                              saturation = tmp_df$saturation)
  
  tmp$NBS_short_name = i # Assign the pathway name to a column
  adoption_perc = rbind(adoption_perc, tmp) # Bind all pathway adoption data together
} # End of adoption calculation loop

# To allow for simple plotting, convert dataframe from wide to long format and rename for plot ease/aesthetics
adoption_perc_alldata = gather(adoption_perc, variable, value, -year, -NBS_short_name) # Create plot-friendly dataframe
# Rename factor levels
adoption_perc_alldata$variable = revalue(adoption_perc_alldata$variable, c(enrol = "Area enrolled", # Cumulative 'area' enrolled to date
                                                                           unenrol = "Area unenrolled", # Cumulative 'area' unenrolled to date
                                                                           net_enrol = "Net change in area", # Difference between cumulative 'area' enrolled to date and cumulative 'area' unenrolled to date
                                                                           diff_enrol = "Annual gross enrollment", # Annual change in 'area' enrolled
                                                                           diff_unenrol = "Annual gross unenrollment", # Annual change in 'area' unenrolled
                                                                           diff_net = "Annual net enrollment")) # Net annual change in 'area' between enrolled and unenrolled
colnames(adoption_perc_alldata)[colnames(adoption_perc_alldata) %in% c("variable")] = "adoption_datatype" # Rename the variable column
colnames(adoption_perc_alldata)[colnames(adoption_perc_alldata) %in% c("value")] = "area_proportion" # Rename the value column

#####
### CONVERT ADOPTION PERCENT TO HECTARES FOR MAXIMUM EXTENT
#####

### Convert the percentage adoption into hectares
adoption_Max_alldata = data.frame() # Create empty dataframe to put data into
for(i in TNCfull_NBSlist) { # Loop over each NBS and allocate based on maximum hectare extent
  tmp = adoption_perc_alldata[adoption_perc_alldata$NBS_short_name == i,] # Area proportion timeline - Subset based on pathway
  tmp$area_Mha = tmp$area_proportion * extent_in$global_extent[extent_in$NBS_short_name == i] # Simply multiply proportions by maximum extent
  adoption_Max_alldata = rbind(adoption_Max_alldata, tmp) # Combine new dataframes
}

#####
### CONVERT HECTARES TO EMISSIONS REDUCTIONS
# MAXIMUMS
#####

### As above, but convert the hectares into maximal TERs
TERs_Max_alldata = data.frame() # Create empty dataframe to fill with Total Emission Reduction data
for(i in TNCfull_NBSlist) { # Loop over each NBS pathway
  tmp = adoption_Max_alldata[adoption_Max_alldata$NBS_short_name == i,] # Area absolute timeline - Subset based on pathway
  tmp2 = adopt_in[adopt_in$NBS_short_name == i,] # Inputs needed for TER calculator function - Subset based on pathway
  ### Importantly, the global extent for certain avoidance NBS pathways is expressed as a rate (i.e. Mha/yr) instead of absolute total (i.e. Mha) so annual ER calculations must use the 'cumulative' net change in area instead of 'annual' net change
  if(extent_in$extent_rate[extent_in$NBS_short_name==i] == 1) { # If the pathway expresses extent as a rate (i.e. Mha/yr) then use 'cumulative' area data
    tmp = tmp[tmp$adoption_datatype=="Net change in area",] # Subset to only include datatype that is relevant - NOTE: 'Net change in area' for those pathways with extents expressed as a rate actually means the gross new enrollment that year
    
    # Use annual_TER_calculator function (user-defined) to calculate lag of emission reductions specific to that pathway over time - uses inputs from adopt_in
    tmp_annual_ERs = annual_TER_calculator(area_timeframe_data = tmp, # Proportion enrollment rate - note, uses the 'diff_net' column as this is the annual proportional change (i.e. enrolled that year - unenrolled that year)
                                           saturation = tmp2$saturation, # 'Saturation' is actually the length of time that credits are generated or hectare must be maintained to generate ERs
                                           function_type = tmp2$function_type, # Function type refers to the temporal profile of how emission reductions are generated for that NBS pathway
                                           years_to_max = tmp2$years_to_max, # Years to max is used by linear function types to calculate the slope of 'ramp up' in ERs (i.e. how long it takes to reach maximum ERs generated per year)
                                           ter_intensity = extent_in$ter_intensity[extent_in$NBS_short_name==i], # Maximum emission reduction intensity used to scale annual ERs
                                           force_average = force_average_ERprofile) # Use previously set input as to whether you want to force the calculated overall average ER intensity to equal the value in extent_in (default to TRUE)
    
    # Because the TER calculations are based off the number of new hectares that year, pathways are expressed as a rate. As a result it's also useful to have a cumulative column showing total number of hectares enrolled to date (i.e. have Mha as well as Mha/yr)
    tmp_annual_ERs$area_Mha_todate = cumsum(tmp_annual_ERs$area_Mha) # Cumulate the areas to that date - note that when area is unenrolled the actually means less and less is protected (assumes after 'saturation' - i.e. 100 years then that area is no longer at threat)
  } else { # The pathway expresses extent as a global absolute value (i.e. Mha) then use 'annual' area data
    tmp = tmp[tmp$adoption_datatype=="Annual gross enrollment",] # Subset to only include datatype that is relevant - NOTE: For these we use 'gross' enrollment instead of 'net' because the calculator uses the same 'saturation' value to limit the years which generate ERs (and therefore we don't need to account for unenrollment as well)
    
    # Use annual_TER_calculator function (user-defined) to calculate lag of emission reductions specific to that pathway over time - uses inputs from adopt_in
    tmp_annual_ERs = annual_TER_calculator(area_timeframe_data = tmp, # Proportion enrollment rate - note, uses the 'diff_net' column as this is the annual proportional change (i.e. enrolled that year - unenrolled that year)
                                           saturation = tmp2$saturation, # 'Saturation' is actually the length of time that credits are generated or hectare must be maintained to generate ERs
                                           function_type = tmp2$function_type, # Function type refers to the temporal profile of how emission reductions are generated for that NBS pathway
                                           years_to_max = tmp2$years_to_max, # Years to max is used by linear function types to calculate the slope of 'ramp up' in ERs (i.e. how long it takes to reach maximum ERs generated per year)
                                           ter_intensity = extent_in$ter_intensity[extent_in$NBS_short_name==i], # Maximum emission reduction intensity used to scale annual ERs
                                           force_average = force_average_ERprofile) # Use previously set input as to whether you want to force the calculated overall average ER intensity to equal the value in extent_in (default to TRUE)
    
    tmp_annual_ERs$area_Mha_todate = cumsum(tmp_annual_ERs$area_Mha) # Cumulate the areas to that date so this 'area' column is consistent with 'avoidance' pathways. Assumption is that once a hectare is enrolled it must be protected indefinitely
  } # End of if statement
  TERs_Max_alldata = rbind(TERs_Max_alldata, tmp_annual_ERs) # Combine all pathways
} # End of loop

##### 
# IMPORTANT DATA CHANGE - ADOPTION AREA DATA FOR 'EXTENT RATE' PATHWAYS NEED CHANGING TO ABSOLUTE AREA (from Mha/yr)
#####

tmp_output_Max = data.frame() # Empty dataframe to put data from loop into
for(NBS in TNCfull_NBSlist) { # Loop over each NBS pathway
  if(extent_in$extent_rate[extent_in$NBS_short_name==NBS]==0) { # Use input parameter of 'extent_rate' to determine whether anything needs changing
    tmpDF = adoption_Max_alldata # Work with duplicate dataset to avoid overwriting issues
    tmpDF = tmpDF[tmpDF$NBS_short_name == NBS,] # If the NBS pathway does not have its potential expressed as a rate (i.e. absolute Mha used instead of Mha/yr) then just use the existing data
  } else { # If NBS pathway IS an extent_rate pathway (i.e. potential is expressed as Mha/yr) then:
    tmpDFx = adoption_Max_alldata # Work with duplicate dataset to avoid overwriting issues
    tmpDFx = tmpDFx[tmpDFx$NBS_short_name == NBS,] # Subset based on the pathway
    tmpDF = data.frame() # Create empty dataframe to put data from loop into
    for(i in levels(as.factor(tmpDFx$adoption_datatype))) { # Loop over each adoption datatype to cumulate for each 
      tmpDFx2 = tmpDFx[tmpDFx$adoption_datatype == i,] # Subset based on adoption_datatype
      tmpDFx2$area_Mha = cumsum(tmpDFx2$area_Mha) # Cumulate the Mha/yr values so they now actually refer to the Mha absolute enrolled to-date
      tmpDF = rbind(tmpDF, tmpDFx2) # Bind with the 'non' extent_rate pathways
    } # End of loop
  } # End of else statement
  tmp_output_Max = rbind(tmp_output_Max, tmpDF) # Bind each NBS pathway to the larger dataframe
} # End of loop

# If properly calculated in the above loop the new dataframes should replace the existing adoption dataframes
# NOTE - because of this, TERs cannot be calculated in the same way as before using these new dataframes
adoption_Max_alldata = tmp_output_Max

##### 
# FINAL FORMATTING TO HELP CLARITY AND WHEN NECESSARY COMBINE DATA FROM ABOVE
#####

adoption_Max_alldata$threshold = "Maximum" # Add column for the threshold
adoption_Max_alldata$adoption_scenario = adoption_input_selection # Add adoption scenario name
TERs_Max_alldata$threshold = "Maximum" # Add column for the threshold
TERs_Max_alldata$adoption_scenario = adoption_input_selection # Add adoption scenario name

# Quick plotcheck
ggplot(TERs_Max_alldata, aes(x=year, y=TERs_MtCO2, colour=NBS_short_name)) +
  geom_line() +
  facet_wrap(~threshold)

#####
### CLEAN UP AND SAVE AS NECESSARY
#####

# Remove all unnecessary objects
rm(list=ls(pattern = "^tmp"))

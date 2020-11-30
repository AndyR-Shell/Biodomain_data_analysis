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
Scenario_data = file.path(DataDir, "SXB Scenarios")

# Load functions
source(file.path(TopDir, "Sky1-5_functions.R")) # Load functions used to calculate proportional adoption over time and derive ERs over time

# Required user input for scenario simulations
scenarios = c("base", "accel", "waves", "islands") # Names of scenarios you want to run - must match the associated input files
starttime = 2000 # What year do you want to start the simulation?
endtime = 2200 # What year do you want to end the simulation?
wavestime = 2035 # What year do you split the islands simulation at?
islandstime = 2050 # What year do you split the islands simulation at?

#####
### RUN BASIC MODEL AND SAVE
#####

# Loop over the scenarios and write to environment
for(scen in scenarios) {
  
  # Load necessary datafiles - Note: newer files may have uncertainty columns as well
  adopt_in = read.csv(file.path(Scenario_data, paste0("adoption_", scen, ".csv")), header=T) # Read in the inputs required to calculate annual adoption rates of each NBS type
  
  # Simple setup for islands so it can use two different extent files separated at the specified year
  if(scen == "islands") {
    
    # Input the extent data specific to the first half of the islands simulation
    extent_in = read.csv(file.path(Scenario_data, paste0("extent_islands.csv")), header=T) # Read in the inputs required to convert adoption rates to absolute values of hectares and emissions reductions
    NBS_in = merge(adopt_in, extent_in) # Create one dataframe that can be used as inputs to the function

    # Run the model and output for the first section of the islands simulation
    results = Annual_NBS_potentials(run_name = scen,
                                    inputs = NBS_in,
                                    timeframe_start = starttime,
                                    timeframe_end = endtime,
                                    output_area_data = F)

    # Input the second half of the extent data and adjust the extent correctly then wrangle the adoption data to match this new timeframe
    extent_in = read.csv(file.path(Scenario_data, paste0("extent_islands", islandstime, ".csv")), header=T) # Read in the inputs required to convert adoption rates to absolute values of hectares and emissions reductions
    tmp_extent = NBS_in[c("NBS_short_name", "extent")] %>% # Get the old extent data needed to limit the remaining extent in phase 2
      rename("phase1_extent" = "extent") # Rename to allow easy merge
    extent_in = merge(extent_in, tmp_extent[c("NBS_short_name", "phase1_extent")])
    extent_in$extent = extent_in$extent - extent_in$phase1_extent # Subtract off the original extent so the next phase only applies to the additional area
    adopt_in$year_50 = (adopt_in$year_50 - adopt_in$year_start) + islandstime # Shift all 50% years up to account for the new specified 'start' date 
    adopt_in$year_start = islandstime # Ensure all pathways are starting at 2050
    NBS_in = merge(adopt_in, extent_in) # Create one dataframe that can be used as inputs to the function
    
    # Run the model and output for the second section of the islands simulation
    results2 = Annual_NBS_potentials(run_name = scen,
                                     inputs = NBS_in,
                                     timeframe_start = islandstime,
                                     timeframe_end = endtime,
                                     output_area_data = F)
    
    # Combine the two sections into a single simulation (essentially join the first and second and then sum those years that overlap)
    results3 = rbind(results, results2) %>% 
      group_by(year, NBS_short_name, adoption_datatype, run_ID) %>% # Create groups of year and NBS pathway, the other two columns simply added for posterity to keep them in the output
      summarise_if(is.numeric, sum, na.rm=T) # Sum all those columns with numeric data, ignoring NAs
    
    # Write and assign the results to the global environment
    assign(scen, results3, envir = .GlobalEnv)
    
  } else { # If not the islands scenario then waves needs the same treatment...
    
    # Simple setup for waves so it can use two different extent files separated at the specified year
    if(scen == "waves") {
      
      # Input the extent data specific to the first half of the waves simulation
      extent_in = read.csv(file.path(Scenario_data, paste0("extent_waves.csv")), header=T) # Read in the inputs required to convert adoption rates to absolute values of hectares and emissions reductions
      NBS_in = merge(adopt_in, extent_in) # Create one dataframe that can be used as inputs to the function
      
      # Run the model and output for the first section of the waves simulation
      results = Annual_NBS_potentials(run_name = scen,
                                      inputs = NBS_in,
                                      timeframe_start = starttime,
                                      timeframe_end = endtime,
                                      output_area_data = F)
      
      # Input the second half of the extent data and adjust the extent correctly then wrangle the adoption data to match this new timeframe
      extent_in = read.csv(file.path(Scenario_data, paste0("extent_waves", wavestime,".csv")), header=T) # Read in the inputs required to convert adoption rates to absolute values of hectares and emissions reductions
      tmp_extent = NBS_in[c("NBS_short_name", "extent")] %>% # Get the old extent data needed to limit the remaining extent in phase 2
        rename("phase1_extent" = "extent") # Rename to allow easy merge
      extent_in = merge(extent_in, tmp_extent[c("NBS_short_name", "phase1_extent")])
      extent_in$extent = extent_in$extent - extent_in$phase1_extent # Subtract off the original extent so the next phase only applies to the additional area
      adopt_in$year_50 = (adopt_in$year_50 - adopt_in$year_start) + wavestime # Shift all 50% years up to account for the new specified 'start' date 
      adopt_in$year_start = wavestime # Ensure all pathways are starting at 2050
      NBS_in = merge(adopt_in, extent_in) # Create one dataframe that can be used as inputs to the function
      
      # Run the model and output for the second section of the islands simulation
      results2 = Annual_NBS_potentials(run_name = scen,
                                       inputs = NBS_in,
                                       timeframe_start = wavestime,
                                       timeframe_end = endtime,
                                       output_area_data = F)
      
      # Combine the two sections into a single simulation (essentially join the first and second and then sum those years that overlap)
      results3 = rbind(results, results2) %>% 
        group_by(year, NBS_short_name, adoption_datatype, run_ID) %>% # Create groups of year and NBS pathway, the other two columns simply added for posterity to keep them in the output
        summarise_if(is.numeric, sum, na.rm=T) # Sum all those columns with numeric data, ignoring NAs
      
      # Write and assign the results to the global environment
      assign(scen, results3, envir = .GlobalEnv)
      
    } else { # If not islands or waves it's much simpler
      
      # Combine the inputs specific to that scenario
      extent_in = read.csv(file.path(Scenario_data, paste0("extent_", scen, ".csv")), header=T) # Read in the inputs required to calculate annual adoption rates of each NBS type
      NBS_in = merge(adopt_in, extent_in) # Create one dataframe that can be used as inputs to the function
      
      # Run the model and write the output to the global environment
      results = Annual_NBS_potentials(run_name = scen,
                                      inputs = NBS_in,
                                      timeframe_start = starttime,
                                      timeframe_end = endtime,
                                      output_area_data = F)
      assign(scen, results, envir = .GlobalEnv)
    } # End of waves if statement
  } # End of both if statements
} # End of loop

#####
### COMBINE THE DIFFERENT SCENARIOS
#####

all_scenarios = rbind(base, accel, waves, islands) # Simply combine

# Quick plotcheck
ggplot(all_scenarios[all_scenarios$year<2101,], aes(x=year, y=TERs_MtCO2, fill=NBS_short_name)) +
  geom_area(colour='black') +
  facet_wrap(~run_ID)


# ONLY USEFUL WHEN RUNNING THE 37-PATHWAY VERSION OF THE MODEL:

# baseALL = merge(baseALL, extent_in[c("NBS_short_name", "NBS_subgroup", "NBS_desc")])
# ggplot(baseALL %>% arrange(NBS_subgroup), aes(x=year, y=TERs_MtCO2/1000, fill=NBS_desc)) +
#   geom_area(color="black") +
#   ylab(expression(paste("NBS emission reductions (Gt ", CO[2], " ", yr^-1, ")"))) +
#   scale_y_continuous(expand=c(0,0)) +
#   scale_x_continuous(expand=c(0,0)) +
#   theme(legend.position = "bottom",
#         legend.title = element_blank())
#   theme
#   facet_wrap(~run_ID)
# 
# baseALL2 = baseALL %>% 
#   group_by(year, NBS_subgroup, run_ID) %>%
#   summarise(TERs_MtCO2 = sum(TERs_MtCO2, na.rm=T))
# 
# ggplot(baseALL, aes(x=year, y=TERs_MtCO2, fill=NBS_subgroup)) +
#   geom_area() +
#   facet_wrap(~run_ID)

#####
### SUMMARIZED PLOT CHECKS
#####

# Simple metric to track change in parameters - Remove-to-Avoid ratio of total emissions
Summary_Data = merge(all_scenarios, extent_in[c("NBS_short_name", "NBS_desc", "active_remove")], by=c("NBS_short_name")) %>% 
  group_by(year, active_remove, run_ID) %>%
  summarise(annual_totals = sum(TERs_MtCO2, na.rm=T)) %>%
  spread(key = active_remove, value = annual_totals) %>%
  rename("Avoid" = "0", "Remove" = "1") %>%
  gather(key, value, -year, -run_ID)

ggplot(Summary_Data, aes(x=year, y=value/1000, colour=key)) +
  geom_line() +
  facet_wrap(~run_ID) +
  ylab(expression(paste("Annual emission reductions (Gt ", CO[2], " / yr)")))

# Single plot to compare scenarios and total TERs
FullSummary_Data = Summary_Data %>% 
  group_by(year, run_ID) %>%
  summarise(grand_totals = sum(value, na.rm=T))

ggplot(FullSummary_Data, aes(x=year, y=grand_totals/1000, colour=run_ID)) +
  geom_line() +
  ylab(expression(paste("Annual emission reductions (Gt ", CO[2], " / yr)")))

# Simple metric to track change in parameters - Remove-to-Avoid ratio of total emissions
Summary_Data = merge(all_scenarios, extent_in[c("NBS_short_name", "NBS_desc", "active_remove")], by=c("NBS_short_name")) %>% 
  group_by(year, active_remove, run_ID) %>%
  summarise(cumulative_totals = sum(TERs_MtCO2_todate, na.rm=T)) %>%
  spread(key = active_remove, value = cumulative_totals) %>%
  rename("Avoid" = "0", "Remove" = "1") %>%
  gather(key, value, -year, -run_ID)

ggplot(Summary_Data, aes(x=year, y=value/1000, colour=key)) +
  geom_line() +
  facet_wrap(~run_ID) +
  ylab(expression(paste("Cumulative emission reductions (Gt ", CO[2], ")")))

# Single plot to compare scenarios and total TERs
FullSummary_Data = Summary_Data %>% 
  group_by(year, run_ID) %>%
  summarise(grand_totals = sum(value, na.rm=T))

ggplot(FullSummary_Data, aes(x=year, y=grand_totals/1000, colour=run_ID)) +
  geom_line() +
  ylab(expression(paste("Cumulative emission reductions (Gt ", CO[2], ")")))

#####
### CLEAN UP AND SAVE AS NECESSARY
#####

# Remove all unnecessary objects
rm(list=ls(pattern = "^tmp"))

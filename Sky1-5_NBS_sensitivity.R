#####################################################################
#####################################################################
### CALCULATING SENSITIVITIES FOR PARAMETERS USED BY THE MODEL TO ###
### ESTIMATE ANNUAL EMISSION REDUCTIONS FOR NBS PATHWAYS          ###
### -- USES SOBOL INDECES FOR VARIANCE-BASED GLOBAL SENSITIVITY   ###
#####################################################################

rm(list=ls()) # Start with a blank slate if needed

# Load libraries
library(tidyverse)
library(data.table)

#####
### WORKSPACE SET UP
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
### CREATE SENSITIVITY RANGES AND RUN THE MODEL X TIMES TO GENERATE BANDS - use avoid and restore totals / yr as metrics
#####

### REQUIRED OPTIONS
# Notes - The values here are required but allow a choice so can be changed to desired simulation details:

# Set seed for reproducible random LHC sample - if you want to re-run but with different sampling, change this number
set.seed(500)

# Sensitivity analysis settings
runs = 1000                # How many Monte Carlo iterations you'll run the sensitivity over (to create the standard deviation)
parallelled = T            # Should you run the sensitivity analysis in parallel mode to use all cores of the machine?
manual_sens = T            # Manually specify the starting value ranges (i.e. uncertainty analysis) or use a set change %? (sensitivity analysis) - SPECIFY SVAR BELOW
svar = 0.2                 # Variability induced to calculate sensitivity (e.g. 0.2 is 20%) - NOT USED IF MANUALLY DEFINED
year_checks = c(2020:2100)#seq(2020, 2100, 10)      # Which years do you want to calculate sensitivities for? (Just choose 2100 if that's all you're interested in, or even 2020:2200 if you want all years)
#2035,
#2050,
#2100,
#2150)

# NBS potentials model settings
starting_year = 2000       # What year do you want to start the simulation on? For example, start at year 2000? Not recommended to start after current year
ending_year = 2100         # What year do you want to end the simulation on? For example, end in 2200? Must be after start year and recommend using 2050 or later
use_full_avgs = T          # Do you want to force the average emission reduction intensity to that reported by Griscom? - Recommend setting to TRUE
save_area_data = F         # Do you want to write the area data to the R environment as well as the emission reductions? - Recommend setting to FALSE
print_to_console = F       # Do you want the model output to print messages to your console? - Recommend setting to FALSE (slower to print)

# Include which parameters?
vary_adopt_pars = T        # Do you want to vary the input parameters used to define the adoption curves? Note - not all are recommended, specify below
vary_extent_pars = T       # Do you want to vary the input parameters used to convert adoption into emission reductions? - Note that if you don't to continuously vary inputs (i.e. this is set to FALSE) you have the option for an alternatives (below)

# Manually specify parameters 
adopt_pars = c("year_start", "year_50", "saturation") # What are the parameters you want to vary that define the S-curves?
extent_pars = c("extent", "ter_intensity") # What are the parameters you want to vary for extent and emission reduction intensity for each NBS?
modvars = c("TERs_MtCO2",  # Which metrics do you want to check sensitivities of - in theory area (Ha and TERs are linked)
            "TERs_MtCO2_todate",
            "area_Mha",
            "area_Mha_todate")

# Directory path to save sensitivity outputs to
if(manual_sens) {
  directory_name = "uncertainty" # If using manual ranges, assign results to 'uncertainty' directory
} else {
  directory_name = "sensitivities" # If using fixed % ranges, asssign results to 'sensitivity' directory
}

if(!dir.exists(file.path(TopDir, directory_name))) { # If there no directory exists, create it, otherwise no need
  dir.create(file.path(TopDir, directory_name))
}

SaveDir = file.path(TopDir, directory_name) # Assign directory path to save all results to

#####
### PREP INPUT PARAMETERS
#####

pathway_list = unique(adopt_in$NBS_short_name) # Get list of NBS pathways you want to run sensitivity for (Note - you can manually specify instead if you know the names)

# Wrangle the input data into a dataset that can be used easily by the NBS_potentials function
if(manual_sens) {
  vals = list() # Create empty list to put results into
  for(NBS_pathway in pathway_list) { # Loop over each pathway in the adoption input parameter set
    if(vary_adopt_pars) { # Use the input to determine which parameters are being varied
      tmp_adopt = adopt_in[adopt_in$NBS_short_name == NBS_pathway,] # Subset to the NBS pathway
      tmp_adopt_mean = as.data.frame(t(tmp_adopt[adopt_pars])) # Select only those columns of parameters of interest and transpose
      colnames(tmp_adopt_mean) = "val" # Name this 'average' input as 'val'
      tmp_adopt_min = as.data.frame(t(tmp_adopt[paste0(adopt_pars,"_mn")])) # Select only those columns of parameters of interest and transpose
      colnames(tmp_adopt_min) = "min" # Name the minimum of the range as 'min'
      tmp_adopt_max = as.data.frame(t(tmp_adopt[paste0(adopt_pars,"_mx")])) # Select only those columns of parameters of interest and transpose
      colnames(tmp_adopt_max) = "max" # Name the maximum of the range as 'max'
      tmp_adopt = as.data.frame(t(cbind(tmp_adopt_mean, tmp_adopt_min, tmp_adopt_max))) # Combine all columns for the min, val and max
    }
    if(vary_extent_pars) { # Use the input to determine which parameters are being varied
      tmp_extent = extent_in[extent_in$NBS_short_name == NBS_pathway,] # Subset to the NBS pathway
      tmp_extent_mean = as.data.frame(t(tmp_extent[extent_pars])) # Select only those columns of parameters of interest and transpose
      colnames(tmp_extent_mean) = "val" # Name this 'average' input as 'val'
      tmp_extent_min = as.data.frame(t(tmp_extent[paste0(extent_pars,"_mn")])) # Select only those columns of parameters of interest and transpose
      colnames(tmp_extent_min) = "min" # Name the minimum of the range as 'min'
      tmp_extent_max = as.data.frame(t(tmp_extent[paste0(extent_pars,"_mx")])) # Select only those columns of parameters of interest and transpose
      colnames(tmp_extent_max) = "max" # Name the maximum of the range as 'max'
      tmp_extent = as.data.frame(t(cbind(tmp_extent_mean, tmp_extent_min, tmp_extent_max))) # Combine all columns for the min, val and max
    }
    if(all(vary_adopt_pars, vary_extent_pars)) { # Use the input to determine which parameters are being varied
      tmp_combined = cbind(tmp_adopt, tmp_extent) # Combine both datasets if they exist
    } else {
      if(exists("tmp_adopt", globalenv())) {
        tmp_combined = tmp_adopt # If just adoption data use that
      } else {
        tmp_combined = tmp_extent # If just extent data use that
      }
    }
    vals[[NBS_pathway]] <- tmp_combined # Assign the value ranges to an object in the list named as the NBS pathway
  }
} else {
  vals = list() # Create empty list to put results into
  for(NBS_pathway in pathway_list) { # Loop over each pathway in the adoption input parameter set
    if(vary_adopt_pars) { # Use the input to determine which parameters are being varied
      tmp_adopt = adopt_in[adopt_in$NBS_short_name == NBS_pathway,] # Subset to the NBS pathway
      tmp_adopt = as.data.frame(t(tmp_adopt[adopt_pars])) # Select only those columns of parameters of interest and transpose
      colnames(tmp_adopt) = NBS_pathway # Give the column name to the pathway, just for ease of tracking in debugging
      tmp_adopt$min = ifelse(tmp_adopt[[NBS_pathway]]>2000,((tmp_adopt[[NBS_pathway]]-2000)*(1-svar))+2000,tmp_adopt[[NBS_pathway]]*(1-svar)) # Use the specified sensitivity range to create the upper and lower bounds - NOTE: Shift by 2000 because that is the base year for all pathways
      tmp_adopt$max = ifelse(tmp_adopt[[NBS_pathway]]>2000,((tmp_adopt[[NBS_pathway]]-2000)*(1+svar))+2000,tmp_adopt[[NBS_pathway]]*(1+svar)) # Use the specified sensitivity range to create the upper and lower bounds - NOTE: Shift by 2000 because that is the base year for all pathways
      tmp_adopt = as.data.frame(t(tmp_adopt)) # Transpose to be in the right format
      rownames(tmp_adopt) = c("val", "min", "max") # Ensure rownames are consistent as 'val' for average, 'min for lower bound and 'max' for upper bound
    }
    if(vary_extent_pars) { # Use the input to determine which parameters are being varied
      tmp_extent = extent_in[extent_in$NBS_short_name == NBS_pathway,] # Subset to the NBS pathway
      tmp_extent = as.data.frame(t(tmp_extent[extent_pars])) # Select only those columns of parameters of interest and transpose
      colnames(tmp_extent) = NBS_pathway # Give the column name to the pathway, just for ease of tracking in debugging
      tmp_extent$min = tmp_extent[[NBS_pathway]]*(1-svar) # Use the specified sensitivity range to create the upper and lower bounds 
      tmp_extent$max = tmp_extent[[NBS_pathway]]*(1+svar) # Use the specified sensitivity range to create the upper and lower bounds 
      tmp_extent = as.data.frame(t(tmp_extent)) # Transpose to be in the right format
      rownames(tmp_extent) = c("val", "min", "max") # Ensure rownames are consistent as 'val' for average, 'min for lower bound and 'max' for upper bound
    }
    if(all(vary_adopt_pars, vary_extent_pars)) { # Use the input to determine which parameters are being varied
      tmp_combined = cbind(tmp_adopt, tmp_extent) # Combine both datasets if they exist
    } else {
      if(exists("tmp_adopt", globalenv())) {
        tmp_combined = tmp_adopt # If just adoption data use that
      } else {
        tmp_combined = tmp_extent # If just extent data use that
      }
    }
    vals[[NBS_pathway]] <- tmp_combined # Assign the value ranges to an object in the list named as the NBS pathway
  }
}

uniquepars = colnames(vals[[1]]) # Get a vector of the parameters exactly as listed in the create list dataframes (same as binding those user-specified parameters at the top)
ALLpars = c("global", uniquepars) # Add a 'global' parameter where the sensitivity analysis will allow for all parameters to change simultaneously

# Create a 'settings' table and priors that can be saved along with the other outputs to explain how the sensitivity analysis was run (queriable for later)
settings = data.frame(Sim_date=format(Sys.Date(), "%Y-%m-%d"), Sim_time=format(Sys.time(),"%H:%M"), # Run time and date
                      Sim_timezone=Sys.timezone(), Sim_user=Sys.info()["user"], # User information
                      Sim_OS=Sys.info()["sysname"], Sim_machID=Sys.info()["nodename"], # System information
                      
                      runs=runs, sim_start=starting_year, sim_end=ending_year, # Generic simulation settings
                      manual_sens=manual_sens, parallelled=parallelled, # Generic simulation settings
                      
                      sens_variability=svar, use_full_avgs=use_full_avgs, # Specific simulation settings 
                      vary_adopt_pars=vary_adopt_pars, vary_extent_pars=vary_extent_pars, # Specific simulation settings 
                      n_par_varied=length(uniquepars), years_checked=length(year_checks)) # Specific simulation settings 

priors = NBS_in[c("NBS_desc", uniquepars)] # So there is a record of the exact priors used for these results

# Save the settings and priors to the same directory
write.csv(priors, file.path(SaveDir, paste0("priors_",runs,"runs_",Sys.Date(),".csv")))
write.csv(settings, file.path(SaveDir, paste0("settings_",runs,"runs_",Sys.Date(),".csv")), row.names=F)

# Can be useful to do a final cleanup of temporary objects before starting simulation
rm(list = ls(pos = ".GlobalEnv")[grep("^tmp", ls(pos = ".GlobalEnv"))], pos = ".GlobalEnv")

#####
### RUN THE SENSITIVITY ANALYSIS
#####

# Set header column names for the results file
header = c("year",
           "NBS_short_name",
           "adoption_datatype",
           "area_proportion",
           "area_Mha",
           "TERs_MtCO2",
           "area_Mha_todate",
           "TERs_MtCO2_todate",
           "run_ID",
           uniquepars)

# Set up and run the sensitivity analysis
if(parallelled) { # If using parallelization, needs additional setup...
  
  library(doParallel) # Load parallel library
  cors = ifelse(length(ALLpars) > detectCores(), detectCores(), length(ALLpars)) # Assign number of cores you want to use. NOTE: When there are more parameters than cores it is most efficient to make the cores exactly divisible
  cl <- makeCluster(cors, outfile="cluster_issues.txt") # Create a cluster of CPU cores with a single output files to write printed messages to (from all cores)
  registerDoParallel(cl) # Register that cluster
  clusterCall(cl, function(x) .libPaths(x), .libPaths()) # Can be useful to explicitly call which packages this simulations needs. Alternatively, just ensure all cores are pointing to the correct library path to load libraries
  ALLpars2 = as.vector(unlist(strsplit(ALLpars,",")),mode="list") # Convert the object naming the parameters into a list
  tpars = split(ALLpars2, seq(cors)) # Assign each of those parameters to a separate numbered core in that list
  print("Using parallelization") # Just inform the user that this simulation is happening over paralleled cores
  
  # Set up simulation loop
  foreach(i = 1:cors, .packages = c("tidyverse")) %dopar% { # For each core, do the following... (note, specifying the necessary packages can be useful if the core can't find the right library path)
    tparspar = unlist(tpars[[i]]) # Get the name of this parameter that you're running the sensitivity for
    for(sim_id in tparspar) { # For each parameter of those given to this core (note, it may just be one), do the following...
      if (sim_id == "global") {
        cat("Running global Monte Carlo simulation\n") # Print a message saying what parameter is being evaluated
      } else {
        cat("Now running Monte Carlo simulations for variable ", sim_id, "\n", sep = "") # Print a message saying what parameter is being evaluated
      }
      
      # Set name for results file
      outfile = file.path(SaveDir, paste("sensitivity-", sim_id, ".txt", sep = "")) # Assign this simulation a file to write outputs to
      
      # Open results file and set to write mode
      dataout = file(outfile, "w") # Create/open file and set the core to write results to this file
      
      # Start writing results to table
      write.table(t(header), dataout, sep = "\t", append = FALSE, quote = FALSE, col.names = FALSE, row.names = FALSE)
      
      # Start a loop of running separate sensitivities for each NBS pathway - could be bypassed for 'holistic' sensitivity using single set of parameter inputs for all pathways regardless (e.g. all pathways would have same ter_intensity, for example) - but not realistic
      for(NBS_pathway in pathway_list) { # Loop over the pathways
        
        cat("\n", "- - - - - - PATHWAY: ", NBS_pathway, " - - - - - -" ,"\n", "\n", sep = "") # Print to console/output what pathway is being run
        
        # Load packages - good to call explicitly within the loop but if the core can't find the library path may throw an error
        library(lhs) # For generating latin-hypercube sampled values
        
        # Build LHC sample and format so they can be used by the NBS_potentials function
        lcube = randomLHS(runs, length(uniquepars)) # Build sampling cube
        colnames(lcube) = uniquepars # Ensure the names are consistent with parameters
        for(par in uniquepars) { # For each parameter convert the LHC sample number (i.e. # between 0-1) to a number that can be used as a parameter input
          lcube[, par] = vals[[NBS_pathway]]["min",par] + ((vals[[NBS_pathway]]["max",par] - vals[[NBS_pathway]]["min",par]) * lcube[, par]) # Convert variables to values to use as run input
        }
        
        # Start main Monte Carlo simulation loop
        for (run_num in 1:runs){ # Repeat the simulation x number of times to sample the full hyperspace
          
          # The sensitivity works by varying all parameters besides the one of interest. Then comparing that to the 'global' sensitivity where all parameters are varied
          # Wrangle the full lcube dataframe into a subset that can be used for sensitivity analyses
          other_pars = data.frame() # Create empty dataframe to put the lcube parameter data into (i.e. all but the parameter of interest)
          for (par in uniquepars) { # Being explicit we can loop over each all parameters and ensure we're getting the right final parameter values
            if(sim_id == "global"){ # If the parameter of interest is 'global' then just use the lcube row of interest
              global_pars = as.data.frame(lcube[run_num,]) # Ensure it's in dataframe format
            } else {
              if (sim_id == par) { # If the par in this loop is the parameter of interest is then take the original, average parameter value from the initial inputs
                this_par = vals[[NBS_pathway]]["val",par] # Get the original value for this parameter for this NBS pathway
                names(this_par) = par # Ensure the name is correct (needed by the final input to NBS_potentials model)
                this_par = as.data.frame((this_par)) # Ensure it's in dataframe format
                colnames(this_par) = "run_parameters" # Rename column so it can be combined with the other parameters (see next section)
              } else { # If the par in this loop is the parameter of interest is then take the lcube-transformed parameter value
                tmp_par = lcube[run_num, par] # Use lcube sampling for that parameter for the row specific to this run
                tmp_par = as.data.frame((tmp_par)) # Ensure it's in dataframe format
                colnames(tmp_par) = "run_parameters" # Rename column so it can be combined name is consistent so can be combined
                other_pars = rbind(other_pars, tmp_par) # Bind each of the parameters together from this loop
              }
            }
          } # End of loop
          
          if(sim_id == "global"){
            varied_pars = as.data.frame(t(global_pars)) # If the parameter of interest is 'global' just transpose and save to new object
          } else {
            varied_pars = as.data.frame(t(rbind(this_par, other_pars))) # If parameter of interest is not global then combine the rest of the parameter data and transpose
          }
          
          # For those parameters that aren't being used for the sensitivity analysis, get their default values and combine with the newly created sampled values
          sens_inputs = NBS_in[NBS_in$NBS_short_name == NBS_pathway,] # Get original default values for that pathway
          sens_inputs = sens_inputs[, -which(names(sens_inputs) %in% colnames(varied_pars))] # Remove those columns that will be pulled in from the sensitivity analysis setup
          sens_inputs = cbind(sens_inputs, varied_pars) # Combine all parameter values
          
          ### RUN THE ACTUAL SIMULATION/MODEL
          output = Annual_NBS_potentials(run_name=paste0("Run", run_num, "_par_", sim_id),
                                         inputs=sens_inputs,
                                         timeframe_start = starting_year,
                                         timeframe_end = ending_year,
                                         force_average_ERprofile = use_full_avgs,
                                         output_area_data = save_area_data,
                                         print_messages = print_to_console)
          
          # Using the output, subset to be just those years of interest - otherwise final output can be unreasonably large!
          summary_out = output[output$year %in% year_checks,] # Subset 
          
          # Can be useful to combine the results with the sensitivity parameter values used to generate these results
          for(par in uniquepars) {
            summary_out[par] = varied_pars[[par]] # For each parameter, create a new column that simply repeats the parameter value for each year
          } # End of loop
          
          # Output data is by default a LOT of decimal places. Reduce this to 6 decimal places (i.e. to the nearest single hectare)
          summary_out = summary_out %>% 
            mutate_if(is.numeric, round, digits=6)
          
          # Write this new data to the table created at the beginning
          write.table(summary_out, dataout, sep = "\t", append = FALSE, quote = FALSE, col.names = FALSE, row.names = FALSE)
          
          # Print to the console/output file what the run number was and which parameter was being evaluated
          cat("Run number ", run_num, " of ", runs, "\n",
              "Variable ", grep(sim_id, tparspar), " of ", length(tparspar), "\n",
              sep = "")
          
        } # End of Monte Carlo simulation loop
        
      }
      # Close output file
      close(dataout)
      cat("Successful Monte Carlo simulation!\n") # Print to give the good news :)
      # End of simulation loop
      
    } # End of parameter loop given to this core
  } # End of foreach loop for all cores
  
} else {
  
  print("Not using parallelization!") # If not using parallelization, no additional setup needed, just print to console
  
  # Set up simulation loop
  for(sim_id in ALLpars) { 
    if (sim_id == "global") {
      cat("Running global Monte Carlo simulation\n") # If parameter of interest is 'global' then varying all parameter values at once
    } else {
      cat("Now running Monte Carlo simulations for variable ", sim_id, "\n", sep = "") # If parameter of interest is anything but 'global' then varying all but the parameter of interest
    }
    
    # Set name for results file
    outfile = file.path(SaveDir, paste("sensitivity-", sim_id, ".txt", sep = "")) # Assign this simulation a file to write outputs to
    
    # Open results file and set to write mode
    dataout = file(outfile, "w") # Create/open file and set the core to write results to this file
    
    # Start writing results to table
    write.table(t(header), dataout, sep = "\t", append = FALSE, quote = FALSE, col.names = FALSE, row.names = FALSE)
    
    # Start a loop of running separate sensitivities for each NBS pathway - could be bypassed for 'holistic' sensitivity using single set of parameter inputs for all pathways regardless (e.g. all pathways would have same ter_intensity, for example) - but not realistic
    for(NBS_pathway in pathway_list) { # Loop over the pathways
      
      cat("\n", "- - - - - - PATHWAY: ", NBS_pathway, " - - - - - -" ,"\n", "\n", sep = "") # Print to console/output what pathway is being run
      
      # Load packages - good to call explicitly within the loop but if the core can't find the library path may throw an error
      library(lhs) # For generating latin-hypercube sampled values
      
      # Build LHC sample and format so they can be used by the NBS_potentials function
      lcube = randomLHS(runs, length(uniquepars)) # Build sampling cube
      colnames(lcube) = uniquepars # Ensure the names are consistent with parameters
      for(par in uniquepars) { # For each parameter convert the LHC sample number (i.e. # between 0-1) to a number that can be used as a parameter input
        lcube[, par] = vals[[NBS_pathway]]["min",par] + ((vals[[NBS_pathway]]["max",par] - vals[[NBS_pathway]]["min",par]) * lcube[, par]) # Convert variables to values to use as run input
      }
      
      # Start main Monte Carlo simulation loop
      for (run_num in 1:runs){ # Repeat the simulation x number of times to sample the full hyperspace
        
        # The sensitivity works by varying all parameters besides the one of interest. Then comparing that to the 'global' sensitivity where all parameters are varied
        # Wrangle the full lcube dataframe into a subset that can be used for sensitivity analyses
        other_pars = data.frame() # Create empty dataframe to put the lcube parameter data into (i.e. all but the parameter of interest)
        for (par in uniquepars) { # Being explicit we can loop over each all parameters and ensure we're getting the right final parameter values
          if(sim_id == "global"){ # If the parameter of interest is 'global' then just use the lcube row of interest
            global_pars = as.data.frame(lcube[run_num,]) # Ensure it's in dataframe format
          } else {
            if (sim_id == par) { # If the par in this loop is the parameter of interest is then take the original, average parameter value from the initial inputs
              this_par = vals[[NBS_pathway]]["val",par] # Get the original value for this parameter for this NBS pathway
              names(this_par) = par # Ensure the name is correct (needed by the final input to NBS_potentials model)
              this_par = as.data.frame((this_par)) # Ensure it's in dataframe format
              colnames(this_par) = "run_parameters" # Rename column so it can be combined with the other parameters (see next section)
            } else { # If the par in this loop is the parameter of interest is then take the lcube-transformed parameter value
              tmp_par = lcube[run_num, par] # Use lcube sampling for that parameter for the row specific to this run
              tmp_par = as.data.frame((tmp_par)) # Ensure it's in dataframe format
              colnames(tmp_par) = "run_parameters" # Rename column so it can be combined name is consistent so can be combined
              other_pars = rbind(other_pars, tmp_par) # Bind each of the parameters together from this loop
            }
          }
        } # End of loop
        
        if(sim_id == "global"){
          varied_pars = as.data.frame(t(global_pars)) # If the parameter of interest is 'global' just transpose and save to new object
        } else {
          varied_pars = as.data.frame(t(rbind(this_par, other_pars))) # If parameter of interest is not global then combine the rest of the parameter data and transpose
        }
        
        # For those parameters that aren't being used for the sensitivity analysis, get their default values and combine with the newly created sampled values
        sens_inputs = NBS_in[NBS_in$NBS_short_name == NBS_pathway,] # Get original default values for that pathway
        sens_inputs = sens_inputs[, -which(names(sens_inputs) %in% colnames(varied_pars))] # Remove those columns that will be pulled in from the sensitivity analysis setup
        sens_inputs = cbind(sens_inputs, varied_pars) # Combine all parameter values
        
        ### RUN THE ACTUAL SIMULATION/MODEL
        output = Annual_NBS_potentials(run_name=paste0("Run", run_num, "_par_", sim_id),
                                       inputs=sens_inputs,
                                       timeframe_start = starting_year,
                                       timeframe_end = ending_year,
                                       force_average_ERprofile = use_full_avgs,
                                       output_area_data = save_area_data,
                                       print_messages = print_to_console)
        
        # Using the output, subset to be just those years of interest - otherwise final output can be unreasonably large!
        summary_out = output[output$year %in% year_checks,] # Subset 
        
        # Can be useful to combine the results with the sensitivity parameter values used to generate these results
        for(par in uniquepars) {
          summary_out[par] = varied_pars[[par]] # For each parameter, create a new column that simply repeats the parameter value for each year
        } # End of loop
        
        # Output data is by default a LOT of decimal places. Reduce this to 6 decimal places (i.e. to the nearest single hectare)
        summary_out = summary_out %>% 
          mutate_if(is.numeric, round, digits=6)
        
        # Write this new data to the table created at the beginning
        write.table(summary_out, dataout, sep = "\t", append = FALSE, quote = FALSE, col.names = FALSE, row.names = FALSE)
        
        # Print to the console/output file what the run number was and which parameter was being evaluated
        cat("Run number ", run_num, " of ", runs, "\n",
            "Variable ", grep(sim_id, ALLpars), " of ", length(ALLpars), "\n",
            sep = "")
        
      }   # End of Monte Carlo simulation loop
      
    } # End of pathway loop
    # Close output file
    close(dataout)
    cat("Successful Monte Carlo simulation!\n") # Print to give the good news :)
    # End of simulation loop
    
  } # End of parameter loop given to this core
} # End of loop

# Close open connections - Not essential but useful for clearing environment, especially when many thousands of simulations are run over many cores
closeAllConnections()
gc() # Clear the cache

#####
### USE THE RESULTS FROM THE MONTE-CARLO TO CALCULATE SENSITIVITIES
#####

### READ IN AND REFORMAT THE RESULTS

# Obtain a list of the sensitivity analysis output files
sens.result.list = list.files(path = SaveDir, pattern = "sensitivity", full.names = TRUE)

# Read each sensitivity results file into R and assign correct names based on variables
results = lapply(sens.result.list, function (x) read.table(x, sep="\t", header = TRUE)) # Note that the read needs to specify the tab separator
resnames = lapply(sens.result.list, function (x) { # For each file, obtain the names of parameter that was evaluated
  sublist = strsplit(sub(".txt", "", x), "-") # Split the name by -
  fname = unlist(sublist)[length(unlist(sublist))] # Get the name 
  return(fname) # And return
} )
names(results) = resnames # Assign these names to the results that were read in

# Optional but can be nice to have this newly formatted results saved as a single R object for use later if needed (means that you don't have to save the text files and can overwrite them)
save(results, file = file.path(SaveDir, paste0("results_", runs, "_runs_", Sys.Date(), ".RData")))
#load(file.path(SaveDir, "results_200_runs_2020-10-27.RData"))
### CALCULATE SENSITIVITIES USING GLOBAL SOBOL INDEX

# Create a blank dataframe to put confidence interval results into
outputs = data.frame()#rn=NA, sdev=NA, ci=NA, diff=NA, metric=NA, year=NA, NBS_short_name=NA) 

# Loop over each pool and calculate the contribution of each parameter to the change in that pool (contribution index)
for(yr in year_checks) { # Calculate sensitivities for each year of data
  for(NBS_pathway in pathway_list) { # Loop over each of the NBS pathways to calculate parameter sensitivities within a pathway AND within a year
    for(mvar in modvars) { # Modvars are the model pools of interest (set at top of script)
      subset_results = lapply(results, function (x) subset(x, year==yr & NBS_short_name==NBS_pathway)) # Subset the full results by pathway and year
      uncert = data.frame(sdev = t(as.data.frame(lapply(subset_results, function (x) sd(x[[mvar]])))), ci = NA) # Calculate the standard deviation for outputs from each metric (i.e. MtCO2) and for each parameter
      globalsd = uncert["global","sdev"] # Get the standard deviation for the global run (when everything was varied)
      uncert$diff = globalsd-uncert$sdev # Get the difference between the global SD and that for each parameter (bigger difference = more sensitive)
      alldiffs = sum(uncert$diff) # Sum the differences
      for(par in uniquepars) { # For each parameter, calculate the 'contribution index' which is simply the global SD - parameter SD / sum of differences
        uncert[par, "ci"] = (globalsd - uncert[par, "sdev"]) / alldiffs # Simple calculation
      }
      uncert$ci = (abs(uncert$ci) / sum(abs(uncert$ci), na.rm = TRUE)) * 100 # Using full variance, convert to parameter contribution towards total - i.e. percentage 
      uncert = setDT(uncert, keep.rownames = TRUE)[] # Make row names a column
      uncert$metric = mvar # Add a column to state what metric this sensitivity refers to
      uncert$year = yr # Add a column to state which year this sensitivity refers to
      uncert$NBS_short_name = NBS_pathway # Add a column to state what NBS pathway this sensitivity refers to
      
      outputs = rbind(outputs, uncert) # Bind results for all pools together into a single dataframe
    } # End of output metric loop
  } # End of pathway loop
} # End of year loop

#####
### REFORMAT AND PLOT THE SENSITIVITY RESULTS
#####

# Wrangle the results into plot-friendly formats
outputs = merge(outputs, NBS_in[c("NBS_desc", "NBS_short_name")]) # Merge the sensitivity results with the full NBS names to help with plotting aesthetics

# If you want you can choose specific metrics, years or NBS pathways to plot - subset
#outputs = outputs[outputs$metric %in% c("TERs_MtCO2", "area_Mha") & outputs$year == 2050 & outputs$NBS_short_name %in% c("Agro", "Refo", "REDD"),]

# Decide what to do with parameters that don't contribute to any sensitivity
outputs$ci[outputs$ci == 0] = NA # When a parameter contributes absolutely nothing to the change in a pool then make that NA
outputs = outputs[complete.cases(outputs$ci),] # Remove any rows where a parameter does not contribute to the change in a pool AT ALL

# Graph as a tile plot
p1 = ggplot(outputs[outputs$rn != "global",],aes(y=NBS_desc,x=rn, fill = ci)) +
  geom_tile(colour = "black",
            width = 0.9, height = 0.9) +
  scale_fill_continuous(low = "#ffffe6", high = "#cc0000", # colorbrewer2 4-class OrRd (Orange-Red scale - change if desired)
                        limits = c(0, 100),
                        guide = guide_colourbar(title.position = "bottom")) +
  geom_text(aes(label=format(round(ci,1),nsmall=1)), size = 3) +
  labs(y = "NBS Pathway",
       x = "Parameter",
       fill = expression(Contribution~index~(italic(ci)))) +
  facet_wrap(~metric*year, nrow=length(modvars)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 9, angle = 90, hjust= 1, vjust=0.5),
        axis.title.x = element_text(vjust = 0, size = 11),
        plot.title = element_text(hjust = 1.8),
        panel.spacing = unit(2, "lines"),
        legend.position = "bottom")

# And save the resulting plot
ggsave(p1, file=file.path(SaveDir,
                          paste("Sensitivities_",runs,"runs_",Sys.Date(),".pdf",sep="")),
       width=600, height=400, units="mm")

# OPTIONAL - Save the final calculated sensitivities to an R object to save having to rerun the above. But could be done if you saved the results object earlier
save(outputs, file = file.path(SaveDir, paste0("outputs_", runs, "_runs_", Sys.Date(), ".RData")))

#####
### CLEAN UP AND SAVE AS NECESSARY
#####

test = results[["global"]]

ggplot(test, aes(x=year, y=TERs_MtCO2/1000)) +
  geom_area(aes(fill=NBS_short_name)) +
  facet_wrap(~run_ID) +
  ylab(expression(paste("Total annual emission reductions (Gt ", CO[2], yr^-1, ")")))

test2 = test %>%
  group_by(year, run_ID) %>%
  summarise(TERs = sum(TERs_MtCO2))

ggplot(test2, aes(x=year, y=TERs/1000)) +
  geom_line(aes(colour=run_ID), alpha=0.5) +
  geom_smooth(se=F) +
  scale_color_manual(values=c(rep("grey50", nlevels(as.factor(test2$run_ID))))) +
  ylab(expression(paste("Total annual emission reductions (Gt ", CO[2], yr^-1, ")"))) +
  theme(legend.position = 'none')

# Remove all unnecessary objects
rm(list=ls(pattern = "^tmp"))

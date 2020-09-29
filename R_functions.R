###################################
### === GENERIC R FUNCTIONS === ###
###################################

# Clear global environment from including anything with a given string - defaults to "tmp"
CleanEnvir <- function(obj_string = "^tmp") {
  objs <- ls(pos = ".GlobalEnv")
  rm(list = objs[grep(obj_string, objs)], pos = ".GlobalEnv")
}

# Sometimes RStudio won't connect to SharePoint. In the first instance, try to connecting to VPN (if off-site)
# Otherwise, we will need to load a local (stored on GitHub) version to work from. NOTE - This may not be up to date!
# There isn't a simple base R function that does "if(error) else..." so we need to write our own function around "try()"
SP_readCSV = function (file.name, has_header=T) { # Requires full directory for SharePoint given as 'file.name'. Also assumes we're using read.csv so has a header condition
  file = try(suppressWarnings(read.csv(file.name, header = has_header)), silent = T) # First attempt to load from SharePoint - silent = T means no error is printed to console
  if (class(file) == "try-error") { # If no error then all good
    cat("WARNING: Could not read from SharePoint, using local version instead. \n NOTE - This may not be the most up-to-date version of", basename(file.name), "\n") # Just a print to the console to show info
    file = read.csv(file.path(Local_DataDir, basename(file.name)), header = has_header) # Read from local directory location instead - NOTE: Assumes exact same file name and not in sub-folder unless specified at the top
  } else {
    cat("Successfully read the file from SharePoint: \n Using most up-to-date version of", basename(file.name), "\n") # Simple print to console to inform us
  } # End of if statement
  file # Return the resulting file!
} # End of function

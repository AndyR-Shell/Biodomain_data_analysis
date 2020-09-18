#######################
### SCRIPT FOR SIMPLE CHECKS ON NATIONAL NBS POTENTIALS
#######################

library(tidyverse)
library(rgdal)
library(leaflet)

# Initial data import from SharePoint folder "Credited_Projects"
SPDir = "https://eu001-sp.shell.com//sites//AAAAB3387//Nature%20Based%20Solutions"
TopDir = "https://eu001-sp.shell.com//sites//AAAAB3387//Nature%20Based%20Solutions//Scenarios"
DataDir = "https://eu001-sp.shell.com//sites//AAAAB3387//Nature%20Based%20Solutions//Scenarios//National%20Potentials%20Tier%202//Standardized%20country%20data//Scripts%20and%20input%20data"
Local_DataDir = "data"

# Sometimes RStudio won't connect to SharePoint. In the first instance, try to connecting to VPN (if off-site)
# Otherwise, we will need to load a local (stored on GitHub) version to work from. NOTE - This may not be up to date!
# There isn't a simple base R function that does "if(error) else..." so we need to write our own function around "try()"
SP_readCSV = function (file.name, has_header=T) { # Requires full directory for SharePoint given as 'file.name'. Also assumes we're using read.csv so has a header condition
  file = try(read.csv(file.name, header = has_header)) # First attempt to load from SharePoint
  if (class(file) == "try-error") { # If no error then all good
    cat("Could not read from SharePoint, using local version. NOTE - This may not be the most up-to-date version of", basename(file.name), "\n") # Just a print to the console to show info
    file = read.csv(file.path(Local_DataDir, basename(file.name)), header = has_header) # Read from local directory location instead - NOTE: Assumes exact same file name and not in sub-folder unless specified at the top
  } else {
    cat("Successfully read the file from SharePoint - using most up-to-date version of", basename(file.name), "\n") # Simple print to console to inform us
  } # End of if statement
  file # Return the resulting file!
} # End of function

### Import and check/analyze the national data
nat_ters = SP_readCSV(file.path(DataDir, "national_extent_ters.csv"), has_header = T) # Read emissions data
nat_ters = nat_ters[,colSums(is.na(nat_ters))<nrow(nat_ters)] # Drop the columns where no data is present at all (i.e. all NA)
nat_other = SP_readCSV(file.path(TopDir, "National Potentials Tier 2", "Standardized country data", "combined_country_data_essential.csv"), has_header = T) # Read other national data
str(nat_ters) # Check structure
str(nat_other) # Check structure

### Import the spatial data for mapping
world_spdf=readOGR(dsn=Local_DataDir, layer="TM_WORLD_BORDERS_SIMPL-0.3")

### Wrangle the ter dataset
nat_ters_lim = nat_ters[c("ISO3", "NBS_desc", "potential")] # Subset to only useful columns
nat_ters_lim = nat_ters_lim %>% spread(key = NBS_desc, value = potential) # Convert from long to wide
colnames(nat_ters_lim) = c("ISO3", gsub(pattern = " ", replacement = ".", colnames(nat_ters_lim[2:length(colnames(nat_ters_lim))]))) # Rename columns to remove spaces

### Wrangle a dataframe that can combine with the world border spdf and give extra columns
tmpdf = data.frame(world_spdf@data) # Get existing spdf
tmpdf = data.frame(tmpdf, nat_other[match(tmpdf$ISO3, nat_other$ISO3),]) # Combine with the national data for all info - NOTE: For whatever reason, merge won't work have to use match()
tmpdf = data.frame(tmpdf, nat_ters_lim[match(tmpdf$ISO3, nat_ters_lim$ISO3),]) # Combine with the ter dataset
world_spdf@data = tmpdf # Add back into the spdf

### Choose what you want to visualize
NBS_to_plot = "Improved plantations"

### Set up to properly visualize spatial data for the chosen column
tmp = world_spdf # Create a subset of the spdf that you can visualize specific to the column chosen
activity_glob = gsub(pattern = " ", replacement = ".", NBS_to_plot) # Simply take the chosen column and replace spaces with .s
topval = floor(quantile(tmp@data[[activity_glob]], probs=0.99, na.rm = T)) # Obtain the maximum value to set your colour scale to
mybins=c(0,topval/20,topval/10,topval/5,topval/2,topval,Inf) # Derive the bins of colours you want to use (default set to 0.05, 0.1, 0.2 and 0.5)
mypalette = colorBin( palette="YlOrBr", domain=tmp@data[[activity_glob]], na.color="grey", bins=mybins) # Generate the palette for that scale

# If you want, create a HTML style popup that will appear on hover over each country (can use other columns from nat_other for example)
mytext=paste("<span style='color: salmon;'><strong>", tmp@data$Country,"</strong></span><br/>", "Area (Mha): ", round(tmp@data$Land.Area/1000,1), "<br/>", "2018 Population (M): ", round(tmp@data$Population/1000, 2),
             "<br/>Maximum ", activity_glob, " ERs (Mt CO2e):", round(tmp@data[[activity_glob]],2),
             sep="") %>%
  lapply(htmltools::HTML) # Simply ensures it's in proper HTML format

### Create basic choropleth map with leaflet
leaflet(tmp) %>% 
  addTiles()  %>% 
  setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( 
    fillColor = ~mypalette(tmp@data[[activity_glob]]), stroke=TRUE, fillOpacity = 0.9, color="white", weight=0.3,
    highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE, fillColor = "black", fillOpacity = 0.3),
    label = mytext,
    labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
  ) %>%
  addLegend( pal=mypalette, values=~tmp@data[[activity_glob]], opacity=0.9, title = paste0(activity_glob, " potential (Mt CO2e)"), position = "bottomleft" )


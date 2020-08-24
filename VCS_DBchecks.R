#####################################################
#####################################################
### ANALYSIS OF VCS PROJECT DATA FOR NBS PROJECTS ###
#####################################################

# Initial data import from SharePoint folder "Credited_Projects"
SPDir = "https://eu001-sp.shell.com//sites//AAAAB3387//Nature%20Based%20Solutions"
TopDir = "https://eu001-sp.shell.com//sites//AAAAB3387//Nature%20Based%20Solutions//Credited_Projects"

### Import VCS database compiled by Biodomain with detailed info from PDDs - up to date as of June 2019
VCS_project_data = read.csv(file.path(TopDir, "VCS_Projects", "VCS_projects_summary.csv"), header=T)
VCS_annual_ER_data = read.csv(file.path(TopDir, "VCS_Projects", "VCS_annual_ERs.csv"), header=T)

### Import trading data for all issued and retired credits up to June 30th 2020 for VCS, CAR, ACR, GS and CDM
# NOTE - This is a very very large file and will take a while to read in
trading_data = read.csv(file.path(TopDir, "CarbonCredits_trading.csv"), header=T)

### Check that a merge works - subset to VCS and to NBS only
VCS_trading_data = trading_data[trading_data$Standard == "VCS" & trading_data$Nature.Based.vs.Non.Nature.Based == "Nature Based",]

# Make sure there's a common column to use where IDs will match
VCS_trading_data$project_ID = paste0(VCS_trading_data$Standard, VCS_trading_data$Project.ID)

# Check vector types of the columns and fix where necessary
str(VCS_trading_data)
VCS_trading_data$Date = as.Date.character(VCS_trading_data$Date, format = "%m/%d/%Y") # Convert to date format from character format
VCS_trading_data$Total.Credits = as.numeric(gsub(",", "", as.character(VCS_trading_data$Total.Credits))) # Remove the commas for thousands and convert character to numeric
VCS_trading_data = VCS_trading_data[complete.cases(VCS_trading_data$Total.Credits),] # Trading data should have credit info, if it doesn't then remove those rows (likely just a legacy of how the CSV was saved)
str(VCS_trading_data) # Check it now looks all ok

# Merge? - Might take a while!
test_merge = merge(VCS_project_data, VCS_trading_data, by="project_ID", all=T)
# Result hints that there are 32 projects without any credits issued or retired...and possibly other projects in the VCS_trading_data which don't have the PDD data compiled yet

test_merge = test_merge[complete.cases(test_merge$Total.Credits),] # Remove rows with no data (i.e. likely the 32 rows mentioned above)

# Quick plot check...
library(tidyverse)
ggplot(test_merge, aes(x=Vintage, y=Total.Credits, fill=Sub.Project.Type)) +
  geom_bar(stat='identity') +
  facet_wrap(~Status)

#####################################################
### CHECKING OUTLIERS FOR NATIONAL NBS POTENTIALS ###
#####################################################

source("National_NBS_potentials.R") # Existing script should already be in the correct folder and can be run

######################################################################################
### AT THIS POINT PLEASE CHECK THAT THE 'DataDir' AND 'Local_DataDir' LOOK CORRECT ###
######################################################################################

### You should now be able to run the shiny app if you want:
#shinyApp(ui, server) # Note the units on hover are not correct for all options

### Read in additional data
GrisGlobal = SP_readCSV("global_extent_ters.csv", has_header = T)

### Create dataset to see which countries/pathways are NA
NAdataset = nat_ters[is.na(nat_ters$potential),] # Limit when there's no data from any source
NAdataset = NAdataset[c("ISO3", "Country", "NBS_desc")] # Subset to columns of interest only 
NAdataset = merge(NAdataset, GrisGlobal[c("NBS_desc", "NBS_group")]) # Add the NBS_group - easy plotting

### Sum ERs from each pathway to compare with Griscom global totals
NBS_sums = nat_ters %>%
  group_by(NBS_desc) %>%
  summarise(BDsums = sum(BD_potential, na.rm=T),
            Gris17 = sum(Griscom.2017.potential, na.rm=T),
            Gris20 = sum(Griscom.2020.potential, na.rm=T))

# Combine with global Griscom 2017 data (i.e. not national sums)
NBS_sums = merge(NBS_sums, GrisGlobal[c("NBS_desc", "NBS_group", "global_ter")])

# Plot to check
ggplot(gather(NBS_sums, key = "Source", value = "Potential", -NBS_desc, -NBS_group), aes(x=NBS_desc, y=Potential/1000, fill=Source)) +
  geom_bar(stat='identity', position = position_dodge(width=0.9)) +
  scale_fill_manual(values = c("blue", "red", "green", "black"), labels=c("BD Country Potential", "Griscom '17 Global", "Griscom '17 Country", "Griscom '20 Tropical")) +
  ylab(expression(paste("Annual NBS Potential (Gt ", CO[2],"e ", yr^-1, ")"))) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle=45, hjust=1, vjust=1),
        legend.title = element_blank()
  )

# Calculate the differences and express as %
for(i in colnames(NBS_sums)[2:4]) {
  NBS_sums[paste0(i, "DIFF")] = ifelse(NBS_sums[[i]]==0,NBS_sums[[i]],(NBS_sums[[i]] - NBS_sums$global_ter)/NBS_sums$global_ter*100)
}

# Plot to check
ggplot(gather(NBS_sums[colnames(NBS_sums)[c(1,5,7:9)]], key = "Source", value = "Difference", -NBS_desc, -NBS_group), aes(x=NBS_desc, y=Difference, fill=Source)) +
  geom_bar(stat='identity', position = position_dodge(width=0.75), colour='black') +
  geom_hline(yintercept = 0, linetype="solid") +
  scale_fill_manual(values = c("blue", "red", "green"), labels=c("BD Country Potential", "Griscom '17 Country", "Griscom '20 Tropical")) +
  ylab(expression(paste("Difference from Griscom 2017 Global (%)"))) +
  annotate("text", x=2, y=120, label="Potential is bigger than Griscom 2017", hjust=0) +
  annotate("text", x=2, y=-120, label="Potential is less than Griscom 2017", hjust=0) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle=45, hjust=1, vjust=1),
        legend.title = element_blank()
  )

###################################################################
### Check outliers from NAs/zeroes and from original input data ###
###################################################################

########
### === CHECK NAs
########
#shinyApp(ui, server) # For a quick check, you're best checking the Shiny map and countries in grey are NA

# You can also check the specific 'NAdataset'
ggplot(NAdataset, aes(x=ISO3, y=NBS_desc)) +
  geom_text(aes(label=ISO3)) +
  facet_wrap(~NBS_group, scales="free_y", ncol=1)

########
### === CHECK NAs
########

# Create dataframe of values < 1 tCO2/yr
nat_zeros = nat_ters[!is.na(nat_ters$potential) & nat_ters$potential < 0.000001,]
nat_zeros = merge(nat_zeros, GrisGlobal[c("NBS_desc", "NBS_group")]) # Add the NBS_group - easy plotting

# Check plot
ggplot(nat_zeros, aes(x=ISO3, y=NBS_desc)) +
  geom_text(aes(label=ISO3)) +
  facet_wrap(~NBS_group, scales="free_y", ncol=1)

########
### === CHECK INPUT DATA
########

# Make sure you have all relevant objects in environment by running the inputs script
source("National_NBS_inputs.R")

########
### === GRASSLAND LOSS
########
### Check grassland loss rates - biggest discrepancy
# Use same code as that used in National_NBS_reformatting.R but add a plot and a break to pause

graz_loss_outliers = data.frame() # Empty dataframe to put data into
for(i in levels(droplevels(as.factor(graz$Area)))) { # loop over the 'Areas' which are countries
  tmp = graz[graz$Area == i,] # Subset by country
  if(max(tmp$Year)>1985) { # Only work with countries where you have data from enough years
    tmp = tmp[tmp$Year>1979,] # Subset so the regression is only calculated from XXXX onwards
    regres = lm(Value~Year, data=tmp) # Calculate linear regression
    slope = summary(regres)[]$coefficients[[2,1]] # Extract slope element from regression
    
    plot(Value~Year, data=tmp, main=i) # Plot
    abline(regres, col="red") # Add the regression line to help visualize
    Sys.sleep(1) # Freeze the run for 5 seconds to see the plot before the next plot comes up - change to more or less if you need more/less time
    
    # Create a temporary dataframe with relevant data from this country
    tmp2 = data.frame(ISO3 = head(tmp$ISO3,1), # ISO country code
                      Country = head(tmp$Country,1), # Country name
                      base_year = min(tmp$Year), # What year the regression started analysis
                      recent_year = max(tmp$Year), # What year the regression finished analysis
                      regression = slope, # The slope of the regression
                      grass_loss = ifelse(slope>0,0,slope*-1)) # If the slope is greater than 0 then loss is assumed to be 0, otherwise, convert it to a positive (it now represents 000s hectares lost per year)
    tmp2$grass_loss_time = tmp2$recent_year - tmp2$base_year # Add a column to show the number of years used to calculate the loss
    
    graz_loss_outliers = rbind(graz_loss_outliers, tmp2) # Combine with all countries
  } # End of if-statement
} # End of loop

### Work with outlier DF to see what can be done...
ggplot(graz_loss_outliers, aes(x=ISO3, y=grass_loss, colour=grass_loss_time)) +
  geom_point()

########
### === OTHERS...
########

### If needed, clean environment
CleanEnvir()

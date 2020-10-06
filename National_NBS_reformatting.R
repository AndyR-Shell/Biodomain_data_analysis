##################################################
### === REFORMAT AND COMBINE NATIONAL DATA === ###
##################################################

### ASSUMES AND REQUIRES YOU TO RUN 'National_NBS_inputs.R' FIRST!

### Wrangling and reformatting for:
# Precipitation (mm/yr)
nat_map = nat_map[c("Country.Code", "X2014")]
nat_map$ISO3 = nat_map$Country.Code
nat_map = merge(nat_map, country_lookups[c("ISO3", "worldshapename")])
nat_map$ISO3 = nat_map$ISO3
nat_map$Country = nat_map$worldshapename
nat_map$MAP_year = 2014
nat_map$MAP = nat_map$X2014
nat_map = nat_map[,c(1,(ncol(nat_map)-2):ncol(nat_map))]
combined_country_data = merge(base_country_data, nat_map, all=T)

# Livestock grazing area (000s HECTARES)
live_graz = live_graz[c("FAO.Country", "Value")]
live_graz$FAOname = ifelse(live_graz$FAO.Country=="",NA,as.character(live_graz$FAO.Country))
live_graz = live_graz[complete.cases(live_graz$FAOname),]
live_graz = merge(live_graz, country_lookups[c("FAOname", "worldshapename", "ISO3")])
live_graz$Country = live_graz$worldshapename
live_graz$GrazYear = 2018
live_graz$`Grazing Area` = live_graz$Value
live_graz = live_graz[,(ncol(live_graz)-3):ncol(live_graz)]
combined_country_data = merge(combined_country_data, live_graz, all=T)

# Livestock numbers (HEAD OF CATTLE)
livestock = livestock[c("FAO.Country", "Value")]
livestock$FAOname = ifelse(livestock$FAO.Country=="",NA,as.character(livestock$FAO.Country))
livestock = livestock[complete.cases(livestock$FAOname),]
livestock = merge(livestock, country_lookups[c("FAOname", "worldshapename", "ISO3")])
livestock$Country = livestock$worldshapename
livestock$LiveYear = 2018
livestock$`Livestock Head` = as.numeric(livestock$Value)/1000 # Convert to 000s head
livestock = livestock[,(ncol(livestock)-3):ncol(livestock)]
combined_country_data = merge(combined_country_data, livestock, all=T)

# Cropland area  (000s HECTARES)
cropland = cropland[c("FAO.Country", "Value")]
cropland$FAOname = ifelse(cropland$FAO.Country=="",NA,as.character(cropland$FAO.Country))
cropland = cropland[complete.cases(cropland$FAOname),]
cropland = merge(cropland, country_lookups[c("FAOname", "worldshapename", "ISO3")])
cropland$Country = cropland$worldshapename
cropland$CropYear = 2018
cropland$`Cropland Area` = cropland$Value
cropland = cropland[,(ncol(cropland)-3):ncol(cropland)]
combined_country_data = merge(combined_country_data, cropland, all=T)

# Seagrass area (HECTARES - note, protection is averaged over 10 years)
seagrass = seagrass[c("ISO3", "restorable", "protectable")]
seagrass = seagrass[complete.cases(seagrass$ISO3),]
seagrass = merge(seagrass, country_lookups[c("worldshapename", "ISO3")])
seagrass = seagrass %>% relocate(ISO3, .after = last_col()) # Move ISO3 column to the end
seagrass$Country = seagrass$worldshapename
seagrass$SeagYear = 2020
seagrass$`Seagrass Restorable Area` = seagrass$restorable/1000 # Convert to 000s hectares
seagrass$`Seagrass Annual Protectable Area` = (seagrass$protectable/1000)/10 # Convert to 1000 hectares and make annual
seagrass = seagrass[,(ncol(seagrass)-4):ncol(seagrass)]
combined_country_data = merge(combined_country_data, seagrass, all=T)

# Salt marsh area (HECTARES - note, protection is averaged over 10 years)
saltmarsh = saltmarsh[c("CIAworldfactbook", "restorable", "protectable")]
saltmarsh = saltmarsh[complete.cases(saltmarsh$CIAworldfactbook),]
saltmarsh = saltmarsh[complete.cases(saltmarsh$restorable),] # Remove non-salt marsh countries
saltmarsh = merge(saltmarsh, country_lookups[c("CIAworldfactbook", "worldshapename", "ISO3")])
saltmarsh$Country = saltmarsh$worldshapename
saltmarsh$SaltYear = 2020
saltmarsh$`Saltmarsh Restorable Area` = saltmarsh$restorable/1000 # Convert to 000s hectares
saltmarsh$`Saltmarsh Annual Protectable Area` = (saltmarsh$protectable/1000)/10 # Convert to 1000 hectares and make annual
saltmarsh = saltmarsh[,(ncol(saltmarsh)-4):ncol(saltmarsh)]
combined_country_data = merge(combined_country_data, saltmarsh, all=T)

# Plantation area (000s HECTARES)
plantation = plantation[c("FAOshort", "Most.recent.year", "Combined")]
plantation$FAOshort = ifelse(plantation$FAOshort=="", NA, as.character(plantation$FAOshort))
plantation = plantation[complete.cases(plantation$FAOshort),]
plantation = merge(plantation, country_lookups[c("FAOshort", "worldshapename", "ISO3")])
plantation$Country = plantation$worldshapename
plantation$PlantationYear = plantation$Most.recent.year
plantation$`Plantation Area` = plantation$Combined
plantation = plantation[,(ncol(plantation)-3):ncol(plantation)]
combined_country_data = merge(combined_country_data, plantation, all=T)

# Natural forest area (000s HECTARES)
natfor = natfor[c("FAOshort", "Most.recent.year", "Combined")]
natfor$FAOshort = ifelse(natfor$FAOshort=="", NA, as.character(natfor$FAOshort))
natfor = natfor[complete.cases(natfor$FAOshort),]
natfor = merge(natfor, country_lookups[c("FAOshort", "worldshapename", "ISO3")])
natfor$Country = natfor$worldshapename
natfor$NatForYear = natfor$Most.recent.year
natfor$`Natural Forest Area` = natfor$Combined
natfor = natfor[,(ncol(natfor)-3):ncol(natfor)]
combined_country_data = merge(combined_country_data, natfor, all=T)

# N fertilizer use - TONNES N 
fertilizer = fertilizer[c("FAO.Country", "Value")]
fertilizer$FAOname = ifelse(fertilizer$FAO.Country=="",NA,as.character(fertilizer$FAO.Country))
fertilizer = fertilizer[complete.cases(fertilizer$FAOname),]
fertilizer = merge(fertilizer, country_lookups[c("FAOname", "worldshapename", "ISO3")])
fertilizer$Country = fertilizer$worldshapename
fertilizer$FertYear = 2018
fertilizer$`Fertilizer Added` = fertilizer$Value/1000 # Convert to 000s tonnes
fertilizer = fertilizer[,(ncol(fertilizer)-3):ncol(fertilizer)]
combined_country_data = merge(combined_country_data, fertilizer, all=T)

# Biochar feedstock data prep (assumes CSV is loaded and groups [cereals, rice, sugarcane, wood residues] are already subset into separate dataframes)
# Cereals (TONNES)
cereals = cereals %>% group_by(FAO.Country) %>%
  summarise(Sum = sum(Value, na.rm=T))
cereals = cereals[c("FAO.Country", "Sum")]
cereals$FAOname = ifelse(cereals$FAO.Country=="",NA,as.character(cereals$FAO.Country))
cereals = cereals[complete.cases(cereals$FAOname),]
cereals = merge(cereals, country_lookups[c("FAOname", "worldshapename", "ISO3")])
cereals$Country = cereals$worldshapename
cereals$CerealYear = 2018
cereals$`Cereal Production` = as.numeric(cereals$Sum)/1000 # Convert to 000s tonnes
cereals = cereals[,(ncol(cereals)-3):ncol(cereals)]
combined_country_data = merge(combined_country_data, cereals, all=T)

# Rice - TONNES
rice = rice[c("FAO.Country", "Value")]
rice$FAOname = ifelse(rice$FAO.Country=="",NA,as.character(rice$FAO.Country))
rice = rice[complete.cases(rice$FAOname),]
rice = merge(rice, country_lookups[c("FAOname", "worldshapename", "ISO3")])
rice$Country = rice$worldshapename
rice$RiceYear = 2018
rice$`Rice Production` = as.numeric(rice$Value)/1000 # Convert to 000s tonnes
rice = rice[,(ncol(rice)-3):ncol(rice)]
combined_country_data = merge(combined_country_data, rice, all=T)

# Sugarcane - TONNES
sugarcane = sugarcane[c("FAO.Country", "Value")]
sugarcane$FAOname = ifelse(sugarcane$FAO.Country=="",NA,as.character(sugarcane$FAO.Country))
sugarcane = sugarcane[complete.cases(sugarcane$FAOname),]
sugarcane = merge(sugarcane, country_lookups[c("FAOname", "worldshapename", "ISO3")])
sugarcane$Country = sugarcane$worldshapename
sugarcane$SugarcaneYear = 2018
sugarcane$`Sugarcane Production` = as.numeric(sugarcane$Value)/1000 # Convert to 000s tonnes
sugarcane = sugarcane[,(ncol(sugarcane)-3):ncol(sugarcane)]
combined_country_data = merge(combined_country_data, sugarcane, all=T)

# Wood residues - m3
wood_residues = wood_residues[c("FAO.Country", "Value")]
wood_residues$FAOname = ifelse(wood_residues$FAO.Country=="",NA,as.character(wood_residues$FAO.Country))
wood_residues = wood_residues[complete.cases(wood_residues$FAOname),]
wood_residues = merge(wood_residues, country_lookups[c("FAOname", "worldshapename", "ISO3")])
wood_residues$Country = wood_residues$worldshapename
wood_residues$WoodResYear = 2018
wood_residues$`Wood Residues` = as.numeric(wood_residues$Value)/1000 # Convert to 000s tonnes
wood_residues = wood_residues[,(ncol(wood_residues)-3):ncol(wood_residues)]
combined_country_data = merge(combined_country_data, wood_residues, all=T)

### GRASSLAND LOSS (000s HECTARES)
# Linear regression used to calculate rate of loss for each country based on last 38 years (since 1980)
graz = graz[c("Area.Code", "Area", "Year", "Value")]
graz$FAOname = ifelse(graz$Area=="",NA,as.character(graz$Area))
graz = graz[complete.cases(graz$FAOname),]
graz = merge(graz, country_lookups[c("FAOname", "worldshapename", "ISO3")], all=T)
graz$Country = graz$worldshapename
graz = graz[complete.cases(graz$Value),]

graz_loss = data.frame()
for(i in levels(droplevels(as.factor(graz$Area)))) {
  tmp = graz[graz$Area == i,]
  if(max(tmp$Year)>1985) {
    tmp = tmp[tmp$Year>1979,]
    regres = lm(Value~Year, data=tmp)
    slope = summary(regres)[]$coefficients[[2,1]]
    tmp2 = data.frame(ISO3 = head(tmp$ISO3,1), Country = head(tmp$Country,1), base_year = min(tmp$Year), recent_year = max(tmp$Year), regression = slope, grass_loss = ifelse(slope>0,0,slope*-1))
    tmp2$grass_loss_time = tmp2$recent_year - tmp2$base_year
    graz_loss = rbind(graz_loss, tmp2)
  }
}

graz_loss = graz_loss[complete.cases(graz_loss$Country),] # Remove old (non-existent countries)
combined_country_data = merge(combined_country_data, graz_loss[c("ISO3", "grass_loss_time", "grass_loss")], all=T)

# Limit dataset to essential data only - not the best way but could subset based on columns of interest...
country_essential = combined_country_data[,c("ISO3",
                                             "Country",
                                             "Land.Area",
                                             "Population",
                                             "Coastline.Length",
                                             "MAT",
                                             "MAP",
                                             "Grazing Area",
                                             "Livestock Head",
                                             "Cropland Area",
                                             "Seagrass Restorable Area",
                                             "Seagrass Annual Protectable Area",
                                             "Saltmarsh Restorable Area",
                                             "Saltmarsh Annual Protectable Area",
                                             "Plantation Area",
                                             "Natural Forest Area",
                                             "Fertilizer Added",
                                             "Cereal Production",
                                             "Rice Production",
                                             "Sugarcane Production",
                                             "Wood Residues",
                                             "grass_loss")]

country_essential = country_essential[complete.cases(country_essential$ISO3),] # Remove when there is no ISO3 code (assumed no real existing country)
names(country_essential)[names(country_essential) == 'grass_loss'] <- 'Grass Loss' # Rename for clarity and consistency

### If needed - clear environment
CleanEnvir()

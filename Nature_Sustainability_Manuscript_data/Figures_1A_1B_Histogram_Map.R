
### Download RestoreID data extraction sheet from Google sheets to run this script
### Download UN SDG World regions to map the data - available https://ourworldindata.org/grapher/world-regions-sdg-united-nations 

library(cowplot)
library(dplyr)
library(tidyr)
library(forcats)
library(sf)
library(rvest)
library(stringr)
library(scales)
library(viridis)
library(ggplot2)
library(scatterpie)
library(ggnewscale)
library(ggthemes)
library(patchwork)
library(ggspatial)

###################################################################################################################################################

####################################
# READ MASTERFILE EXTRACTION SHEET #
####################################

data_masterfile <- read.csv("C:/Users/Adam/OneDrive - University of Stirling/RestoreID_Project_2024_2025/Excel_documents/R_Script_datafiles/RestoreID data extraction sheet - data extraction-sheet.csv")

#######################
# READ in UN SDG DATA #
#######################

sdg_region_df <- read.csv("C:/Users/Adam/OneDrive - University of Stirling/RestoreID_Project_2024_2025/Excel_documents/R_Script_datafiles/world-regions-sdg-united-nations.csv")

####################################
# READ in Need For Research Coords #
####################################

nfr_coords <- read.csv("C:/Users/Adam/OneDrive - University of Stirling/RestoreID_Project_2024_2025/Excel_documents/R_Script_datafiles/NFR_xy.csv")

###################################################################################################################################################

###########################################
# EDIT MASTERFILE INTO MAPPING STUDIES DF #
###########################################

### Keep only necessary columns
masterfile_subset <- data_masterfile[, c(4, 5, 8:10, 12:18, 21)]

### Replace NAs with Not Applicable - so to match with other drop down options
masterfile_subset[is.na(masterfile_subset)] <- "Not Applicable"

### Convert columns to factors for future plotting
masterfile_subset[, 7:13] <- lapply(masterfile_subset[, 7:13], as.factor)

### Check for any mistakes in the factor levels
for (i in 7:13) {
  cat("Column", i, "levels:\n")
  print(levels(masterfile_subset[[i]]))
  cat("\n")}

### Some repeated factor levels in general diseases and vectors - replace these
masterfile_subset <- masterfile_subset %>% mutate(
  General.disease = fct_recode(General.disease,
    "Hantavirus" = "Hantavirus ", "Multiple" = "Multiple ", "Multiple" = "Mutliple",
    "West Nile virus" = "West Nile virus ", "Yellow fever" = "Yellow fever "),
  General.Vectors = fct_recode(General.Vectors,
    "Bats" = "Bats ", "Rodents" = "Rodents "))
  
### Include study number to df
masterfile_subset <- masterfile_subset %>%
  mutate(Study_number = seq_len(n())) %>%
  select(Study_number, everything())

### Introduce duplicate rows for studies that have multiple countries
mapping_df <- masterfile_subset %>%
  separate_rows(Country, sep = ", ") 

### Convert Country column to factors for future plotting
mapping_df[, 5] <- lapply(mapping_df[, 5], as.factor)

### Check for any mistakes in the factor levels of countries
for (i in 5) {
  cat("Column", i, "levels:\n")
  print(levels(mapping_df[[i]]))
  cat("\n")}

### Some repeated factor levels - replace these
mapping_df <- mapping_df %>% mutate(
  Country = fct_recode(Country,
    "Brazil" = "Brazil ","India" = "India ","Tanzania" = "Tanzania "))

### Add column for UN SDG Region
anti_join(sdg_region_df, mapping_df, by = c('Entity' = 'Country'))

sdg_region_df <- sdg_region_df %>%  mutate(Entity = recode(Entity, `United States` = 'USA'
                                             , `United Kingdom` = 'UK'
                                             , `Democratic Republic of Congo` = 'Democratic Republic of the Congo'
                                             , `Cote d'Ivoire` = 'Ivory Coast'
                                             , `Czechia` = 'Czech Republic'
                                             , `Congo` = 'Republic of Congo'
                                             , `Eswatini` = 'Swaziland'
                                             , `Hong Kong` = 'China'))

mapping_df <- mapping_df %>%
  left_join(sdg_region_df, by = c("Country" = "Entity")) %>%
  rename(UN_SDG_Regions = Sustainable.Development.Goals..SDG..Regions)

###################################################################################################################################################

###########
# GET MAP #
###########

world_map <- map_data('world')

##################################################################
# Join map and UN SDG Regions together - check for any anomalies #
##################################################################

anti_join(sdg_region_df, world_map, by = c('Entity' = 'region'))

### Ignore all other mismatches - we do not have studies form these places
map_sdg <- left_join(world_map, sdg_region_df, by = c('region' = 'Entity')) 

### Rename SDG column
map_sdg <- map_sdg %>%
  rename(SDG_regions = Sustainable.Development.Goals..SDG..Regions)

### Remove countries (mainly Antarctica) without SDG code 
map_sdg <- map_sdg %>%
  filter(!is.na(SDG_regions))

#############################################################
# GET AVERAGE COORDS FOR EACH SDG REGION TO PLOT PIE CHARTS #
#############################################################

av_coords <- map_sdg %>%
  group_by(SDG_regions) %>%
  summarize(
    long = mean(long, na.rm = TRUE),
    lat = mean(lat, na.rm = TRUE))

###################################################################################################################################################

#############################################
### MAP 1 - NUMBER OF STUDIES PER COUNTRY ###
#############################################

######################################################
# Filter data to count number of studies per country #
######################################################

count_df <- mapping_df %>%
  filter(Country != "Not Applicable" & Country != "Global")

count_df <- count_df %>%
  group_by(Country) %>%
  summarise(Study_Count = n())

count_df <- count_df %>%
  mutate(Count_categories = case_when(
    Study_Count >= 1 & Study_Count <= 5 ~ "1-5",
    Study_Count >= 6 & Study_Count <= 10 ~ "6-10",
    Study_Count >= 11 & Study_Count <= 15 ~ "11-15",
    Study_Count >= 16 & Study_Count <= 20 ~ "16-20",
    Study_Count >= 21 & Study_Count <= 25 ~ "21-25",
    Study_Count >= 26 & Study_Count <= 30 ~ "26+", 
    TRUE ~ "0"))

count_df$Count_categories <- factor(count_df$Count_categories,
                                    levels = c("0", "1-5", "6-10", "11-15", "16-20", "21-25", "26+"))

# Calculate proportion of studies per country
total_studies <- sum(count_df$Study_Count)
count_df <- count_df %>%
  mutate(Percentage = (Study_Count / 138)*100)

### Create a proportion grouping column
# Add the new group column based on the specified percentage ranges
count_df <- count_df %>%
  mutate(Percentage_Group = case_when(
    Percentage > 0 & Percentage <= 0.8 ~ "0 - 0.8",
    Percentage > 0.8 & Percentage <= 2.1 ~ "0.8 - 2.1",
    Percentage > 2.1 & Percentage <= 5.1 ~ "2.1 - 5.1",
    Percentage > 5.1 & Percentage <= 7.5 ~ "5.1 - 7.5",
    Percentage > 7.5 & Percentage <= 20.5 ~ "7.5 - 20.5",
    TRUE ~ "Other"  # To handle any values that might fall outside the defined ranges
  ))

############################################################
# Join map and proportions - check for any anomalies #
############################################################

anti_join(count_df, world_map, by = c('Country' = 'region'))

count_map <- left_join(world_map, count_df, by = c('region' = 'Country')) 

### Remove Antarctica for plotting
count_map <- count_map %>%
  filter(region != "Antarctica")

############
# PLOT HEATMAP (PERCENTAGE) #
############

map1_plot <- ggplot(count_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Percentage_Group), color = "black") +
  scale_fill_manual(
    values = c("#BABADD", "#9A8BC9", "#7A68B6", "#5A46A3", "#450082"), 
    na.value = "lightgrey",
    na.translate = TRUE,
    labels = c("0 - 0.8", "0.8 - 2.1", "2.1 - 5.1", "5.1 - 7.5", "7.5 - 20.5", "No data")) +  
  guides(fill = guide_legend(reverse = F, ncol = 2)) + 
  labs(fill = 'Proportion of studies\nper country (%), N = 138',
       x = NULL,
       y = NULL) +
  theme(
    text = element_text(family = 'serif', color = '#EEEEEE'),
    axis.ticks = element_blank(), 
    axis.text = element_blank(), 
    panel.grid = element_blank(),
    panel.background = element_rect(fill = '#474747'), 
    plot.background = element_rect(fill = '#474747'), 
    legend.position = c(.15, .30), 
    legend.background = element_blank(), 
    legend.key = element_blank(), 
    legend.text = element_text(size = 40),
    legend.title = element_text(size = 40, margin = margin(b=1, unit = "cm")), 
    legend.key.height = unit(1.5, 'cm'),
    legend.key.width = unit(1.5, 'cm'),
    legend.key.spacing.y = unit(20, "pt"),
    legend.key.spacing.x = unit(20, "pt")
  ) +
  annotation_north_arrow(location = "tr", 
                         which_north = "true", 
                         style = north_arrow_fancy_orienteering(fill = c("black", "white")),
                         height = unit(4, "cm"),
                         width = unit(4, "cm"),)



ggsave("Figure_2_studies_map.png", plot = map1_plot, width = 35, height = 18, dpi = 400)

###################################################################################################################################################

#############################################################
### PLOT HISTOGRAM FOR THE NUMBER OF STUDIES ACROSS YEARS ###
#############################################################

### Filter studies to either be restoration or degradation study types

histogram_df <- data_masterfile %>%
  mutate(`Type of study` = if_else(!is.na(Evidence.for.Landscape.restoration.on.disease.outbreaks), "1", "0"))

histogram_df <- histogram_df[, c(4, 5, 43)]

histogram_df <- histogram_df %>%
  mutate(`Rest or Degra` = if_else(`Type of study` == "1", "Restoration", "Degradation"))

histogram_df$`Type of study` <- as.factor(histogram_df$`Type of study`)
histogram_df$`Rest or Degra` <- as.factor(histogram_df$`Rest or Degra`)

# Define the range of years that you want in the plot
all_years <- seq(2000, 2024)

# Summarize the data to count the number of studies per Year and Rest/Degradation
histogram_df_complete <- histogram_df %>%
  group_by(Year, `Rest or Degra`) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  complete(Year = all_years, `Rest or Degra`, fill = list(n = 0)) # Ensure all years and Rest/Degradation combos exist

# Plot the histogram
hist_plot <- ggplot(histogram_df_complete, aes(x = as.factor(Year), y = n, fill = `Rest or Degra`)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), alpha = 0.7, width = 0.7) +
  scale_fill_manual(values = c("#9A8BC9", "#450082")) +
  labs(fill = "Theme of study", x = "Year of publication", y = "Number of studies") +
  scale_x_discrete(breaks = seq(2000, 2024, by = 2)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 21), breaks = seq(0, 20, by = 2)) +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 50, vjust = -1),
    axis.text.x = element_text(size = 50),
    axis.title.y = element_text(size = 50, vjust = +2),
    axis.text.y = element_text(size = 50),
    legend.title = element_blank(),
    legend.text = element_text(size = 50),
    legend.position = c(0.1, 0.9),
    legend.key.size = unit(2, 'lines'),
    legend.key.spacing.y = unit(20, "pt"),
    text = element_text(family = "serif"),
    plot.margin = margin(l = 25, b = 25, r = 25, t = 25)
  )

print(hist_plot)

ggsave("Figure_1_Hist.png", plot = hist_plot, width = 30, height = 15, dpi = 300)


###################################################################################################################################################















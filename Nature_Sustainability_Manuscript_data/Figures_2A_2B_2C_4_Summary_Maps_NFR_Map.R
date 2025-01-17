
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

data_masterfile <- read.csv("C:/Users/adf2/OneDrive - University of Stirling/RestoreID_Project_2024_2025/Excel_documents/R_Script_datafiles/RestoreID data extraction sheet - data extraction-sheet.csv")

#######################
# READ in UN SDG DATA #
#######################

sdg_region_df <- read.csv("C:/Users/adf2/OneDrive - University of Stirling/RestoreID_Project_2024_2025/Excel_documents/R_Script_datafiles/world-regions-sdg-united-nations.csv")

####################################
# READ in Need For Research Coords #
####################################

nfr_coords <- read.csv("C:/Users/adf2/OneDrive - University of Stirling/RestoreID_Project_2024_2025/Excel_documents/R_Script_datafiles/NFR_xy.csv")

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
anti_join(sdg_region_df, mapping_df, by = c('Country' = 'Country'))

mapping_df <- mapping_df %>%
  left_join(sdg_region_df, by = c("Country" = "Country")) %>%
  rename(UN_SDG_Regions = Sub.region.Name)

###########
# GET MAP #
###########

world_map <- map_data('world')

##################################################################
# Join map and UN SDG Regions together - check for any anomalies #
##################################################################

anti_join(sdg_region_df, world_map, by = c('Country' = 'region'))

### Ignore all other mismatches - we do not have studies form these places
map_sdg <- left_join(world_map, sdg_region_df, by = c('region' = 'Country')) 

### Rename SDG column
map_sdg <- map_sdg %>%
  rename(SDG_regions = Sub.region.Name)

### Remove countries (mainly Antarctica) without SDG code 
map_sdg <- map_sdg %>%
  filter(!is.na(SDG_regions))

#############################################################
# GET AVERAGE COORDS FOR EACH SDG REGION TO PLOT PIE CHARTS #
#############################################################

### Remove Cocos Islands and Christmas Island

map_sdg <- map_sdg[!map_sdg$region %in% c("Cocos Islands", "Christmas Island"), ]

av_coords <- map_sdg %>%
  group_by(SDG_regions) %>%
  summarize(
    long = mean(long, na.rm = TRUE),
    lat = mean(lat, na.rm = TRUE))

av_coords$long[av_coords$SDG_regions == "Oceania"] <- 160.0
av_coords$lat[av_coords$SDG_regions == "Southern Africa"] <- -35
av_coords$long[av_coords$SDG_regions == "Western Africa"] <- -15
av_coords$long[av_coords$SDG_regions == "Eastern Africa"] <- 45
av_coords$long[av_coords$SDG_regions == "Europe"] <- 35
av_coords$lat[av_coords$SDG_regions == "Northern Africa"] <- 30

###################################################################################################################################################

####################################################
### MAP 1 - RESEARCH THEME ACROSS UN SDG REGIONS ###
####################################################

###################################################################
# Remove Global studies from new Mapping df (NA in World Regions) #
###################################################################

themes_df <- mapping_df %>%
  filter(!is.na(UN_SDG_Regions))

######################################
# GROUP DATA TOGETHER AND PIVOTWIDER #
######################################

###  Step 1: Get unique values for world regions and research types
world_regions <- unique(themes_df$UN_SDG_Regions)
research_themes <- unique(themes_df$Research.type)

theme_combinations <- expand.grid(
  UN_SDG_Regions = world_regions,
  Research.type = research_themes)

### Step 2: Group and summarize the data by counting unique studies per world region and research type
study_counts <- themes_df %>%
  group_by(UN_SDG_Regions, Research.type) %>%
  summarize(number_of_studies = n_distinct(Study_number), .groups = 'drop')

### Step 3: Perform a left join to include all combinations, filling in missing values with 0
complete_data <- theme_combinations %>%
  left_join(study_counts, by = c("UN_SDG_Regions", "Research.type")) %>%
  replace_na(list(number_of_studies = 0))

### Step 4: Pivot the dataframe
theme_result <- complete_data %>%
  pivot_wider(names_from = Research.type, values_from = number_of_studies)

### Step 5: Add a total column
theme_result <- theme_result %>%
  rowwise() %>%
  mutate(Total = sum(c_across(where(is.numeric)), na.rm = TRUE)) %>%
  ungroup()

### Step 6: Calculate proportions for each research type
theme_result_with_proportions <- theme_result %>%
  rowwise() %>%
  mutate(across(where(is.numeric), ~ . / Total * 100, .names = "prop_{col}")) %>%
  ungroup()

### Step 7: Reorder columns to place proportions next to their respective counts
base_columns <- names(theme_result)[-1]

### Create an interleaved order for the columns
ordered_columns <- c("UN_SDG_Regions")
for (col in base_columns) {
  ordered_columns <- c(ordered_columns, col, paste0("prop_", col))}
ordered_columns <- ordered_columns[ordered_columns != "prop_Total"]

### Reorder the columns
theme_result_with_proportions <- theme_result_with_proportions %>%
  select(all_of(ordered_columns))

themes_df <- merge(x = theme_result, y = av_coords, by.x = "UN_SDG_Regions", by.y = "SDG_regions")

##########################################################
# PLOT RESEARCH THEME PIECHARTS ON UN SDG REGION BASEMAP #
##########################################################

map1_plot <- ggplot(map_sdg, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = SDG_regions), color = "black") +
  scale_fill_manual(values = c("#FF9FBA", "#8E44AD", "#F1D27A", "#A24D8E", "#4A90E2", 
                               "#E6A87E", "#F5C27D", "#D95B66", "#4B9CD3", "#2A669E", 
                               "#E4736D", "#F6DB5F", "#4BA2C8", "#E8D03D", "#A05D9D")) +
  guides(fill = "none") +
  ggnewscale::new_scale_fill() +
  geom_scatterpie(aes(x=long, y=lat, r = 8),
                 data = themes_df, cols = colnames(themes_df[, c(2:6)]), color = 'white') +
  scale_fill_manual(values = c("#4CAF50", "#8BC34A", "#A5D6A7", "#FFA726", "#FF8C00")) +
  labs(fill = 'Research themes', x = NULL ,y = NULL) +
  theme(text = element_text(family = 'serif', color = '#EEEEEE')
        ,axis.ticks = element_blank()
        ,axis.text = element_blank()
        ,panel.grid = element_blank()
        ,panel.background = element_rect(fill = '#474747')
        ,plot.background = element_rect(fill = '#474747')
        ,legend.position = c(.15,.30)
        ,legend.background = element_blank()
        ,legend.key = element_blank()
        ,legend.text = element_text(size=40)
        ,legend.title = element_text(size=40, margin = margin(b=1, unit = "cm"))
        ,legend.key.height= unit(1.5, 'cm')
        ,legend.key.width= unit(1.5, 'cm')
        ,legend.key.spacing.y = unit(20, "pt"))+
  annotation_north_arrow(location = "tr", 
                         which_north = "true", 
                         style = north_arrow_fancy_orienteering(fill = c("black", "white")),
                         height = unit(4, "cm"),
                         width = unit(4, "cm"),)

ggsave("Figure_2a_themes_map.png", plot = map1_plot, width = 35, height = 15, dpi = 400)

###################################################################################################################################################

##################################################
### MAP 2 - VECTOR TYPES ACROSS UN SDG REGIONS ###
##################################################

##############################################
# Remove NAs in General vectors column in df #
##############################################

vectors_df <- mapping_df %>%
  filter(UN_SDG_Regions != "Not Applicable")

vectors_df <- vectors_df %>%
  filter(General.Vectors != "Not Applicable")

######################################
# GROUP DATA TOGETHER AND PIVOTWIDER #
######################################

### Step 1: Get unique values for world regions and research types
world_regions <- unique(vectors_df$UN_SDG_Regions)
vector_types <- unique(vectors_df$General.Vectors)

### Create a dataframe with all possible combinations
vector_combinations <- expand.grid(
  UN_SDG_Regions = world_regions,
  General.Vectors = vector_types)

### Step 2: Group and summarize the data by counting unique studies per world region and research type
study_counts <- vectors_df %>%
  group_by(UN_SDG_Regions, General.Vectors) %>%
  summarize(number_of_studies = n_distinct(Study_number), .groups = 'drop')

### Step 3: Perform a left join to include all combinations, filling in missing values with 0
complete_data <- vector_combinations %>%
  left_join(study_counts, by = c("UN_SDG_Regions", "General.Vectors")) %>%
  replace_na(list(number_of_studies = 0))

### Step 4: Pivot the dataframe
vector_result <- complete_data %>%
  pivot_wider(names_from = General.Vectors, values_from = number_of_studies)

### Step 5: Add a total column
vector_result <- vector_result %>%
  rowwise() %>%
  mutate(Total = sum(c_across(where(is.numeric)), na.rm = TRUE)) %>%
  ungroup()

### Step 6: Calculate proportions for each research type
vector_result_with_proportions <- vector_result %>%
  rowwise() %>%
  mutate(across(where(is.numeric), ~ . / Total * 100, .names = "prop_{col}")) %>%
  ungroup()

### Step 7: Reorder columns to place proportions next to their respective counts
base_columns <- names(vector_result)[-1] 

### Create an interleaved order for the columns
ordered_columns <- c("UN_SDG_Regions")
for (col in base_columns) {
  ordered_columns <- c(ordered_columns, col, paste0("prop_", col))}
ordered_columns <- ordered_columns[ordered_columns != "prop_Total"] 

### Reorder the columns
vector_result_with_proportions <- vector_result_with_proportions %>%
  select(all_of(ordered_columns))

vectors_df <- merge(x = vector_result, y = av_coords, by.x = "UN_SDG_Regions", by.y = "SDG_regions")

### Combine vectors
new_vectors_df <- vectors_df[, !(names(vectors_df) %in% c("Deer", "Primates", "Small mammals", "Sand fly", "Tsetse fly", "Triatomine bug"))]
new_vectors_df$'Other mammals' <- vectors_df$Deer + vectors_df$Primates + vectors_df$`Small mammals`
new_vectors_df$'Other arthropods' <- vectors_df$`Sand fly` + vectors_df$`Tsetse fly` + vectors_df$`Triatomine bug`

new_vectors_df <- new_vectors_df %>%
  select(1:7, 11, 12, 8:10, everything())


#######################################################
# PLOT VECTOR TYPE PIECHARTS ON UN SDG REGION BASEMAP #
#######################################################

map2_plot <- ggplot(map_sdg, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = SDG_regions), color = "black") +
  scale_fill_manual(values = c("#FF9FBA", "#8E44AD", "#F1D27A", "#A24D8E", "#4A90E2", 
                               "#E6A87E", "#F5C27D", "#D95B66", "#4B9CD3", "#2A669E", 
                               "#E4736D", "#F6DB5F", "#4BA2C8", "#E8D03D", "#A05D9D")) +
  guides(fill = "none") +
  labs(fill = 'SDG Regions') +
  ggnewscale::new_scale_fill() +
  geom_scatterpie(aes(x=long, y=lat, r = 8), data = new_vectors_df, cols = colnames(new_vectors_df[, c(2:7,11,12)]), color = 'white') +
  scale_fill_manual(values = palette <- c("#4CAF50", "#8BC34A", "#A5D6A7", "#FFA726", "#FF8C00", 
                                          "#388E3C", "#FFB74D", "#800000")
  ) +
  guides(fill = guide_legend(ncol = 2)) +
  labs(fill = 'Targeted vector species', x = NULL, y = NULL) +
  theme(text = element_text(family = 'serif', color = '#EEEEEE')
        ,axis.ticks = element_blank()
        ,axis.text = element_blank()
        ,panel.grid = element_blank()
        ,panel.background = element_rect(fill = '#474747')
        ,plot.background = element_rect(fill = '#474747')
        ,legend.position = c(.15,.30)
        ,legend.background = element_blank()
        ,legend.key = element_blank()
        ,legend.text = element_text(size=40)
        ,legend.title = element_text(size=40, margin = margin(b=1, unit = "cm"))
        ,legend.key.height= unit(1, 'cm')
        ,legend.key.width= unit(1, 'cm')
        ,legend.key.spacing.y = unit(20, "pt")
        ,legend.key.spacing.x = unit(20, "pt"))+
  annotation_north_arrow(location = "tr", 
                         which_north = "true", 
                         style = north_arrow_fancy_orienteering(fill = c("black", "white")),
                         height = unit(4, "cm"),
                         width = unit(4, "cm"),)

ggsave("Figure_2b_vectors_map.png", plot = map2_plot, width = 35, height = 15, dpi = 400)

###################################################################################################################################################

#####################################################
### MAP 43- PATHOGEN GROUPS ACROSS UN SDG REGIONS ###
##################################################### 

##############################################
# Remove NAs in General vectors column in df #
##############################################

pathogen_df <- mapping_df %>%
  filter(UN_SDG_Regions != "Not Applicable")

pathogen_df <- pathogen_df %>%
  filter(Pathogen.group != "Not Applicable")

######################################
# GROUP DATA TOGETHER AND PIVOTWIDER #
######################################

### Step 1: Get unique values for world regions and research types
world_regions <- unique(pathogen_df$UN_SDG_Regions)
pathogen_types <- unique(pathogen_df$Pathogen.group)

### Create a dataframe with all possible combinations
pathogen_combinations <- expand.grid(
  UN_SDG_Regions = world_regions,
  Pathogen.group = pathogen_types)

### Step 2: Group and summarize the data by counting unique studies per world region and research type
study_counts <- pathogen_df %>%
  group_by(UN_SDG_Regions, Pathogen.group) %>%
  summarize(number_of_studies = n_distinct(Study_number), .groups = 'drop')

### Step 3: Perform a left join to include all combinations, filling in missing values with 0
complete_data <- pathogen_combinations %>%
  left_join(study_counts, by = c("UN_SDG_Regions", "Pathogen.group")) %>%
  replace_na(list(number_of_studies = 0))

### Step 4: Pivot the dataframe
pathogen_result <- complete_data %>%
  pivot_wider(names_from = Pathogen.group, values_from = number_of_studies)

### Step 5: Add a total column
pathogen_result <- pathogen_result %>%
  rowwise() %>%
  mutate(Total = sum(c_across(where(is.numeric)), na.rm = TRUE)) %>%
  ungroup()

### Step 6: Calculate proportions for each research type
pathogen_result_with_proportions <- pathogen_result %>%
  rowwise() %>%
  mutate(across(where(is.numeric), ~ . / Total * 100, .names = "prop_{col}")) %>%
  ungroup()

### Step 7: Reorder columns to place proportions next to their respective counts
base_columns <- names(pathogen_result)[-1]

# Create an interleaved order for the columns
ordered_columns <- c("UN_SDG_Regions")
for (col in base_columns) {
  ordered_columns <- c(ordered_columns, col, paste0("prop_", col))}
ordered_columns <- ordered_columns[ordered_columns != "prop_Total"]

# Reorder the columns
pathogen_result_with_proportions <- pathogen_result_with_proportions %>%
  select(all_of(ordered_columns))

pathogen_df <- merge(x = pathogen_result, y = av_coords, by.x = "UN_SDG_Regions", by.y = "SDG_regions")

### Rename columns - capitalise names
pathogen_df <- pathogen_df %>%
  rename(
    Bacteria = bacteria,
    Protozoan = protozoan,
    Multiple = multiple,
    Helminth = helminth)

########################################################
# PLOT DISEASE TYPE PIECHARTS ON UN SDG REGION BASEMAP #
########################################################

map3_plot <- ggplot(map_sdg, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = SDG_regions), color = "black") +
  scale_fill_manual(values = c("#FF9FBA", "#8E44AD", "#F1D27A", "#A24D8E", "#4A90E2", 
                               "#E6A87E", "#F5C27D", "#D95B66", "#4B9CD3", "#2A669E", 
                               "#E4736D", "#F6DB5F", "#4BA2C8", "#E8D03D", "#A05D9D")) +
  guides(fill = "none") +
  labs(fill = 'SDG Regions') +
  ggnewscale::new_scale_fill() +
  geom_scatterpie(aes(x=long, y=lat, r = 8), data = pathogen_df, cols = colnames(pathogen_df[, c(2:6)]), color = 'white') +
  scale_fill_manual(values = c("#4CAF50", "#8BC34A", "#A5D6A7", "#FFA726", "#FF8C00")) +
  guides(fill = guide_legend(ncol = 1)) +
  labs(fill = 'Pathogen groups', x = NULL, y = NULL) +
  theme(text = element_text(family = 'serif', color = '#EEEEEE')
        ,axis.ticks = element_blank()
        ,axis.text = element_blank()
        ,panel.grid = element_blank()
        ,panel.background = element_rect(fill = '#474747')
        ,plot.background = element_rect(fill = '#474747')
        ,legend.position = c(.12,.30)
        ,legend.background = element_blank()
        ,legend.key = element_blank()
        ,legend.text = element_text(size=40)
        ,legend.title = element_text(size=40, margin = margin(b=1, unit = "cm"))
        ,legend.key.height= unit(1, 'cm')
        ,legend.key.width= unit(1, 'cm')
        ,legend.key.spacing.y = unit(20, "pt"))+
  annotation_north_arrow(location = "tr", 
                         which_north = "true", 
                         style = north_arrow_fancy_orienteering(fill = c("black", "white")),
                         height = unit(4, "cm"),
                         width = unit(4, "cm"),)

ggsave("Figure_2c_pathogens_map.png", plot = map3_plot, width = 35, height = 15, dpi = 400)

###################################################################################################################################################

###################################################################################################################################################

#####################################
### MAP 4 - NEED FOR RESEARCH MAP ###
#####################################

map4_plot <- ggplot(map_sdg, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = SDG_regions), color = "black") +
  geom_point(data = nfr_coords, aes(x = x, y = y), color = "black", size = 8, stroke = 2, shape = 21, fill = "#77DD77", inherit.aes = FALSE) +
  scale_fill_manual(values = c("#FF9FBA", "#8E44AD", "#F1D27A", "#A24D8E", "#4A90E2", 
                               "#E6A87E", "#F5C27D", "#D95B66", "#4B9CD3", "#2A669E", 
                               "#E4736D", "#F6DB5F", "#4BA2C8", "#E8D03D", "#A05D9D")) +
  guides(fill = "none") +
  labs(fill = 'SDG Regions', x = NULL, y = NULL) +
  theme(text = element_text(family = 'serif', color = '#EEEEEE'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = '#474747'),
        plot.background = element_rect(fill = '#474747'))+
  annotation_north_arrow(location = "tr", 
                         which_north = "true", 
                         style = north_arrow_fancy_orienteering(fill = c("black", "white")),
                         height = unit(4, "cm"),
                         width = unit(4, "cm"),)

inset_map_africa <- ggplot(map_sdg, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = SDG_regions), color = "black") +
  geom_point(data = nfr_coords, aes(x = x, y = y), color = "black", size = 6, stroke = 1.2, shape = 21, fill = "#77DD77", inherit.aes = FALSE) +  # Smaller points for inset
  scale_fill_manual(values = c("#FF9FBA", "#8E44AD", "#F1D27A", "#A24D8E", "#4A90E2", 
                               "#E6A87E", "#F5C27D", "#D95B66", "#4B9CD3", "#2A669E", 
                               "#E4736D", "#F6DB5F", "#4BA2C8", "#E8D03D", "#A05D9D")) +
  coord_sf(xlim = c(-15, 14), ylim = c(-8, 16), expand = FALSE) +
  guides(fill = "none") +
  labs(fill = 'SDG Regions', x = NULL, y = NULL) +
  theme(text = element_text(family = 'serif', color = '#EEEEEE'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = '#474747'),
        plot.background = element_rect(fill = '#474747'))

inset_map_asia <- ggplot(map_sdg, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = SDG_regions), color = "black") +
  geom_point(data = nfr_coords, aes(x = x, y = y), color = "black", size = 6, stroke = 1.2, shape = 21, fill = "#77DD77", inherit.aes = FALSE) +  # Smaller points for inset
  scale_fill_manual(values = c("#FF9FBA", "#8E44AD", "#F1D27A", "#A24D8E", "#4A90E2", 
                               "#E6A87E", "#F5C27D", "#D95B66", "#4B9CD3", "#2A669E", 
                               "#E4736D", "#F6DB5F", "#4BA2C8", "#E8D03D", "#A05D9D")) +
  coord_sf(xlim = c(95, 125), ylim = c(-7, 20), expand = FALSE) +
  guides(fill = "none") +
  labs(fill = 'SDG Regions', x = NULL, y = NULL) +
  theme(text = element_text(family = 'serif', color = '#EEEEEE'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = '#474747'),
        plot.background = element_rect(fill = '#474747'))

# Combine main plot and insets using cowplot
final_plot <- ggdraw() +
  draw_plot(map4_plot) +  # Main plot
  draw_plot(inset_map_africa, x = 0.55, y = 0.04, width = 0.27, height = 0.27) +  # Inset Africa (adjust position)
  draw_plot(inset_map_asia, x = 0.79, y = 0.42, width = 0.27, height = 0.27)  # Inset Asia (adjust position)

# Display the final plot
final_plot

ggsave("Figure_4_NFR_map.png", plot = final_plot, width = 35, height = 15, dpi = 400)

###################################################################################################################################################








































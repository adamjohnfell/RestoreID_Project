
library(devtools)

install_github("jokergoo/ComplexHeatmap")

library(ComplexHeatmap)
library(circlize)
library(grid)

setwd("C:/Users/Adam/OneDrive - University of Stirling/RestoreID_Project_2024_2025/Excel_documents/R_Script_datafiles")

### This is the Matrix sent from Soushieta - has 0.00000001 in place of NAs - I just add this in the script below instead
#SJ_matrix <- read.csv("heatmap_matrix_SJ.csv", sep = ";")

### Read in dataframe
heatmap_df <- read.csv("heatmap_matrix.csv", sep=",")

### Add 0.00000001 in place of NAs
heatmap_df[is.na(heatmap_df)] <- 0.00000001

### Convert to matrix and set row names
data_matrix <- as.matrix(heatmap_df[, -1])
rownames(data_matrix) <- heatmap_df$Disease_transmission
colnames(data_matrix) <- colnames(heatmap_df)[-1]

### Define hierarchical structure for disease transmission types
disease_annotation <- data.frame(
  Disease_Group = c("Vector_borne", "Zoonoses", "Zoonoses","General",
                    "Vector_borne", "Vector_borne", "Zoonoses","General",
                    "Vector_borne", "Zoonoses", "Zoonoses",
                    "Vector_borne", "General", "Zoonoses"),
  Disease_Transmission = c("Athropod_borne", "Avian", "Bat_borne", "General",
                           "Mosquito_borne", "Multiple_VBD", "Multiple_Zoonoses","Neglected_diseases",
                           "Other_vector_borne", "Other_zoonoses", "Rodent_borne", 
                           "Tick_borne","Tuberculosis","Wildlife_diseases")
)

### Define hierarchical structure for landscape alteration
landscape_annotation <- data.frame(
  Landscape_Group = c("Restoration", "Degradation", "Degradation", "Climate_change", 
                      "Restoration", "Degradation", "Restoration", "Degradation",
                      "Degradation", "Degradation", "Restoration", "Restoration",
                      "Degradation", "Degradation", "Restoration", "Degradation"),
  landscape_type = c("Afforestation", "Anthropization", "Biodiversity_loss", "Climate",
                      "Conservation", "Deforestation", "Dilution_effect","Fragmentation",
                      "Habitat_degradation", "Landuse_alteration", "Reforestation", 
                      "Restoration_services", "Species_removal", "Urbanisation",
                      "Wetland_restoration", "Wildfire")
)
															
### Standardisation of matrix
standardized_matrix <- scale(data_matrix)

### Check number of columns and length of landscape annotation match
ncol(standardized_matrix) 
length(landscape_annotation$landscape_type) 

### Check number of rows and length of disease annotation match
nrow(data_matrix)
length(disease_annotation$Disease_Transmission)


### Disease Group Annotation
ha_disease <- rowAnnotation(
  Disease_Group = disease_annotation$Disease_Group,
  col = list(Disease_Group = c("Vector_borne" = "#D8BFD8", "Zoonoses" = "#9370DB", "General" = "#6A0DAD")),
  annotation_legend_param = list(Disease_Group = list(title = "Disease Group")),
  show_legend = FALSE, show_annotation_name = FALSE)

### Landscape Group Annotation
ha_landscape <- HeatmapAnnotation(
  Landscape_Group = landscape_annotation$Landscape_Group,
  col = list(Landscape_Group = c("Restoration" = "#638567", "Degradation" = "#A85F69", "Climate_change" = "#6684A1")),
  annotation_legend_param = list(Landscape_Group = list(title = "Landscape Group")),
  show_legend = FALSE, show_annotation_name = FALSE)

### Plot heat map
Heatmap(standardized_matrix, name = "mat", row_km = 6)

col_fun = colorRamp2(c(-2, 0, 2), c("lightcoral", "lightyellow", "lightblue"))
col_fun(seq(-3, 3))

### plot with "restoration colours"
Heatmap(standardized_matrix, name = "mat", col = col_fun, row_km = 6,  row_gap = unit(2, "mm"))

### Define the desired order for landscape changes and disease transmission types
landscape_order <- c("Degradation", "Restoration", "Climate_change")
disease_order <- c("Vector_borne", "Zoonoses", "General")

### Create factors with the specified levels
landscape_group_factor <- factor(landscape_annotation$Landscape_Group, levels = landscape_order)
disease_group_factor <- factor(disease_annotation$Disease_Group, levels = disease_order)

### Plot the heatmap with specified slice orders
ht <- Heatmap(
  standardized_matrix, 
  name = "Influence",
  top_annotation = ha_landscape,
  right_annotation = ha_disease,
  column_split = landscape_group_factor,
  row_split = disease_group_factor,
  cluster_row_slices = FALSE,  # Disable clustering for row slices
  cluster_column_slices = FALSE,  # Disable clustering for column slices
  clustering_distance_rows = "euclidean",
  clustering_distance_columns = "euclidean",
  clustering_method_rows = "complete",
  clustering_method_columns = "complete",
  heatmap_legend_param = list(title = NULL, legend_height = unit(8, "cm"),
                              labels_gp = gpar(fontsize = 30, fontfamily = "serif"), 
                              grid_width = unit(1.5, "cm"),
                              at = c(-2, 0, 2),
                              labels = c("Negative", "No Effect", "Positive")),
  show_row_dend = TRUE,
  show_column_dend = TRUE,
  row_gap = unit(3, "mm"),
  column_gap = unit(3, "mm"),
  col = col_fun,
  column_title_gp = grid::gpar(fontsize = 30, fontfamily = "serif"),
  column_names_gp = grid::gpar(fontsize = 28, rot = 45, fontfamily = "serif"),
  row_title_gp = grid::gpar(fontsize = 30, fontfamily = "serif"),
  row_names_gp = grid::gpar(fontsize = 30, fontfamily = "serif"),
  column_names_rot = 45,
  border = TRUE,
  rect_gp = gpar(col = "black", lwd = 1))



draw(ht, merge_legend = FALSE, column_dend_side = "top", heatmap_legend_side = "right",
     padding = unit(c(1.5, 2, 1.5, 2.5), "cm")) 











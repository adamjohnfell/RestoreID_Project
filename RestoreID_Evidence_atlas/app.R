

# Load necessary libraries
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(tidyr)
library(plotly)
library(shinycssloaders)
library(leaflet.extras)
library(shinythemes)

setwd("data")

scientific_lit_data <- read.csv("Evidence_map_centroids.csv")

case_reports_data <- read.csv("case_reports_evidence_map.csv")

policy_briefs <- read.csv("policy_brief_restoreid.csv")

nfr_df <- read.csv("NFR_xy.csv")

piechart_df <- read.csv("Shinyapp_piechart_dataframe.csv", check.names = FALSE)

country_shapes_geojson <- geojsonsf::geojson_sf("simplified_UN_region_shapes.geojson")

#########################################################################################################################################################
# Create a named list for the color palette
custom_colors <- list(
  "Central America" = "#FF9FBA", "Central Asia" = "#8E44AD", "Eastern Africa" = "#F1D27A", "Eastern Asia" = "#A24D8E", "Europe" = "#4A90E2", 
  "Middle Africa" = "#E6A87E", "Northern Africa" = "#F5C27D", "Northern America" = "#D95B66", "Southern Asia" = "#4B9CD3", "South-eastern Asia" = "#2A669E", 
  "South America" = "#E4736D", "Southern Africa" = "#F6DB5F", "Oceania" = "#4BA2C8", "Western Africa" = "#E8D03D", "Western Asia" = "#A05D9D")

# Prepare unique regions and color schemes
unique_regions <- unique(piechart_df$UN_SDG_Regions)
theme_colors <- c("#4CAF50", "#8BC34A", "#A5D6A7", "#FFA726", "#FF8C00")
pathogen_colors <- c("#4CAF50", "#8BC34A", "#A5D6A7", "#FFA726", "#FF8C00")
vector_colors <- c("#4CAF50", "#8BC34A", "#A5D6A7", "#FFA726", "#FF8C00", "#388E3C", "#FFB74D", "#800000")
vector_order <- c("Rodents", "Mosquito", "Ticks", "Birds", "Multiple vectors", "Bats", "Other mammals", "Other arthropods")
pathogen_order <- c("Virus", "Bacteria", "Protozoan", "Multiple pathogens", "Helminth")
theme_order <- c("Field study", "Descriptive", "Review", "Experimental", "Hypothesis/theoretical")

nfr_icon <- makeAwesomeIcon(
  icon = "exclamation-circle fa-2x",         
  iconColor = "red",   
  markerColor = "white",  
  library = "fa" 
)
#########################################################################################################################################################

ui <- fluidPage(
  theme = shinytheme("spacelab"),
  titlePanel("Interactive Evidence Map and Regional Data Summary"),
  # Add custom CSS to make tab titles bold
  tags$head(tags$style(HTML("
    .chart-container {
      display: flex;
      align-items: center; /* Aligns items vertically in the center */
      justify-content: center; /* Centers the items horizontally */
      position: relative; /* Set position to relative for z-index */
      width: 100%; /* Set to 100% to take full width */
    }
    .chart-pie {
      max-width: 800px; /* Set a max width for the pie chart */
      width: 100%; /* Set to 100% to take full width up to max-width */
    }
    .selectize-input, .select {
        position: relative;
      }
      .selectize-input::after, .select::after {
        content: '\\25BC';  /* Unicode for a downward arrow */
        position: absolute;
        right: 10px;
        top: 50%;
        transform: translateY(-50%);
        color: #555;
        pointer-events: none;
      }
      body {
                transform: scale(0.93);
                transform-origin: top middle;
                margin-top: -20px; /* Move content closer to the top of the screen */
            }
  "))),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      
      tabsetPanel(
        tabPanel("Search scientific literature", 
                 tags$br(),
                 textInput("search_title", "Search by Keywords in Article Title:", ""),
                 textInput("search_author", "Search by Author:", ""),
                 selectizeInput("year_filter", "Select Year:", choices = sort(unique(scientific_lit_data$Year.of.publication)), selected = NULL, multiple = TRUE, options = list(placeholder = "Select year")),
                 selectizeInput("scale_filter", "Select Scale of Study:", choices = sort(unique(scientific_lit_data$Scale.of.study)), selected = NULL, multiple = TRUE, options = list(placeholder = "Select scale")),
                 selectizeInput("research_type_filter", "Select Research Type:", choices = sort(unique(scientific_lit_data$Research.type)), selected = NULL, multiple = TRUE, options = list(placeholder = "Select research type")),
                 selectizeInput("focus_filter", "Select Primary Focus:", choices = sort(unique(scientific_lit_data$Primary.focus.areas)), selected = NULL, multiple = TRUE, options = list(placeholder = "Select primary focus")),
                 selectizeInput("disease_filter", "Select General Disease:", choices = sort(unique(scientific_lit_data$General.disease)), selected = NULL, multiple = TRUE, options = list(placeholder = "Select disease")),
                 selectizeInput("vector_filter", "Select General Vector:", choices = sort(unique(scientific_lit_data$General.vector)), selected = NULL, multiple = TRUE, options = list(placeholder = "Select vector")),
                 selectizeInput("type_filter", "Select Type of Study:", choices = sort(unique(scientific_lit_data$Type.of.study)), selected = NULL, multiple = TRUE, options = list(placeholder = "Select study type")),
                 
                 actionButton("reset_button", "Reset Filters"),
                 br(),
                 br(),
                 verbatimTextOutput("study_count"),
                 br(),
                 downloadButton("download_filtered_data", "Download Filtered Data"),
                 br(), 
                 br()
        ),  
        
        tabPanel("Regional data summary",
                 tags$br(),
                 selectInput("region_filter", "Select Region:", choices = unique(unique_regions), selected = NULL),
                 tags$h4(HTML("Proportion of the different Vectors,<br>Pathogens, and Themes"), style = "text-align: center;"),
                 br(),
                 div(class = "chart-container",
                     withSpinner(plotlyOutput("pie_chart", height = "900px", width = "100%"))  # Set width to 100%
                 ),
                 br(),
                 br()
        )
      ) 
    ),  
    
    mainPanel(
      width = 8,
      withSpinner(leafletOutput("mymap", width = "100%", height = "900px"))  # Set width to 100%
    ) 
  ) 
)


# Define server logic for the Shiny app
server <- function(input, output, session) {
  
  # Reactive expression to filter data based on user inputs
  filtered_data <- reactive({
    scientific_lit_data %>%
      filter(if (input$search_title != "") grepl(input$search_title, Title, ignore.case = TRUE) else TRUE,
             if (input$search_author != "") grepl(input$search_author, Authors, ignore.case = TRUE) else TRUE,
             if (!is.null(input$year_filter) && length(input$year_filter) > 0) Year.of.publication %in% input$year_filter else TRUE,
             if (!is.null(input$scale_filter) && length(input$scale_filter) > 0) Scale.of.study %in% input$scale_filter else TRUE,
             if (!is.null(input$research_type_filter) && length(input$research_type_filter) > 0) Research.type %in% input$research_type_filter else TRUE,
             if (!is.null(input$focus_filter) && length(input$focus_filter) > 0) Primary.focus.areas %in% input$focus_filter else TRUE,
             if (!is.null(input$disease_filter) && length(input$disease_filter) > 0) General.disease %in% input$disease_filter else TRUE,
             if (!is.null(input$vector_filter) && length(input$vector_filter) > 0) General.vector %in% input$vector_filter else TRUE,
             if (!is.null(input$type_filter) && length(input$type_filter) > 0) Type.of.study %in% input$type_filter else TRUE)
  })
  # Reset Filters button functionality
  observeEvent(input$reset_button, {
    # Reset all inputs
    updateTextInput(session, "search_title", value = "")
    updateTextInput(session, "search_author", value = "")
    updateSelectInput(session, "year_filter", selected = character(0))
    updateSelectInput(session, "scale_filter", selected = character(0))
    updateSelectInput(session, "research_type_filter", selected = character(0))
    updateSelectInput(session, "focus_filter", selected = character(0))
    updateSelectInput(session, "disease_filter", selected = character(0))
    updateSelectInput(session, "vector_filter", selected = character(0))
    updateSelectInput(session, "type_filter", selected = character(0))
  })
  # Display the number of matching studies
  output$study_count <- renderText({
    num_studies <- nrow(filtered_data())
    paste("Number of matching scientific studies:", num_studies)
  })
  # Download the filtered data as CSV
  output$download_filtered_data <- downloadHandler(
    filename = function() {
      paste("filtered_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  # Reactive expression for generating pie charts based on selected region
  output$pie_chart <- renderPlotly({
    region <- input$region_filter
    region_data <- piechart_df %>% filter(UN_SDG_Regions == region)
    # Check if the region data is not empty
    if (nrow(region_data) == 0) {
      return(NULL)  # Return NULL if no data is available for the selected region
    }
    # Extract data for pie charts
    vector_data <- as.numeric(region_data[2:9])
    pathogen_data <- as.numeric(region_data[11:15])
    theme_data <- as.numeric(region_data[17:21])
    # Arrange data to match order
    vector_data <- vector_data[match(vector_order, names(region_data)[2:9])]
    pathogen_data <- pathogen_data[match(pathogen_order, names(region_data)[11:15])]
    theme_data <- theme_data[match(theme_order, names(region_data)[17:21])]
    # Create the Plotly figure
    fig <- plot_ly() %>%
      add_pie(labels = vector_order, values = vector_data, name = "Vectors", domain = list(row = 0, column = 0), textinfo = 'label+percent', textposition = "inside", insidetextfont = list(color = '#FFFFFF'), 
              marker = list(colors = vector_colors, line = list(color = '#FFFFFF', width = 2))) %>%
      add_pie(labels = pathogen_order, values = pathogen_data, name = "Pathogens", domain = list(row = 1, column = 0), textinfo = 'label+percent', textposition = "inside", insidetextfont = list(color = '#FFFFFF'), 
              marker = list(colors = pathogen_colors, line = list(color = '#FFFFFF', width = 2))) %>%
      add_pie(labels = theme_order, values = theme_data, name = "Themes", domain = list(row = 2, column = 0), textinfo = 'label+percent', textposition = "inside", insidetextfont = list(color = '#FFFFFF'), 
              marker = list(colors = theme_colors, line = list(color = '#FFFFFF', width = 2))) %>%
      layout(margin = list(l = 0, r = 0, t = 10, b = 10), 
             showlegend = FALSE,
             autosize = TRUE,
             paper_bgcolor = 'rgba(0,0,0,0)',  # Remove paper background color
             plot_bgcolor = 'rgba(0,0,0,0)', showlegend = FALSE, grid = list(rows = 3, columns = 1))
    
    fig
  })
  # Render the leaflet map
  output$mymap <- renderLeaflet({
    # Get the filtered data
    data_to_plot <- filtered_data()
    # Check if there are no studies to display
    if (nrow(data_to_plot) == 0) {
      leaflet() %>%
        addProviderTiles(providers$Esri.WorldStreetMap) %>% 
        setView(lng = mean(scientific_lit_data$Longitude, na.rm = TRUE), 
                lat = mean(scientific_lit_data$Latitude, na.rm = TRUE), 
                zoom = 3) %>%
        addPopups(
          lng = mean(scientific_lit_data$Longitude, na.rm = TRUE), 
          lat = mean(scientific_lit_data$Latitude, na.rm = TRUE), 
          content = "No studies match the current filter criteria.",
          options = popupOptions(closeOnClick = TRUE)
        )
    } else {
      # Filter out 'Global' studies if they are not selected
      if (!("Global" %in% input$scale_filter)) {
        data_to_plot <- data_to_plot[data_to_plot$Scale.of.study != "Global", ]
      }
      # Create the popup content for each point with larger font and line breaks
      popup_content <- paste(
        "<div style='font-size: 14px; padding: 10px; border: 1px solid #ccc;'>",
        "<span style='font-size: 20px; font-weight: bold;'>",
        "<a href='https://doi.org/", data_to_plot$DOI, "' target='_blank'>", data_to_plot$Title, "</a>",
        "</span><br><br>",
        "<b>Authors:</b> ", data_to_plot$Authors, "<br><br>",
        "<b>Year:</b> ", data_to_plot$Year.of.publication, "<br><br>",
        "<b>Scale:</b> ", data_to_plot$Scale.of.study, "<br><br>",
        "<b>Research type:</b> ", data_to_plot$Research.type, "<br><br>",
        "<b>Primary focus:</b> ", data_to_plot$Primary.focus.areas, "<br><br>",
        "<b>General disease:</b> ", data_to_plot$General.disease, "<br><br>",
        "<b>General vector:</b> ", data_to_plot$General.vector, "<br><br>",
        "<b>Main findings:</b> ", data_to_plot$Main.findings, "<br>",
        "</div>",
        sep = ""
      )
      popup_content1 <- paste(
        "<div style='font-size: 14px; padding: 10px; border: 1px solid #ccc;'>", 
        "<span style='font-size: 20px; font-weight: bold;'>",
        "<a href='", case_reports_data$URL, "' target='_blank'>", case_reports_data$Title, "</a>",
        "</span><br><br>",
        "<b>Year:</b> ", case_reports_data$Year, "<br><br>",
        "<b>Region:</b> ", case_reports_data$Country, "<br><br>",
        "<b>Theme:</b> ", case_reports_data$theme, "<br><br>",
        "<b>Results:</b> ", case_reports_data$Results, "<br><br>",
        "<b>Lessons learnt:</b> ", case_reports_data$Lessons.learnt, "<br><br>",
        "</div>",
        sep = ""
      )
      popup_content2 <- paste(
        "<div style='font-size: 14px; padding: 10px; border: 1px solid #ccc;'>", 
        "<span style='font-size: 20px; font-weight: bold;'>",
        "<a href='", policy_briefs$URL, "' target='_blank'>", policy_briefs$Title, "</a>",
        "</span><br><br>",
        "<b>Year:</b> ", policy_briefs$Year, "<br><br>",
        "<b>Region:</b> ", policy_briefs$Country, "<br><br>",
        "<b>Theme:</b> ", policy_briefs$recommendations, "<br><br>",
        "</div>",
        sep = ""
      )
      # Create a leaflet map with Esri World Imagery 
      leaflet() %>%
        addProviderTiles(providers$Esri.WorldStreetMap) %>%
        setView(lng = mean(data_to_plot$Longitude, na.rm = TRUE), lat = mean(data_to_plot$Latitude, na.rm = TRUE), zoom = 2) %>%
        # Add regions with popups and hide them initially
        addPolygons(data = country_shapes_geojson, 
                    fillColor = ~colorFactor(palette = unlist(custom_colors), levels = names(custom_colors))(country_shapes_geojson$Sb_rg_N), 
                    color = "black", 
                    weight = 1, 
                    fillOpacity = 0.5, 
                    popup = ~paste("Country: ", country_shapes_geojson$COUNTRY, "<br>", "Region: ", country_shapes_geojson$Sb_rg_N),
                    group = "Regions") %>%  
        
        addLegend(position = "bottomright", 
                  colors = unlist(custom_colors),
                  labels = names(custom_colors), 
                  title = "UN Geoscheme Regions",
                  opacity = 0.9) %>%
        # Add study markers with a higher zIndex
        addCircleMarkers(
          lng = data_to_plot$Longitude, 
          lat = data_to_plot$Latitude, 
          fillColor = ifelse(data_to_plot$Type.of.study == "Restoration", "#39FF14", "#FF6A00"),
          radius = 6, 
          color = "black",
          weight = 2,
          fillOpacity = 0.9,
          popup = popup_content,
          group = "Studies",
          options = markerOptions(zIndex = 1000)  
        ) %>%
        addLegend(position = "bottomleft", 
                  colors = c("transparent", "#39FF14", "transparent", "#FF6A00"),
                  labels = c("", "Restoration", "", "Degradation"),
                  title = "Type of Study",  
                  opacity = 0.9) %>%
        # Add case reports markers as a separate layer
        addCircleMarkers(
          lng = case_reports_data$Longitude, 
          lat = case_reports_data$Latitude, 
          fillColor = "#FF5CAD", 
          radius = 6, 
          color = "black",
          weight = 2,
          fillOpacity = 0.9,
          stroke = TRUE,
          popup = popup_content1,
          group = "Case Reports",
          options = markerOptions(zIndex = 1000)
        ) %>%
        # Add policy briefs as a separate layer
        addCircleMarkers(
          lng = policy_briefs$Longitude, 
          lat = policy_briefs$Latitude, 
          fillColor = "#B84DFF", 
          radius = 6, 
          color = "black",
          weight = 2,
          fillOpacity = 0.9,
          stroke = TRUE,
          popup = popup_content2,
          group = "Policy Briefs",
          options = markerOptions(zIndex = 1000)
        ) %>%
        # Add case reports markers as a separate layer
        addAwesomeMarkers(
          lng = nfr_df$Longitude, 
          lat = nfr_df$Latitude, 
          icon = nfr_icon,
          group = "Need for research" 
        ) %>%
        # Add layers control with regions layer toggled off
        addLayersControl(
          baseGroups = c("Street Map"),  
          overlayGroups = c("Studies", "Case Reports", "Policy Briefs", "Need for research", "Regions"),         
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        hideGroup("Regions")  %>% 
        hideGroup("Case Reports") %>%
        hideGroup("Need for research") %>%
        hideGroup("Policy Briefs")
    }
  })
}
# Run the app
shinyApp(ui, server)

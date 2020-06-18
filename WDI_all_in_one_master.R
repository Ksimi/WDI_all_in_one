library(tidyverse)
library(leaflet)
# library(dplyr)
library(sf)
library(htmltools)
library(htmlwidgets)
library(RColorBrewer)
library(viridis)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinybusy)
library(ggflags)
library(leaflet.extras)
library(ggforce)
library(extrafont)
library(dashboardthemes)

# Reading the world polygon file
World <- read_sf(dsn = "data-raw/ne_50m_admin_0_countries.shp") %>%
  select("ISO_A3", "geometry")

# Reading list of countries with shortened names and ISO A2 codes
ISO_cont_reg <- read_csv("data-raw/ISO_cont_reg.csv") %>%
  select("ISO_A3", "ISO_A2", "Country", "Continent", "Subregion")

# Setting a "no data" sign
No_data <- "-----"

# Reading the WDI_CETS file and renaming some columns
WDI_CETS <- read_csv("data-raw/WDI_CETS.csv") %>%
  rename("Indicator_code" = "Series Code",
         "Indicator_name" = "Series Name"
  ) %>%
  replace_na(list(SubTopic2 = No_data, SubTopic3 = No_data)) 

# Reading the WDISeries file
WDISeries <- read_csv("data-raw/WDISeries.csv") %>% 
  rename("Indicator_code" = "Series Code",
         "Definition" = "Long definition",
         "Aggregation" = "Aggregation method",
         "Lim_and_excep" = "Limitations and exceptions",
         "Stat_conc" = "Statistical concept and methodology",
         "Dev_rel" = "Development relevance",
         "Lic" = "License Type") %>% 
  select("Indicator_code",
         "Definition",
         "Periodicity",
         "Aggregation",
         "Source",
         "Lim_and_excep",
         "Stat_conc",
         "Dev_rel",
         "Lic"
         )

# Merging the info files for full info box
Info_full <- merge(WDI_CETS, WDISeries, by = "Indicator_code")

# Extracting a topic list for the first selector
Topic_list <- Info_full %>%
  select(Topic) %>%
  unique() %>%
  arrange_all()

# Creating a structured list of the colors for palette selection
Palette_tibble <- tibble(
  Pal_name = c(
    "Yellow-Green-Blue",
    "Blue-Green",
    "Blue-Purple",
    "Blues",
    "Green-Blue",
    "Greens",
    "Greys",
    "Inferno",
    "Magma",
    "Orange-Red",
    "Oranges",
    "Plasma",
    "Purple-Blue",
    "Purple-Blue-Green",
    "Purple-Red",
    "Purples",
    "Red-Purple",
    "Reds",
    "Viridis",
    "Yellow-Green",
    "Yellow-Orange-Brown",
    "Yellow-Orange-Red",
    "Red-Yellow-Green",
    "Brown-White-Purple (colorblind safe)",
    "Pink-White-Green (colorblind safe)",
    "Purple-White-Green (colorblind safe)",
    "Red-White-Blue (colorblind safe)",
    "Red-White-Grey",
    "Red-Yellow-Blue (colorblind safe)",
    "Spectral"
    ),
  Pal_value = c(
    "YlGnBu",
    "BuGn",
    "BuPu",
    "Blues",
    "GnBu",
    "Greens",
    "Greys",
    "inferno",
    "magma",
    "OrRd",
    "Oranges",
    "plasma",
    "PuBu",
    "PuBuGn",
    "PuRd",
    "Purples",
    "RdPu",
    "Reds",
    "viridis",
    "YlGn",
    "YlOrBr",
    "YlOrRd",
    "RdYlGn",
    "PuOr",
    "PiYG",
    "PRGn",
    "RdBu",
    "RdGy",
    "RdYlBu",
    "Spectral"
    )
)

# Extracting palette names for using in a palette selector
Palette_list <- list(
  "Sequential" = c(
    "Yellow-Green-Blue",
    "Blue-Green",
    "Blue-Purple",
    "Blues",
    "Green-Blue",
    "Greens",
    "Greys",
    "Inferno",
    "Magma",
    "Orange-Red",
    "Oranges",
    "Plasma",
    "Purple-Blue",
    "Purple-Blue-Green",
    "Purple-Red",
    "Purples",
    "Red-Purple",
    "Reds",
    "Viridis",
    "Yellow-Green",
    "Yellow-Orange-Brown",
    "Yellow-Orange-Red"
  ),
  "Diverging" = c(
    "Red-Yellow-Green",
    "Brown-White-Purple (colorblind safe)",
    "Pink-White-Green (colorblind safe)",
    "Purple-White-Green (colorblind safe)",
    "Red-White-Blue (colorblind safe)",
    "Red-White-Grey",
    "Red-Yellow-Blue (colorblind safe)",
    "Spectral"
  )
)

# Creating a separate viridis tibble for bar chart
Pal_viridis <- tibble(
  P_V_name = c(
    "Inferno",
    "Magma",
    "Plasma",
    "Viridis"),
  P_V_option = c(
    "B",
    "A",
    "C",
    "D"
    )
  )

# Creating the list of map tiles providers
Provider_list <- c(
  "Esri.WorldShadedRelief",
  "Esri.WorldTerrain",
  "Esri.WorldPhysical",
  "Stamen.TonerBackground",
  "Stamen.Watercolor",
  "Stamen.TerrainBackground",
  "CartoDB.PositronNoLabels",
  "CartoDB.DarkMatterNoLabels",
  "CartoDB.VoyagerNoLabels"
)

# Creating a control panel
Control_panel <- dashboardSidebar(
  tags$head(
  includeCSS("styles_darkgrey.css")
  ),
  selectizeInput("Sel_top", "Topic",
                 choices = sort(unique(Info_full$Topic))
                 ),
  selectizeInput("Sel_subtop1", "Subtopic 1",
                 choices = NULL),
  selectizeInput("Sel_subtop2", "Subtopic 2",
                 choices = NULL),
  selectizeInput("Sel_subtop3", "Subtopic 3",
                 choices = NULL),
  selectizeInput("Sel_indic", "Indicator",
                 choices = NULL),
  selectizeInput("years",
                 label = "Select a year",
                 choices = NULL),
  selectizeInput("Choose_palette",
                 label = "Select color palette",
                 choices = Palette_list),
  selectizeInput("Choose_provider",
                 label = "Select map view",
                 choices = Provider_list),
  radioButtons(
    "Sel_pal_type",
    label = "Select palette type (for World map)",
    choices = c("Continuous",
                "Improved bins",
                "Quantile"),
    selected = "Continuous"
  ),
  radioButtons(
    "Sel_geo",
    label = "Select map type",
    choices = c("Regular",
                "Scaled (normalized)"),
    selected = "Regular"
  ),
  radioButtons("Top_or_bottom", 
               label = "Select top 10 / bottom 10",
               choices = c("Top 10",
                           "Bottom 10"),
               selected = "Top 10"),
  tags$div(
    h3("Coding and design (c) Maxim Kobzev", 
       align = "center",
       style = "padding: 10px;
                font-family: Segoe Script!important;
                font-size:12px"
       )
  )
)

# Creating the panels for dashboard boxes

Map_panel <- leafletOutput("WDI_map",
                           width = "100%",
                           height = 460)

Mini_map_panel <- leafletOutput("WDI_mini_map",
                           width = "100%",
                           height = 220)

Chart_panel <- plotOutput("WDI_barchart",
                          width = "100%",
                          height = "220px")

Graph_panel <- plotOutput("WDI_graph",
                           width = "100%",
                           height = "220px")

Mini_title_panel_left <- fluidPage(
  id = "title",
  htmlOutput("Mini_title_WDI_left")
)

Mini_title_panel_right <- fluidPage(
  id = "title",
  htmlOutput("Mini_title_WDI_right")
)

Signature_panel <- fluidPage(id = "sign",
                             htmlOutput("Signature")
                             )

Name_panel <- fluidPage(
  id = "name",
  htmlOutput("Name_WDI")
)

Value_panel <- fluidPage(
  id = "value",
  height = "auto",
  htmlOutput("Value_box")
)

Info_panel <- fluidPage(
  id = "def",
  height = "auto",
  htmlOutput("Info_WDI"),
  h4("")
)

Source_panel <- fluidPage(
  id = "source",
  height = "auto",
  htmlOutput("Src_WDI")
)

# Creating a spinner for showing when loading data
Busy_spinner <- add_busy_spinner(
  spin = "fading-circle",
  position = "full-page",
  height = "75px",
  width = "75px",
  color = "grey"
)


#### Creating the UI ####
ui <- dashboardPage(
  # skin = "green",
  dashboardHeader(title = "World Development Indicators",
                  titleWidth = 300),
  dashboardSidebar(Control_panel,
                   width = 300
                   ),
  dashboardBody(
    shinyDashboardThemes(theme = "grey_dark"),
    fluidRow(box(width = 8,
                 Map_panel),
             column(
               width = 4,
               box(
                 width = NULL,
                 background = "teal",
                 style = "overflow: auto;
                 height: 483px;",
                 Info_panel
               )
             )),
    fluidRow(
      box(
      width = 12,
      box(
        width = 8,
        Mini_title_panel_left,
        box(Mini_map_panel),
        box(Chart_panel)
      ),
      box(width = 4,
          Mini_title_panel_right,
          box(width = NULL,
              Graph_panel))
    )),
    Busy_spinner
  )
)



#### Creating the server part ####
##################################

server <- function (input, output, session) {
  
  # Extracting a series code
  Series_code <- reactive({
    Info_full %>% 
      filter(Indicator_name == input$Sel_indic) %>%
      select(Indicator_code)
  })
  
  # Topic selector  
  Top <- reactive({
    req(Topic_list)
    filter(Info_full, Topic == input$Sel_top) %>%
      arrange(SubTopic1)
  })
  
  # Subtopic 1 selector
  Subtop1 <- reactive({
    req(input$Sel_subtop1)
    filter(Top(), SubTopic1 == input$Sel_subtop1) %>%
      arrange(SubTopic2)
  })
  
  # Subtopic 2 selector
  Subtop2 <- reactive({
    req(input$Sel_subtop2)
    if(input$Sel_subtop2 == No_data) {
      filter(Subtop1(), SubTopic1 == input$Sel_subtop1) %>%
        arrange(Indicator_name)
    } else {
      filter(Top(), SubTopic2 == input$Sel_subtop2) %>%
        arrange(SubTopic3)
    }
  })
  
  # Subtopic 3 selector
  Subtop3 <- reactive({
    req(input$Sel_subtop3)
    if(input$Sel_subtop3 == No_data) {
      filter(Subtop2(), SubTopic2 == input$Sel_subtop2) %>%
        arrange(Indicator_name)
    } else {
      filter(Subtop2(), SubTopic3 == input$Sel_subtop3) %>%
        arrange(Indicator_name)
    }
  })
  
  # Indicator selector
  Indic <- reactive({
    req(input$Sel_indic)
    if(input$Sel_subtop2 == No_data & input$Sel_subtop3 == No_data) {
      filter(Subtop1(), SubTopic1 == input$Sel_subtop1) %>%
        arrange(Indicator_name)
    } else {
      if(input$Sel_subtop3 == No_data) {
        filter(Subtop2(), SubTopic2 == input$Sel_subtop2) %>%
          arrange(Indicator_name)
      } else {
        filter(Subtop3(), Indicator_name == input$Sel_indic) %>%
          arrange_all()
      }
    }
  })
 
  
  # Creating raw data file for subsequent filtering
  WDI_data_raw <- reactive({
    req(Indic())
    
    # Removing (refreshing) a "temp" folder for storing an indicator data
    unlink("temp", recursive = T)
    
    #Create a link for downloading an indicator data
    Series_URL <- paste0("http://api.worldbank.org/v2/en/indicator/", 
                         Series_code(), 
                         "?downloadformat=csv")
    
    # Downloading a zip file, using "wb" mode for a binary (compressed) one
    download.file(Series_URL, "Ind", mode = "wb")
    
    # Extracting the csv file into a "temp" subdirectory
    unzip("Ind", exdir = "temp")
    unlink("Ind", recursive = T)
    
    # Reading a.csv file starting with "API" into a data frame, skipping first four rows
    # Converting a wide data set into a long one, renaming columns, removing empty column
    
    list.files(path = "temp",
               pattern = paste0("^API_", Series_code()),
               full.names = T) %>%
      map_df(~read_csv(., skip = 4)) %>% 
      pivot_longer(cols = c(5:last_col()),
                   names_to = "Year",
                   values_to = "Value",
                   values_ptypes = c(integer, double) 
      ) %>%
      rename("ISO_A3" = "Country Code",
             "Indicator_code" = "Indicator Code",
             "Indicator_name" = "Indicator Name"
      ) %>%
      select("ISO_A3",
             "Indicator_code",
             "Indicator_name",
             "Year",
             "Value"
      ) 
    
  })
  
  Values_year_clean <- reactive ({
    filter(WDI_data_raw(), Year == input$years) %>%
      merge(ISO_cont_reg, by = "ISO_A3") %>%
      drop_na()
  })
  
  # Checking if minimal value is negative
  Neg_min <- reactive ({
    min(Values_year_clean()$Value) < 0
  })
  
  # Summarizing the data for a time series graph at a World level
  World_series <- reactive({
      WDI_data_raw() %>%
      drop_na() %>% 
      select("Year", "Value", "ISO_A3") %>% 
      filter(ISO_A3 == "WLD")
  })
  

  # Filtering out the years without any data
  Year_check <- reactive({
    req(input$Sel_indic)
    
    WDI_data_raw() %>% 
      group_by(Year,ISO_A3, Indicator_code, Indicator_name, Value) %>%
      # Removing the years with erroneous 0 values
      summarize(Sum = sum(Value)) %>% 
      filter(Sum != 0) %>% 
      ungroup() %>%
      select(Year) %>% 
      unique() %>%
      arrange()
  })
  
  
  # Creating the map master data file
  Master_data_map <- reactive({
    Values_year_clean() %>%
      merge(World, by = "ISO_A3") %>%
      st_as_sf()
  })
  
  # Creating a "Yes/No" indicator type flag
  Yes_No <- reactive ({
    str_detect(input$Sel_indic, "1=yes", negate = T)
  })
 
  # Creating the bar chart master data file &
  # Replacing 0s with NAs with the exception of "Yes/No" datasets
  Master_data_bar <- reactive({
    if (Yes_No()) {
      filter(WDI_data_raw(), Year == input$years) %>%
        merge(ISO_cont_reg, by = "ISO_A3") %>%
        na_if("0") %>%
        drop_na()
    } else {
      Values_year_clean()
    }
    
  })
  
  # Creating sorted top or bottom list of countries
  Bar_chart_sorted <- reactive ({
    Master_data_bar() %>%
    mutate(Sort_order = rank(Rank_sign()*Value, ties.method = "first")) %>%
    filter(Sort_order <= Cnt_mix)
    
  })
  
  Master_data_mini_map <- reactive({
    Values_year_clean() %>%
      mutate(Sort_order = rank(Rank_sign()*Value, ties.method = "first")) %>%
      filter(Sort_order <= Cnt_mix) %>%
      merge(World, by = "ISO_A3") %>%
      st_as_sf()
  })
  
  Value_box_number <- reactive ({
    WDI_data_raw() %>%
      select("Year", "Value", "ISO_A3") %>% 
      filter(ISO_A3 == "WLD" & Year == input$years) %>% 
      select("Value") %>% 
      round(2)
      })
  
  Value_box_text <- reactive ({
    ifelse(is.na(Value_box_number()),
           paste0(
             h4(
             "World level value not available"
             ),
             h4("Year: ", input$years)
             ),
           paste0(
             h4(
               "World level value: ",
               prettyNum(Value_box_number(),
                         big.mark = ",")
             ),
             h4("Year: ", input$years)
             )
           )
  })
  
  
  # Extracting the values for plotting
  Master_to_show <- reactive ({
    Master_data_map()$Value
  })
 
  # Extracting the values for plotting
  Master_to_show_mini <- reactive ({
    Master_data_mini_map()$Value
  }) 
  
  # Selecting an indicator to draw a map
  Select_data <- reactive({
    input$Sel_indic
  })
  
  # Extracting a year number for showing on the legend, popups etc.
  Year_filter <- reactive ({
    input$years
  })
  
  # Selecting top N / bottom N countries
  Cnt_mix = 10
  
  # Creating rank sign dependent on Top 10/Bottom 10 switch
  Rank_sign <- reactive ({
    if(input$Top_or_bottom == "Top 10") {-1.0} else {1.0}
  })
  
  # Creating palette selector
  Select_palette_master <- reactive ({
    if (min(Master_to_show()) < 0 &
        !(
          input$Choose_palette %in% c(
            "Red-Yellow-Green",
            "Brown-White-Purple (colorblind safe)",
            "Pink-White-Green (colorblind safe)",
            "Purple-White-Green (colorblind safe)",
            "Red-White-Blue (colorblind safe)",
            "Red-White-Grey",
            "Red-Yellow-Blue (colorblind safe)",
            "Spectral"
            )
          )
        )
    {
      showNotification(
        paste0(
            "Please select a diverging palette"
            ),
        duration = 8,
        type = "message"
      )
      filter(Palette_tibble,
             Pal_name == input$Choose_palette) %>%
        select(Pal_value) %>%
        as.character()
    } else {
      filter(Palette_tibble,
             Pal_name == input$Choose_palette) %>%
        select(Pal_value) %>%
        as.character()
    }
    
  })
  
  #### Big map palette part begin ####
 
  #### Creating the improved bins palette for all possible value combinations ####
  # Extracting breakpoints for a value range
  
  Palette_master <- function(Master_data) {
  
  Breakpoints <- ggplot2:::breaks(Master_data, "number", 10)
  
  # Finding the max/min breakpoints
  Max_point <- max(Master_data, na.rm = T)
  Min_point <- min(Master_data, na.rm = T)
  
  # Finding the second highest value in breakpoints
  Second_to_max <- sort(Breakpoints, partial = length(Breakpoints) - 1)[length(Breakpoints) - 1]
  
  # Defining the data ranges
  Data_range_1 <- Second_to_max - Min_point
  Data_range_2 <- Max_point - Second_to_max
  
  Data_range_negative <- 0 - Min_point
  Data_range_positive <- Max_point - 0
  
  # Finding the scale ratio
  Scale_ratio_1 <- Second_to_max/Min_point
  Scale_ratio_2 <- Max_point/Second_to_max
  Sc_1_to_Sc_2 <- Scale_ratio_1/Scale_ratio_2
  
  # Getting the range steps for positive palettes
  Range_1_step_less <- Data_range_1/6
  Range_2_step_less <- Data_range_2/3
  Range_1_step_more <- Data_range_1/3
  Range_2_step_more <- Data_range_2/6
  
  # Getting the range steps for diverging palettes
  Range_neg_step = Data_range_negative/5
  Range_pos_step = Data_range_positive/5
  
  # Adjusting the breaks for positive palettes
  Breaks_adjusted_less <- c(Min_point,
                            Min_point + Range_1_step_less,
                            Min_point + Range_1_step_less*2,
                            Min_point + Range_1_step_less*3,
                            Min_point + Range_1_step_less*4,
                            Min_point + Range_1_step_less*5,
                            Min_point + Range_1_step_less*6,
                            Second_to_max + Range_2_step_less,
                            Second_to_max + Range_2_step_less*2,
                            Max_point)
  
  Breaks_adjusted_more <- c(Min_point,
                            Min_point + Range_1_step_more,
                            Min_point + Range_1_step_more*2,
                            Min_point + Range_1_step_more*3,
                            Second_to_max + Range_2_step_more,
                            Second_to_max + Range_2_step_more*2,
                            Second_to_max + Range_2_step_more*3,
                            Second_to_max + Range_2_step_more*4,
                            Second_to_max + Range_2_step_more*5,
                            Max_point)
  
  # Adjusting the breaks for diverging palettes
  Breaks_diverging <- c(Min_point,
                        Min_point + Range_neg_step,
                        Min_point + Range_neg_step*2,
                        Min_point + Range_neg_step*3,
                        Min_point + Range_neg_step*4,
                        Min_point + Range_neg_step*5,
                        0 + Range_pos_step,
                        0 + Range_pos_step*2,
                        0 + Range_pos_step*3,
                        0 + Range_pos_step*4,
                        0 + Range_pos_step*5)
  
  # Switching between the breaks depending on the scale ratio
  Breaks_adjusted <- if(Min_point < 0) {
    Breaks_diverging
  } else {
    if(Scale_ratio_2 < 20) {
      Breaks_adjusted_less
    } else {
      Breaks_adjusted_more
    }
  }
  
  # Advising on switching to continuous palette when breaks in a data range are not unique
  ifelse(Max_point == Second_to_max & input$Sel_pal_type != "Continuous",
         showNotification("Please select a continuous palette",
                          duration = 8,
                          type = "error"),
         "")
  

  # Advising on selecting a sequential palette when displaying a positive data
  ifelse(
    min(Master_to_show()) >= 0 &
      (
        input$Choose_palette %in% c(
          "Red-Yellow-Green",
          "Brown-White-Purple (colorblind safe)",
          "Pink-White-Green (colorblind safe)",
          "Purple-White-Green (colorblind safe)",
          "Red-White-Blue (colorblind safe)",
          "Red-White-Grey",
          "Red-Yellow-Blue (colorblind safe)",
          "Spectral"
        )
      ),
    showNotification(
      paste0("Please select a sequential palette"),
      duration = 8,
      type = "error"
    ),
    ""
  )
  
  
  # Choosing a palette type between continuous/improved bins/quantile
  if(input$Sel_pal_type == "Continuous") {
    colorNumeric(
      Select_palette_master(),
      Master_data)
  } else {
    if(input$Sel_pal_type == "Improved bins") {
      colorBin(
        Select_palette_master(),
        Master_data,
        bins = Breaks_adjusted,
        reverse = F
      ) } else {
        colorQuantile(
          Select_palette_master(),
          Master_data,
          10
        )
      }
  }
  
  }
  
  #### Palette part end ####
  
  
  #### Choropleth map part begin ####
  
  # Filtering the Master_data file for a map
  Indic_map <- reactive({

    # Selecting a map type between regular an scaled (normalized)
    What_to_show <- if(input$Sel_geo == "Regular") {
      Master_data_map()
    } else {
      # Extracting geometry and calculating the scale factor
      Geometry_what_to_show <- Master_data_map() %>%
        st_geometry()
      Point_what_to_show <- st_point_on_surface(Geometry_what_to_show)
      Scale_factor <- Master_data_map()$Value/max(Master_data_map()$Value)

      # Changing the geometry as per scale factor
      Scaled_geo <- (Geometry_what_to_show - Point_what_to_show)*Scale_factor +
        Point_what_to_show

      # Rewriting the geometry column in a copy of What_to_show dataset with a scaled one
      What_to_show_scaled <- Master_data_map()
      What_to_show_scaled$geometry <- Scaled_geo
      What_to_show_scaled$Value <- Scale_factor
      What_to_show_scaled
    }

    # Creating labels for pop-ups with HTML
    Popups_map <- sprintf("<strong> %s, </strong> <br/>
                          %s <br/>
                          Year: %s <br/>
                          Value: %s",
                          What_to_show$Country,
                          Select_data(),
                          What_to_show$Year,
                          prettyNum(round(Master_data_map()$Value, 1),
                                    scientific = F,
                                    big.mark = ",")
    ) %>%
      lapply(htmltools::HTML)


    #### Defining the leaflet components as functions ####
    # Plotting the coloured countries with the values as per master data
    Polygons_map <- function(map) {
      addPolygons(
        map,
        options = leafletOptions(pane = "Countries"),
        group = "Countries",
        smoothFactor = 0.2,
        fillOpacity = 1,
        dashArray = "",
        color = "lightgray",
        weight = 0.7,
        opacity = 0.7,
        fillColor = ~ Palette_master(Master_to_show())(Master_to_show()),
        highlight = highlightOptions(
          weight = 1.5,
          color = "darkgray",
          dashArray = "",
          fillOpacity = 1,
          bringToFront = TRUE
        ),
        label = Popups_map,
        labelOptions = labelOptions(
          style = list(
            "border" = 0,
            "font-weight" = "lighter",
            "color" = "#ffffff",
            "background-color" = "#44505a",
            "line-height" = "15px",
            padding = "2px 2px"
          ),
          textsize = "13px",
          opacity = 0.7,
          direction = "auto"
        )
      )
    }

    # Countries pane as a background (zIndex = 420)
    Countries_pane <- function(map, zIndex){
      addMapPane(map, "Countries", zIndex = 420)
    }

    # Names pane on top of countries (zIndex = 430)
    Names_pane <- function(map, zIndex){
      addMapPane(map, "Names", zIndex = 430)
    }

    # Tiles with no names as a background
    Provider_tiles_Nonames <- function(map) {
      addProviderTiles(map,
                       input$Choose_provider,
                       group = "Map tiles")
    }

    # Name tiles
    Provider_tiles_names <- function(map) {
      addProviderTiles(
        map,
        "CartoDB.PositronOnlyLabels",
        options = leafletOptions(pane = "Names"),
        group = "Names"
      )
    }

    # Creating the legend
    Legend_map <- function(map) {
      addLegend(
        map,
        group = "Legend",
        pal = Palette_master(Master_to_show()),
        values = ~ Master_to_show(),
        labFormat = labelFormat(digits = 2),
        opacity = 1,
        title = "Values",
        position = "topright"
      )
    }

    # Creating the layers control
    Layer_control <- function(map){
      addLayersControl(map,
                       overlayGroups = c(
                         "Map tiles",
                         "Legend",
                         "Names"
                       ),
                       options = layersControlOptions(collapsed = F),
                       position = "bottomleft"
      )
    }
    
    #### Assembling the map from the code & functions blocks ####
    
    
    # Advising on regular map when data is diverging
    if (min(Master_to_show()) < 0 &
        input$Sel_geo == "Scaled (normalized)") {
      showNotification("Please select a regular map",
                       duration = 8,
                       type = "error")
      }
    
    else {
      leaflet(What_to_show,
              options = leafletOptions(zoomControl = FALSE)) %>%
        setView(20, 20, 2) %>%
        Countries_pane %>%
        Names_pane %>%
        Provider_tiles_Nonames %>%
        Provider_tiles_names %>%
        addFullscreenControl() %>% 
        Polygons_map %>%
        Legend_map %>%
        Layer_control
        
    }
    
  })

  #### Choropleth map part end ####
  
  #### mini map part begin ####
  
  # Filtering the Master_data file for a map
  Indic_mini_map <- reactive({

    # Creating labels for pop-ups with HTML
    Popups_mini_map <- 
      if(
      input$Top_or_bottom == "Top 10") {
      sprintf(
        "%s, <br/>
        Rank from top: %s <br/>
        Value: %s",
        Master_data_mini_map()$Country,
        Bar_chart_sorted()$Sort_order,
        prettyNum(
          round(Master_to_show_mini(), 2),
          scientific = F,
          big.mark = ","
          )
        ) %>%
          lapply(htmltools::HTML)
        } else {
      sprintf(
        "%s, <br/>
        Rank from bottom: %s <br/>
        Value: %s",
        Master_data_mini_map()$Country,
        Bar_chart_sorted()$Sort_order,
        prettyNum(
          round(Master_to_show_mini(), 2),
          scientific = F,
          big.mark = ","
          )
        )
        } %>%
      lapply(htmltools::HTML)

    lim_mini <- max(abs(Master_to_show_mini()))*c(-1, 1)
    
    Palette_mini_map <- reactive ({
      if (Neg_min()) {
        colorNumeric(Select_palette_master(),
                     domain = lim_mini,
                     Master_to_show_mini())
      }
      else
      {
        colorNumeric(Select_palette_master(),
                     Master_to_show_mini())
      }
    })
    

    #### Defining the leaflet components as functions ####
    # Plotting the colored countries with the values as per master data
    Polygons_mini_map <- function(mini_map) {
      addPolygons(
        mini_map,
        smoothFactor = 0.2,
        fillOpacity = 1,
        dashArray = "",
        color = "lightgray",
        weight = 0.7,
        opacity = 0.7,
        fillColor = ~ Palette_mini_map()(Master_to_show_mini()),
        highlight = highlightOptions(
          weight = 1.5,
          color = "darkgray",
          dashArray = "",
          fillOpacity = 1,
          bringToFront = TRUE
        ),
        label = Popups_mini_map,
        labelOptions = labelOptions(
          style = list(
            "border" = 0,
            "font-weight" = "lighter",
            "color" = "#ffffff",
            "background-color" = "#44505a",
            "line-height" = "14px",
            padding = "2px 2px"
          ),
          textsize = "12px",
          opacity = 0.7,
          direction = "auto"
        )
      )
    }

    # Tiles with no names as a background
    Provider_tiles_mini_Nonames <- function(mini_map){
      addProviderTiles(mini_map,
                       input$Choose_provider,
                       group = "Map tiles")
    }
    
    
    #### Assembling the map from the code & functions blocks ####
    
    if (Yes_No() == F) {
    }
    else
    {
      leaflet(Master_data_mini_map(),
              options = leafletOptions(zoomControl = FALSE)) %>%
        setView(20, 20, 1) %>%
        Provider_tiles_mini_Nonames %>%
        addFullscreenControl() %>%
        Polygons_mini_map
    }

  })
  
  #### Mini map part end ####
  

  #### Bar chart part begin ####
  Indic_data_bar <- reactive({
  
    # Value_sign <- if(Bar_chart_sorted$Value < 0) {-1} else {1}
    
    Max_value <- max(Bar_chart_sorted()$Value)
    Min_value <- min(Bar_chart_sorted()$Value)
    
    Offset_dist <- max(abs(c(Max_value*0.01, Min_value*0.01)))
    Offset_number <- Offset_dist*4
    Offset_country <- Offset_dist*24
    Offset_flag <- Offset_dist*12
    
    # Creating palette
    Select_palette <- filter(Palette_tibble,
                             Pal_name == input$Choose_palette) %>%
      select(Pal_value) %>%
      as.character()
    
    Text_color = "white"
    
    # Creating bars for charting
    Bars <- geom_bar(aes(fill = ifelse(Bar_chart_sorted()$Value == 0,
                                       0.01,
                                       Value)),
                     stat = "identity",
                     show.legend = F)
    
    # Creating country names
    Chart_text_country <- geom_text(
      aes(y = ifelse(Bar_chart_sorted()$Value > 0,
                     -Offset_country,
                     Offset_country),
          label = Country),
      family = "Segoe UI",
      hjust = ifelse(Bar_chart_sorted()$Value >= 0,
                     "right",
                     "left"),
      size = 4,
      color = Text_color
    )
    
    # Creating numbers
    Chart_text_number <- geom_text(
      aes(
        y = ifelse(Bar_chart_sorted()$Value > 0,
                   Value + Offset_number,
                   Value - Offset_number
        ),
        label = prettyNum(round(Value, 2),
                          scientific = F,
                          big.mark = ","
        )
      ),
      family = "Segoe UI",
      hjust = ifelse(Bar_chart_sorted()$Value >= 0,
                     "left",
                     "right"),
      size = 4,
      color = Text_color
    )
    
    # Creating country flags
    Chart_flags <- geom_flag(
      aes(country = tolower(ISO_A2),
          y = ifelse(Bar_chart_sorted()$Value > 0,
                     -Offset_flag,
                     ifelse(Bar_chart_sorted()$Value == 0,
                            0,
                            Offset_flag)
          )
      ),
      size = 6
    )
    
    # Creating grey circles for drawing around flags
    Chart_flags_bkgr <- geom_point(
      aes(y = ifelse(Bar_chart_sorted()$Value > 0,
                     -Offset_flag,
                     ifelse(Bar_chart_sorted()$Value == 0,
                            0,
                            Offset_flag)
      )
      ),
      size = 6.7,
      pch = 1,
      color = "#D3D3D3",
      show.legend = F
    )
    
    
    # Creating a subset for showing a viridis palette
    Option_P_V <- filter(Pal_viridis,
                         P_V_name == input$Choose_palette) %>% 
      select(P_V_option) %>% 
      as.character()
    
    # Reversing the bar colors in case of a diverging palette
    Revers_div <- ifelse(input$Choose_palette %in% c(
      "Red-Yellow-Green",
      "Brown-White-Purple (colorblind safe)",
      "Pink-White-Green (colorblind safe)",
      "Purple-White-Green (colorblind safe)",
      "Red-White-Blue (colorblind safe)",
      "Red-White-Grey",
      "Red-Yellow-Blue (colorblind safe)",
      "Spectral"
    ),
    F,
    T)
    

    # Centering a diverging scale at 0
    lim <- max(abs(Bar_chart_sorted()$Value)) * c(-1, 1)
    
    
    # Filling the bars with colors
    Scale_filler <-
      if (input$Choose_palette %in% c("Inferno", "Magma", "Plasma", "Viridis"))
      {
        scale_fill_viridis_c(option = Option_P_V)
      }
    else {
      if (Neg_min()) {
        scale_fill_distiller(palette = Select_palette_master(),
                             limit = lim,
                             direction = 1)
      } else {
        scale_fill_distiller(palette = Select_palette_master(),
                             direction = 1)
      }
    }
    
    # Creating chart themes
    Chart_theme <-
      theme(
        panel.background = element_rect(fill = "#44505a"),
        plot.background = element_rect(fill= "#44505a", color = "#44505a"),
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(size = 16, 
                                  family = 'Segoe UI', 
                                  hjust = 0.5,
                                  color = "white")
      )
    
    Chart_theme_null <- 
      theme(
        panel.background = element_rect(fill = "#343e48"),
        plot.background = element_rect(fill= "#343e48", color = "#343e48")
      )
    
    # Assembling a bar chart
    # Not showing the bars when the data is of "Yes/No" type
    if (Yes_No() == F) {
      Chart <- ggplot() +
        Chart_theme_null
      
    # Putting all pieces together for plotting a bar chart
      Chart
      
    }
    else
    {
      Chart <- ggplot(Bar_chart_sorted(),
                      aes(Sort_order,
                          y = Value)) +
        Bars +
        Chart_text_country +
        Chart_text_number +
        Chart_flags +
        Chart_flags_bkgr +
        Chart_theme +
        Scale_filler +
        scale_y_continuous(expand = expand_scale(mult = c(0.8, 0.8))) +
        scale_x_reverse() +
        coord_flip()
      
      # Putting all pieces together for plotting a bar chart
      Chart
    }
    
  })
  
  
  #### Graph part begin ####
  
  Indic_data_graph <- reactive ({
      
       Graph <- ggplot(World_series(),
                       aes(x = Year,
                           y = Value)) +
         geom_point(size = 2,
                    color = "#3d9970",
                    alpha = 0.8) +
         geom_point(
           aes(x = input$years,
               y = World_series()$Value[Year == input$years]),
           size = 7,
           color = "#3d9970",
           alpha = 1
           ) +
         scale_x_discrete(breaks = seq(min(World_series()$Year), 
                                       max(World_series()$Year), 10),
                          expand = expand_scale(mult = c(0.1,0.1))
                          ) +
         scale_y_continuous(breaks = seq(min(World_series()$Value),
                                         max(World_series()$Value),
                                       (max(World_series()$Value) - min(World_series()$Value))/2 
                                       ),
                            label = scales::comma
                            ) +
         theme(
           panel.background = element_rect(fill = "#44505a"),
           plot.background = element_rect(fill= "#44505a", color = "#44505a"),
           axis.title.y = element_blank(),
           panel.grid.major.x = element_blank(),
           panel.grid.major.y = element_blank(),
           panel.grid.minor.x = element_blank(), 
           panel.grid.minor.y = element_blank(),
           axis.ticks.x = element_blank(),
           axis.ticks.y = element_blank(),
           axis.title.x = element_blank(),
           axis.text.x = element_text(color = "white"),
           axis.text.y = element_text(color = "white"),
           plot.title = element_text(
             size = 16,
             family = 'Segoe UI',
             hjust = 0.5,
             color = "white"
           )
         )

    Graph
              
  })
  
  # Creating info module
  WDI_info_master <- reactive ({
    req(Indic())
    Info_full %>%
     filter(Indicator_name == input$Sel_indic)
    })
  

  WDI_info <- reactive ({
    req(Series_code())
    paste0(
      h2(WDI_info_master()$Indicator_name),
      h3(""),
      h4("Code:"),
      h5(WDI_info_master()$Indicator_code),
      h4(""),
      h4("Definition:"),
      h5(WDI_info_master()$Definition),
      h4(""),
      h4("Development relevance:"),
      h5(WDI_info_master()$Dev_rel),
      h4(""),
      h4("Aggregation:"),
      h5(WDI_info_master()$Aggregation),
      h4(""),
      h4("Periodicity:"),
      h5(WDI_info_master()$Periodicity),
      h4(""),
      h4("Source:"),
      h5(WDI_info_master()$Source),
      h4(""),
      h4("Limitations and exceptions:"),
      h5(WDI_info_master()$Lim_and_excep),
      h4(""),
      h4("Statistical concept and methodology:"),
      h5(WDI_info_master()$Stat_conc),
      h4(""),
      h4("Licence:"),
      h5(WDI_info_master()$Lic)
    )
    })
  
  WDI_mini_title <- reactive ({
    req(Series_code())
    req(Indic())
    
    if (Yes_No() == F) {
      paste0(h4("Top 10/bottom 10 data not available for this indicator",
                align = "center"))
    } else {
      if (input$Top_or_bottom == "Top 10") {
        paste0(h4("Top 10 countries, ",
                  Year_filter(),
                  align = "center"))
      } else {
        paste0(h4("Bottom 10 countries, ",
                  Year_filter(),
                  align = "center"))
      }
    }
    
  })
  
  WDI_mini_title_right <- reactive ({
    req(Year_filter())
    if (is.na(Value_box_number()))
    {
      paste0(h4("World level value not available",
                align = "center"))
    } else {
      paste0(h4("World level value, ",
                Year_filter(),
                ":        ",
                prettyNum(Value_box_number(),
                          scientific = F,
                          big.mark = ","),
                align = "center"))
    }
  })
  
  Signage <- paste0(
    style = "z-index: 500;
                opacity: 0.9;
                padding: 10px;
                border-radius: 4px;
                font-family: Segoe Script;
                font-size:12px",
    fixed = T, 
    draggable = F, 
    height = "auto",
    "Coding and design (c) Maxim Kobzev"
  )
  
  # Observing the map view selection
  updateSelectInput(
    session,
    "Choose_provider",
    choices = Provider_list
  )
  
  
  # Observing the palette selection
  updateSelectInput(
    session,
    "Choose_palette",
    choices = Palette_list
  )
  
  # Observing the Top/Bottom selection
  updateSelectInput(
    session,
    "Top_or_bottom"
  )
  
  # Sending the map into ShinyApp
  output$WDI_map <- renderLeaflet(Indic_map())
  
  output$WDI_mini_map <- renderLeaflet(Indic_mini_map())
  
  output$WDI_barchart <- renderPlot(Indic_data_bar())
  
  output$WDI_graph <- renderPlot(Indic_data_graph())
  
  output$Name_WDI <- renderText(WDI_name())
  
  output$Info_WDI <- renderText(WDI_info())
  
  output$Mini_title_WDI_left <- renderText(WDI_mini_title())
  
  output$Mini_title_WDI_right <- renderText(WDI_mini_title_right())
  
  output$Signature <- renderText(Signage)
  
  
  # Observing the topic selection
  observeEvent(Top(), {
    updateSelectizeInput(session, "Sel_subtop1", choices = unique(Top()$SubTopic1), selected = character())
  })
  
  # Observing the Subtopic selection
  observeEvent(Subtop1(), {
    updateSelectizeInput(session, "Sel_subtop2", choices = unique(Subtop1()$SubTopic2), selected = character())
    updateSelectizeInput(session, "Sel_indic", choices = unique(Subtop1()$Indicator_name), selected = character())
  })
  
  # Observing the Subtopic selection
  observeEvent(Subtop2(), {
    updateSelectizeInput(session, "Sel_subtop3", choices = unique(Subtop2()$SubTopic3), selected = character())
    updateSelectizeInput(session, "Sel_indic", choices = unique(Subtop2()$Indicator_name), selected = character())
  })
  
  # Observing the Subtopic selection
  observeEvent(Subtop3(), {
    updateSelectizeInput(session, "Sel_indic", choices = unique(Subtop3()$Indicator_name), selected = character())
  })
  
  # Observing the year selection
  observeEvent(Year_check(), {
    updateSelectizeInput(session, "years", choices = sort(Year_check()$Year))
  })
  
}

shinyApp(ui, server)

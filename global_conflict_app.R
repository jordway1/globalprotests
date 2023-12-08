library(shiny)
library(tidyverse)
library(leaflet)
library(zoo)
library(DT)

################################################################################
# Reading data
################################################################################

conflict_data <- read_csv("fatal_data.csv") 

event_type_levels <- conflict_data %>%
  group_by(event_type) %>% 
  summarize(total_fatalities = sum(fatalities, na.rm = TRUE)) %>%
  arrange(desc(total_fatalities))

conflict_data$event_type <- factor(conflict_data$event_type, 
                                   levels = event_type_levels$event_type)

sub_event_type_levels <- conflict_data %>%
  group_by(event_type, sub_event_type) %>%
  summarize(total_fatalities = sum(fatalities, na.rm = TRUE)) %>%
  arrange(event_type, desc(total_fatalities))

conflict_data <- conflict_data %>%
  mutate(sub_event_type = factor(sub_event_type, 
                                 levels = sub_event_type_levels$sub_event_type),
         date = dmy(event_date),
         civilian_fatalities = 
           ifelse(civilian_targeting ==  "Civilian targeting", 
                  fatalities, NA))

fatal_data <- conflict_data %>% filter(fatalities >= 10)
yemen_data <- conflict_data %>% filter(country == "Yemen")

my_palette <- c("#3567A3", "#8dcbcc", "#a65b5b", "#78a5a3", "#b8bb88", "#6E8B73")
theme_update(plot.title = element_text(hjust = 0.5))

ukraine_actors <- read_csv("ukraine_actors.csv")
yemen_actors <- read_csv("yemen_actors.csv")

################################################################################
# UI, inputs
################################################################################

ui <- fluidPage(
  tags$head(
    tags$style(HTML("body {font-family: 'serif';}"))
  ),
  titlePanel(HTML("<h1 style='text-align:center;'>Global Armed Conflict Data</h1>")),
  tags$p(
    HTML("<span style='font-size: 18px;'>This dashboard displays information from 
    the Armed Conflict Location 
         and Event Data Project (ACLED) regarding deadly conflicts around the 
         world from November 2020 to November 2023. Numbers of fatalities appear
         to be dramatically lower than what has been reported in the media for 
         certain conflicts (for example, only about 60,000 fatalities in Ukraine 
         can be found in the data). The purpose of this dashboard is to display
         the data from ACLED as-is. Information about ACLED's data collection 
         process can be found on"),
    tags$a("their website.", href = "https://acleddata.com/data-export-tool/")),
  tabsetPanel(

  ##############################################################################
  # Global tab inputs
  ##############################################################################

    tabPanel("Global Overview",
             fluidRow(
               column(2,
                      dateRangeInput("date_range", "Select Date Range",
                                     start = as.character(min(conflict_data$date)),
                                     end = as.character(max(conflict_data$date)),
                                     min = as.character(min(conflict_data$date)),
                                     max = as.character(max(conflict_data$date))
                                     ), 
                      checkboxGroupInput("region", 
                                         "Select Region(s)", choices = 
                                           unique(conflict_data$region),
                                         selected = unique(conflict_data$region))),
               column(10, leafletOutput("globalmap", height = "800px"))
             ),
             fluidRow(
               column(4, DTOutput("country_fatalities_table")),
               column(8, plotOutput("region_fatalities", height = "600px"))
             ),
             fluidRow(
               column(4, DTOutput("actor_fatalities")),
               column(8, plotOutput("fatality_event_graph", height = "600px"))
             )
             ),
  
  ##############################################################################
  # Ukraine tab inputs
  ##############################################################################
  
    tabPanel("Ukraine",
             fluidRow(
               column(2,
                      dateRangeInput("ukraine_date_range", "Select Date Range",
                                     start = as.character(min(conflict_data$date)),
                                     end = as.character(max(conflict_data$date)),
                                     min = as.character(min(conflict_data$date)),
                                     max = as.character(max(conflict_data$date))
                                     ),
                      checkboxGroupInput("ukraine_sides", "Select Side", 
                                         choices = unique(ukraine_actors$side),
                                         selected = unique(ukraine_actors$side))),
               column(10, leafletOutput("ukraine_map", height = "600px"))
            ),
            fluidRow(
              column(4,
                     DTOutput("ukraine_side_fatalities", height = "600px")),
              column(8,
                     plotOutput("ukraine_subevent_fatalities", height = "600px"))
            ),
            fluidRow(
              column(8, offset = 2,
                     plotOutput("ukraine_fatalities_over_time", height = "600px"))
            )
    ),
  
  ##############################################################################
  # Yemen tab inputs
  ##############################################################################
  
    tabPanel("Yemen",
             fluidRow(
               column(2, 
                      dateRangeInput("yemen_date_range", "Select Date Range",
                                     start = as.character(min(conflict_data$date)),
                                     end = as.character(max(conflict_data$date)),
                                     min = as.character(min(conflict_data$date)),
                                     max = as.character(max(conflict_data$date))
                      ),
                      checkboxGroupInput("yemen_factions",
                                         "Select Side",
                                         choices = unique(yemen_actors$side),
                                         selected = unique(yemen_actors$side))),
               column(10, leafletOutput("yemen_map", height = "600px"))
             ),
             fluidRow(
               column(4, 
                      DTOutput("yemen_faction_fatalities")),
               column(8, plotOutput("yemen_subevent_graph", height = "600px"))
             ),
             fluidRow(
               column(8, offset = 2,
                      plotOutput("yemen_fatalities_over_time", height = "600px"))
             )
    )
  )
  
)

################################################################################
# Server
################################################################################

server <- function(input, output) {

################################################################################
# Filtered table for global tab
################################################################################
  
  filtered_conflict_data <- reactive({
    filtered_data <- conflict_data %>%
      filter(date >= input$date_range[1],
             date <= input$date_range[2],
             region %in% input$region)
    return(filtered_data)
  })

################################################################################
# Global tab: world map
################################################################################ 
  
  output$globalmap <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 3)) %>%
      addProviderTiles(provider = "CartoDB") %>%
      setView(lat = 10, lng = 0, zoom = 3) %>%
      setMaxBounds(lat1 = -90,
                   lat2 = 90,
                   lng1 = -180,
                   lng2 = 180) %>%
      addCircleMarkers(data = filtered_conflict_data() 
                       %>% filter(fatalities >= 10),
                       lat = ~latitude,
                       lng = ~longitude,
                       radius = ~sqrt(fatalities) + 1,
                       stroke = TRUE,
                       fillOpacity = .3,
                       color = ~my_palette[event_type],
                       popup = ~paste("Actor: ", actor1, 
                                      "<br>Event: ", event_type,
                                      "<br>Fatalities: ", fatalities,
                                      "<br>Notes: ", notes)) %>%
      addLegend("bottomright", 
                colors = my_palette,
                labels = levels(fatal_data$event_type),
                title = "Event Type",
                opacity = .7) %>%
    addControl(
      html = "<div style='font-size: 14px;'>Only includes events with 10+ fatalities</div>",
      position = "bottomleft")
  })
  
################################################################################
# Global tab: country fatalities table
################################################################################
  
  
  output$country_fatalities_table <- renderDataTable({
    filtered_conflict_data() %>% 
      group_by(country) %>%
      filter(date >= input$date_range[1],
             date <= input$date_range[2]) %>%
      summarize(total_fatalities = sum(fatalities), 
                total_civilian_fatalities = sum(civilian_fatalities, 
                                                na.rm = TRUE),
                total_fatal_events = sum(fatalities > 0)) %>%
      arrange(desc(total_fatalities)) %>%
      rename(Country = country,
             'Total Fatal Events' = total_fatal_events,
             'Total Fatalities Inflicted' = total_fatalities,
             'Fatalities Inflicted With Civilian Targeting' = total_civilian_fatalities) %>%
      datatable(options = list(pageLength = 10, 
                               searching = TRUE, ordering = TRUE), 
                caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:black;  font-size:200%; font-family: serif','Fatalities by Country'))
  })
  
################################################################################
# Global tab: fatality event chart
################################################################################

    output$fatality_event_graph <- renderPlot({
    filtered_conflict_data() %>%
      ungroup() %>%
      group_by(event_type, sub_event_type) %>%
      summarize(total_fatalities = sum(fatalities, na.rm = TRUE)) %>%
      filter(total_fatalities > 0) %>%
      ggplot(aes(sub_event_type, total_fatalities, fill = event_type)) +
      geom_col() +
      labs(y = "Total Fatalities", x = "Sub-Event Type", title = "Fatalities by Event Type and Subtype", fill = "Event Type") +
      theme(text = element_text(family = "serif", size = 18),
            plot.title = element_text(size = 22),
            axis.text.x = element_text(angle = 70, hjust = 1)) +
      scale_fill_manual(values = my_palette) +
      scale_y_continuous(labels = scales::comma)
  })

################################################################################
# Global tab: actor fatalities table
################################################################################

  output$actor_fatalities <- renderDataTable({
    filtered_conflict_data() %>% 
      group_by(actor1, country) %>%
      summarize(total_fatalities = sum(fatalities), 
                total_civilian_fatalities = sum(civilian_fatalities, na.rm = TRUE),
                fatal_events = sum(fatalities > 0)) %>%
      filter(total_fatalities > 0) %>%
      arrange(desc(total_fatalities)) %>%
      rename(Actor = actor1,
             Country = country,
             'Total Fatal Events' = fatal_events,
             'Total Fatalities Inflicted' = total_fatalities,
             'Fatalities Inflicted With Civilian Targeting' = total_civilian_fatalities) %>%
      datatable(options = list(pageLength = 10, searching = TRUE, ordering = TRUE), 
                caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:black;  font-size:200%; font-family: serif','Fatalities by Actor'))
  })
  
################################################################################
# Global tab: regional fatalities graph
################################################################################

  output$region_fatalities <- renderPlot({
    fatal_regions <- filtered_conflict_data() %>%
      group_by(region) %>%
      summarize(total_fatalities = sum(fatalities, na.rm = TRUE)) %>%
      arrange(desc(total_fatalities)) %>%
      head(6)
    
    filtered_conflict_data() %>% 
      filter(region %in% fatal_regions$region,
             date >= input$date_range[1], 
             date <= input$date_range[2]) %>%
      mutate(region = factor(region, levels = fatal_regions$region)) %>%
      group_by(region, date) %>%
      summarize(fatalities = sum(fatalities, na.rm = TRUE)) %>%
      mutate(rolling_fatalities = rollmean(fatalities, 14, fill = NA)) %>%
      ggplot(aes(date, rolling_fatalities, color = region)) +
      geom_line(size = 1) +
      scale_color_manual(values = my_palette) +
      labs(x = "Date", y = "Fatalities", title = 
             "14-Day Rolling Fatalities by Region", 
           caption = "6 Highest-Fatality Regions", color = "Region") +
      theme(text = element_text(family = "serif", size = 16),
            title = element_text(size = 20))
  })
  
################################################################################
# Filtered Ukraine data
################################################################################
  
  filtered_ukraine_data <- reactive({
    filtered_data <- conflict_data %>%
      filter(country == "Ukraine",
             date >= input$ukraine_date_range[1],
             date <= input$ukraine_date_range[2]) %>%
      left_join(select(ukraine_actors, -fatalities), by = 'actor1') %>%
      filter(side %in% input$ukraine_sides)
    filtered_data$side <- factor(filtered_data$side,
                                 levels = c("Ukraine", "Russia", "Unidentified"))
    return(filtered_data)
  })
  
################################################################################
# Ukraine map
################################################################################
  
  output$ukraine_map <- renderLeaflet({
    min_zoom <- 6
    ukraine_palette <- c("#005BBB","#D52B1E", "#8D8D8D")
    ukraine_latlon <- c(48.3794, 31.1656)
    leaflet(options = leafletOptions(minZoom = min_zoom)) %>%
      addProviderTiles(provider = "CartoDB") %>%
      setView(lat = ukraine_latlon[1], lng = ukraine_latlon[2], zoom = 6) %>%
      setMaxBounds(lng1 = ukraine_latlon[2] + 10,
                   lng2 = ukraine_latlon[2] - 10,
                   lat1 = ukraine_latlon[1] + 7,
                   lat2 = ukraine_latlon[1] - 7) %>%
      addControl(
        html = "<div style='font-size: 10px;'>Only includes events with 10+ fatalities</div>",
        position = "bottomleft") %>%
      addCircleMarkers(data = filtered_ukraine_data() %>%
                         filter(fatalities >= 10),
                       lat = ~latitude,
                       lng = ~longitude,
                       radius = ~sqrt(fatalities) + 1,
                       color = ~ukraine_palette[side],
                       stroke = TRUE,
                       fillOpacity = .5,
                       popup = ~paste("Actor: ", actor1, 
                                      "<br>Event: ", sub_event_type,
                                      "<br>Fatalities: ", fatalities,
                                      "<br>Notes: ", notes)) %>%
      addLegend("bottomright", 
                colors = ukraine_palette,
                labels = levels(filtered_ukraine_data()$side),
                opacity = .7)
  })

################################################################################
# Ukraine side fatalities table
################################################################################

  output$ukraine_side_fatalities <- renderDataTable({
    filtered_ukraine_data() %>%
      group_by(side) %>%
      summarize(fatal_events = n(),
                total_fatalities_inflicted = sum(fatalities),
                fatalities_civilian = sum(civilian_fatalities, na.rm = TRUE)) %>%
      arrange(desc(total_fatalities_inflicted)) %>%
      rename('Side' = side,
             'Total Fatal Events' = fatal_events,
             'Total Fatalities Inflicted' = total_fatalities_inflicted,
             'Fatalities Inflicted With Civilian Targeting' = fatalities_civilian) %>%
      datatable(options = list(pageLength = 10, searching = TRUE, ordering = TRUE), 
                caption = 
                  htmltools::tags$caption(style = 'caption-side: top; text-align: 
                                          center; color:black;  font-size:200%; 
                                          font-family: serif','Fatalities by Side'))
  })

################################################################################
# Ukraine sub-event fatalities plot
################################################################################
  
  output$ukraine_subevent_fatalities <- renderPlot({
    filtered_ukraine_data() %>%
      ungroup() %>%
      group_by(event_type, sub_event_type) %>%
      summarize(total_fatalities = sum(fatalities, na.rm = TRUE)) %>%
      filter(total_fatalities > 0) %>%
      ggplot(aes(sub_event_type, total_fatalities, fill = event_type)) +
      geom_col() +
      labs(y = "Total Fatalities", x = "Sub-Event Type", 
           title = "Fatalities by Event Type and Subtype", fill = "Event Type") +
      theme(text = element_text(family = "serif", size = 18),
            plot.title = element_text(size = 22),
            axis.text.x = element_text(angle = 70, hjust = 1)) +
      scale_fill_manual(values = my_palette)
  })
  
################################################################################
# Ukraine fatalities over time plot
################################################################################
  
  output$ukraine_fatalities_over_time <- renderPlot({
    filtered_ukraine_data() %>%
      group_by(date) %>%
      summarize(fatalities = sum(fatalities)) %>% 
      mutate(rolling_fatalities = rollmean(fatalities, k = 14, fill = NA)) %>%
      ggplot(aes(date, rolling_fatalities)) + 
      geom_line(size = 1, color = "#3567A3") +
      labs(x = "Date", y = "Fatalities (14-day mean)", title = "Fatalities Over Time") +
      theme(text = element_text(family = "serif", size = 16),
            plot.title = element_text(size = 22),
            plot.margin = margin(40, 40, 50, 40))
  })
  
  
################################################################################
# Filtered Yemen data
################################################################################
  
  filtered_yemen_data <- reactive({
    filtered_data <- conflict_data %>%
      filter(country == "Yemen",
             date >= input$yemen_date_range[1],
             date <= input$yemen_date_range[2]) %>%
      left_join(select(yemen_actors, -fatalities), by = 'actor1') %>%
      filter(side %in% input$yemen_factions)
    filtered_data$side <- factor(filtered_data$side,
                                 levels = c("Houthi Movement", "Republic of Yemen",
                                            "Republic of Yemen: Southern Transitional Council",
                                            "Other"))
    return(filtered_data)
  })
################################################################################
# Yemen tab: map
################################################################################

  output$yemen_map <- renderLeaflet({
    min_zoom <- 6
    yemen_palette <- c("#3567A3", "#8dcbcc", "#a65b5b", "#6E8B73")
    
    yemen_latlon <- c(15.5527, 48.5164)
    
    leaflet(options = leafletOptions(minZoom = min_zoom)) %>%
      addProviderTiles(provider = "CartoDB") %>%
      setView(lat = yemen_latlon[1], lng = yemen_latlon[2], zoom = 6) %>%
      setMaxBounds(lng1 = yemen_latlon[2] + 7,
                   lng2 = yemen_latlon[2] - 7,
                   lat1 = yemen_latlon[1] + 4,
                   lat2 = yemen_latlon[1] - 4) %>%
      addControl(
        html = "<div style='font-size: 10px;'>Only includes events with 10+ fatalities</div>",
        position = "bottomleft") %>%
      addCircleMarkers(data = filtered_yemen_data() %>%
                         filter(fatalities >= 10),
                       lat = ~latitude,
                       lng = ~longitude,
                       radius = ~sqrt(fatalities) + 1,
                       color = ~yemen_palette[side],
                       stroke = TRUE,
                       fillOpacity = .5,
                       popup = ~paste("Actor: ", actor1, 
                                      "<br>Event: ", sub_event_type,
                                      "<br>Fatalities: ", fatalities,
                                      "<br>Notes: ", notes)) %>%
      addLegend("bottomright", 
                colors = yemen_palette,
                labels = levels(filtered_yemen_data()$side),
                opacity = .7)
   
  })
  
################################################################################
# Yemen tab: faction fatalities graph
################################################################################
  
  output$yemen_faction_fatalities <- renderDataTable({
    filtered_yemen_data() %>%
      filter(!is.na(side)) %>%
      group_by(side) %>%
      summarize(total_fatalities = sum(fatalities),
                civilian_fatalities = sum(civilian_fatalities, na.rm = TRUE),
                events = n()) %>%
      arrange(desc(total_fatalities)) %>%
      rename(Faction = side,
             'Total Fatalities Inflicted' = total_fatalities,
             'Fatalities Inflicted With Civilian Targeting' = civilian_fatalities,
             Events = events) %>%
      datatable(caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:black;  font-size:200%; font-family: serif','Fatalities by Faction'))
  })
################################################################################
# Yemen tab: faction fatalities graph
################################################################################

  output$yemen_fatalities_over_time <- renderPlot({
    yemen_data %>%
      group_by(date) %>%
      summarize(fatalities = sum(fatalities)) %>% 
      mutate(rolling_fatalities = rollmean(fatalities, k = 14, fill = NA)) %>%
      ggplot(aes(date, rolling_fatalities)) + 
      geom_line(size = 1, color = "#3567A3") +
      geom_label(aes(x = ymd("2022-04-01"), y = 50,
                     label = "UN Cease-Fire, April 2022"), vjust = 0, hjust = 0, size = 4,
                 family = "serif") +
      geom_segment(
        aes(x = ymd("2022-6-01"), xend = ymd("2022-04-01"), y = 50, yend = 40),
        arrow = arrow(type = "closed", length = unit(0.2, "inches")), linejoin = "round") +
      labs(x = "Date", y = "Fatalities (14-day mean)", title = "Fatalities Over Time") +
      theme(text = element_text(family = "serif", size = 16),
            plot.title = element_text(size = 22),
            plot.margin = margin(40, 40, 50, 40))
  })

################################################################################
# Yemen tab: subevent graph
################################################################################
   
  output$yemen_subevent_graph <- renderPlot({
    filtered_yemen_data() %>%
      ungroup() %>%
      group_by(event_type, sub_event_type) %>%
      summarize(total_fatalities = sum(fatalities, na.rm = TRUE)) %>%
      filter(total_fatalities > 0) %>%
      ggplot(aes(sub_event_type, total_fatalities, fill = event_type)) +
      geom_col() +
      labs(y = "Total Fatalities", x = "Sub-Event Type", 
           title = "Fatalities by Event Type and Subtype", fill = "Event Type") +
      theme(text = element_text(family = "serif", size = 18),
            plot.title = element_text(size = 22),
            axis.text.x = element_text(angle = 70, hjust = 1)) +
      scale_fill_manual(values = my_palette)
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

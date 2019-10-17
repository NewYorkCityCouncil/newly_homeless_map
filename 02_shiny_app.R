library(shiny)
library(leaflet)
library(dplyr)
library(sf)

df <- read_sf('geo_hom/geo.shp') %>% 
  st_transform('+proj=longlat +datum=WGS84') %>% 
  mutate(rprt_dt = as.POSIXct(rprt_dt))
df$cas_typ <- gsub('_', ' ', df$cas_typ)


# Define UI for application that shows a map
ui <- fluidPage(
  
  dashboardPage(
    dashboardHeader(title = 'Newly Homeless'),
    dashboardSidebar(
      sidebarMenu(
        shinyWidgets::sliderTextInput(inputId = "date",
                                      label = "Date",
                                      choices = unique(df$rprt_dt),
                                      animate = animationOptions(interval = 1400,
                                                                 playButton = HTML("<h3>Play</h3>"))),
        
        # Input: select metric
        radioButtons("fam_radio", label = h3("Family Category"),
                     choices = "",
                     selected = "Single Adult"),
        # Input: select metric
        radioButtons("m_radio", label = h3("Measure"),
                     choices = "",
                     selected = "Individual")
      )
    ),
  # Main panel for Output
    dashboardBody(
          fluidRow(box(
            leafletOutput("map"),
            tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
            width = 18
          )
          )
    )
  )
)


server <- function(input, output, session){
  
  fam_radio_choices = unique(df$cas_typ)
  m_radio_choices = unique(df$measure)
  
  pal = colorFactor(palette = "Oranges", domain = df$count)
  
  # initial checkbox values
  observe ({
    updateRadioButtons(session, "fam_radio",
                             choices = fam_radio_choices)
    updateRadioButtons(session, "m_radio",
                       choices = m_radio_choices)
  })
  
  dfm = reactive({
    df %>% 
      filter(as.character(rprt_dt) %in% input$date) %>% 
      filter(cas_typ %in% input$fam_radio) %>% 
      filter(measure %in% input$m_radio)
    
  })
  
  
  # show map using leaflet
    output$map = renderLeaflet({
      leaflet(df) %>%
        addProviderTiles("CartoDB.Positron") %>% 
        setView(-74.00, 40.71, zoom = 11)
      })
    
    observe({
        proxy = leafletProxy("map", data = dfm())

        proxy %>%
        addPolygons(weight = 1,
                    fillOpacity = .5,
                    color = "#777777",
                    fillColor = pal(dfm()$count),
                    popup = paste("cases:", dfm()$count))
        proxy %>% addLegend(position = "bottomright",
                            pal = pal, values = ~count)
  })
}


shinyApp(ui, server)

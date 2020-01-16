library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(tidyverse)

df <- read_sf('geo_hom/geo.shp') %>% 
  st_transform('+proj=longlat +datum=WGS84') %>% 
  mutate(rprt_dt = as.POSIXct(rprt_dt))
df$cas_typ <- gsub('_', ' ', df$cas_typ)
df$measure <- stringr::str_to_title(df$measure)


# Define UI for application that shows a map
ui <- fluidPage(
  
  dashboardPage(
    dashboardHeader(title = 'Newly Homeless'),
    dashboardSidebar(
      sidebarMenu(
        # Input: select Family Type
        checkboxGroupInput("fam_check", label = h3("Family Type"),
                     choices = ""),
        # Input: select Number Of
        hr(),
        radioButtons("m_radio", label = h3("Number of..."),
                     choices = "",
                     selected = "Individuals"),
        hr(),
        shinyWidgets::sliderTextInput(inputId = "date",
                                      label = "Date",
                                      choices = unique(df[order(df$rprt_dt),]$rprt_dt),
                                      animate = animationOptions(interval = 1400,
                                                                 playButton = HTML("<h3>Play</h3>")))
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
  
  fam_check_choices = unique(df$cas_typ)
  m_radio_choices = unique(df$measure)

  
  
  # initial checkbox values
  observe ({
    updateCheckboxGroupInput(session, "fam_check",
                             choices = fam_check_choices)
    updateRadioButtons(session, "m_radio",
                       choices = m_radio_choices)
  })
  
  dfm = reactive({
    # tmp <- 
      df %>% 
      filter(as.character(rprt_dt) %in% input$date) %>% 
      filter(cas_typ %in% input$fam_check) %>% 
      filter(measure %in% input$m_radio)
    # agg <- aggregate(tmp$count,
    #                  by = list(puma = puma,
    #                            cd = cmmnty_,
    #                            rprt_dt = rprt_dt,
    #                            measure = measure),
    #                  fun(x){sum(x)}) %>% 
    #   rename(count = x)
    # tmp_geo <- left_join(agg, tmp[,c('puma', 'geometry')])
    # return(tmp_geo)
  })
  
  colorpal <- reactive({
    colorBin(palette = "Oranges", domain = dfm()$count, bins = 7)
  })
  
  
  
  # show map using leaflet
    output$map = renderLeaflet({
      leaflet(df) %>%
        addProviderTiles("CartoDB.Positron") %>% 
        setView(-74.00, 40.71, zoom = 11) #%>% 
      })
    
    observe({
        proxy = leafletProxy("map", data = dfm())
        
        pal <- colorpal()

        proxy %>%
        addPolygons(weight = 1,
                    fillOpacity = .5,
                    color = "#777777",
                    fillColor = ~pal(count),
                    popup = paste('Puma:', dfm()$puma, '<br>',
                                  "Count:", dfm()$count))
       proxy %>%
         clearControls() %>% 
         addLegend(position = "bottomright",
                   pal = colorpal(),
                   values = dfm()$count)
          
  })
}


shinyApp(ui, server)



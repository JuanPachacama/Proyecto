library(shiny)
library(readr)
library(tidyverse)
library(rgdal)
library(leaflet)
library(plotly)
# Datos Tabla
dataset <- read_delim("Data/nacimientos2015_2.csv", 
                      ";", escape_double = FALSE, 
                      trim_ws = TRUE,locale = locale(encoding = "ISO-8859-1")
                     ) 
load(file = "Data/Datos.RData")
#Inputs Iniciales 



# Datos Mapa
shppro <- readOGR(dsn='shp', layer='provincias', stringsAsFactors=FALSE)
shppro$CODPRO <- as.integer(shppro$CODPRO)
proy <- CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')
shppro <- sp::spTransform(shppro,proy)


#!!!!!!!!!!!!!!!!  INTERFAZ DE USUARIO  !!!!!!!!!!!!!!!!!!!
ui <- fluidPage(
  tags$head(tags$link(rel='stylesheet', type='text/css', href='estilo.css'),
  tags$link(rel='shortcut icon', href='icono.ico', type='image/x-icon')),
  #Titulos ------------------
  h1("Proyecto Final",align="center"),
  h4("Juan Pachacama",align="center"),
  h4("(Mayo, 2018)",align="center"),
  # Panel Lateral -----------
  sidebarPanel(width=4,
               selectInput(inputId = "provincia",
                           label = "Selecciona Provincia",
                           choices = unique(dataset$PROVINCIA)),
               selectInput(inputId = "canton",
                           label = "Selecciona Canton",
                           choices = unique(dataset$CANTON)),
               sliderInput(inputId = "bins",
                           label="Numero de Bins:",
                           min = 5,
                           max =25,
                           value = 10),
               plotlyOutput("grafico",height = '280px')
               
  ),
  # Panel Principal ---------
  mainPanel(
    titlePanel("Nacimientos en Ecuador (2015)"
    ),
    tabsetPanel(
      tabPanel("Tabla Resumen",
               #Tabla de Nacimientos
               dataTableOutput("tabla")   
               
      ),
      tabPanel("Mapa de Ecuador",
               #Mapa de Ecuador
               leafletOutput("mapa")
               
      )
      
    )
  )
  
)


#!!!!!!!!!!!!!!!!  SERVIDOR  !!!!!!!!!!!!!!!!!!!

server <- function(input, output, session) {
  #Reactividad de Inputs
  observe({
    
    updateSelectInput(session=session,
                      inputId = "canton",
                      choices = unique(dataset$CANTON[dataset$PROVINCIA==input$provincia]))
    
  })
  
  #Tabla Nacimientos
  output$tabla = renderDataTable(expr = {
    
    dataset %>% filter(PROVINCIA==input$provincia,CANTON==input$canton)
    
  })
  
  # Plot : Grafico ----------
  output$grafico = renderPlotly(expr = {
    dat_graf = dataset %>% filter(PROVINCIA==input$provincia,CANTON==input$canton) %>% 
      select(TOTAL)
    dat_graf = unlist(dat_graf)
    plot_ly(x = dat_graf, type = "histogram",nbinsx=input$bins)
    
  })
  
  
  
  
  #Mapa   ---------------
  output$mapa = renderLeaflet(expr = {
    paleta_colores <- colorBin(c("red","yellow","green"), datos_mapa$Porcentaje, 5)
    clic <- paste0('<strong>Provincia: </strong>',
                   datos_mapa$PROVINCIA,
                   '<br><strong>Nacimientos: </strong>',
                   datos_mapa$Nacimientos,
                   '<br><strong>Porcentaje: </strong>',
                   datos_mapa$Porcentaje)
    
    leaflet(shppro) %>%
      addTiles(group = 'Mapa Base',
               options = tileOptions(opacity = 1)) %>%
      setView(-83, -1.4, zoom = 6) %>% 
      addPolygons(layerId = shppro$CODPRO,
                  popup = clic,
                  #label = over,
                  color='#000000',
                  fillOpacity = 0.7,
                  fillColor = paleta_colores(datos_mapa$Porcentaje)) %>% 
      addLegend('bottomright',
                pal = paleta_colores,
                values = ~datos_mapa$Porcentaje,
                title = 'Nacimientos por provincia (%)',
                opacity = 1)
  })

}


#!!!!!!!!!!!!!!!!  RUN APP  !!!!!!!!!!!!!!!!!!!
shinyApp(ui, server)



library(shiny)
library(leaflet)
library(readxl)
library(ggplot2)

ui <- fluidPage(
  
  titlePanel(title = "DCs and Customers locations Dashboard"),
  
  sidebarLayout(sidebarPanel(
    fileInput('file1', 'Choose xlsx file with DCs data',
              accept = c(".xlsx")),
    fileInput('file2', 'Choose xlsx file with Customer data',
              accept = c(".xlsx")), 
    plotOutput("plot2")
  ),
  
  mainPanel(
    
    leafletOutput("mymap"),
    fluidRow(
      splitLayout(cellWidths = c("50%", "50%"), tableOutput('file1'), tableOutput('file2'))
    )
    #tableOutput('file1'),
    #tableOutput('file2')
    
  )
  )
)

server <- function(input, output, session) {
  
  output$file2 <- renderTable({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    readxl::read_excel(inFile$datapath)
  })
  
  output$file1 <- renderTable({
    inFile <- input$file2
    if (is.null(inFile))
      return(NULL)
    readxl::read_excel(inFile$datapath)
  })
  
  m <- leaflet() %>%
    setView(lng = 60,
            lat = 55,
            zoom = 4) %>%
    addProviderTiles(providers$CartoDB.Positron)
  
  output$mymap <- renderLeaflet(m)
  
  observe({
    req(c(input$file1,input$file2))
    inFile <- input$file1
    inFile2 <- input$file2
    if (is.null(inFile))
      return(NULL)
    if (is.null(inFile2))
      return(NULL)
    
    exceldata <- readxl::read_excel(inFile$datapath)
    df = data.frame(exceldata)
    exceldata2 <- readxl::read_excel(inFile2$datapath)
    df2 = data.frame(exceldata2)
    proxy <- leafletProxy("mymap", data = df, session)
    proxy %>% addProviderTiles(providers$Esri.NatGeoWorldMap, group= "NatGeo")
    proxy %>% addProviderTiles(providers$OpenTopoMap, group= "TopoMap") 
    proxy %>% addProviderTiles(providers$OpenRailwayMap, group= "Rail") 
    proxy %>% addProviderTiles(providers$Esri.WorldImagery, group="Satellite") 
    proxy %>% addProviderTiles("NASAGIBS.ViirsEarthAtNight2012", group="NASA")  
    proxy %>% addCircles(df2$long,df2$lat, radius = (df2$demand)/30, popup = df2$city, color = "green")
    
    proxy %>% addMarkers(df$long, df$lat, group= df$group, popup = df$ID)
    proxy %>% addLayersControl(baseGroups=c("Drawing", "Satellite", "NatGeo", "TopoMap", "NASA", "Rail"),
                               overlayGroups=c("1"),
                               options=layersControlOptions(collapsed=FALSE))
    
    output$plot2<-renderPlot(
      ggplot(df2) +
        aes(x = reorder(city,demand), y = demand) +
        geom_col(fill = "red") +
        labs(
          x = "City",
          y = "Demand",
          title = "Demand distribution",
          subtitle = "30 cities",
          caption = "data from anyLogistix"
        ) +
        coord_flip() +
        theme_minimal()
    )
    
  })
}
shinyApp(ui = ui, server = server)
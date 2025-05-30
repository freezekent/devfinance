library(shiny)
library(tidyverse)
library(data.table)
library(leaflet)
library(bslib)

of <- fread("of.csv")

ui <- page_fluid(
  # Application title
  titlePanel("Official Development Finance"),
  navset_pill( 
    nav_panel("Global Map", 
              # Sidebar with a slider input for number of bins 
              sidebarLayout(
                sidebarPanel(
                  selectInput("funder",
                              "Funding Country or Group",
                              unique(of$donor_display_short)),
                  selectInput("year",
                              "Year",
                              unique(of$year))
                ),
                
                # Show a plot of the generated distribution
                mainPanel(
                  leafletOutput("worldmap", height=800)
                ))), 
    nav_panel("Trends Over Time", "Page B content"), 
    nav_panel("Inflows by Country", "Page C content")
  ), 
  id = "tab" 
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$worldmap <- renderLeaflet({
    
    ofsmallnw <- of %>%
      filter(year==input$year, donor_display_short==input$funder) %>%
      group_by(recipient_display_short, year) %>%
      summarize(totalaid = sum(commitdef, na.rm = T)) %>%
      ungroup()
    
    ofsmall <- world %>%
      left_join(ofsmallnw, by = c("name_long" = "recipient_display_short"))
    
    maplbl = paste(round(ofsmall$totalaid, 3))
    
    mypalette <- colorQuantile(palette="plasma", domain=ofsmall$totalaid, na.color="transparent", n = 5)
    
    mypal_colors <- unique(mypalette(sort(ofsmall$totalaid)))
    mypal_labs <- round(quantile(ofsmall$totalaid, seq(0, 1, 0.2), na.rm=T), 2)
    mypal_labs <- paste(lag(mypal_labs), mypal_labs, sep = " - ")[-1]
    
    leaflet(ofsmall) %>%
      addTiles() %>%
      setView(lat=10, lng=0, zoom=2) %>%
      addPolygons( fillColor = ~mypalette(totalaid), 
                   stroke = FALSE,
                   fillOpacity = 0.5,
                   highlightOptions = highlightOptions(
                     weight = 3,
                     color = "#666",
                     fillOpacity = 1,
                     bringToFront = TRUE),
                   label = maplbl) %>%
      addLegend(colors = mypal_colors, labels = mypal_labs, opacity = 0.5, title = NULL, position = "bottomright")
    
    
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

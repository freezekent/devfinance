library(shiny)
library(tidyverse)
library(data.table)
library(leaflet)
library(plotly)
library(bslib)
library(spData)
library(sf)

of <- readRDS("of.rds")

of$year <- as.integer(of$year)


ui <- page_fluid(
  # Application title
  titlePanel("Official Development Finance"),
              sidebarLayout(
                sidebarPanel(
                  selectInput("funder",
                              "Funding Country, Multilateral Organization, or NGO",
                              unique(of$donor_display_short),
                              selected = "World"),
                  selectInput("recipient",
                              "Recipient Country or Region",
                              unique(of$recipient_display_short),
                              selected = "World",
                              multiple = T),
                  sliderInput("year",
                              "Years of Data",
                              min = min(of$year, na.rm = T),
                              max = max(of$year, na.rm = T),
                              value = c(min(of$year), max(of$year)),
                              round = T,
                              ticks = F,
                              sep = ""),
                  selectInput(
                    "flow",
                    "Type of Funding",
                    c("All", unique(of$flow)), 
                    selected = "All",
                    multiple = T),
                  selectInput(
                    "topsec",
                    "Top Sector",
                    c("All", unique(of$topsec)),
                    selected = "All",
                    multiple = T),
                  selectInput(
                    "sectorname",
                    "Sub-Sector",
                    c("All", unique(of$sectorname)),
                    selected = "All",
                    multiple = T)
                ),
                mainPanel(
                  navset_pill( 
                    nav_panel("Global Map",  
                  leafletOutput("worldmap", height=800)
                ), 
    nav_panel("Trends Over Time", 
              plotlyOutput("linechart", height=800))
  )))
)


server <- function(input, output) {
  
  output$worldmap <- renderLeaflet({
    
    if (input$flow == "All") {
      flowpick <- unique(of$flow)
    } else {
      flowpick <- input$flow
    }
    
    if (input$topsec == "All") {
      topcat <- unique(of$topsec)
    } else {
      topcat <- input$topsec
    }
    
    if (input$sectorname == "All") {
      sectpick <- unique(of$sectorname)
    } else {
      sectpick <- input$sectorname
    }
    
    ofsmallnw <- of %>%
      filter(recipientagg==0 & year %inrange% input$year & donor_display_short==input$funder 
             & flow %in% flowpick & topsec %in% topcat
             & sectorname %in% sectpick) %>%
      group_by(recipient_isocc) %>%
      summarize(totalaid = sum(commit, na.rm = T)) %>%
      ungroup() %>%
      mutate(amtlbl = ifelse(totalaid > 999999999, paste0("$", round(totalaid/1000000000, 2), " Billion"), 
                       ifelse(totalaid < 999999999 & totalaid > 999999, paste0("$", round(totalaid/1000000, 2), " Million"),
                        ifelse(totalaid < 999999 & totalaid >1000, paste0("$", round(totalaid/1000, 2), " Thousand"),
                               paste0("$", round(totalaid, 0))))))
    
    ofsmall <- world %>%
      left_join(ofsmallnw, by = c("iso_a2" = "recipient_isocc"))
    
    maplbl = ofsmall$amtlbl
    
    mypalette <- colorQuantile(palette="Blues", domain=ofsmall$totalaid, na.color="lightgray", n = 5)
    
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
  
  output$linechart <- renderPlotly({
    
    if (input$flow == "All") {
      flowpick <- unique(of$flow)
    } else {
      flowpick <- input$flow
    }
    
    if (input$topsec == "All") {
      topcat <- unique(of$topsec)
    } else {
      topcat <- input$topsec
    }
    
    if (input$sectorname == "All") {
      sectpick <- unique(of$sectorname)
    } else {
      sectpick <- input$sectorname
    }
    
    ofsmallline <- of %>%
      filter(year %inrange% input$year & donor_display_short==input$funder 
             & recipient_display_short %in% input$recipient & flow %in% flowpick & topsec %in% topcat
             & sectorname %in% sectpick) %>%
      group_by(year) %>%
      summarize(totalaid = sum(commit, na.rm = T)) %>%
      ungroup() %>%
      mutate(amtlbl = ifelse(totalaid > 999999999, paste0("$", round(totalaid/1000000000, 2), " Billion"), 
                             ifelse(totalaid < 999999999 & totalaid > 999999, paste0("$", round(totalaid/1000000, 2), " Million"),
                                    ifelse(totalaid < 999999 & totalaid >1000, paste0("$", round(totalaid/1000, 2), " Thousand"),
                                           paste0("$", round(totalaid, 0))))))
    
    plot_ly(
      ofsmallline,
      x = ~year,
      y = ~totalaid,
      type = "scatter",
      mode = "lines",
      showlegend = F
    ) %>%
      layout(
        xaxis = list(title = 'Year'),
        yaxis = list (title = 'Total Development Assistance')) 
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

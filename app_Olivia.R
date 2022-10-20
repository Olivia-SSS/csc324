#load packages
library(readr)
library(dplyr)
library(tidyr)
library(leaflet)
library(geojsonio)
library(shiny)
library(ggplot2)
library(factoextra)
library(shinythemes)
library(caret)
library(data.table) 
library(shinyWidgets)




# per.capita <- read.csv("~/Desktop/CSC324/Energy Consumption/per-capita-energy-use.csv")
# per.capita <- mutate(per.capita, energy <- round(Energy))
# 
# countrylist <- read.csv("https://pkgstore.datahub.io/core/country-list/data_csv/data/d7c9d7cfb42cb69f4422dec222dbbaa8/data_csv.csv")
# country.change <- read.csv("~/Desktop/CSC324/Energy Consumption/change-energy-consumption.csv")
# country.change[, 4] <- round(country.change[, 4], digits = 2)
# country.change1 <- filter(country.change, Entity %in% countrylist$Name)
# 
# 
# source <- read.csv("~/Desktop/CSC324/Energy Consumption/source.csv")
# WorldCountry <- geojson_read("https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json", what = "sp")
# #Load data for countries' longitudes and latitudes
# location<-read.csv("https://developers.google.com/public-data/docs/canonical/countries_csv")
# colnames(location)[4] <- "Entity"
# #Add countries' longitudes and latitudes to Covid data
# per.capita<-full_join(per.capita,location, by= "Entity")


per.capita <- read.csv("per-capita-energy-use.csv")
per.capita <- mutate(per.capita, energy <- round(Energy))

countrylist <- read.csv("https://pkgstore.datahub.io/core/country-list/data_csv/data/d7c9d7cfb42cb69f4422dec222dbbaa8/data_csv.csv")
country.change <- read.csv("change-energy-consumption.csv")
country.change[, 4] <- round(country.change[, 4], digits = 2)
country.change1 <- filter(country.change, Entity %in% countrylist$Name)


source <- read.csv("source.csv")
WorldCountry <- geojson_read("https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json", what = "sp")


ui <- bootstrapPage(
  navbarPage(
    theme = shinytheme ("flatly"),
    "Energy Consumption",
    tabPanel("Overview of Energy Consumption",
      div(
        class="outer",
        tags$head(includeCSS("style.css")),
        leafletOutput("percapita", width="100%", height="100%"),
        absolutePanel(id = "controls", class = "panel panel-default",
                      top = 75, left = 55, width = 250, fixed=TRUE,
                      draggable = TRUE, height = "auto",
                      span(tags$i(h5("Use slider to get overview of energy consumption in each year")), style="color:#045a8d"),
          
            sliderInput("year", 
                        label = h5("Select mapping year"),
                        min = 1980, max = 2021,
                        value = 2000),
          ),
        )
      ),
     
    #end of tab 1
    tabPanel("Changes over years",
             
             sidebarLayout(
               sidebarPanel(
                 
                 span(tags$i(h5("The plot on the right shows changes of energy concumption in selected regions over years")), style="color:#045a8d"),

                 pickerInput("level_select", "Level:",   
                             choices = c( "Continent", "Country"), 
                             selected = c("Country"),
                             multiple = FALSE),
                 
                 pickerInput("region_select", "Country/Region:",   
                             choices = unique(country.change1$Entity), 
                             options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                             selected = unique(country.change1$Entity)[1:10],
                             multiple = TRUE), 
                 sliderInput("minimum_year",
                             "Minimum year:",
                             min = 1980,
                             max = 2021,
                             value= 2000),
                 "Select level, regions, and minimum year to update plots."
               ),
               
               mainPanel(
                  plotOutput("country_plot_cumulative"))
                 
               )
             ), # end of tab 2
    tabPanel("Sources of Energy",
             
             sidebarLayout(
               sidebarPanel(
                 
                 span(tags$i(h5("The plot on the right shows concumption of different sources of energy over years")), style="color:#045a8d"),
    
                 pickerInput("source_select1", "Sources",   
                             choices = unique(source$Source), 
                             options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                             selected = unique(source$Source),
                             multiple = TRUE), 
                 sliderInput("minimum_year1",
                             "Minimum year:",
                             min = 1800,
                             max = 2021,
                             value= 1910),
                 "Select source and minimum year to update plots."
               ),
               
               mainPanel(
                 plotOutput("source_plot")))),
               
             
    
    
    tabPanel("About this site",
             tags$div(
               tags$h4("Background"), 
               "In contemporary society, there are various of environmental problems that are directly or indirectly resulted from 
               increasing energy consumption. These problems include air pollution, climate change, water pollution and so on. 
               Among these problems, climate change is one of the most serious problems that people cannot ignore in 2022. 
               According to UN news, it is now or never to limit global warming. Climate change is not only affecting people’s life 
               by aggravating extreme weather events, which will further harm food production and human health, or even raising the 
               tax since government may need more money to build constructions that can reduce the damage brought by extreme weather events;
               but also will be detrimental to the whole ecosystem. It is also mentioned in UN news that the decision we make now can secure a
               livable future. And it is mentioned almost every authoritative environmental organization’s article that reducing the energy 
               consumption will be a viable method.Thus, paying more attention to energy consumption in countries and areas world widely is important and this is also why this app
               is designed and developed.",
               tags$br(),tags$br(),
               tags$h4("How to use this app"), 
               "In the first tab of this app, visualization of energy consumption per capita is provided. Users can select the 
               year that they are interested in to interact with the map. In the second tab, users can choose the continent(s) or countries that they are 
               interested in and adjust the minimum year slider to view how energy consumption in the selected regions has changed over different years till
               now. In the third tab, users can choose different sources and different minimum year to view the visualizations of how the consumption of 
               different sources has changed.",
               tags$br(),tags$br(),
               img(src = "earth.png", height = 300, width = 300),
               tags$br(),tags$br(),tags$br(),
               tags$h4("Design Process"),
               tags$h5("Wireframes"),
               tags$br(),tags$br(),
               img(src = "design1.png", height = 900, width = 700),
               tags$br(),tags$br(),
               img(src = "design2.png", height = 900, width = 700),
               tags$br(),tags$br(),
               "I first drew out the wireframe and then built a screen mockup, which is a digital version of my wireframe. Then I started to
               write code for the RShiny app. During this process, I used the Essence of Software book for reference to make sure this app provides an efficient and enjoyable experience from users' perspectives. Also, I employed Dimensions 
               of Evaluation for User-System Performance from SYSTEM DESIGN as evaluation criteria for refinement of this app. In addition, user testing is involved in 
               the design and development of this app to ensure better performance.",
               
               tags$br(),tags$br(),
               tags$br(),
               tags$h4("References"),
               "SYSTEM DESIGN: http://www.chilton-computing.org.uk/acd/literature/books/mi/p04.htm#c4p4",
               tags$br(),
               "The Essence of Software: Why Concepts Matter for Great Design: Jackson. (2021). The Essence of Software: Why Concepts Matter for Great Design. Princeton University Press.
",
               tags$br(),
               "Code Repository for Reference: ",
               "https://github.com/eparker12/nCoV_tracker/blob/master/app.R",
               tags$br(),
               "Data Source 1: https://ourworldindata.org/energy-production-consumption",
               tags$br(),
               "Data Source 2: https://pkgstore.datahub.io/core/country-list/data_csv/data/d7c9d7cfb42cb69f4422dec222dbbaa8/data_csv.csv",
               tags$br(),
               "Image by",
               tags$a(href="https://www.freepik.com/free-vector/ecosystem-pollution-design-flat-style_2730078.htm#query=pollution&position=5&from_view=keyword", "Freepik"),
               tags$br(),tags$br(),tags$br(),
               tags$h4("Acknowledgement"),
               "Thanks for professor Eliott's help and advice on this app.",
               tags$br(),tags$br(),tags$br(),tags$br(),tags$br()
               
             )
            
    )
    ))
 

server <- function(input, output, session) {
  ## tab 1
  data1 <- reactive({
    left_join(
      data.frame(id = WorldCountry$id),
      filter(per.capita, Year == input$year),
      by = c("id" = "Code")
    )
  })
  output$percapita <- renderLeaflet({
    pal <- colorNumeric("Reds", domain = data1()$Engery)
    leaflet(WorldCountry) %>%
      addTiles() %>%
      addPolygons(
        fillColor = pal(data1()$energy),
        weight = 1,
        opacity = 1,
        color = "white",
        fillOpacity = 0.5,
        highlight = highlightOptions(
          weight = 2,
          color = "red",
          fillOpacity = 0.7
        ),
        popup = paste(
          "<strong>",
          "Country:",
          data1()$Entity,
          "</strong>",
          "<br/>",
          "Energy Consumption per capita:",
          data1()$energy,
          "(TWh)"
        )
      )%>%
      # addLegend(
      #   pal = pal,
      #   values = data1()$Energy,
      #   title = "Energy Consumption per capita",
      #   position = "bottomright"
      # )
      fitBounds(~-100,-60,~60,80) %>%
      # addCircleMarkers(data = data1(), lat = ~ Latitude, lng = ~ Longitude, weight = 1, radius = ~(energy)^(1/1000), 
      #                  fillOpacity = 0.2, color ="Red") %>%
    addLegend("bottomright", pal = pal, values = data1()$energy,
              title = "<small>Energy Consumption per capita</small>") 
  })
  
  ## tab 2
  # update region selections
  observeEvent(input$level_select, {
    if (input$level_select=="Continent") {
      updatePickerInput(session = session, inputId = "region_select", 
                        choices = c("Africa", "Asia", "Europe", "North America", "South America"), 
                        selected = c("Africa", "Asia", "Europe", "North America", "South America"))
    }
    if (input$level_select=="Country") {
      updatePickerInput(session = session, inputId = "region_select", 
                        choices = unique(country.change1$Entity), 
                        selected = unique(country.change1$Entity)[1:10])
    }
  }, ignoreInit = TRUE)
  
  # create dataframe with selected countries
  country_reactive <- reactive({
    country.change %>% filter(Entity %in% input$region_select) %>% filter(Year > input$minimum_year)
  })
  
  
  # country-specific plots
  output$country_plot_cumulative = renderPlot({
    ggplot(country_reactive()) +
      geom_line(mapping = aes(x = Year, y = Change, colour = Entity)) + 
      labs (x = "Years", y = "Changes of energy consumed(%)", title = "Energy Consumption Changes") + 
      scale_colour_discrete(name = "Country")
  }, height = 700, width = 950)
  
  ## tab 3
  source_reactive <- reactive ({
    source %>% filter(Year > input$minimum_year1) %>% filter(Source %in% input$source_select1)
  })
  
  # country-specific plots
  output$source_plot = renderPlot({
    ggplot(source_reactive()) +
      geom_line(mapping = aes(x = Year, y = Amount, colour = Source)) + 
      labs (x = "Years", y = "Selected Source of Energy Consumed(TWh)", title = "Sources of Energy") + 
      scale_colour_discrete(name = "Source")
  }, height = 700, width = 950)
  
}

shinyApp(ui=ui, server=server)

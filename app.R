library(shinydashboard)
library(shiny)
library(dplyr)
library(tm)
library(stringr)
library(tidyr)
library(leaflet)
library(leaflet.extras)
library(plotrix)
library(ggplot2)
library(wordcloud)
library(magrittr)

listings_amsterdam <- read.csv(file='data_cleansed/amsterdam/2020-09-09/listings.csv', header=TRUE)
listings_amsterdam <- subset(listings_amsterdam, city!="city")

listings_barca <- read.csv(file='data_cleansed/barcelona/2020-09-12/listings.csv', header=TRUE)
listings_barca <- subset(listings_barca, city!="city")

listings_bordeaux <- read.csv(file='data_cleansed/bordeaux/2020-09-19/listings.csv', header=TRUE)
listings_bordeaux <- subset(listings_bordeaux, city!="city")

listings_all_cities <- read.csv(file='data_cleansed/all_cities.csv')
listings_all_cities <- subset(listings_all_cities, city!="city")

amsterdam_top3<-tail(names(sort(table(listings_amsterdam$neighbourhood_cleansed))), 3)

barca_top3<-tail(names(sort(table(listings_barca$neighbourhood_cleansed))), 3)

bordeaux_top3<-tail(names(sort(table(listings_bordeaux$neighbourhood_cleansed))), 3)

## app.R ##
server <- function(input, output) {
  
  output$date_am <- renderPrint({ input$select_date_am })
  output$date_ba <- renderPrint({ input$select_date_ba })
  output$date_bo <- renderPrint({ input$select_date_bo })
  
  output$distPlot <- renderPlot({
    hist(rnorm(input$obs), col = 'darkgray', border = 'white')
  })
  output$mymap_am <- renderLeaflet({
    mymap_am<-leaflet() 
    mymap_am<-addTiles(mymap_am)  # Add default OpenStreetMap map tiles
    mymap_am<-addCircleMarkers(map=mymap_am, lng=as.numeric(listings_amsterdam_map()$longitude), lat=as.numeric(listings_amsterdam_map()$latitude), radius = 1, label = listings_amsterdam_map()$price)
  })
  
  output$mymap_ba <- renderLeaflet({
    mymap_ba<-leaflet() 
    mymap_ba<-addTiles(mymap_ba)  # Add default OpenStreetMap map tiles
    mymap_ba<-addCircleMarkers(map=mymap_ba, lng=as.numeric(listings_barca_map()$longitude), lat=as.numeric(listings_barca_map()$latitude), radius = 1, label = listings_barca_map()$price)
  })
  
  output$mymap_bo <- renderLeaflet({
    mymap_bo<-leaflet() 
    mymap_bo<-addTiles(mymap_bo)  # Add default OpenStreetMap map tiles
    mymap_bo<-addCircleMarkers(map=mymap_bo, lng=as.numeric(listings_bordeaux_map()$longitude), lat=as.numeric(listings_bordeaux_map()$latitude), radius = 1, label = listings_bordeaux_map()$price)
  })
  
  listings_amsterdam_map <- reactive({
    if(input$select_room1=="Any" )
    {
      listings_amsterdam <- subset(listings_amsterdam, between(beds, input$slider_beds1[1], input$slider_beds1[2])
                                   & data_date==input$select_date_am
      )
    }
    else{
      listings_amsterdam <- subset(listings_amsterdam,room_type==input$select_room1
                                   & between(beds, input$slider_beds1[1], input$slider_beds1[2])
                                   & data_date==input$select_date_am
      )
    }
    
  })
  
  listings_barca_map <- reactive({
    if(input$select_room2=="Any" )
    {
      listings_barca <- subset(listings_barca, between(beds, input$slider_beds2[1], input$slider_beds2[2])
                               & data_date==input$select_date_ba
      )
    }
    else{
      listings_barca <- subset(listings_barca,room_type==input$select_room2
                               & between(beds, input$slider_beds2[1], input$slider_beds2[2])
                               &data_date==input$select_date_ba
      )
    }
    
  })
  
  listings_bordeaux_map <- reactive({
    if(input$select_room3=="Any" )
    {
      listings_bordeaux <- subset(listings_bordeaux, between(beds, input$slider_beds3[1], input$slider_beds3[2])
                                  &data_date==input$select_date_bo
      )
    }
    else{
      listings_bordeaux <- subset(listings_bordeaux,room_type==input$select_room3
                                  & between(beds, input$slider_beds3[1], input$slider_beds3[2])
                                  & data_date==input$select_date_bo
      )
    }
    
  })
  
  listing_amsterdam_ava <- reactive({
    listings_amsterdam <- subset(listings_amsterdam, availability_30>=input$selected_min_nights1
                                 & data_date==input$select_date_am
                                 & between(price, input$selected_price_2_1[1], input$selected_price_2_1[2])
                                 )
  })
  
  output$plot1 <- renderPlot({
    ggplot(listing_amsterdam_ava(), aes(bedrooms)) + 
      geom_bar(aes(fill=room_type), position = position_dodge())
  })
  
  
  listing_barca_ava <- reactive({
    listings_barca <- subset(listings_barca, availability_30>=input$selected_min_nights2
                             & data_date==input$select_date_ba
                             & between(price, input$selected_price_2_2[1], input$selected_price_2_2[2])
                             )
  })
  
  output$plot2 <- renderPlot({
    ggplot(listing_barca_ava(), aes(bedrooms)) + 
      geom_bar(aes(fill=room_type), position = position_dodge())
  })
  
  
  listing_bordeaux_ava <- reactive({
    listings_bordeaux <- subset(listings_bordeaux, availability_30>=input$selected_min_nights3
                                & data_date==input$select_date_bo
                                & between(price, input$selected_price_2_3[1], input$selected_price_2_3[2])
                                )
  })
  
  output$plot3 <- renderPlot({
    ggplot(listing_bordeaux_ava(), aes(bedrooms)) + 
      geom_bar(aes(fill=room_type), position = position_dodge())
  })
  
  output$plot0_1 <- renderPlot({
    
    #x <- switch(
    #  input$select_feature1,
    #  "room_type"=listings_amsterdam$room_type,
    #  "bedrooms"=listings_amsterdam$bedrooms,
    #  "beds"=listings_amsterdam$beds,
    #  "property_type"=listings_amsterdam$property_type
    #  
    #  )

    listings_city_selected1<-subset(listings_all_cities, city==input$select_city1)
    
    if(input$select_feature=="Room Type")
    {
      x<-listings_city_selected1$room_type
    }
    
    if(input$select_feature=="Property Type")
    {
      x<-listings_city_selected1$property_type
    }
    
    if(input$select_feature=="Availability")
    {
      x<-listings_city_selected1$availability_30
    }
    
    ggplot(listings_city_selected1,aes(x)) +
      geom_bar(aes(fill=x), position = position_dodge())
  }) 
  
  output$plot0_2 <- renderPlot({
    
    listings_city_selected2<-subset(listings_all_cities, city==input$select_city2)

    if(input$select_feature=="Room Type")
    {
      y<-listings_city_selected2$room_type
    }
    
    if(input$select_feature=="Property Type")
    {
      y<-listings_city_selected2$property_type
    }
    
    if(input$select_feature=="Availability")
    {
      y<-listings_city_selected2$availability_30
    }
    
      ggplot(listings_city_selected2,aes(y)) +
        geom_bar(aes(fill=y), position = position_dodge())   
    

  })
  
  
  
}

ui <- dashboardPage(skin = "red",
                    dashboardHeader(title = "Airbnb Project"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Cross-Cities Analysis", tabName = "cross_city", icon = icon("chart-pie")
                        ),
                        menuItem("Amsterdam", tabName = "amsterdam", icon = icon("home")
                        ),
                        menuItem("Barcelona", tabName = "barcelona", icon = icon("home")
                        ),
                        menuItem("Bordeaux", tabName = "bordeaux", icon = icon("home")
                        )
                      )
                    ),
                    
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "cross_city",
                                fluidRow(
                                  box(width=4,
                                  column(10,
                                         
                                         radioButtons(inputId = "select_feature", label = h4("Select feature to compare"), choices=c("Room Type"="Room Type","Property Type"="Property Type","Availability"="Availability"))),
                                        ),
                                
                                
                                hr(),
                                selectInput("select_city1", label = h4("Select one city"), 
                                            c(sort(unique(listings_all_cities[,"city"]))), 
                                            selected = 1),
                                plotOutput('plot0_1'),
                                selectInput("select_city2", label = h4("Select another city"), 
                                            c(sort(unique(listings_all_cities[,"city"]))), 
                                            selected = 1),
                                plotOutput('plot0_2'),
                                ),
                        ),

                        tabItem(tabName = "amsterdam",
                                
                                selectInput("select_date_am", label = h4("Select the date of your trip :"), 
                                            c(sort(unique(listings_amsterdam[,"data_date"]))), 
                                            selected = 1),
                                
                                tabsetPanel(
                                  tabPanel("Map",icon = icon("map"),
                                           hr(),
                                           fluidRow(
                                        leafletOutput("mymap_am"),
                                          
                                      
                                           box(
                                           radioButtons(inputId = "select_room1", label = h4("1 - Select Room Type :"), choices=c(unique(listings_amsterdam$room_type),"Any"="Any")),
                                           hr(),
                                           sliderInput(inputId = "slider_beds1", label = h4("2 - Select Number of Beds :"), min=1, max=20, value=c(1,5))
                                           ),),
                                  ),
                                  
                                  tabPanel("Availability",icon = icon("bar-chart-o"),
                                           fluidRow(
                                             hr(),
                                             plotOutput('plot1'),
                                             hr(),
                                             box(sliderInput("selected_price_2_1",
                                                             "1 - Pick a price range (in $) :",
                                                             min=1, max= 1000, value = c(1, 200)),
                                                 hr(),
                                                 sliderInput("selected_min_nights1",
                                                             "2 - Pick a minimum number of nights available :",
                                                             min = 1, max = 30, value = 5, step=1)
                                             )
                                             
                                           ),        
                                  )
                                  
                                )
                        ),
                        
                        tabItem(tabName = "barcelona",
                                
                                selectInput("select_date_ba", label = h4("Select the date of your trip :"), 
                                            c(sort(unique(listings_barca[,"data_date"]))), 
                                            selected = 1),
                                
                                tabsetPanel(
                                  tabPanel("Map",icon = icon("map"),
                                           hr(),
                                           fluidRow(
                                           
                                             leafletOutput("mymap_ba"),
                                          
                                           hr(),
                                           box(
                                           radioButtons(inputId = "select_room2", label = h4("1 - Select Room Type :"), choices=c(unique(listings_barca$room_type),"Any"="Any")),
                                           hr(),
                                           sliderInput(inputId = "slider_beds2", label = h4("2 - Select Number of Beds :"), min=1, max=20, value=c(1,5))
                                           ),),
                                  ),
                                  
                                  tabPanel("Availability",icon = icon("bar-chart-o"),
                                           fluidRow(
                                             hr(),
                                             plotOutput('plot2'),
                                             hr(),
                                             box(sliderInput("selected_price_2_2",
                                                             "1 - Pick a price range (in $) : ",
                                                             min=1, max= 1000, value = c(1, 200)),
                                                 hr(),
                                                 sliderInput("selected_min_nights2",
                                                             "2 - Pick a minimum number of nights available : ",
                                                             min = 1, max = 30, value = 5, step=1)
                                             )
                                             
                                           ),        
                                  )
                                  
                                )
                        ),
                        
                        tabItem(tabName = "bordeaux",
                                
                                selectInput("select_date_bo", label = h4("Select the date of your trip :"), 
                                            c(sort(unique(listings_bordeaux[,"data_date"]))), 
                                            selected = 1),
                                
                                tabsetPanel(
                                  tabPanel("Map",icon = icon("map"),
                                           hr(),
                                           fluidRow(
                                             leafletOutput("mymap_bo"),
                                           
                                           hr(),
                                           box(
                                           radioButtons(inputId = "select_room3", label = h4("1 - Select Room Type :"), choices=c(unique(listings_bordeaux$room_type),"Any"="Any")),
                                           hr(),
                                           sliderInput(inputId = "slider_beds3", label = h4("2 - Select Number of Beds :"), min=1, max=20, value=c(1,5))
                                           ),),
                                  ),
                                  
                                  tabPanel("Availability",icon = icon("bar-chart-o"),
                                           fluidRow(
                                             hr(),
                                             plotOutput('plot3'),
                                             hr(),
                                             box(sliderInput("selected_price_2_3",
                                                             "1 - Pick a price range (in $) :",
                                                             min=1, max= 1000, value = c(1, 200)),
                                                 hr(),
                                                 sliderInput("selected_min_nights3",
                                                             "2 - Pick a minimum number of nights available :",
                                                             min = 1, max = 30, value = 5, step=1)
                                             )
                                             
                                           ),        
                                  )
                                  
                                  
                                )
                        )
                        
                      )
                    )
)

shinyApp(ui = ui, server = server)


library(shiny)
library(tidyverse)
library(shinythemes)
library(gridExtra)

# User Interface
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  # Title
  titlePanel("White Walker WIPEOUT"),
  
   navbarPage("Winter is Coming",
             
              #Introduction Tab
             tabPanel("Introduction",
                      h1("Winter is Coming"),
                      h2("And Only You Can Stop It"),
                      p("The White Walkers have broken through the wall and are descending on the citizens of Westeros. Only you can can stop the army of the dead, but first you must decide where, how, and with whom you will fight them. Choose your alliances carefully and decide on a location to make your stand. Devise a battle plan and find out what you chances of victory are. Winter is coming..."),
                      h3("Data Summary"),
                      p("Data used in this app was adapted from Chris Albon's War of the Five Kings data set. 
                        The original contains a dataset of the battles in the War of the Five Kingsfrom George R.R. Martin's A Song Of Ice And Fire series.
                        Original data can be found at: https://github.com/chrisalbon/war_of_the_five_kings_dataset")
                      
                      ),
             
             #Exploration and Alliances Tab
             tabPanel("Explore House Statistics and Pick Your Alliances",
                      
                      # Sidebar: Sidebar with 3 house selections for alliance
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("House", 
                                      "Select House 1 for Alliance:",
                                      choices = c("Stark","Lannister","Baratheon","Tully","Greyjoy","Frey","Free folk","Darry","Thenns","Bolton","Brotherhood without Banners","Giants","Brave Companions","Karstark","Mormont","Glover","Tyrell","Blackwood")
                                      ),
                          selectInput("House",
                                      "Select House 2 for Alliance:",
                                      choices = c("Stark","Lannister","Baratheon","Tully","Greyjoy","Frey","Free folk","Darry","Thenns","Bolton","Brotherhood without Banners","Giants","Brave Companions","Karstark","Mormont","Glover","Tyrell","Blackwood")
                                      ),
                          selectInput("House",
                                      "Select House 3 for Alliance",
                                      choices = c("Stark","Lannister","Baratheon","Tully","Greyjoy","Frey","Free folk","Darry","Thenns","Bolton","Brotherhood without Banners","Giants","Brave Companions","Karstark","Mormont","Glover","Tyrell","Blackwood")
                                      )
                                    ),
                        
                        # PLACEHOLDER: Show a plot of the generated distribution
                        mainPanel(
                          plotOutput("distPlot")
                        )
                      )),
             
             #Battle Tab
             tabPanel("Battle",
                      
                      # pLACEHOLDER: Sidebar with a slider input for number of bins 
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("ww_army_size",
                                      "White Walker Army Size",
                                      min = 1,
                                      max = 150000,
                                      value = 100000),
                          sliderInput("living_army_size",
                                      "Alliance Army Size",
                                      min = 1,
                                      max = 150000,
                                      value = 50000)
                          
                                    ),
                        
                        # pLACEHOLDER: Show a plot of the generated distribution
                        mainPanel(
                          plotOutput("scatter")
                        )
                      ))
             
  )
  
)









# pLACEHOLDER: Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = input$color, border = 'white')
  })
  
  output$scatter <- renderPlot({
    
    ggplot(faithful, aes(x = waiting, y = eruptions)) +
      geom_point(color = input$scattercolor) +
      geom_smooth(method = "lm", se = FALSE)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


library(shiny)
library(tidyverse)
library(shinythemes)
library(gridExtra)
battle <- read_csv("battles_updated_2_25_2019.csv")

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
          
                      ,
             
             #Explore Alliances Tab
             tabPanel("Explore Alliances",
                      
                      # Sidebar: Sidebar with 3 house selections for alliance
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("house1_explore", 
                                      "Explore House 1 Battle Stats:",
                                      choices = c("Stark","Lannister","Baratheon","Tully","Greyjoy","Frey","Bolton","Karstark","Mormont","Glover","Tyrell")
                                      ),
                          selectInput("house2_explore",
                                      "Explore House 2 Battle Stats:",
                                      choices = c("Stark","Lannister","Baratheon","Tully","Greyjoy","Frey","Bolton","Karstark","Mormont","Glover","Tyrell")
                                      ),
                          selectInput("house3_explore",
                                      "Explore House 3 Battle Stats",
                                      choices = c("Stark","Lannister","Baratheon","Tully","Greyjoy","Frey","Bolton","Karstark","Mormont","Glover","Tyrell")
                                      )
                                    ),
                        
                        # PLACEHOLDER: Show a plot of the generated distribution
                        mainPanel(
                          plotOutput("battle_type_hist")
                        )
                      )),
             
             #Pick Alliances Tab
             tabPanel("Pick Alliances",
                      
                      # Sidebar: Sidebar with a slider for army sizes and radio buttons for picking alliance 
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
                                      value = 50000),
                          selectInput("house1_pick", 
                                      "Select House 1 for Alliance:",
                                      choices = c("Stark","Lannister","Baratheon","Tully","Greyjoy","Frey","Bolton","Karstark","Mormont","Glover","Tyrell")
                          ),
                          selectInput("house2_pick",
                                      "Select House 2 for Alliance:",
                                      choices = c("Stark","Lannister","Baratheon","Tully","Greyjoy","Frey","Bolton","Karstark","Mormont","Glover","Tyrell")
                          ),
                          selectInput("house3_pick",
                                      "Select House 3 for Alliance",
                                      choices = c("Stark","Lannister","Baratheon","Tully","Greyjoy","Frey","Bolton","Karstark","Mormont","Glover","Tyrell")
                          )
                                    ),
                        
                        # pLACEHOLDER: Show a plot of the generated distribution
                        mainPanel(
                          plotOutput("scatter")
                        )
                      )),
             #Battle Results
             tabPanel("Battle Results",
                      
                      # PLACEHOLDER: Show a plot of the generated distribution
                        mainPanel(
                          plotOutput("battle_type_hist")
                        )
                      ))
             
  )
  










# Server
server <- function(input, output) {
  
  output$battle_type_hist <- renderPlot({
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


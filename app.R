library(shiny)
library(tidyverse)
library(shinythemes)
library(gridExtra)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  # Application title
  titlePanel("White Walker WIPEOUT"),
  
   navbarPage("Winter is Coming",
             
             tabPanel("Introduction",
                      h1("Winter is Coming"),
                      h2("And Only You Can Stop It"),
                      p("The White Walkers have broken through the wall and are descending on the citizens of Westeros. Only you can can stop the army of the dead, but first you must decide where, how, and with whom you will fight them. Choose your alliances carefully and decide on a location to make your stand. Devise a battle plan and find out what you chances of victory are. Winter is coming..."),
                      h3("Data Summary"),
                      p("Data used in this app was adapted from Chris Albon's War of the Five Kings data set. 
                        The original contains a dataset of the battles in the War of the Five Kingsfrom George R.R. Martin's A Song Of Ice And Fire series.
                        Original data can be found at: https://github.com/chrisalbon/war_of_the_five_kings_dataset")
                      
                      ),
             
             tabPanel("Explore House Statistics and Pick Your Alliances",
                      
                      # PLACEHOLDER: Sidebar with a slider input for number of bins 
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("bins", 
                                      "select bins:",
                                      min = 1,
                                      max = 50,
                                      value = 30),
                          
                          selectInput("color", 
                                      "Select histogram color:",
                                      choices = c("purple","blue","orange"))
                        ),
                        
                        # PLACEHOLDER: Show a plot of the generated distribution
                        mainPanel(
                          plotOutput("distPlot")
                        )
                      )),
             
             
             tabPanel("Battle",
                      
                      # pLACEHOLDER: Sidebar with a slider input for number of bins 
                      sidebarLayout(
                        sidebarPanel(
                          
                          radioButtons("scattercolor", 
                                      "Select scatterplot color:",
                                      choices = c("red","blue","gray50"))
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


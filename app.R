library(shiny)
library(tidyverse)
library(shinythemes)
library(shinydashboard)
library(gridExtra)
library(readr)
library(readxl)

#make house_stats data frame
house_stats <- read_excel("house_stats.xlsx", 
                          sheet = "Sheet1")

house_stats$outcome <- as.factor(house_stats$outcome)
house_stats$battle_type <- as.factor(house_stats$battle_type)
house_stats$house <- as.factor(house_stats$house)

#################### User Interface
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
                        Original data can be found at: https://github.com/chrisalbon/war_of_the_five_kings_dataset"),
                      img(src = "winter-is-here.jpg")
                      )
             
             ,
             
             #Explore Alliances Tab
             tabPanel("Explore Alliances",
                        
                      # Sidebar: Sidebar with 3 house selections for alliance
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("house1_explore", 
                                      "Explore House 1 Battle Stats:",
                                      choices = c(Stark="Stark",Lannister="Lannister",Baratheon="Baratheon",Tully="Tully",Greyjoy="Greyjoy",Frey="Frey",Bolton="Bolton",Karstark="Karstark",Mormont="Mormont",Glover="Glover",Tyrell="Tyrell")
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
                          #Fluid row controls the layout in the main panel 
                          
                          fluidRow(
                            column(width = 12,
                                   "Describe results below")
                          ), 
                          
                          # Column Headers
                          fluidRow(
                            column(width = 6,
                                   "House 1 "),
                            column(width = 6,
                                   "House 2")
                            ),  
                          
                         
                          # Battle type stats graphs
                          fluidRow(
                            column(6, plotOutput("battle_type_hist1")),
                            column(6, wellPanel(p("Battle Stats 2")))
                            ),
                          
                          
                          # Army size tables
                          fluidRow(
                            column(6, wellPanel(p("Army Stats 1"))),
                            column(6, wellPanel(p("Army Stats 2")))

                          ),
                          
                          fluidRow(
                            column(6, wellPanel(p("Map 1"))),
                            column(6, wellPanel(p("Map 2")))
                          )
                            
                        
                        
                      ))),
             
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











########################## Server
server <- function(input, output) {
  
  #Create battle type histogram 1 on Explore tab based on house 1 selection
  output$battle_type_hist1 <- renderPlot({
    
    battle_type_data <- house_stats %>%
      filter(house == input$house1_explore)
  
    
    ggplot(battle_type_data, aes(x=battle_type))+
      geom_bar(aes(fill= outcome), position = "stack") +
      theme_classic() +
      theme(legend.position = "", 
            title = element_text(size=20, face = "bold"),
            plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 15)
      ) +
      scale_fill_manual(values = c("red", "darkgreen")) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, 6), breaks = c(0,1,2,3,4,5)) +
      ylab("")  +
      xlab("") +
      ggtitle(input$house1_explore)+
      coord_flip()
    
    
  })
  
  output$scatter <- renderPlot({
    
    ggplot(faithful, aes(x = waiting, y = eruptions)) +
      geom_point(color = input$scattercolor) +
      geom_smooth(method = "lm", se = FALSE)
    
  })
  


  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)



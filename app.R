library(shiny)
library(tidyverse)
library(shinythemes)
library(gridExtra)
library(readr)
library(readxl)
library(leaflet)
library(sf)
library(tmap)
library(plotly)
library(htmltools)


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
                      
                      
                      # PLACEHOLDER: Show a plot of the generated distribution
                      mainPanel(
                        
                        # text describing what graphs/outputs are and what they mean
                        fluidRow(
                          column(width = 12,
                                 "Battle statistics and experience are shown for each house selected for exploration")
                        ), 
                        
                        
                        
                        # Column Headers
                        fluidRow(
                          column(4, selectInput("house1_explore", 
                                                "House 1 Battle Stats:",
                                                choices = c(Stark="Stark",Lannister="Lannister",Baratheon="Baratheon",Tully="Tully",Greyjoy="Greyjoy",Frey="Frey",Bolton="Bolton",Karstark="Karstark",Mormont="Mormont",Glover="Glover",Tyrell="Tyrell")
                          )),
                          
                          column(4, selectInput("house2_explore",
                                                "House 2 Battle Stats:",
                                                choices = c(Stark="Stark",Lannister="Lannister",Baratheon="Baratheon",Tully="Tully",Greyjoy="Greyjoy",Frey="Frey",Bolton="Bolton",Karstark="Karstark",Mormont="Mormont",Glover="Glover",Tyrell="Tyrell")
                          )),
                          column(4, selectInput("house3_explore",
                                                "House 3 Battle Stats:",
                                                choices = c(Stark="Stark",Lannister="Lannister",Baratheon="Baratheon",Tully="Tully",Greyjoy="Greyjoy",Frey="Frey",Bolton="Bolton",Karstark="Karstark",Mormont="Mormont",Glover="Glover",Tyrell="Tyrell")
                          ))
                        ),  
                        
                        
                        
                        
                        # Battle type stats graphs
                        fluidRow(
                          column(4, plotOutput("battle_type_hist1")),
                          column(4, plotOutput("battle_type_hist2")),
                          column(4, plotOutput("battle_type_hist3"))
                        ),
                        
                        
                        # Regional battle experience
                        fluidRow(
                          column(4, wellPanel(p("map_1"))),
                          column(4, wellPanel(p("map_2"))),
                          column(4, wellPanel(p("map_3")))
                        ),
                        
                        
                        # Army size boxplot
                        fluidRow(
                          column(4, plotOutput("army_boxplot1")),
                          column(4, plotOutput("army_boxplot2")),
                          column(4, plotOutput("army_boxplot3"))
                        )
                        
                        
                        
                      )),
             
             
             #Pick Alliances Tab
             tabPanel("Pick Alliances",
                      
                      # Sidebar: Sidebar with a slider for army sizes and radio buttons for picking alliance 
                      sidebarLayout(
                        sidebarPanel(
                          
                          selectInput("house1_pick", 
                                      "Select House 1 for Alliance:",
                                      choices = c(Stark="Stark",Lannister="Lannister",Baratheon="Baratheon",Tully="Tully",Greyjoy="Greyjoy",Frey="Frey",Bolton="Bolton",Karstark="Karstark",Mormont="Mormont",Glover="Glover",Tyrell="Tyrell")
                          ),
                          selectInput("house2_pick",
                                      "Select House 2 for Alliance:",
                                      choices = c(Stark="Stark",Lannister="Lannister",Baratheon="Baratheon",Tully="Tully",Greyjoy="Greyjoy",Frey="Frey",Bolton="Bolton",Karstark="Karstark",Mormont="Mormont",Glover="Glover",Tyrell="Tyrell")
                          ),
                          selectInput("house3_pick",
                                      "Select House 3 for Alliance",
                                      choices = c(Stark="Stark",Lannister="Lannister",Baratheon="Baratheon",Tully="Tully",Greyjoy="Greyjoy",Frey="Frey",Bolton="Bolton",Karstark="Karstark",Mormont="Mormont",Glover="Glover",Tyrell="Tyrell")
                          )
                          
                        ), #close sidebar panel
                        
                        # pLACEHOLDER: Show a plot of the generated distribution
                        mainPanel(
                          
                          # text describing what graphs/outputs are and what they mean
                          fluidRow(
                            column(width = 12,
                                   "Below are the combined battle statistics and experience for your choosen alliance")),
                          
                          fluidRow(plotOutput("battle_type_alliance")),
                          fluidRow(plotOutput("alliance_army_boxplot"))
                          

                        ) #close main panel
                      ) #close sidebar layout
                    ), #close pick alliance tab
             
        
             #Battle Results
             tabPanel("Battle Results",
                    sidebarLayout(
                        sidebarPanel(
                          
                          #input army size
                          sliderInput("living_army_size",
                                      "Alliance Army Size",
                                      min = 1,
                                      max = 59000,
                                      value = 50000),
                          
                          #input region
                          selectInput("region", 
                                      "Select the region to fight WW",
                                      choices = c(Crownlands="Crownlands",North="North",Reach="Reach",Riverlands="Riverlands",Stormlands="Stormlands",Westerlands="Westerlands")
                          ),
                          
                          #input battle types
                          selectInput("battle_type", 
                                      "Select battle types",
                                      choices = c(ambush="ambush", pitched_battle="pitched_battle", razing="razing", siege="siege")
                          ),
                          
                          
                          #select dragon preferences
                          selectInput("dragons", 
                                      "do you want dragons?",
                                      choices = c(Yes="Yes", No="No"))
                          
                        ), #close sidebar panel
                      
                         # PLACEHOLDER: Show a plot of the generated distribution
                      mainPanel(
                        textOutput("win_percent")
                      
                            ) #close main panel
                        ) #close sidebar layout
                    ) #close pick alliance tab

             
             
      ) #close navbar page
  
  ) #close whole thin











########################## Server
server <- function(input, output) {
  
  ### EXPLORE FIGURES ###
  
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
      xlab("")+
      ylab("# of battles")+
      ggtitle(input$house1_explore)+
      coord_flip()
    
  })
  
  #Create battle type histogram 2 on Explore tab based on house 1 selection
  output$battle_type_hist2 <- renderPlot({
    
    battle_type_data <- house_stats %>%
      filter(house == input$house2_explore)
    
    
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
      xlab("")+
      ylab("# of battles")+
      ggtitle(input$house2_explore)+
      coord_flip()
    
  })
  
  #Create battle type histogram 3 on Explore tab based on house 1 selection
  output$battle_type_hist3 <- renderPlot({
    
    battle_type_data <- house_stats %>%
      filter(house == input$house3_explore)
    
    
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
      xlab("")+
      ylab("# of battles")+
      ggtitle(input$house3_explore)+
      coord_flip()
    
  })
  
  #create a map of regional experience based on house input


  
  # create army size boxplot
  
  output$army_boxplot1 <- renderPlot({
    
    army_stats <- house_stats %>% 
      select(house, army_size) %>% 
      filter(house == input$house1_explore)
    
    ggplot(army_stats, aes(x= house, y=army_size)) +
      geom_boxplot(fill = "slateblue") +
      ylab("Army Size\n") +
      xlab("")+
      theme_classic() +
      theme(legend.position = "", 
            title = element_text(size=20, face = "bold"),
            plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 15))
    
  })
  
  output$army_boxplot2 <- renderPlot({
    
    army_stats <- house_stats %>% 
      select(house, army_size) %>% 
      filter(house == input$house2_explore)
    
    ggplot(army_stats, aes(x= house, y=army_size)) +
      geom_boxplot(fill = "slateblue") +
      ylab("Army Size\n") +
      xlab("")+
      theme_classic() +
      theme(legend.position = "", 
            title = element_text(size=20, face = "bold"),
            plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 15))
    
  })
  
  
  output$army_boxplot3 <- renderPlot({
    
    army_stats <- house_stats %>% 
      select(house, army_size) %>% 
      filter(house == input$house3_explore)
    
    ggplot(army_stats, aes(x= house, y=army_size)) +
      geom_boxplot(fill = "slateblue") +
      ylab("Army Size\n") +
      xlab("")+
      theme_classic() +
      theme(legend.position = "", 
            title = element_text(size=20, face = "bold"),
            plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 15))
    
  })
  

  
  ### ALLIANCE FIGURES ###
  
  #Create battle type plot on Alliance tab based on alliances selection
  output$battle_type_alliance <- renderPlot({
    
    battle_type_data <- house_stats %>%
      filter(house == input$house1_pick | house == input$house2_pick | house == input$house3_pick)
    
    
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
      scale_y_continuous(expand = c(0, 0), breaks = c(0,2,4,6,8,10,12,14,16,18,20,22,24,26)) +
      xlab("")+
      ylab("# of battles")+
      ggtitle("Your Alliance's Battle Experience")+
      coord_flip()
    
  })
  
  # create army size histogram for combined alliance
  output$alliance_army_boxplot <- renderPlot({
    
    army_stats <- house_stats %>% 
      select(house, army_size) %>% 
      filter(house == input$house1_pick | house == input$house2_pick |house == input$house3_pick)
    
    ggplot(army_stats, aes(x= house, y=army_size)) +
      geom_boxplot(fill = "slateblue") +
      ylab("Army Size\n") +
      xlab("")+
      theme_classic() +
      theme(legend.position = "", 
            title = element_text(size=20, face = "bold"),
            plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 15))
    
  })
  
### BATTLE! ###
  

  output$win_percent <- renderText({
    
#read in new sheet from excel data
  house_stats_summary <- read_excel("house_stats.xlsx", sheet = "final_stats") 

#filter by alliance houses. select only columns with input of battle type and input of region
score <- house_stats_summary %>% 
  filter(house == input$house1_pick | house == input$house2_pick | house == input$house3_pick) %>% 
  select(input$battle_type, input$region)  

battle_type_score_df <- score %>% #create just a dataframe for the battle_type score so this can be summed and sized into one number that is the score on a scale of 1-10 below
  select(input$battle_type)

region_score_df <- score %>% #create a dataframe for the region score so this can be summed and sized into one number that is a score on a scale of 1-10 below
  select(input$region)

#calculate scores
battle_score = (sum(battle_type_score_df)/2) #the sum of the battle type scores is measured on a scale of 0-20 (based on the maximum total number of battles for a given battle type by an alliance of three houses) and is resized by dividing by 2 into a reference score on a scale of 0-10
region_score = (sum(region_score_df)/2.5) #the sum of the region scores is measured on a scale of 0-25 (based on the maximum total number of battles for a given region by an alliance of three houses) and is resized by dividing by 2.5 into a reference score on a scale of 0-10
army_score = ((input$living_army_size/ 100000)*10) #input from slider bar (that will calculate the minimum and maximum combined army size based on the historical min and max army sizes for the smallest and largest three house armies) for army size is divided by 100,000 (the estimated size of the white walker army after their battle at Hardhome north of the wall where they added about this number of Freefolk to their army of the dead) to get a ratio of the living army size to the army of the dead size. It is then multiplied by 10 to get an army size score on a reference range of 0-10.

#calculate two possible scores: with and without dragons
with_dragon_score = (battle_score + region_score + army_score + 10) #the battle type, region, and army size scores are summed and the with dragon score assumes that dragons are worth an additional 10 reference points towards the total alliance score
without_dragon_score = (battle_score + region_score + army_score) #no additional points awarded if you don't have dragons

#based on dragon input (yes or no) return the appropriate of the two scores (with or without dragons)
final_score <- if("Yes" %in% input$dragons) {final_score <- with_dragon_score} else {final_score <- without_dragon_score}

#survival probability calculation
survival_probability <- ((final_score / 40) * 100) #the final score is based out of a maximum score of 10 for battle type experience, 10 for region experience, 10 for army size for a total max score of 30. The Night King has an ice dragon which is assumed to be worth 10 additional points so the denominator for determining the survival probability is 40. It is then multiplied by 100 to get it into a percent.

paste("Your Chance of Winning is ", sprintf("%.1f %%", survival_probability)) #output text before the score and format the survival probability with one number after the decimal point and with a % sign.

  }) #close out$win_percent renderText

  
  
  
  
  
  
  
  
  
  
}

# Run the application 

shinyApp(ui = ui, server = server)

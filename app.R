library(shiny)
library(tidyverse)
library(shinythemes)
library(readr)
library(readxl)
library(leaflet)
library(sf)
library(htmltools)
library(giphyr)

### Upfront Data Wrangling ###

#make house_stats data frame
house_stats <- read_excel("house_stats.xlsx", 
                          sheet = "Sheet1")

house_stats$outcome <- as.factor(house_stats$outcome)
house_stats$battle_type <- as.factor(house_stats$battle_type)
house_stats$house <- as.factor(house_stats$house)


#################### User Interface  ########################################################
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
#Title
  titlePanel("White Walker WIPEOUT"),
  
  navbarPage("Winter is Here",
          
                
################## Introduction Tab

             tabPanel("Introduction",
                      h1("Winter is Here"),
                      h2("And Only You Can Stop It"),
                      p("The White Walkers have broken through the wall and are descending on the citizens of Westeros. Only you can stop the army of the dead, but first you must decide where, how, and with whom you will fight them. Choose your alliances carefully and decide on a location to make your stand. Devise a battle plan and find out what your chances of victory are. Winter is here..."),
                      img(src = "winter-is-here.jpg"),
                      h3("Data Summary"),
                      p("Data used in this app was adapted from Chris Albon's War of the Five Kings data set. 
                        The original contains a dataset of the battles in the War of the Five Kingsfrom George R.R. Martin's A Song Of Ice And Fire series.
                        Original data can be found at: https://github.com/chrisalbon/war_of_the_five_kings_dataset. Supplemental data from A Wiki of Ice and Fire was used when necessary and can be found at: https://awoiaf.westeros.org/index.php/Military_strength#The_Reach."),
                      
             p("Created by: Alexander Stejskal, Savannah Tjaden, and Austin Melcher")
             ),
       
      
################# Explore Alliances Tab

             tabPanel("Explore Alliances",
                      
                      mainPanel(
                        
                        # text describing what graphs/outputs are and what they mean
                        h3("Explore House Battle Stats"),
                        fluidRow(
                          column(width = 12,
                                 "Battle statistics and experience are shown for each house selected for exploration. 
                                  The first output is a bar graph showing the type of battles each house has fought in. Red indicates a loss and green indicates a win. 
                                  The second output is a map showing the number of battles fought in each region.
                                  The last output is a boxplot showing the army sizes of each house in different battles.")
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
                          column(4, plotOutput("map_1")),
                          column(4, plotOutput("map_2")),
                          column(4, plotOutput("map_3"))
                        ),
                      
                        # Army size boxplot
                        fluidRow(
                          column(4, plotOutput("army_boxplot1")),
                          column(4, plotOutput("army_boxplot2")),
                          column(4, plotOutput("army_boxplot3"))
                        )
                      )),
         
   
################### Pick Alliances Tab

             tabPanel("Pick Alliances",
                      
                      # Sidebar with a slider for army sizes and radio buttons for picking alliance 
                      sidebarLayout(
                        sidebarPanel(
                          h3("Choose Your Alliance"),
                          
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
                                  ), #close sidebar panel
                        
                        mainPanel(
                          
                          # text describing what graphs/outputs are and what they mean
                          fluidRow(
                            column(width = 12,
                                   "Below are the combined battle statistics and experience for your chosen alliance.
                                    The first output is a bar graph showing the combined battle experience of the selected alliance. Red indicates a loss and green indicates a win. 
                                    The second output is a map showing the combined regional experience of the selected alliance.
                                    The last output is a boxplot showing the army sizes of each house in your alliance.
                                   ")),
                          
                          fluidRow(plotOutput("battle_type_alliance")),
                          fluidRow(plotOutput("alliance_army_boxplot"))
                        ) #close main panel
                      ) #close sidebar layout
                    ), #close pick alliance tab
             
        
################## Battle Results Tab

             tabPanel("Battle Results",
                    sidebarLayout(
                        sidebarPanel(
                          h3("Devise Your Battle Plan"),
                          
                          #input army size
                          uiOutput("army_size_slider"),
                          
                          #input region
                          selectInput("region", 
                                      "Select where to fight",
                                      choices = c("The Crownlands"="Crownlands","The North"="North","The Reach"="Reach","The Riverlands"="Riverlands","The Stormlands"="Stormlands","The Westerlands"="Westerlands")
                          ),
                          
                          #input battle types
                          selectInput("battle_type", 
                                      "Select the type of battle to fight",
                                      choices = c("Ambush"="ambush", "Pitched Battle"="pitched_battle", "Razing"="razing", "Siege"="siege")
                          ),
                          
                          #select dragon preferences
                          radioButtons("dragons", 
                                      "Do you want to battle with dragons?",
                                      choices = c(Yes="Yes", No="No")),
                          
                        #battle action button
                        actionButton("ShowCond", "Battle!")
                            
                        ), #close sidebar panel
                      
                         #Output win or lose text and gif
                      mainPanel(
                        h3(textOutput("win_percent")),
                        fluidRow(conditionalPanel(condition = "output.win_lose == 'The living likely triumph'",
                                                  img(src = "winner.gif"))),
                        fluidRow(conditionalPanel(condition = "output.win_lose == 'The dead likely triumph'",
                                                  img(src = "loser1.gif"))),
                        fluidRow(verbatimTextOutput("win_lose"))
                        
                            ) #close main panel
                        ) #close sidebar layout
                    ) #close pick alliance tab
      ) #close navbar page
  ) #close UI




########################## Server  #########################################################
server <- function(input, output) {
  
############ EXPLORE TAB FIGURES
  
### Battle Type Histograms ###
  
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
  
### Maps of Regional Experience ###

  
  output$map_1 <- renderImage({
  
    filename <- normalizePath(file.path('.',
                                      paste('', input$house1_explore,
                                            '.png', sep = '')))
  
  list(src = filename)
}, deleteFile = FALSE)

#####################  
  

  output$map_2 <- renderImage({
  
    filename <- normalizePath(file.path('.',
                                      paste('', input$house2_explore,
                                            '.png', sep = '')))
  
  list(src = filename)
}, deleteFile = FALSE)

  

######################  
  
  
  output$map_3 <- renderImage({
  
    filename <- normalizePath(file.path('.',
                                      paste('', input$house3_explore,
                                            '.png', sep = '')))
  
  list(src = filename)
}, deleteFile = FALSE)

  
  
  
  
  
  
  
  
  
  
  
  

  
### Army Size Box Plots ###
  
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
  
  
################# ALLIANCE TAB FIGURES
  
### Battle Type Histogram ###
  
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
  
### Army Size Box Plot ###
  
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
  
  
################## BATTLE TAB OUTPUTS
  
### Win Percentage Text Output ###
  
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

paste("The three-eyed raven has seen that your chance of winning is... ", sprintf("%.1f %%", survival_probability)) #output text before the score and format the survival probability with one number after the decimal point and with a % sign.

  })

  
### Dynamic Slider Inputs ###
  
  #create a dynamic slide bar for army size calculating the min and max based on house alliance selections
  output$army_size_slider <- renderUI({
    
    #read in new sheet from excel data
    house_stats_summary <- read_excel("house_stats.xlsx", 
                                      sheet = "final_stats")
    
    #create a min army number based on the sum of selected houses for alliance
    minimum_army <- house_stats_summary %>% 
      filter(house == input$house1_pick | house == input$house2_pick | house == input$house3_pick) %>% 
      select(min_army)
    
    mini_army <- data.matrix(minimum_army)
    min_army_size = sum(mini_army)
    
    #create a max army number based on the sum of selected houses for alliance
    maximum_army <- house_stats_summary %>% 
      filter(house == input$house1_pick | house == input$house2_pick | house == input$house3_pick) %>% 
      select(max_army)
    
    max2_army <- data.matrix(maximum_army)
    max_army_size = sum(max2_army)
    
    #create an average where the slider bar can start
    average_army_size = ((min_army_size + max_army_size)/2)
    average_army_size <- as.numeric(average_army_size)
    
    #create a reactive input to be called in the ui with uiOutput
    tagList(
      sliderInput("living_army_size",
                  "Select your alliance army size",
                  min = min_army_size,
                  max = max_army_size,
                  value = average_army_size)
    ) #tagList close
    
  }) #close the renderUI for army_size_slider

  
### Result Text Output ###
  
  battle_result <- eventReactive(input$ShowCond, {
    
    #repeat survival_probability calculations
        house_stats_summary <- read_excel("house_stats.xlsx", sheet = "final_stats") 
    
    score <- house_stats_summary %>% 
      filter(house == input$house1_pick | house == input$house2_pick | house == input$house3_pick) %>% 
      select(input$battle_type, input$region)  
    
    battle_type_score_df <- score %>% 
      select(input$battle_type)
    
    region_score_df <- score %>% 
      select(input$region)
    
    battle_score = (sum(battle_type_score_df)/2) 
    region_score = (sum(region_score_df)/2.5)
    army_score = ((input$living_army_size/ 100000)*10) 
    
    with_dragon_score = (battle_score + region_score + army_score + 10)
    without_dragon_score = (battle_score + region_score + army_score) 
    final_score <- if("Yes" %in% input$dragons) {final_score <- with_dragon_score} else {final_score <- without_dragon_score}
    
    survival_probability <- ((final_score / 40) * 100) 
    
    result_text <- if(survival_probability>50) {result_text <- "The living likely triumph"} else {final_score <- "The dead likely triumph"}
result_text #output conditional text statement based on survival_probability
  })
  
  output$win_lose <- renderText({
    paste(battle_result())
  })
  
} #close server



### Run the application ###

shinyApp(ui = ui, server = server)

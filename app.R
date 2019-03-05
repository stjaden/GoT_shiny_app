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
                          
                          # text describing what graphs/outputs are and what they mean
                          fluidRow(
                            column(width = 12,
                                   "Below are the combined battle statistics and experience for your choosen alliance")),
                          
                          fluidRow(plotOutput("battle_type_alliance")),
                          fluidRow(plotOutput("alliance_army_boxplot"))
                          
                         
                          
                          
                          
                          
                          
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
  #load in data for maps and join 
  battle_location_map3 <- read_excel("battle_location_map3.xlsx")
  got_region <- st_read(dsn = ".", layer = "Political")
  
  region_df <- full_join(got_region, battle_location_map3)
  
  # wrangling
  df_region <- region_df %>% 
    replace_na(list(name = "Beyond the Wall", Baratheon = "0", Bolton = "0", Frey = "0", Glover = "0", Greyjoy = "0", Karstark = "0", Lannister = "0", Mormont = "0", Stark = "0", Tully = "0", Tyrell = "0"))
  
  df_region$Baratheon = as.numeric(df_region$Baratheon)
  df_region$Bolton = as.numeric(df_region$Bolton)
  df_region$Frey = as.numeric(df_region$Frey)
  df_region$Glover = as.numeric(df_region$Glover)
  df_region$Greyjoy = as.numeric(df_region$Greyjoy)
  df_region$Karstark = as.numeric(df_region$Karstark)
  df_region$Lannister = as.numeric(df_region$Lannister)
  df_region$Mormont = as.numeric(df_region$Mormont)
  df_region$Stark = as.numeric(df_region$Stark)
  df_region$Tully = as.numeric(df_region$Tully)
  df_region$Tyrell = as.numeric(df_region$Tyrell)
  
  
  # Make the map
  
  output$map_1 <- renderLeaflet({
  
  mytext_bar=paste("Region: ", df_region$name, "<br/>", "# of battles: ", df_region$input$house1_explore)%>% 
    lapply(htmltools::HTML)
  
  
  pal_bar <- colorNumeric(
    palette = "Blues",
    domain = df_region$input$house1_explore)
  
  
 leaflet(df_region, options = leafletOptions(crs = leafletCRS(crsClass = "L.CRS.Simple"))) %>% 
    addPolygons(fillColor = ~pal_bar(input$house1_explore), 
                stroke=TRUE, 
                fillOpacity = 0.9, 
                color="black", 
                weight=0.9,
                label = mytext_bar, 
                labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), 
                                            textsize = "13px", direction = "auto")) %>% 
    addLegend( pal=pal_bar, 
               values=~Baratheon, 
               opacity=1, 
               title = "Number of Battles per Region", 
               position = "bottomleft",
               bins = 3)
  
  
  })
  
  
  

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


}

# Run the application 
shinyApp(ui = ui, server = server)

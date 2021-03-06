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
library(gameofthrones)
library(giphyr)


#make house_stats data frame
house_stats <- read_excel("house_stats.xlsx", 
                          sheet = "Sheet1")

house_stats$outcome <- as.factor(house_stats$outcome)
house_stats$battle_type <- as.factor(house_stats$battle_type)
house_stats$house <- as.factor(house_stats$house)







battle_location_map3 <- read_excel("battle_location_map3.xlsx")

got_region <- st_read(dsn = ".", layer = "Political")

region_df <- full_join(got_region, battle_location_map3)

df_region <- region_df %>% 
  replace_na(list(name = "Beyond the Wall", 
                  Baratheon = "0", 
                  Bolton = "0", 
                  Frey = "0", 
                  Glover = "0",
                  Greyjoy = "0", 
                  Karstark = "0", 
                  Lannister = "0", 
                  Mormont = "0", 
                  Stark = "0", 
                  Tully = "0", 
                  Tyrell = "0"))

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






#################### User Interface
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  # Title
  titlePanel("White Walker WIPEOUT"),
  
  navbarPage("Winter is Here",
             
             #Introduction Tab
             tabPanel("Introduction",
                      h1("Winter is Here"),
                      h2("And Only You Can Stop It"),
                      p("The White Walkers have broken through the wall and are descending on the citizens of Westeros. Only you can can stop the army of the dead, but first you must decide where, how, and with whom you will fight them. Choose your alliances carefully and decide on a location to make your stand. Devise a battle plan and find out what your chances of victory are. Winter is here..."),
                      img(src = "winter-is-here.jpg"),
                      h3("Data Summary"),
                      p("Data used in this app was adapted from Chris Albon's War of the Five Kings data set. 
                        The original contains a dataset of the battles in the War of the Five Kingsfrom George R.R. Martin's A Song Of Ice And Fire series.
                        Original data can be found at: https://github.com/chrisalbon/war_of_the_five_kings_dataset. Supplemental data from A Wiki of Ice and Fire was used when necessary and can be found at: https://awoiaf.westeros.org/index.php/Military_strength#The_Reach.")
                      ),
             p("Created by: Alexander Stejskal, Savannah Tjaden, and Austin Melcher")
             
             ,
             
             #Explore Alliances Tab
             tabPanel("Explore Alliances",
                      
                      
                      # PLACEHOLDER: Show a plot of the generated distribution
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
                          column(4, leafletOutput("map_1")),
                          column(4, leafletOutput("map_2")),
                          column(4, leafletOutput("map_3"))
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
                          h3("Choose Your Alliance"),
                          
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
             
        
             #Battle Results
             tabPanel("Battle Results",
                    sidebarLayout(
                        sidebarPanel(
                          h3("Devise Your Battle Plan"),
                          
                          #input army size
                          uiOutput("army_size_slider"),
                          
                          #input region
                          selectInput("region", 
                                      "Select where to fight",
                                      choices = c(Crownlands="Crownlands",North="North",Reach="Reach",Riverlands="Riverlands",Stormlands="Stormlands",Westerlands="Westerlands")
                          ),
                          
                          #input battle types
                          selectInput("battle_type", 
                                      "Select the type of battle to fight",
                                      choices = c(ambush="ambush", pitched_battle="pitched_battle", razing="razing", siege="siege")
                          ),
                          
                          
                          #select dragon preferences
                          radioButtons("dragons", 
                                      "Do you want dragons?",
                                      choices = c(Yes="Yes", No="No")),
                          
                        #action button
                        actionButton("ShowCond", "Battle!")
                            
                        ), #close sidebar panel
                      
                         # PLACEHOLDER: Show a plot of the generated distribution
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

  output$map_1 <- renderLeaflet({
    
    
    ## baratheon
    
    mytext_bar=paste("Region: ", df_region$name, "<br/>", "# of battles: ", df_region$Baratheon)%>% 
      lapply(htmltools::HTML)
    
    
    pal_bar <- colorNumeric(
      palette = "Blues",
      domain = df_region$Baratheon)
    
    
   baratheon_leaflet <- leaflet(df_region, options = leafletOptions(crs = leafletCRS(crsClass = "L.CRS.Simple"))) %>% 
      addPolygons(fillColor = ~pal_bar(Baratheon), 
                  stroke=TRUE, 
                  fillOpacity = 0.9, 
                  color="black", 
                  weight=0.9,
                  label = mytext_bar, 
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), 
                                              textsize = "13px", direction = "auto")) 
   
   
   ## bolton
   
   
   mytext_bol = paste("Region: ", df_region$name, "<br/>", "# of battles: ", df_region$Bolton)%>% 
     lapply(htmltools::HTML)
   
   
   pal_bol <- colorNumeric(
     palette = "Blues",
     domain = df_region$Bolton)
   
   
   bolton_leaflet <- leaflet(df_region, options = leafletOptions(crs = leafletCRS(crsClass = "L.CRS.Simple"))) %>% 
     addPolygons(fillColor = ~pal_bol(Bolton), 
                 stroke=TRUE, 
                 fillOpacity = 0.9, 
                 color="black", 
                 weight=0.9,
                 label = mytext_bol, 
                 labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), 
                                             textsize = "13px", direction = "auto"))
    
   
   
   ## frey
   
   mytext_frey = paste("Region: ", df_region$name, "<br/>", "# of battles: ", df_region$Frey)%>% 
     lapply(htmltools::HTML)
   
   
   pal_frey <- colorNumeric(
     palette = "Blues",
     domain = df_region$Frey)
   
   
   frey_leaflet <- leaflet(df_region, options = leafletOptions(crs = leafletCRS(crsClass = "L.CRS.Simple"))) %>% 
     addPolygons(fillColor = ~pal_frey(Frey), 
                 stroke=TRUE, 
                 fillOpacity = 0.9, 
                 color="black", 
                 weight=0.9,
                 label = mytext_frey, 
                 labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), 
                                             textsize = "13px", direction = "auto"))
     
   
   
   ## glover
   
   mytext_glo = paste("Region: ", df_region$name, "<br/>", "# of battles: ", df_region$Glover)%>% 
     lapply(htmltools::HTML)
   
   
   pal_glo <- colorNumeric(
     palette = "Blues",
     domain = df_region$Glover)
   
   
   glover_leaflet <- leaflet(df_region, options = leafletOptions(crs = leafletCRS(crsClass = "L.CRS.Simple"))) %>% 
     addPolygons(fillColor = ~pal_glo(Glover), 
                 stroke=TRUE, 
                 fillOpacity = 0.9, 
                 color="black", 
                 weight=0.9,
                 label = mytext_glo, 
                 labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), 
                                             textsize = "13px", direction = "auto"))
    
    

    ## greyjoy
   
   mytext_grey = paste("Region: ", df_region$name, "<br/>", "# of battles: ", df_region$Greyjoy)%>% 
     lapply(htmltools::HTML)
   
   
   pal_grey <- colorNumeric(
     palette = "Blues",
     domain = df_region$Greyjoy)
   
   
   greyjoy_leaflet <- leaflet(df_region, options = leafletOptions(crs = leafletCRS(crsClass = "L.CRS.Simple"))) %>% 
     addPolygons(fillColor = ~pal_grey(Greyjoy), 
                 stroke=TRUE, 
                 fillOpacity = 0.9, 
                 color="black", 
                 weight=0.9,
                 label = mytext_grey, 
                 labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), 
                                             textsize = "13px", direction = "auto"))
   
   
   
   ## karstark
   
   mytext_kar = paste("Region: ", df_region$name, "<br/>", "# of battles: ", df_region$Karstark)%>% 
     lapply(htmltools::HTML)
   
   
   pal_kar <- colorNumeric(
     palette = "Blues",
     domain = df_region$Karstark)
   
   
   karstark_leaflet <- leaflet(df_region, options = leafletOptions(crs = leafletCRS(crsClass = "L.CRS.Simple"))) %>% 
     addPolygons(fillColor = ~pal_kar(Karstark), 
                 stroke=TRUE, 
                 fillOpacity = 0.9, 
                 color="black", 
                 weight=0.9,
                 label = mytext_kar, 
                 labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), 
                                             textsize = "13px", direction = "auto"))
   
   ## lannister
   
   mytext_lan = paste("Region: ", df_region$name, "<br/>", "# of battles: ", df_region$Lannister)%>% 
     lapply(htmltools::HTML)
   
   
   pal_lan <- colorNumeric(
     palette = "Blues",
     domain = df_region$Lannister)
   
   
   lannister_leaflet <- leaflet(df_region, options = leafletOptions(crs = leafletCRS(crsClass = "L.CRS.Simple"))) %>% 
     addPolygons(fillColor = ~pal_lan(Lannister), 
                 stroke=TRUE, 
                 fillOpacity = 0.9, 
                 color="black", 
                 weight=0.9,
                 label = mytext_lan, 
                 labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), 
                                             textsize = "13px", direction = "auto"))
   
   
   
   ## mormont
   
   mytext_mor = paste("Region: ", df_region$name, "<br/>", "# of battles: ", df_region$Mormont)%>% 
     lapply(htmltools::HTML)
   
   
   pal_mor <- colorNumeric(
     palette = "Blues",
     domain = df_region$Mormont)
   
   
   mormont_leaflet <- leaflet(df_region, options = leafletOptions(crs = leafletCRS(crsClass = "L.CRS.Simple"))) %>% 
     addPolygons(fillColor = ~pal_mor(Mormont), 
                 stroke=TRUE, 
                 fillOpacity = 0.9, 
                 color="black", 
                 weight=0.9,
                 label = mytext_mor, 
                 labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), 
                                             textsize = "13px", direction = "auto"))
   
   
   
   ## stark
   
   mytext_sta = paste("Region: ", df_region$name, "<br/>", "# of battles: ", df_region$Stark)%>% 
     lapply(htmltools::HTML)
   
   
   pal_sta <- colorNumeric(
     palette = "Blues",
     domain = df_region$Stark)
   
   
   stark_leaflet <- leaflet(df_region, options = leafletOptions(crs = leafletCRS(crsClass = "L.CRS.Simple"))) %>% 
     addPolygons(fillColor = ~pal_sta(Stark), 
                 stroke=TRUE, 
                 fillOpacity = 0.9, 
                 color="black", 
                 weight=0.9,
                 label = mytext_sta, 
                 labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), 
                                             textsize = "13px", direction = "auto"))
   
   
   
   ## tully
   
   mytext_tul = paste("Region: ", df_region$name, "<br/>", "# of battles: ", df_region$Tully)%>% 
     lapply(htmltools::HTML)
   
   
   pal_tul <- colorNumeric(
     palette = "Blues",
     domain = df_region$Tully)
   
   
   tully_leaflet <- leaflet(df_region, options = leafletOptions(crs = leafletCRS(crsClass = "L.CRS.Simple"))) %>% 
     addPolygons(fillColor = ~pal_tul(Tully), 
                 stroke=TRUE, 
                 fillOpacity = 0.9, 
                 color="black", 
                 weight=0.9,
                 label = mytext_tul, 
                 labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), 
                                             textsize = "13px", direction = "auto"))
   
   
   
   ## tyrell
   
   mytext_tyr = paste("Region: ", df_region$name, "<br/>", "# of battles: ", df_region$Tyrell)%>% 
     lapply(htmltools::HTML)
   
   
   pal_tyr <- colorNumeric(
     palette = "Blues",
     domain = df_region$Tyrell)
   
   
   tyrell_leaflet <- leaflet(df_region, options = leafletOptions(crs = leafletCRS(crsClass = "L.CRS.Simple"))) %>% 
     addPolygons(fillColor = ~pal_tyr(Tyrell), 
                 stroke=TRUE, 
                 fillOpacity = 0.9, 
                 color="black", 
                 weight=0.9,
                 label = mytext_tyr, 
                 labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), 
                                             textsize = "13px", direction = "auto"))
    
    
    
    
    
    
    
    
    
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

paste("The three-eyed raven has seen that your chance of winning is... ", sprintf("%.1f %%", survival_probability)) #output text before the score and format the survival probability with one number after the decimal point and with a % sign.

  }) #close out$win_percent renderText

  

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
                  "Select Your Alliance Army Size",
                  min = min_army_size,
                  max = max_army_size,
                  value = average_army_size)
    ) #tagList close
    
    
  }) #close the renderUI for army_size_slider

  
  var <- eventReactive(input$ShowCond, {
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
result_text
  })
  
  output$win_lose <- renderText({
    paste(var())
  })
  
  
  
}

# Run the application 

shinyApp(ui = ui, server = server)

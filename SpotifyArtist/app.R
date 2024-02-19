#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(devtools)
library(spotifyr)
library(tidyverse)
library(knitr)
library(ggjoy)
library(viridis)
library(plotly)


#Authentication
SPOTIFY_CID<- Sys.setenv(SPOTIFY_CLIENT_ID = '9e38cf3fe1184c878249539f2da32090')
SPOTIFY_CIS<-Sys.setenv(SPOTIFY_CLIENT_SECRET = '645a6d7f3eb24239bf6da40cd8f19923')


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  navbarPage(
    title = "Spotify Artist Insights",
    tabPanel("Valence Insights",
             h2("Valence Insights"),
             sidebarLayout(
               sidebarPanel(
                 textInput(inputId = "selectionArtist",
                           label = "Enter Name of Artist"),
                 textInput(inputId = "selectionSong",
                           label = "Filter by Album"),
                 actionButton(inputId = "clicks",
                              label = "Select")
                
                 ),
               mainPanel(textOutput('mytext'),
                         br(),
                         p("1. Be sure to enter Album Name exactly how it appears on Spotify"),
                         p("2. Please wait as certain Artists with larger discographies will take longer to generate"),
                         #textOutput('mytext2'),
                         #textOutput('mytext3'),
                         br(),
                         tabsetPanel(
                           
                           tabPanel("Valence Trend",plotOutput("distPlot")),
                           tabPanel("Valence Range",br(),plotlyOutput("distPlotvalence2")),
                           tabPanel("Table", tableOutput("tableV"))
                         )
               )
               ),
             ),
    tabPanel("Danceability Insights",
             mainPanel(h2("Danceability Insights"),
               tabsetPanel(
                 tabPanel("Danceability Trend",plotOutput("distPlotDanceability")),
                 tabPanel("Danceability Range",br(),plotlyOutput("distPlotDanceabilityRange")),
                 tabPanel("Table",tableOutput("tableD"))
               )
             )
            ),
    tabPanel("Energy Insights",
             mainPanel(h2("Energy Insights"),
                       tabsetPanel(
                         #plotOutput("distPlotDanceability"),
                         tabPanel("Energy Trend",plotOutput("distPlotEnergy")),
                         tabPanel("Energy Range",br(),plotlyOutput("distPlotEnergyRange")),
                         tabPanel("Table", tableOutput("tableE"))
                       )
             
             )
    ),
    
    tabPanel(title = "About",includeMarkdown("about.Rmd"))
    ),
  )

# Define server logic required
server <- function(input, output) {
 
#access_token <- get_spotify_access_token()
 
  # output$mytext2<- renderText({
  #   print("1. Be sure to enter Album Name exactly how it appears on Spotify")
  # })
  
  # output$mytext3<- renderText({
  # print("2. Please wait as certain Artists with larger discographies will take longer to generate")
  # })
  
  observeEvent(input$clicks, {
    artist <- get_artist_audio_features(input$selectionArtist)
    
    #########VALENCE SECTION ##########
    if(!isTruthy(input$selectionSong)){
      output$distPlot <- renderPlot({
      ggplot(artist, aes(x = valence, y = album_name, fill = ..x..)) + geom_density_ridges_gradient() +
        scale_fill_gradient(low = "white", high = "goldenrod1")+
        theme(panel.background = element_rect(fill = "white")) +
        theme(plot.background = element_rect(fill = "white")) +
        theme(legend.position = "none")
        
    })
      artist1 <- artist %>%
        group_by(album_name)%>%
        mutate(max=max(valence))%>%
        mutate(min=min(valence))%>%
        select(album_name, max, min)%>%
        unique()
      output$distPlotvalence2<-renderPlotly({
        plot_ly(artist1, color = I("gray80"),  
                hoverinfo = 'text') %>%
          add_segments(x = ~max, xend = ~min, y = ~album_name, yend = ~album_name, showlegend = FALSE) %>%
          add_markers(x = ~max, y = ~album_name, name = "Maximum Valence Value", color = 'rgb(17, 157, 255)', text=~paste('Max Valence: ', max)) %>%
          add_markers(x = ~min, y = ~album_name, name = "Minimum Valence Value", color ='rgb(231, 99, 250)', text=~paste('Min Valence: ', min))%>%
          layout(
            title = "Album Valence Range",
            xaxis = list(title = "Valence"),
            yaxis= list(title=""))
        
      })
      output$tableV <- renderTable({artist %>%
          group_by (album_name) %>%
          group_by (track_name) %>%
          arrange(-valence)%>%
          select(artist_name,track_name,album_name,album_type,valence)})
    
    }else
    {
      FilteredArtist<-artist%>%
        group_by(artist_name, album_name) %>% 
        mutate(artist_name = iconv(artist_name, to = "UTF-8")) %>% 
        filter(album_name == input$selectionSong)
        output$distPlot<- renderPlot({
          ggplot(FilteredArtist,aes(x = valence, y = album_name, fill = ..x..)) + geom_density_ridges_gradient() +
            scale_fill_gradient(low = "white", high = "goldenrod1")+
            theme(panel.background = element_rect(fill = "white")) +
            theme(plot.background = element_rect(fill = "white")) +
            theme(legend.position = "none")
        })
        
        artist1 <- FilteredArtist %>%
          group_by(album_name)%>%
          mutate(max=max(valence))%>%
          mutate(min=min(valence))%>%
          select(album_name, max, min)%>%
          unique()
        output$distPlotvalence2<-renderPlotly({
          plot_ly(artist1, color = I("gray80"),  
                  hoverinfo = 'text') %>%
            add_segments(x = ~max, xend = ~min, y = ~album_name, yend = ~album_name, showlegend = FALSE) %>%
            add_markers(x = ~max, y = ~album_name, name = "Maximum Valence Value", color = 'rgb(17, 157, 255)', text=~paste('Max Valence: ', max)) %>%
            add_markers(x = ~min, y = ~album_name, name = "Minimum Valence Value", color ='rgb(231, 99, 250)', text=~paste('Min Valence: ', min))%>%
            layout(
              title = "Album Valence Range",
              xaxis = list(title = "Valence"),
              yaxis= list(title=""))
          
        })
        
        output$displotPositivity<-renderPlot({
          ggplot(FilteredArtist,aes(x= valence, y= energy, color= album_name)) +
            geom_jitter(show.legend = FALSE) +
            scale_color_viridis(discrete= TRUE, option="D") +
            geom_vline(xintercept = 0.5) +
            geom_hline(yintercept = 0.5) +
            scale_x_continuous(breaks= seq(0, 1, 0.25)) +
            scale_y_continuous(breaks= seq(0, 1, 0.25)) +
            labs(title= "How positive your artist?") +
            theme_light()
        })
        
        output$tableV<- renderTable({artist %>%
            group_by (album_name) %>%
            filter(album_name == input$selectionSong)%>%
            arrange(-valence)%>%
            select(artist_name,track_name,album_name,valence)
            })
       
      
    }
    ######################
    
    
    ########DANCE SECTION#########
    if(!isTruthy(input$selectionSong)){
    output$distPlotDanceability<-renderPlot({
      ggplot(artist, aes(x = danceability, y = album_name,fill = ..x..)) + geom_density_ridges_gradient() +
        scale_fill_gradient(low = "white", high = "mediumpurple1")+
        theme(panel.background = element_rect(fill = "white")) +
        theme(plot.background = element_rect(fill = "white")) +
        theme(legend.position = "none")
    })

    artist1 <- artist %>%
      group_by(album_name)%>%
      mutate(max=max(danceability))%>%
      mutate(min=min(danceability))%>%
      select(album_name, max, min)%>%
      unique()
    output$distPlotDanceabilityRange<-renderPlotly({
      plot_ly(artist1, color = I("gray80"),  
                    hoverinfo = 'text') %>%
      add_segments(x = ~max, xend = ~min, y = ~album_name, yend = ~album_name, showlegend = FALSE) %>%
      add_markers(x = ~max, y = ~album_name, name = "Maximum Danceability Value", color = 'rgb(17, 157, 255)', text=~paste('Max Danceability: ', max)) %>%
      add_markers(x = ~min, y = ~album_name, name = "Minimum Danceability Value", color ='rgb(231, 99, 250)', text=~paste('Min Danceability: ', min))%>%
      layout(
        title = "Album Danceability Range",
        xaxis = list(title = "Danceability"),
        yaxis= list(title=""))
    })
    
    output$tableD <- renderTable({artist %>%
        group_by(album_name) %>% 
        mutate(mdance = mean(danceability))%>%
        arrange(desc(mdance)) %>% 
        select(artist_name,track_name,album_name,mdance)})

    }
    else
    {
      FilteredArtist<-artist%>%
        group_by(artist_name, album_name) %>% 
        mutate(artist_name = iconv(artist_name, to = "UTF-8")) %>% 
        filter(album_name == input$selectionSong)
      output$distPlotDanceability<- renderPlot({
        ggplot(FilteredArtist, aes(x = danceability, y = album_name, fill = ..x..)) + geom_density_ridges_gradient() +
          scale_fill_gradient(low = "white", high = "mediumpurple1")+
          theme(panel.background = element_rect(fill = "white")) +
          theme(plot.background = element_rect(fill = "white")) +
          theme(legend.position = "none")
      })
      
      artist1 <- FilteredArtist %>%
        group_by(album_name)%>%
        mutate(max=max(danceability))%>%
        mutate(min=min(danceability))%>%
        select(album_name, max, min)%>%
        unique()
      output$distPlotDanceabilityRange<-renderPlotly({
        plot_ly(artist1, color = I("gray80"),  
                hoverinfo = 'text') %>%
          add_segments(x = ~max, xend = ~min, y = ~album_name, yend = ~album_name, showlegend = FALSE) %>%
          add_markers(x = ~max, y = ~album_name, name = "Maximum Danceability Value", color = 'rgb(17, 157, 255)', text=~paste('Max Danceability: ', max)) %>%
          add_markers(x = ~min, y = ~album_name, name = "Minimum Danceability Value", color ='rgb(231, 99, 250)', text=~paste('Min Danceability: ', min))%>%
          layout(
            title = "Album Danceability Range",
            xaxis = list(title = "Danceability"),
            yaxis= list(title=""))
        
      })
      
      output$tableD <- renderTable({artist %>%
          group_by(album_name) %>% 
          mutate(mdance = mean(danceability))%>%
          filter(album_name == input$selectionSong)%>%
          arrange(-danceability) %>% 
          select(artist_name,track_name,album_name,danceability)
          })
      
    }
    #####################
    
    
    ##########ENERGY SECTION############
    if(!isTruthy(input$selectionSong)){
   output$distPlotEnergy <- renderPlot({
      ggplot(artist, aes(x = energy, y = album_name,fill = ..x..)) + geom_density_ridges_gradient() +
       scale_fill_gradient(low = "white", high = "darkolivegreen1")+
       theme(panel.background = element_rect(fill = "white")) +
       theme(plot.background = element_rect(fill = "white")) +
       theme(legend.position = "none")
     
    })
   artist1 <- artist %>%
     group_by(album_name)%>%
     mutate(max=max(energy))%>%
     mutate(min=min(energy))%>%
     select(album_name, max, min)%>%
     unique()
   output$distPlotEnergyRange<-renderPlotly({
     plot_ly(artist1, color = I("gray80"),  
             hoverinfo = 'text') %>%
       add_segments(x = ~max, xend = ~min, y = ~album_name, yend = ~album_name, showlegend = FALSE) %>%
       add_markers(x = ~max, y = ~album_name, name = "Maximum Energy Value", color = 'rgb(17, 157, 255)', text=~paste('Max Energy: ', max)) %>%
       add_markers(x = ~min, y = ~album_name, name = "Minimum Energy Value", color ='rgb(231, 99, 250)', text=~paste('Min Energy: ', min))%>%
       layout(
         title = "Album Energy Range",
         xaxis = list(title = "Energy"),
         yaxis= list(title=""))
     
   })
   
   output$tableE <- renderTable({artist %>%
       group_by(album_name) %>%
       mutate(menergy = mean(energy))%>%
       arrange(desc(menergy)) %>% 
       select(artist_name,track_name,album_name,menergy)})
    }
    else{
      FilteredArtist<-artist%>%
        group_by(artist_name, album_name) %>% 
        mutate(artist_name = iconv(artist_name, to = "UTF-8")) %>% 
        filter(album_name == input$selectionSong)
      output$distPlotEnergy<- renderPlot({
        ggplot(FilteredArtist, aes(x = energy, y = album_name, fill = ..x..)) + geom_density_ridges_gradient() +
          scale_fill_gradient(low = "white", high = "darkolivegreen1")+
          theme(panel.background = element_rect(fill = "white")) +
          theme(plot.background = element_rect(fill = "white")) +
          theme(legend.position = "none")
      })
      
      artist1 <- FilteredArtist %>%
        group_by(album_name)%>%
        mutate(max=max(energy))%>%
        mutate(min=min(energy))%>%
        select(album_name, max, min)%>%
        unique()
      output$distPlotEnergyRange<-renderPlotly({
        plot_ly(artist1, color = I("gray80"),  
                hoverinfo = 'text') %>%
          add_segments(x = ~max, xend = ~min, y = ~album_name, yend = ~album_name, showlegend = FALSE) %>%
          add_markers(x = ~max, y = ~album_name, name = "Maximum Energy Value", color = 'rgb(17, 157, 255)', text=~paste('Max Energy: ', max)) %>%
          add_markers(x = ~min, y = ~album_name, name = "Minimum Energy Value", color ='rgb(231, 99, 250)', text=~paste('Min Energy: ', min))%>%
          layout(
            title = "Album Energy Range",
            xaxis = list(title = "Energy"),
            yaxis= list(title=""))
        
      })
      output$displotEnergyDance<- renderPlot({
        ggplot(FilteredArtist,aes(x= danceability, y= energy, color= album_name,text = paste(album_name))) +
          geom_jitter(show.legend= FALSE) +
          scale_color_viridis(discrete= TRUE, option="C") +
          labs(title= "Does it have enough energy?") +
          theme_light()
      })
      output$tableE <- renderTable({artist %>%
          group_by(album_name) %>%
          #mutate(menergy = mean(energy))%>%
          filter(album_name == input$selectionSong)%>%
          arrange(-energy) %>% 
          select(artist_name,track_name,album_name,energy)
        })
      
      
    }
    #######################
   
  
   
   output$mytext <- renderText({
     c("Server time:", as.character(Sys.time()), as.character(Sys.timezone()))
   })
   
                          
    })

}
  
# Run the application 
shinyApp(ui = ui, server = server)

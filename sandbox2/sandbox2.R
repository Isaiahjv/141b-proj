#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(spotifyr)
library(tidyverse)
library(knitr)
library(ggplot2)
library(fmsb)
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
Sys.setenv(SPOTIFY_CLIENT_ID = 'f0d212500a7e4c74baaab9025ea186c3')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '4985198dd20d47c7bdc5e52a1cf01e3f')

access_token <- get_spotify_access_token()
pop <- get_genre_artists(genre = "pop", market = NULL, limit = 4,
                         offset = 0, authorization = access_token)


blues <- get_genre_artists(genre = "blues", market = NULL, limit = 4,
                           offset = 0, authorization = access_token)

wonky <- get_genre_artists(genre = "wonky", market = NULL, limit = 4,
                           offset = 0, authorization = access_token)

rock <- get_genre_artists(genre = "rock", market = NULL, limit = 4,
                          offset = 0, authorization = access_token)



blues <- blues %>% select(name,genre,popularity,followers.total)
pop <- pop %>% select(name,genre,popularity,followers.total)
wonky <-  wonky %>% select(name,genre,popularity,followers.total)
rock <- rock %>% select(name,genre,popularity,followers.total)

temp <- bind_rows(blues,pop, wonky, rock)
t1 <- temp %>% select(name) %>% slice(1) %>% pull()
dat <- get_artist_audio_features(t1) 
dat <- dat %>% bind_cols(temp %>% filter(name %in% t1) %>% select(genre:followers.total) %>% uncount(nrow(dat)))


for (i in 2: (temp %>% select(name) %>% nrow()) ){
    #ndat <- get_artist_audio_features(artistnames[i])
    #dat <- bind_rows(ndat)
    
    t1 <- temp %>% select(name) %>% slice(i) %>% pull()
    ndat <- get_artist_audio_features(t1) 
    ndat <- ndat %>% bind_cols(temp %>% filter(name %in% t1) %>% select(genre:followers.total) %>% uncount(nrow(ndat)))
    dat <- dat %>% bind_rows(ndat)
    
}
dat %>% select(valence) %>% summary()
library(shiny)

    ui = fluidPage(
        sidebarPanel( #designates location of following items
            htmlOutput("genre_selector"),#add selectinput boxs
            htmlOutput("artist_selector")# from objects created in server
        ),
        
        mainPanel(
            tabPanel("valence", verbatimTextOutput("summary")),
            plotlyOutput('plot'),
            plotOutput("radarPlot"),
            plotlyOutput("lengthPlot"),
            plotlyOutput("albumLoudnessPlot"),
            textOutput("recommendedArtists")
        )
        
    )
    server = shinyServer(function(input, output){
        
        output$genre_selector = renderUI({ #creates State select box object called in ui
            selectInput(inputId = "genre", #name of input
                        label = "Genre:", #label displayed in ui
                        choices = as.character(unique(c("pop","blues", "wonky", "rock"))),
                        multiple = T,
                        # calls unique values from the State column in the previously created table
                        selected = "pop") #default choice (not required)
        })
        
       
        output$artist_selector = renderUI({#creates County select box object called in ui
            
            data_available = dat %>% filter(genre %in% input$genre) %>% select(artist_name) %>% pull() 
            #creates a reactive list of available counties based on the State selection made
            
            selectInput(inputId = "artist", #name of input
                        label = "Artist:", #label displayed in ui
                        choices = unique(data_available), #calls list of available counties
                        multiple = T,
                        selected = unique(data_available[1]) )
        })
        
   
    output$summary = renderPrint({
        dat %>% filter(artist_name %in% input$artist) %>% select(valence) %>% summary()
    })
        
    output$plot <- renderPlotly(
        
        plot1 <- dat %>% filter(artist_name %in% input$artist) %>%  plot_ly(
            x = ~valence,
            y = ~liveness, 
            color =~(as.factor(artist_name)),
            type = 'scatter',
            mode = 'markers')
    )
    
    output$radarPlot <- renderPlot({
        artist_radar <- dat %>% filter(artist_name %in% input$artist) 
        artist_radar <- artist_radar %>% group_by(artist_name) %>%  summarize_all(funs(mean)) 
        artist_radar <- artist_radar %>% 
            select(danceability,energy, acousticness, valence, loudness) %>% mutate(loudness = loudness / -60) 
        colnames(artist_radar) = c('Danceability', 'Energy', 'Acousticness', 'Valence', 'loudness') 
        rownames(artist_radar) = c("Artist 1", "Artist 2", "Artist 3")
        artist_radar <- rbind(rep(1,6) , rep(0,6) , artist_radar)
        
        col_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
        col_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
        
        radarchart(artist_radar, axistype=1, 
                   #custom polygon
                   pcol= col_border , pfcol=col_in , plwd=4 , plty = 1,
                   #custom the grid
                   cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,1,5), cglwd=0.8,
                   #custom labels
                   vlcex=0.8 )
        
    })
    
    output$lengthPlot <- renderPlotly({
        tracklength <- dat %>%  filter(artist_name %in% input$artist)  %>% 
            select(artist_name, duration_ms, album_release_year) 
        tracklength$artist_name <- as.factor(tracklength$artist_name)
        tracklength %>% plot_ly(x= ~(duration_ms / 60000), color = ~factor(artist_name),nbinsx = 10) %>% 
            add_histogram(histnorm = "", alpha = 0.7) %>%
            layout(barmode = "overlay", title = "Histogram of Average Track length corresponding to Artist") 
        
        
    })
    
    output$albumLoudnessPlot <- renderPlotly({
        trackTempo <-  dat %>%  filter(artist_name %in% input$artist)  %>% 
            select(artist_name, duration_ms, valence,tempo) 
        trackTempo$artist_name <- as.factor(trackTempo$artist_name)
        trackTempo %>% 
            plot_ly(x= ~tempo) %>% 
            add_lines(y= ~valence, color = ~factor(artist_name)) %>% 
            layout(title = "Time series plot of Average Track length corresponding to Artist")
        
        
        
        
    })
    
    output$recommendedArtists <- renderPrint({
        tempID <- dat %>% filter(artist_name %in% input$artist) %>% 
            select(artist_id) %>% distinct()
        atistID <- tempID[[1]]
        recommendation <- get_recommendations(limit = 15,seed_artists = artistID, authorization = get_spotify_access_token())
        recommendations <- NULL
        for (i in 4:15){
            recommendation1 <- recommendation[1][i,1]
            recommendation1 <- recommendation1[[1]][3] %>% pull()
            recommendations <- append(recommendations, recommendation1)
        }
        recommendations
        
    })
    
    
   
})
    
shinyApp(ui = ui, server = server)

#mpg %>%
#plot_ly(x = ~hwy, color = ~ factor(cyl)) %>%
#add_histogram(histnorm = "", alpha = 0.7) %>% # histnorm could be "", "probability", "density" and "probability density"
#layout(barmode = "overlay")

#output$columns = renderUI({
#   mydata = get(input$dataset)
#  selectInput('columns2', 'Columns', mydata$name)
#})


#data <- reactive({
#   dat %>% filter(artist_name == input$columns2)
# })

#artistnames <- c(pop$name,blues$name) 

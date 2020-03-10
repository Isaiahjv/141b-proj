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
Sys.setenv(SPOTIFY_CLIENT_ID = 'f0d212500a7e4c74baaab9025ea186c3')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '4985198dd20d47c7bdc5e52a1cf01e3f')

access_token <- get_spotify_access_token()
pop <- get_genre_artists(genre = "pop", market = NULL, limit = 7,
                   offset = 0, authorization = access_token)


blues <- get_genre_artists(genre = "blues", market = NULL, limit = 7,
                  offset = 0, authorization = access_token)

blues <- blues %>% select(name,genre,popularity,followers.total)
pop <- pop %>% select(name,genre,popularity,followers.total)

temp <- bind_rows(blues,pop)
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
            plotlyOutput('plot')
        )
        
    )
    server = shinyServer(function(input, output){
        
        output$genre_selector = renderUI({ #creates State select box object called in ui
            selectInput(inputId = "genre", #name of input
                        label = "Genre:", #label displayed in ui
                        choices = as.character(unique(c("pop","blues"))),
                        # calls unique values from the State column in the previously created table
                        selected = "pop") #default choice (not required)
        })
        
       
        output$artist_selector = renderUI({#creates County select box object called in ui
            
            data_available = dat %>% filter(genre %in% input$genre) %>% select(artist_name) %>% pull() 
            #creates a reactive list of available counties based on the State selection made
            
            selectInput(inputId = "artist", #name of input
                        label = "Artist:", #label displayed in ui
                        choices = unique(data_available), #calls list of available counties
                        selected = unique(data_available[1]) )
        })
        
   
    output$summary = renderPrint({
        dat %>% filter(artist_name %in% input$artist) %>% select(valence) %>% summary()
    })
        
    output$plot <- renderPlotly(
        
        plot1 <- dat %>% filter(artist_name %in% input$artist) %>% plot_ly(
            x = ~valence,
            y = ~liveness, 
            type = 'scatter',
            mode = 'markers')
    )
    
    
   
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

library("shiny")
library("readr")
library("dplyr")
library("tidyverse")
library("treemap")
library("ggplot2")
library("dplyr")
library("tidyr")
library("hrbrthemes")
library("ggrepel")
library("shinythemes")
#setwd("~/GitHub/fsbmat-ufv.github.io/blog_posts/26-03-2020/Shiny/Corona")
data <- read_csv("covid19.csv")


data$deaths[is.na(data$deaths)] <- 0
data$date <- as.Date(data$date)
data <- data[order(data$date) , ]

data <- data %>%
  dplyr::filter(place_type == "state") %>%
  dplyr::group_by(state,date, confirmed,deaths) %>% 
  select(date, state, confirmed, deaths, estimated_population_2019)
names(data) <- c("date", "state", "confirmed", "deaths", "Pop")


ndeaths <- aggregate(data$deaths, by=list(data$date), sum)
names(ndeaths) <- c("date","deaths")
nconfirmed <- aggregate(data$confirmed, by=list(data$date), sum)
names(nconfirmed) <- c("date","confirmed")

aggSetor <-data%>%filter(date==last(data$date))%>%group_by(state) %>% summarise(quantidade = sum(deaths), 
                                                                                confirmedM = mean(confirmed))
aggSetor$escala <- scale(aggSetor$confirmedM) #necessario para criar valores negativos para deixar as disparidades mais evidentes

tabPanelSobre <- source("sobre.r")$value

ui <- fluidPage(theme=shinytheme("united"),
                headerPanel(
                  HTML(
                    '<div id="stats_header">
			Coronavirus no Brasil
			<a href="https://maf105.github.io/" target="_blank"><img align="right" alt="fsbmat Logo" src="./img/fsbmat.png" /></a>
			</div>'
                  ),
                  "Coronavirus no Brasil"
                ),
  # App title ----
  #titlePanel("Coronavirus no Brasil"),
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      uiOutput("codePanel"),
      tags$p("Autor: Fernando de Souza Bastos - Professor da Universidade Federal de Vicosa - MG")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Formatted text for caption ----
      h3(textOutput("caption")),
      
      # Output: 1 ----
      plotOutput("deathsPlot", height = 300,
                 dblclick = "deathsPlot_dblclick",
                 brush = brushOpts(
                   id = "deathsPlot_brush",
                   resetOnNew = TRUE
                 )
      ),
      
      plotOutput("confirmedPlot", height = 300,
                 dblclick = "confirmedPlot_dblclick",
                 brush = brushOpts(
                   id = "confirmedPlot_brush",
                   resetOnNew = TRUE
                 )
      ),
      
      plotOutput("dayPlot", height = 300,
                 dblclick = "dayPlot_dblclick",
                 brush = brushOpts(
                   id = "dayPlot_brush",
                   resetOnNew = TRUE
                 )
      ),
      
      DT::dataTableOutput("text")
      
    )
    
    
    
    
  ),
  
  fluidRow(
    column(width = 4, class = "well",
           h4("Numero de Mortes por Covid19 no Brasil"),
           plotOutput("plot1", height = 300,
                      dblclick = "plot1_dblclick",
                      brush = brushOpts(
                        id = "plot1_brush",
                        resetOnNew = TRUE
                      )
           )
    ),
    column(width = 4, class = "well",
           h4("Numero de Confirmados com Covid19 no Brasil"),
           plotOutput("plot2", height = 300,
                      dblclick = "plot1_dblclick",
                      brush = brushOpts(
                        id = "plot1_brush",
                        resetOnNew = TRUE
                      )
           )
    ),
    column(width = 4, class = "well",
           h4("Treemap das mortes e numero de confirmados por Estado"),
           plotOutput("plot3", height = 300,
                      dblclick = "plot1_dblclick",
                      brush = brushOpts(
                        id = "plot1_brush",
                        resetOnNew = TRUE
                      )
           )
    )
    
  ),
  tabPanelSobre()
)

server <- function(input, output) {
  
  
  filt <- selectInput("codeInput",label ="Escolha um Estado",
                      choices = as.list(unique(data$state)))
  
  output$codePanel <- renderUI({ filt
    
  })
  
  dataset<-reactive({ 
    
    subset(data, state == input$codeInput)  
    
  })
  
  
  
  dataset2<-reactive({
    df <- dataset()
    teste1 <- dplyr::lag(df$deaths)
    teste1[is.na(teste1)] <- 0
    teste2 <- dplyr::lag(df$confirmed)
    teste2[is.na(teste2)] <- 0
    df$teste1 <- teste1
    df$teste2 <- teste2
    df$deaths_day <- df$deaths-df$teste1
    df$confirmed_day <- df$confirmed-df$teste2
    df <- df %>% select(1:5,8:9)
    return(df)
  })
  
  # output$caption and output$mpgPlot functions
  formulaText <- reactive({
    paste("Resultados Referentes ao Estado de", input$codeInput)
  })
  
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })
  
  output$text<-renderDataTable(dataset())
  
  # # Generate a plot of the requested variable against mpg ----
  # # and only exclude outliers if requested
    output$deathsPlot <- renderPlot({
      xlab <- "Data"
      legenda <- "fonte: https://brasil.io/dataset/covid19/caso"
      
      ggplot2::ggplot(dataset(), aes(x = date, y = deaths)) +
        geom_bar(stat = "identity", alpha = .7, color = "red", fill = "red") +
        scale_x_date(date_breaks = "1 day",
                     date_labels = "%d/%m") +
        scale_y_continuous(limits = c(0, max(dataset()$deaths+20, na.rm = TRUE) + 3),
                           expand = c(0, 0)) +
        geom_text(aes(label=deaths), position=position_dodge(width=0.9), vjust=-0.25) +
        labs(x = xlab,
             y = "Numero de Mortes",
             title = "Numero de Mortes por COVID-19",
             caption = legenda) +
        theme_minimal() +
        theme(axis.text.x =  element_text(angle = 90))
      
    })
  
  
  output$confirmedPlot <- renderPlot({
    xlab <- "Data"
    legenda <- "fonte: https://brasil.io/dataset/covid19/caso"
    
    ggplot2::ggplot(dataset(), aes(x = date, y = confirmed)) +
      geom_bar(stat = "identity", alpha = .7, color = "red", fill = "red") +
      scale_x_date(date_breaks = "1 day",
                   date_labels = "%d/%m") +
      scale_y_continuous(limits = c(0, max(dataset()$confirmed+100, na.rm = TRUE) + 3),
                         expand = c(0, 0)) +
      geom_text(aes(label=confirmed), position=position_dodge(width=0.9), vjust=-0.25) +
      labs(x = xlab,
           y = "Numero de Confirmados",
           title = "Numero de Casos Confirmados com Covid19",
           caption = legenda) +
      theme_minimal() +
      theme(axis.text.x =  element_text(angle = 90))
    
    })
  
  
  output$dayPlot <- renderPlot({
    xlab <- "Data"
    legenda <- "fonte: https://brasil.io/dataset/covid19/caso"
    
    #Grafico com o numero de casos diarios confirmados
    ggplot(dataset2(), aes(x=date, y=confirmed_day))+
      geom_line( color="steelblue")+ 
      geom_point() + 
      geom_text_repel(aes(label=confirmed_day), size = 3)+
      xlab("Data") + ylab("Numero de casos diarios confirmados")+
      theme_ipsum() +
      theme(axis.text.x=element_text(angle=60, hjust=1))+
      scale_x_date(date_breaks = "2 day", date_labels = "%d %b")
    
    
    
    
  })
  
  
  # -------------------------------------------------------------------
  # Single zoomable plot (on left)
  #ranges <- reactiveValues(x = date, y = confirmed)
  
  output$plot1 <- renderPlot({
    xlab <- "Data"
    legenda <- "fonte: https://brasil.io/dataset/covid19"
    
    ggplot2::ggplot(ndeaths, aes(x = date, y = deaths)) +
      geom_bar(stat = "identity", alpha = .7, color = "red", fill = "red") +
      scale_x_date(date_breaks = "1 day",
                   date_labels = "%d/%m") +
      scale_y_continuous(limits = c(0, max(ndeaths$deaths+20, na.rm = TRUE) + 3),
                         expand = c(0, 0)) +
      geom_text(aes(label=deaths), position=position_dodge(width=0.9), vjust=-0.25) +
      labs(x = xlab,
           y = "Numero de Mortes",
           title = " ",
           caption = legenda) +
      theme_minimal() +
      theme(axis.text.x =  element_text(angle = 90))
  })
  
  # -------------------------------------------------------------------
  # Linked plots (middle and right)
  #ranges2 <- reactiveValues(x = NULL, y = NULL)
  
  output$plot2 <- renderPlot({
    xlab <- "Data"
    legenda <- "fonte: https://brasil.io/dataset/covid19/caso"
    ggplot2::ggplot(nconfirmed, aes(x = date, y = confirmed)) +
      geom_bar(stat = "identity", alpha = .7, color = "red", fill = "red") +
      scale_x_date(date_breaks = "1 day",
                   date_labels = "%d/%m") +
      scale_y_continuous(limits = c(0, max(nconfirmed$confirmed+300, na.rm = TRUE) + 3),
                         expand = c(0, 0)) +
      geom_text(aes(label=confirmed), position=position_dodge(width=0.5), vjust=-0.25) +
      labs(x = xlab,
           y = "Numero de Confirmados",
           title = " ",
           caption = legenda) +
      theme_minimal() +
      theme(axis.text.x =  element_text(angle = 90))
  })
  
  output$plot3 <- renderPlot({
    treemap(aggSetor, index = "state", vSize = "quantidade", vColor = "escala",
            type = "value", palette = "-RdGy", lowerbound.cex.labels = 0.3,
            title  =  "Cor relacionada as mortes - Tamanho relacionado aos confirmados")
  })
  
  
  
}

shinyApp(ui = ui, server = server)

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$distPlot <- renderPlot({
data <- read.csv("covid19.txt", sep="")
        # generate bins based on input$bins from ui.R
library(ggplot2)
library(dplyr)
library(tidyr)
library(hrbrthemes)
library("ggrepel")

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
ggplot(ndeaths, aes(x=date, y=deaths))+
  geom_line( color="steelblue")+ 
  geom_point() + 
  geom_text_repel(aes(label=deaths), size = 3)+
  xlab("Data") + ylab("Numero acumulado de mortes")+
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))+scale_x_date(date_breaks = "2 day", date_labels = "%d %b")

xlab <- "Data"
ylab <- "Casos confirmados"
legenda <- "fonte: https://brasil.io/dataset/covid19/caso"

ggplot2::ggplot(ndeaths, aes(x = date, y = deaths)) +
  geom_bar(stat = "identity", alpha = .7, color = "red", fill = "red") +
  scale_x_date(date_breaks = "1 day",
               date_labels = "%d/%m") +
  scale_y_continuous(limits = c(0, max(ndeaths$deaths+20, na.rm = TRUE) + 3),
                     expand = c(0, 0)) +
  geom_text(aes(label=deaths), position=position_dodge(width=0.9), vjust=-0.25) +
  labs(x = xlab,
       y = "Numero de Mortes",
       title = "Numero de Mortes por COVID-19",
       caption = legenda) +
  theme_minimal() +
  theme(axis.text.x =  element_text(angle = 90))

    })

})

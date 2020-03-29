rm(list=ls())
cat("\014")
setwd("~/GitHub/fsbmat-ufv.github.io/blog_posts/26-03-2020")
library(readr)
data <- read_csv("covid19.csv")
#data <- read_csv("CoronaVirus.csv")
#View(data)
#state2 <- unique(data$state)
# Library
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
  xlab("Data") + ylab("Número acumulado de mortes")+
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



library("rlang")
library("dplyr")
library("tidyverse")
library("treemap")
#Treemap do numero de mortes
aggSetor <-data%>%filter(date==last(data$date))%>%group_by(state) %>% summarise(quantidade = sum(deaths), 
                                                                             confirmedM = mean(confirmed))
aggSetor$escala <- scale(aggSetor$confirmedM) #necessário para criar valores negativos para deixar as disparidades mais evidentes
x <- treemap(aggSetor, index = "state", vSize = "quantidade", vColor = "escala",
             type = "value", palette = "-RdGy", lowerbound.cex.labels = 0.3,
             title  =  "Treemap das mortes e número de confirmados por Estado")

library("htmlTable")
data %>% 
  filter(date==last(data$date))%>% 
  group_by(Estado=state)%>% 
  summarise(Mortes = sum(deaths))%>%
  arrange(desc(Mortes))  %>% 
  htmlTable(caption="Numero de mortes por Estado")

#Treemap do numero de casos confirmados
aggSetor <-data%>%filter(date==last(data$date))%>%group_by(state) %>% summarise(quantidade = sum(confirmed), 
                                                                             deathsM = mean(deaths))
aggSetor$escala <- scale(aggSetor$deathsM) #necessário para criar valores negativos para deixar as disparidades mais evidentes
x <- treemap(aggSetor, index = "state", vSize = "quantidade", vColor = "escala",
             type = "value", palette = "-RdGy", lowerbound.cex.labels = 0.3,
             title  =  "Treemap do número de casos confirmados e mortes por Estado")


SP <- data %>% filter(state=="SP") %>% select(date, state, confirmed, deaths, Pop)
teste1 <- dplyr::lag(SP$deaths)
teste1[is.na(teste1)] <- 0
teste2 <- dplyr::lag(SP$confirmed)
teste2[is.na(teste2)] <- 0
SP$teste1 <- teste1
SP$teste2 <- teste2
SP$deaths_day <- SP$deaths-SP$teste1
SP$confirmed_day <- SP$confirmed-SP$teste2
SP <- SP %>% select(1:5,8:9)
SPdeaths <- aggregate(SP$deaths, by=list(SP$date), sum)
names(SPdeaths) <- c("date","deaths")


#Grafico com o numero acumulado de mortes
ggplot(SPdeaths, aes(x=date, y=deaths))+
  geom_line( color="steelblue")+ 
  geom_point() + 
  geom_text_repel(aes(label=deaths), size = 3)+
  xlab("Data") + ylab("Número acumulado de mortes")+
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  scale_x_date(date_breaks = "2 day", date_labels = "%d %b")
  #scale_x_date(limit=c(as.Date("2020-02-26"),as.Date("2020-03-27")))

ggplot2::ggplot(SP, aes(x = date, y = deaths)) +
  geom_bar(stat = "identity", alpha = .7, color = "red", fill = "red") +
  scale_x_date(date_breaks = "1 day",
               date_labels = "%d/%m") +
  scale_y_continuous(limits = c(0, max(SPdeaths$deaths+20, na.rm = TRUE) + 3),
                     expand = c(0, 0)) +
  geom_text(aes(label=deaths), position=position_dodge(width=0.9), vjust=-0.25) +
  labs(x = xlab,
       y = "Numero de Mortes",
       title = "Numero de Mortes por COVID-19",
       caption = legenda) +
  theme_minimal() +
  theme(axis.text.x =  element_text(angle = 90))

#Grafico com o numero acumulado de casos confirmados
ggplot(SP, aes(x=date, y=confirmed))+
  geom_line( color="steelblue")+ 
  geom_point() + 
  geom_text_repel(aes(label=confirmed), size = 3)+
  xlab("Data") + ylab("Número acumulado de casos confirmados")+
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  scale_x_date(date_breaks = "2 day", date_labels = "%d %b")

ggplot2::ggplot(SP, aes(x = date, y = confirmed)) +
  geom_bar(stat = "identity", alpha = .7, color = "red", fill = "red") +
  scale_x_date(date_breaks = "1 day",
               date_labels = "%d/%m") +
  scale_y_continuous(limits = c(0, max(SP$confirmed+300, na.rm = TRUE) + 3),
                     expand = c(0, 0)) +
  geom_text(aes(label=confirmed), position=position_dodge(width=0.5), vjust=-0.25) +
  labs(x = xlab,
       y = "Numero de Mortes",
       title = "Numero de Casos Confirmados por COVID-19",
       caption = legenda) +
  theme_minimal() +
  theme(axis.text.x =  element_text(angle = 90))

#Grafico com o numero de casos diários confirmados
ggplot(SP, aes(x=date, y=confirmed_day))+
  geom_line( color="steelblue")+ 
  geom_point() + 
  geom_text_repel(aes(label=confirmed_day), size = 3)+
  xlab("Data") + ylab("Número de casos confirmados")+
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  scale_x_date(date_breaks = "2 day", date_labels = "%d %b")

ggplot2::ggplot(SP, aes(x = date, y = confirmed_day)) +
  geom_bar(stat = "identity", alpha = .7, color = "red", fill = "red") +
  scale_x_date(date_breaks = "1 day",
               date_labels = "%d/%m") +
  scale_y_continuous(limits = c(0, max(SP$confirmed_day+300, na.rm = TRUE) + 3),
                     expand = c(0, 0)) +
  geom_text(aes(label=confirmed_day), position=position_dodge(width=0.5), vjust=-0.25) +
  labs(x = xlab,
       y = "Numero de Mortes",
       title = "Numero de Mortes por COVID-19",
       caption = legenda) +
  theme_minimal() +
  theme(axis.text.x =  element_text(angle = 90))


#Grafico com o numero de mortes diaria
ggplot(SP, aes(x=date, y=deaths_day))+
  geom_line( color="steelblue")+ 
  geom_point() + 
  geom_text_repel(aes(label=deaths_day), size = 3)+
  xlab("Data") + ylab("Número de mortes")+
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  scale_x_date(date_breaks = "2 day", date_labels = "%d %b")

ggplot2::ggplot(SP, aes(x = date, y = deaths_day)) +
  geom_bar(stat = "identity", alpha = .7, color = "red", fill = "red") +
  scale_x_date(date_breaks = "1 day",
               date_labels = "%d/%m") +
  scale_y_continuous(limits = c(0, max(SP$deaths_day+10, na.rm = TRUE) + 3),
                     expand = c(0, 0)) +
  geom_text(aes(label=deaths_day), position=position_dodge(width=0.5), vjust=-0.25) +
  labs(x = xlab,
       y = "Numero de Mortes",
       title = "Numero de Mortes por COVID-19",
       caption = legenda) +
  theme_minimal() +
  theme(axis.text.x =  element_text(angle = 90))


##Minas Gerais

MG <- data %>% filter(state=="MG") %>% select(date, state, confirmed, deaths, Pop)
teste1 <- dplyr::lag(MG$deaths)
teste1[is.na(teste1)] <- 0
teste2 <- dplyr::lag(MG$confirmed)
teste2[is.na(teste2)] <- 0
MG$teste1 <- teste1
MG$teste2 <- teste2
MG$deaths_day <- MG$deaths-MG$teste1
MG$confirmed_day <- MG$confirmed-MG$teste2
MG <- MG %>% select(1:5,8:9)
MGdeaths <- aggregate(MG$deaths, by=list(MG$date), sum)
names(MGdeaths) <- c("date","deaths")


#Grafico com o numero acumulado de mortes
ggplot(MGdeaths, aes(x=date, y=deaths))+
  geom_line( color="steelblue")+ 
  geom_point() + 
  geom_text_repel(aes(label=deaths), size = 3)+
  xlab("Data") + ylab("Número acumulado de mortes")+
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  scale_x_date(date_breaks = "2 day", date_labels = "%d %b")
#scale_x_date(limit=c(as.Date("2020-02-26"),as.Date("2020-03-27")))

ggplot2::ggplot(MG, aes(x = date, y = deaths)) +
  geom_bar(stat = "identity", alpha = .7, color = "red", fill = "red") +
  scale_x_date(date_breaks = "1 day",
               date_labels = "%d/%m") +
  scale_y_continuous(limits = c(0, max(MGdeaths$deaths+20, na.rm = TRUE) + 3),
                     expand = c(0, 0)) +
  geom_text(aes(label=deaths), position=position_dodge(width=0.9), vjust=-0.25) +
  labs(x = xlab,
       y = "Numero de Mortes",
       title = "Numero de Mortes por COVID-19 em MG",
       caption = legenda) +
  theme_minimal() +
  theme(axis.text.x =  element_text(angle = 90))

#Grafico com o numero acumulado de casos confirmados
ggplot(MG, aes(x=date, y=confirmed))+
  geom_line( color="steelblue")+ 
  geom_point() + 
  geom_text_repel(aes(label=confirmed), size = 3)+
  xlab("Data") + ylab("Número acumulado de casos confirmados em MG")+
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  scale_x_date(date_breaks = "2 day", date_labels = "%d %b")

ggplot2::ggplot(MG, aes(x = date, y = confirmed)) +
  geom_bar(stat = "identity", alpha = .7, color = "red", fill = "red") +
  scale_x_date(date_breaks = "1 day",
               date_labels = "%d/%m") +
  scale_y_continuous(limits = c(0, max(MG$confirmed+300, na.rm = TRUE) + 3),
                     expand = c(0, 0)) +
  geom_text(aes(label=confirmed), position=position_dodge(width=0.5), vjust=-0.25) +
  labs(x = xlab,
       y = "Casos Confirmados",
       title = "Número acumulado de casos confirmados em MG por COVID-19",
       caption = legenda) +
  theme_minimal() +
  theme(axis.text.x =  element_text(angle = 90))

#Grafico com o numero de casos diários confirmados
ggplot(MG, aes(x=date, y=confirmed_day))+
  geom_line( color="steelblue")+ 
  geom_point() + 
  geom_text_repel(aes(label=confirmed_day), size = 3)+
  xlab("Data") + ylab("Número de casos confirmados diariamente")+
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  scale_x_date(date_breaks = "2 day", date_labels = "%d %b")

ggplot2::ggplot(MG, aes(x = date, y = confirmed_day)) +
  geom_bar(stat = "identity", alpha = .7, color = "red", fill = "red") +
  scale_x_date(date_breaks = "1 day",
               date_labels = "%d/%m") +
  scale_y_continuous(limits = c(0, max(MG$confirmed_day+300, na.rm = TRUE) + 3),
                     expand = c(0, 0)) +
  geom_text(aes(label=confirmed_day), position=position_dodge(width=0.5), vjust=-0.25) +
  labs(x = xlab,
       y = "Numero casos confirmados",
       title = "Número de casos confirmados diariamente por COVID-19",
       caption = legenda) +
  theme_minimal() +
  theme(axis.text.x =  element_text(angle = 90))


#Grafico com o numero de mortes diaria
ggplot(MG, aes(x=date, y=deaths_day))+
  geom_line( color="steelblue")+ 
  geom_point() + 
  geom_text_repel(aes(label=deaths_day), size = 3)+
  xlab("Data") + ylab("Número de mortes")+
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  scale_x_date(date_breaks = "2 day", date_labels = "%d %b")

ggplot2::ggplot(MG, aes(x = date, y = deaths_day)) +
  geom_bar(stat = "identity", alpha = .7, color = "red", fill = "red") +
  scale_x_date(date_breaks = "1 day",
               date_labels = "%d/%m") +
  scale_y_continuous(limits = c(0, max(MG$deaths_day+10, na.rm = TRUE) + 3),
                     expand = c(0, 0)) +
  geom_text(aes(label=deaths_day), position=position_dodge(width=0.5), vjust=-0.25) +
  labs(x = xlab,
       y = "Numero de Mortes",
       title = "Numero de Mortes por COVID-19",
       caption = legenda) +
  theme_minimal() +
  theme(axis.text.x =  element_text(angle = 90))


# %>%
#   dplyr::summarise(recovered = sum(confirmed)-sum(deaths)) %>%
#   tidyr::pivot_wider(
#     names_from = state,
#     values_from = deaths
#   ) %>%
#   dplyr::mutate(recovered = confirmed - ifelse(is.na(deaths), 0, deaths)) %>%
#   dplyr::arrange(-confirmed) %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(country = dplyr::if_else(Country.Region == "United Arab Emirates", "UAE", Country.Region)) %>%
#   dplyr::mutate(country = dplyr::if_else(country == "Mainland China", "China", country)) %>%
#   dplyr::mutate(country = dplyr::if_else(country == "North Macedonia", "N.Macedonia", country)) %>%
#   dplyr::mutate(country = trimws(country)) %>%
#   dplyr::mutate(country = factor(country, levels = country))


























ndeaths <- aggregate(data$deaths, by=list(data$date), sum)
#View(ndeaths)
plot(ndeaths$x~ndeaths$Group.1 , type="b" , lwd=3 , col=rgb(0.1,0.7,0.1,0.8) , ylab="Número de Mortes" , xlab="data" , bty="l" , pch=10 , cex=1)
abline(h=seq(0,100,10) , col="grey", lwd=0.8)


data$date <- as.Date( data$date, '%d/%m/%Y')
require(ggplot2)
ggplot( data = data, aes(date, deaths)) + geom_line() 

# load the library
library(forcats)
data  <- data %>% filter(date=="2020-03-25", place_type=="state")
data$deaths[is.na(data$deaths)] <- 0
data$confirmed[is.na(data$confirmed)] <- 0
# Reorder following the value of another column:
data %>%
  mutate(state = fct_reorder(state, (confirmed))) %>%
  ggplot( aes(x=state, y=confirmed)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  geom_text(aes(label=confirmed), vjust=0.5, color="black", size=3.5)+
  coord_flip() +
  xlab("") +
  theme_bw()

# Reorder following the value of another column:
data %>%
  mutate(state = fct_reorder(state, deaths)) %>%
  ggplot( aes(x=state, y=deaths)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  geom_text(aes(label=deaths), vjust=1.6, color="black", size=3.5)+
  coord_flip() +
  xlab("") +
  theme_bw()




```{r}
shinyApp(
  
  
  # Define UI for random distribution app ----
  ui <- fluidPage(
    
    # App title ----
    titlePanel("SIR Model with Mortality"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(fluid = TRUE,
                  
                  # Sidebar panel for inputs ----
                  sidebarPanel(
                    # Teste
                    sliderInput("range1", "How many people will an infected person contaminate?",
                                min = 1, max = 100,
                                value = 5, step = 1,
                                animate = animationOptions(interval = 100, loop = TRUE)),
                    
                    # br() element to introduce extra vertical spacing ----
                    br(),
                    
                    numericInput(inputId = "Population",
                                 label = "Population of cities:",
                                 value = 10000 )),
                  
                  # Main panel for displaying outputs ----
                  mainPanel(
                    
                    # Output: Tabset w/ plot, summary, and table ----
                    tabsetPanel(type = "tabs",
                                tabPanel("Plot", plotOutput("plot")),
                                tabPanel("Histogram", plotly::plotlyOutput("histogram")),
                                tabPanel("Table", tableOutput("table"))
                    )
                    
                  )
    )
  ),
  
  # Define server logic for random distribution app ----
  server <- function(input, output) {
    
    output$plot <-  renderPlot({
      require(deSolve)
      t=150
      N <- input$Population
      deaths <- 0.003*N
      tinfec <- 14
      gamma <- 1/tinfec
      R0 <- input$range1 #numero medio de pessoas infectadas a partir de um infectado
      beta <- (gamma*R0)/N
      mu=deaths/N
      
      init <- c(S=N-1,I=1,R=0, D=0)
      parameters <- c(bet=beta,gamm=gamma, mmu=mu)
      time <- seq(0,t,by=1)
      eqn <- function(time,state,parameters){
        with(as.list(c(state,parameters)),{
          dS <- -(bet*S*I)
          dI <- (bet*S*I)-gamm*I-mmu*I
          dR <- gamm*I
          dD <- mmu*I
          return(list(c(dS, dI, dR, dD)))})
      }
      out<-ode(y=init,times=time,eqn,parms=parameters)
      out.df<-as.data.frame(out)
      
      require(ggplot2)
      mytheme <- theme_bw() +
        theme(text=element_text(colour="black")) +
        theme(panel.grid = element_line(colour = "white")) +
        theme(panel.background = element_rect(fill = "#B2B2B2"))
      theme_set(mytheme)
      title <- bquote("SIR Model with mortality")
      subtit <- bquote(list(beta==.(parameters[1]),~gamma==.(parameters[2]),~mu==.(parameters[3])))
      
      res<-ggplot(out.df,aes(x=time))+
        ggtitle(bquote(atop(bold(.(subtit)))))+
        geom_line(aes(y=S,colour="Susceptible"))+
        geom_line(aes(y=I,colour="Infected"))+
        geom_line(aes(y=R,colour="Recovered"))+
        geom_line(aes(y=D,colour="Deaths"))+
        ylab(label="Population")+
        xlab(label="Time (days)")+
        theme(legend.justification=c(1,0), legend.position=c(1,0.5))+
        theme(legend.title=element_text(size=12,face="bold"),
              legend.background = element_rect(fill='#FFFFFF',
                                               size=0.5,linetype="solid"),
              legend.text=element_text(size=10),
              legend.key=element_rect(colour="#FFFFFF",
                                      fill='#C2C2C2',
                                      size=0.25,
                                      linetype="solid"))+
        scale_colour_manual("Compartments",
                            breaks=c("Susceptible","Infected","Recovered", "Deaths"),
                            values=c("blue","red","darkgreen", "black"))
      print(res)
      
    })
    
    d <- reactive({
      require(deSolve)
      t=150
      N <- input$Population
      deaths <- 0.003*N
      tinfec <- 14
      gamma <- 1/tinfec
      R0 <- input$range1 #numero medio de pessoas infectadas a partir de um infectado
      beta <- (gamma*R0)/N
      mu=deaths/N
      
      init <- c(S=N-1,I=1,R=0, D=0)
      parameters <- c(bet=beta,gamm=gamma, mmu=mu)
      time <- seq(0,t,by=1)
      eqn <- function(time,state,parameters){
        with(as.list(c(state,parameters)),{
          dS <- -(bet*S*I)
          dI <- (bet*S*I)-gamm*I-mmu*I
          dR <- gamm*I
          dD <- mmu*I
          return(list(c(dS, dI, dR, dD)))})
      }
      out<-ode(y=init,times=time,eqn,parms=parameters)
      out.df<-as.data.frame(out)
      
      out.df$Day <- out.df$time
      out.df$Infected <- ceiling(out.df$I)
      out.df$Recovered <- ceiling(out.df$R)
      out.df$Susceptible <- ceiling(out.df$S)
      out.df$Deaths <- ceiling(out.df$D)
      out.df <- out.df %>% select(Day, Susceptible, Infected, Recovered, Deaths)
      out.df
    })
    
    output$histogram <-  plotly::renderPlotly({
      require(deSolve)
      t=150
      N <- input$Population
      deaths <- 0.003*N
      tinfec <- 14
      gamma <- 1/tinfec
      R0 <- input$range1 #numero medio de pessoas infectadas a partir de um infectado
      beta <- (gamma*R0)/N
      mu=deaths/N
      
      init <- c(S=N-1,I=1,R=0, D=0)
      parameters <- c(bet=beta,gamm=gamma, mmu=mu)
      time <- seq(0,t,by=1)
      eqn <- function(time,state,parameters){
        with(as.list(c(state,parameters)),{
          dS <- -(bet*S*I)
          dI <- (bet*S*I)-gamm*I-mmu*I
          dR <- gamm*I
          dD <- mmu*I
          return(list(c(dS, dI, dR, dD)))})
      }
      out<-ode(y=init,times=time,eqn,parms=parameters)
      out.df<-as.data.frame(out)
      out.df$Day <- out.df$time
      out.df$Infected <- ceiling(out.df$I)
      out.df$Recovered <- ceiling(out.df$R)
      out.df$Susceptible <- ceiling(out.df$S)
      out.df$Deaths <- ceiling(out.df$D)
      library(dplyr)
      out.df <- out.df %>% select(Day, Susceptible, Infected, Recovered, Deaths)
      
      confirmed_color <- rgb(234, 240, 67, maxColorValue = 255)
      infected_color <- rgb(220, 40, 40, maxColorValue = 255, alpha = 230)
      recovered_color <- rgb(25, 209, 86, maxColorValue = 255, alpha = 230)
      death_color <- rgb(71, 71, 69, maxColorValue = 255, alpha = 230)
      
      plotly::ggplotly(
        plotly::plot_ly(out.df,
                        x = ~ Day, 
                        y = ~ Susceptible, 
                        type = "bar", 
                        name = "Susceptible",
                        marker = list(color = confirmed_color),
                        opacity = 1) %>%
          plotly::add_trace(y = ~ Infected,
                            name = "Infected",
                            marker = list(color = infected_color),
                            opacity = 0.7) %>%
          plotly::add_trace(y = ~ Recovered,
                            name = "Recovered",
                            marker = list(color = recovered_color),
                            opacity = 0.5) %>%
          plotly::add_trace(y = ~ Deaths,
                            name = "Deaths",
                            marker = list(color = death_color),
                            opacity = 0.8) %>%
          plotly::layout(barmode = 'overlay',
                         yaxis = list(title = "Population"),
                         xaxis = list(title = ""),
                         legend = list(x = 0.2, y = 1),
                         hovermode = "compare"))
    })
    
    
    # Generate an HTML table view of the data ----
    output$table <- renderTable({
      d()
    },digits=0)
    
  }
  
)


```
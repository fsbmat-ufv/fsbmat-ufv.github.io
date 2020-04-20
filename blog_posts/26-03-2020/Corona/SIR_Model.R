#SIR.model <- function(t, b, g, m){
require(deSolve)
t=250
deaths <- 3
N <- 1000
tinfec <- 14
gamma <- 1/tinfec
R0 <- 5 #numero medio de pessoas infectadas a partir de um infectado
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
mytheme4 <- theme_bw() +
  theme(text=element_text(colour="black")) +
  theme(panel.grid = element_line(colour = "white")) +
  theme(panel.background = element_rect(fill = "#B2B2B2"))
  theme_set(mytheme4)
  title <- bquote("SIR Model with mortality")
  subtit <- bquote(list(beta==.(parameters[1]),~gamma==.(parameters[2]),~mu==.(parameters[3])))

res<-ggplot(out.df,aes(x=time))+
    ggtitle(bquote(atop(bold(.(subtit)))))+
    geom_line(aes(y=S,colour="Susceptible"))+
    geom_line(aes(y=I,colour="Infected"))+
    geom_line(aes(y=R,colour="Recovered"))+
    geom_line(aes(y=D,colour="Deaths"))+
    ylab(label="Proportion")+
    xlab(label="Time (days)")+
    theme(legend.justification=c(1,0), legend.position=c(1,0.5))+
    theme(legend.title=element_text(size=12,face="bold"),
          legend.background = element_rect(fill='#FFFFFF',
                                           size=1,linetype="solid"),
          legend.text=element_text(size=10),
          legend.key=element_rect(colour="#FFFFFF",
                                  fill='#C2C2C2',
                                  size=0.5,
                                  linetype="solid"),
          plot.title = element_text(color = "black", size = 12, face = "bold"),
          plot.subtitle = element_text(size = 10, hjust = 0.5))+
    scale_colour_manual("Compartments",
                        breaks=c("Susceptible","Infected","Recovered", "Deaths"),
                        values=c("blue","red","darkgreen", "black"))
res
out.df$Day <- out.df$time
out.df$Infected <- ceiling(out.df$I)
out.df$Recovered <- ceiling(out.df$R)
out.df$Susceptible <- ceiling(out.df$S)
out.df$Deaths <- ceiling(out.df$D)
library(dplyr)
View(out.df <- out.df %>% select(Day, Susceptible, Infected, Recovered, Deaths))  
  
  # ggsave(plot=res,
  #        filename=paste0("SIRplot_","time",t,"beta",b,"gamma",g,"mu",m,".png"),
  #        width=8,height=6,dpi=180)
 # getwd()}

confirmed_color <- rgb(234, 240, 67, maxColorValue = 255)
death_color <- rgb(220, 40, 40, maxColorValue = 255, alpha = 230)
recovered_color <- rgb(25, 209, 86, maxColorValue = 255, alpha = 230)

plotly::ggplotly(plotly::plot_ly(out.df, 
                                   x = ~ Day, 
                                   y = ~ Susceptible, 
                                   type = "bar", 
                                   name = "Susceptible",
                                   marker = list(color = confirmed_color)) %>%
                     plotly::add_trace(y = ~ Infected, 
                                       name = "Infected",
                                       marker = list(color = death_color)) %>%
                   plotly::add_trace(y = ~ Recovered, 
                                     name = "Recovered",
                                     marker = list(color = recovered_color)) %>%
                     plotly::layout(barmode = 'overlay',
                                    yaxis = list(title = "Population"),
                                    xaxis = list(title = ""),
                                    legend = list(x = 0.2, y = 1),
                                    hovermode = "compare"))


###SIR com demografia

#SIR.model <- function(t, b, g, m){
require(deSolve)
t=150
deaths <- 3
N <- 1000
tinfec <- 14
gamma <- 1/tinfec
mu=deaths/N
R0 <- 10 #numero medio de pessoas infectadas a partir de um infectado
beta <- (R0*(gamma+mu))/N


init <- c(S=N-1,I=1,R=0)
parameters <- c(bet=beta,gamm=gamma, mmu=mu)
time <- seq(0,t,by=1)
eqn <- function(time,state,parameters){
  with(as.list(c(state,parameters)),{
    dS <- mmu*N-(bet*S*I)-mmu*S
    dI <- (bet*S*I)-gamm*I-mmu*I
    dR <- gamm*I-mmu*R
    return(list(c(dS, dI, dR)))})
}
out<-ode(y=init,times=time,eqn,parms=parameters)
out.df<-as.data.frame(out)
require(ggplot2)
mytheme4 <- theme_bw() +
  theme(text=element_text(colour="black")) +
  theme(panel.grid = element_line(colour = "white")) +
  theme(panel.background = element_rect(fill = "#B2B2B2"))
theme_set(mytheme4)
title <- bquote("SIR Model with mortality")
subtit <- bquote(list(beta==.(parameters[1]),~gamma==.(parameters[2]),~mu==.(parameters[3])))

res<-ggplot(out.df,aes(x=time))+
  ggtitle(bquote(atop(bold(.(subtit)))))+
  geom_line(aes(y=S,colour="Susceptible"))+
  geom_line(aes(y=I,colour="Infected"))+
  geom_line(aes(y=R,colour="Recovered"))+
  ylab(label="Proportion")+
  xlab(label="Time (days)")+
  theme(legend.justification=c(1,0), legend.position=c(1,0.5))+
  theme(legend.title=element_text(size=12,face="bold"),
        legend.background = element_rect(fill='#FFFFFF',
                                         size=1,linetype="solid"),
        legend.text=element_text(size=10),
        legend.key=element_rect(colour="#FFFFFF",
                                fill='#C2C2C2',
                                size=0.5,
                                linetype="solid"),
        plot.title = element_text(color = "black", size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10, hjust = 0.5))+
  scale_colour_manual("Compartments",
                      breaks=c("Susceptible","Infected","Recovered"),
                      values=c("blue","red","darkgreen"))
res


####################################################

require(deSolve)
t=250
deaths <- 5
N <- 1000
tinfec <- 14
gamma <- 1/tinfec
R0 <- 20 #numero medio de pessoas infectadas a partir de um infectado
beta <- (gamma*R0)/N
q <- 0.8
delta=deaths/N
k=1

#0<=q<=1
init <- c(S=N-1,E=0, I=1, R=0, D=0)
parameters <- c(bet=beta,k=k, gamm=gamma, delt=delta, q=q)
time <- seq(0,t,by=1)
eqn <- function(time,state,parameters){
  with(as.list(c(state,parameters)),{
    dS <- -bet*S*(I+(1-q)*E)
    dE <- bet*S*(I+q*E)-k*E
    dI <- k*E-gamm*I-delt*I
    dR <- gamm*I
    dD <- delt*I
    return(list(c(dS, dE, dI, dR, dD)))})
}
out<-ode(y=init,times=time,eqn,parms=parameters)
out.df<-as.data.frame(out)

require(ggplot2)
mytheme4 <- theme_bw() +
  theme(text=element_text(colour="black")) +
  theme(panel.grid = element_line(colour = "white")) +
  theme(panel.background = element_rect(fill = "#B2B2B2"))
theme_set(mytheme4)
title <- bquote("SIR Model with mortality")
subtit <- bquote(list(beta==.(parameters[1]),~gamma==.(parameters[2]),~mu==.(parameters[3])))

res<-ggplot(out.df,aes(x=time))+
  ggtitle(bquote(atop(bold(.(subtit)))))+
  geom_line(aes(y=S,colour="Susceptible"))+
  geom_line(aes(y=E,colour="Exposto"))+
  geom_line(aes(y=I,colour="Infected"))+
  geom_line(aes(y=R,colour="Recovered"))+
  geom_line(aes(y=D,colour="Morto"))+
  ylab(label="Proportion")+
  xlab(label="Time (days)")+
  theme(legend.justification=c(1,0), legend.position=c(1,0.5))+
  theme(legend.title=element_text(size=12,face="bold"),
        legend.background = element_rect(fill='#FFFFFF',
                                         size=1,linetype="solid"),
        legend.text=element_text(size=10),
        legend.key=element_rect(colour="#FFFFFF",
                                fill='#C2C2C2',
                                size=0.5,
                                linetype="solid"),
        plot.title = element_text(color = "black", size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10, hjust = 0.5))+
  scale_colour_manual("Compartments",
                      breaks=c("Susceptible","Exposto", "Infected", "Recovered", "Morto"),
                      values=c("blue","yellow","red","green", "black"))
res

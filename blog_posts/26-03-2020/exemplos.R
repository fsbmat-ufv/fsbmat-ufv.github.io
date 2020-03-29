remotes::install_github("liibre/coronabr")
library(coronabr)
dados <- get_corona_br(by_uf = TRUE)
head(dados)
library(dplyr)
dados <- dados %>% select(5,11,8,3,7)
dados_jhu <- get_corona_jhu()
df <- dados
xlab <- "Data"
ylab <- "Casos confirmados"
legenda <- "fonte: https://brasil.io/dataset/covid19/caso"
p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$date,
                                      y = .data$confirmed,
                                      color = "red")) +
  ggplot2::geom_line(alpha = .7) +
  ggplot2::geom_point(size = 2) +
  ggplot2::labs(x = xlab,
                y = ylab,
                title = "Casos confirmados de COVID-19 no Brasil",
                caption = legenda) +
  ggplot2::scale_x_date(date_breaks = "1 day",
                        date_labels = "%d/%m") +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                 legend.position = "none")


df$delta_cases <- df$confirmed - dplyr::lag(df$confirmed)
df$diff_perc <- round(df$delta_cases/df$confirmed, 3) * 100
df$label <- paste(df$delta_cases, "%")

ggplot2::ggplot(df,ggplot2::aes(x = .data$date, y = .data$delta_cases)) +
  ggplot2::geom_bar(stat = "identity", alpha = .7, color = "red", fill = "red") +
  ggplot2::scale_x_date(date_breaks = "1 day",
                        date_labels = "%d/%m") +
  ggplot2::scale_y_continuous(limits = c(0, max(df$delta_cases, na.rm = TRUE) + 3),
                              expand = c(0, 0)) +
  ggplot2::geom_text(ggplot2::aes(label = .data$label),
                     size = 2.5,
                     vjust = -0.5) +
  ggplot2::labs(x = xlab,
                y = "% de aumento",
                title = "Aumento nos casos de COVID-19 confirmados",
                caption = legenda) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x =  ggplot2::element_text(angle = 90))

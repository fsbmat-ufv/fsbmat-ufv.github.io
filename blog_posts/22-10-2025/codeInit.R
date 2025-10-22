rm(list = ls())
cat("\014")
library(tidyverse)
library(sampleSelection)
library(ssmodels)
# Lê o CSV com atenção para strings vazias ou lixo
df <- read.csv("data/Incubation_data.csv",
               na.strings = c("", "NA", "?", "NULL", "null", "–", " ", "-", "N/A"))
str(df) 

# ============================================================
# Heckman (CL, tS, SK, BS, Ge) – Incubation_data.csv
# Pacote: ssmodels
# Banco: df  (estrutura fornecida)
# ============================================================

# 0) Pacote
# install.packages("ssmodels") # se necessário
library(ssmodels)

# --- 1) Limpeza dos -9999 (como antes) ---
clean_neg9999 <- function(data, cols) {
  data <- as.data.frame(data)
  for (cc in cols) {
    if (cc %in% names(data)) {
      v <- data[[cc]]
      data[[cc]][!is.na(v) & (v == -9999 | v == -9999.0)] <- NA
    } else {
      warning(sprintf("Coluna '%s' não encontrada.", cc))
    }
  }
  data
}

df <- clean_neg9999(df, cols = "Total_aggregate_associated_C")

# Fatores
df$Soil_type.x     <- factor(df$Soil_type.x,     levels = c("Sandy","Loamy","Clayey"))
df$Soil_moisture.x <- factor(df$Soil_moisture.x, levels = c("Steady","Transient"))

# Seleção: 1 se há valor observado no outcome
df$sel <- as.integer(!is.na(df$Total_aggregate_associated_C))

# (opcional) log do desfecho se todos > 0
df$log_TAAC <- ifelse(!is.na(df$Total_aggregate_associated_C) &
                            df$Total_aggregate_associated_C > 0,
                          log(df$Total_aggregate_associated_C), NA_real_)

# --- 2) FÓRMULAS CORRIGIDAS 
sel_form <- sel ~ Soil_type.x + Days.x + Mean_weight_diameter
out_form <- log_TAAC ~ Soil_type.x + Soil_moisture.x + Mean_weight_diameter

#Modelo Probit
fit1<-glm(sel_form,family=binomial(link=probit),data=df)
summary(fit1)

# 1) Preditor linear por linha (η = Xβ)
eta  <- predict(fit1, type = "link")      # vetor n×1
df$Xbeta <- eta
# 2) CDF e PDF da Normal padrão
Phi  <- pnorm(eta) 
phi  <- dnorm(eta)
df$Phi  <- pnorm(eta)                        # Φ(η)
df$phi  <- dnorm(eta)                        # φ(η)

# 3) Razão de Mills inversa para selecionados (U=1)
#    λ_i = φ(η_i) / Φ(η_i). Use um eps para evitar divisão por ~0.
eps  <- .Machine$double.eps
df$IMR  <- ifelse(df$sel == 1, phi / pmax(Phi, eps), NA_real_)

# (opcional) probabilidade prevista de seleção
p_hat <- predict(fit1, type = "response") # = Φ(η)


#Modelo de Regressão Linear Simples com wage>0
fit2<- lm(log_TAAC ~ Soil_type.x + Soil_moisture.x + Mean_weight_diameter+IMR, data = df[df$sel==1,])
summary(fit2)




theta_HC <- HeckmanCL(selection = sel_form, outcome = out_form, data = df)
summary(theta_HC)

#############Predicao e Anova

X_outcome <- model.matrix(
  ~ Soil_type.x + Soil_moisture.x + Mean_weight_diameter,
  data = df
)

beta_outcome <- theta_HC$coefficients[c("(Intercept)",
                                        "Soil_type.xLoamy",
                                        "Soil_type.xClayey",
                                        "Soil_moisture.xTransient",
                                        "Mean_weight_diameter")]
df$log_TAAC_pred <- as.vector(X_outcome %*% beta_outcome)

# 5.1 Matriz da seleção
X_sel <- model.matrix(
  ~ Soil_type.x + Days.x + Mean_weight_diameter,
  data = df
)

beta_sel <- theta_HC$coefficients[c("(Intercept)",
                                    "Soil_type.xLoamy",
                                    "Soil_type.xClayey",
                                    "Days.x",
                                    "Mean_weight_diameter")]

# 5.2 Índice linear do Probit
xb_sel <- as.vector(X_sel %*% beta_sel)

# 5.3 Razão de Mills inversa (lambda = φ(xb)/Φ(xb))
lambda <- dnorm(xb_sel) / pnorm(xb_sel)

# 5.4 Acrescentar o termo de Mills na equação de resultado
# rho * sigma é o coeficiente da correção
rho <- theta_HC$coefficients["rho"]
sigma <- theta_HC$coefficients["sigma"]

df$log_TAAC_pred_corr <- df$log_TAAC_pred + rho * sigma * lambda

# 1. Substituir valores ausentes de log_TAAC pelas predições corrigidas
df$log_TAAC_complete <- ifelse(
  is.na(df$log_TAAC),
  df$log_TAAC_pred_corr,
  df$log_TAAC
)

# 2. Criar a versão no nível original (TAAC)
df$TAAC_complete <- exp(df$log_TAAC_complete)

# 3. Conferir rapidamente
summary(df$log_TAAC_complete)
summary(df$TAAC_complete)

# 4. Verificar se não sobrou nenhum NA
colSums(is.na(df[, c("log_TAAC_complete", "TAAC_complete")]))

###########Anova de Singh

# ------------------------------------------------------------
# ANOVA para TAAC (corrigido pelo Heckman)
# ------------------------------------------------------------

# Modelo completo com fatores de interesse
anova_model <- aov(
  TAAC_complete ~ Soil_type.x * Soil_moisture.x,
  data = df
)

# Resumo da ANOVA
summary(anova_model)

# Médias ajustadas (efeitos marginais)
library(emmeans)
emmeans(anova_model, ~ Soil_type.x * Soil_moisture.x)

# Comparações múltiplas (Tukey HSD)
TukeyHSD(anova_model)

# ------------------------------------------------------------
# Caso queira analisar também no log (mais próximo da suposição de normalidade)
anova_model_log <- aov(
  log_TAAC_complete ~ Soil_type.x * Soil_moisture.x,
  data = df
)
summary(anova_model_log)

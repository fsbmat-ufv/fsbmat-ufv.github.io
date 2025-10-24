rm(list = ls())
cat("\014")
library(tidyverse)
library(sampleSelection)
library(ssmodels)
# Lê o CSV com atenção para strings vazias ou lixo
df <- read.csv("data/Incubation_data.csv",
               na.strings = c("", "NA", "?", "NULL", "null", "–", " ", "-", "N/A"))
str(df) 
names(df) <- c("ID", 
               "Days.x", 
               "Stype.x", 
               "Smoisture.x",             
               "Rep", 
               "Prop_less_than_53_um", 
               "Prop_250_53_um", 
               "Prop_2000_250_um", 
               "Prop_greater_than_2mm", 
               "MWD", 
               "EOC", 
               "MBC", 
               "TAAC")
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

df <- clean_neg9999(df, cols = "TAAC")

# Fatores
df$Stype.x     <- factor(df$Stype.x,     levels = c("Sandy","Loamy","Clayey"))
df$Smoisture.x <- factor(df$Smoisture.x, levels = c("Steady","Transient"))

# Seleção: 1 se há valor observado no outcome
df$sel <- as.integer(!is.na(df$TAAC))

# (opcional) log do desfecho se todos > 0
df$logTAAC <- ifelse(!is.na(df$TAAC) &
                            df$TAAC > 0,
                          log(df$TAAC), NA_real_)

# --- 2) FÓRMULAS CORRIGIDAS 
sel_form <- sel ~ Stype.x + Days.x + MWD
out_form <- logTAAC ~ Stype.x + Smoisture.x + MWD

#Modelo Probit
fit1<-glm(sel ~ Stype.x + Days.x + MWD, family=binomial(link=probit),data=df)
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
fit2<- lm(logTAAC ~ Stype.x + Smoisture.x + MWD+IMR, data = df[df$sel==1,])
summary(fit2)

library(sampleSelection)

two <- heckit( sel_form, out_form, data = df,
        method = "2step")
summary(two)

m <- selection(selection = sel_form, outcome = out_form, data = df)
summary(m)

theta_HC <- HeckmanCL(selection = sel_form, outcome = out_form, data = df)
summary(theta_HC)

#############Predicao e Anova

X_outcome <- model.matrix(
  ~ Stype.x + Smoisture.x + MWD,
  data = df
)

beta_outcome <- theta_HC$coefficients[c("(Intercept)",
                                        "Stype.xLoamy",
                                        "Stype.xClayey",
                                        "Smoisture.xTransient",
                                        "MWD")]
df$logTAAC_pred <- as.vector(X_outcome %*% beta_outcome)

# 5.1 Matriz da seleção
X_sel <- model.matrix(
  ~ Stype.x + Days.x + MWD,
  data = df
)

beta_sel <- theta_HC$coefficients[c("(Intercept)",
                                    "Stype.xLoamy",
                                    "Stype.xClayey",
                                    "Days.x",
                                    "MWD")]

# 5.2 Índice linear do Probit
xb_sel <- as.vector(X_sel %*% beta_sel)

# 5.3 Razão de Mills inversa (lambda = φ(xb)/Φ(xb))
lambda <- dnorm(xb_sel) / pnorm(xb_sel)

# 5.4 Acrescentar o termo de Mills na equação de resultado
# rho * sigma é o coeficiente da correção
rho <- theta_HC$coefficients["rho"]
sigma <- theta_HC$coefficients["sigma"]

df$logTAAC_pred_corr <- df$logTAAC_pred + rho * sigma * lambda

# 1. Substituir valores ausentes de logTAAC pelas predições corrigidas
df$logTAAC_complete <- ifelse(
  is.na(df$logTAAC),
  df$logTAAC_pred_corr,
  df$logTAAC
)

# 2. Criar a versão no nível original (TAAC)
df$TAAC_complete <- exp(df$logTAAC_complete)

# 3. Conferir rapidamente
summary(df$logTAAC_complete)
summary(df$TAAC_complete)

# 4. Verificar se não sobrou nenhum NA
colSums(is.na(df[, c("logTAAC_complete", "TAAC_complete")]))

###########Anova de Singh

# ------------------------------------------------------------
# ANOVA para TAAC (corrigido pelo Heckman)
# ------------------------------------------------------------

# Modelo completo com fatores de interesse
anova_model <- aov(
  TAAC_complete ~ Stype.x * Smoisture.x,
  data = df
)

# Resumo da ANOVA
summary(anova_model)

# Médias ajustadas (efeitos marginais)
library(emmeans)
emmeans(anova_model, ~ Stype.x * Smoisture.x)

# Comparações múltiplas (Tukey HSD)
TukeyHSD(anova_model)

# ------------------------------------------------------------
# Caso queira analisar também no log (mais próximo da suposição de normalidade)
anova_model_log <- aov(
  logTAAC_complete ~ Stype.x * Smoisture.x,
  data = df
)
summary(anova_model_log)

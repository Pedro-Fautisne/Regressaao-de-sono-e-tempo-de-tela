library(dplyr)
library(bnlearn)
library(Rgraphviz)
library(ggplot2)
library(lmtest)
library(corrplot)
library(car)
library(faraway)


dados = read.csv("sleep_mobile_stress_dataset_15000.csv")

dados = dados %>%
  select(c(-user_id, -age, -gender))

View(dados)

modelo_completo = lm(stress_level ~ sleep_duration_hours + sleep_quality_score + mental_fatigue_score
              + phone_usage_before_sleep_minutes + daily_screen_time_hours
              + caffeine_intake_cups + physical_activity_minutes
              + notifications_received_per_day, data = dados)

summary(modelo_completo)

selecao_variaveis = step(modelo_completo, direction = "both")
selecao_variaveis

dados_grafo = dados
grafo_bn = hc(dados_grafo)

pdf("grafo_bayesiano.pdf", width = 12, height = 10) 
graphviz.plot(grafo_bn, shape = "ellipse", main = "Rede Bayesiana")
dev.off()

#VARIAVEIS ESCOLHIDOS, com base no seu nível de significância pelo P-valor e pela Rede Bayesiana
#Variável dependente - Nível de Estresse
#Variáveis independentes - Horas de sono, Qualidade de sono, Fadiga Mental e Tempo diário de tela

modelo_selecionado = lm(stress_level ~ sleep_duration_hours + sleep_quality_score
                        + mental_fatigue_score + daily_screen_time_hours, data = dados)


summary(modelo_selecionado)

#TESTES DE PRÉ-REQUISITOS

#Normalidade dos resíduos
#1 - Teste Shapiro Wilk com reamostragem para 5 mil amostras

set.seed(123)
shapiro.test(sample(resid(modelo_selecionado), 5000))

#2 - Teste Kolmogorov-Smirnov
ks.test(resid(modelo_selecionado), "pnorm", mean(resid(modelo_selecionado)), sd(resid(modelo_selecionado)))

#3 - Analise Grafica
plot(modelo_selecionado, 2)

hist(resid(modelo_selecionado), breaks = 50, col = "lightblue", 
     main = "Distribuição dos Resíduos", xlab = "Resíduos")

#Grafico Half-Normal Plot

residuos_modelo <- rstudent(modelo_selecionado)

halfnorm(residuos_modelo, 
         nlab = 3,
         ylab = "Resíduos Studentizados Absolutos", 
         main = "Half-Normal Plot - Modelo de Estresse")

#OS REÍDUOS TEM UMA DISTRIBUIÇÃO APROXIMADAMENTE NORMAL


#Linearidade das variáveis

plot(modelo_selecionado, 1)

lmtest::raintest(modelo_selecionado)

#AS VARIÁVEIS SÃO LINEARES ENTRE SI


#Multicolinearidade (Correlação entre as variáveis independentes)

dados = dados %>%
  select(c(-phone_usage_before_sleep_minutes, -caffeine_intake_cups, -occupation,
           -physical_activity_minutes, -notifications_received_per_day))

(corrplot::corrplot(cor(dados)))

car::vif(modelo_selecionado)

#PARECE QUE ESTE MODELO NÃO TEM MULTICOLINEARIDADE


#Teste da Homocedasticidade (Homogeneidade da Variância)

lmtest::bptest(modelo_selecionado)

#Os resíduos têm homocedasticidade, ou seja, a variância deles não é homogênea no conjunto de dados

#Teste da Autocorrelação

acf(resid(modelo_selecionado))

lmtest::dwtest(modelo_selecionado)

#OS RESÍDUOS NÃO TEM AUTOCORRELAÇÃO


#Explorando o modelo selecionado, passou em quase todos os testes de pré-requisitos
anova(modelo_selecionado)

#O modelo é significativo para todas as variáveis
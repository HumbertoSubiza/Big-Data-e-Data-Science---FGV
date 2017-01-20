# Curso de Big Data e Data Science - FGV
# Prof: Pedro Costa Ferreira
# github.com/pedrocostaferreira
# Aluno: Walter Humberto Subiza Pina
# walter.pina1954@gmail.com

# Análise de serie temporal do IBGE sobre o volume de vendas do comercio
# varejista ampliado. Serie em:
#http://seriesestatisticas.ibge.gov.br/series.aspx?no=2&op=0&vcodigo=MC67&t=volume-vendas-comercio-varejista-ampliado-tipos
# extraída em 02/12/2016

# carregando bibliotecas necessárias
require(BETS)
require(urca)
require(TSA)
require(forecast)
require(normtest)
require(FinTS)
require(xlsx)
require(dygraphs)
require(nortest)

# carregando arquivo de dados
vendas <- read.csv("/data/IBGE_VVCVA.csv", 
                   header=FALSE, 
                   dec=".", 
                   stringsAsFactors = F)
#transposição de linha a coluna em dataframe
vendas2 <- as.data.frame(t(vendas))

# transformação em serie temporal
st_Vendas <- ts(vendas2, 
                start     = c(2003,1), 
                frequency = 12)
#plotagem da serie
ts.plot(st_Vendas)
#plotagem da serie, nota-se variação sazonal muito semelhante
# até o ano de 2008.
dygraph(st_Vendas)

# Gráfico para análise de médias e tendencia
monthplot(st_Vendas)

# decomposição da série nas principais componentes
plot(decompose(st_Vendas))

# observase que o valor mínimo de vendas está proximo de 100 e o máximo de 190.
# a componente de tendencia afeta fortemente a série (em torno de 70%). A componente
# sazonal presente na série afeta em torno de 25% e o RB é de pequeno valor e não
# aparenta estar contaminado pela componente sazonal
# Precisa-se modelar as componentes de tendencia e sazonal, o que amerita o uso
# do modelo SARIMA

# função de autocorrelação
# BETS.corrgram(ts, lag.max = 12, type = "correlation", mode = "simple",
#               ci = 0.95, style = "plotly", knit = F)
# ainda nao eh estacionaria, pelo grafico, já que não tem
# decrescimento exponencial ou senoidal
BETS.corrgram(st_Vendas, 
              lag.max = 36) # 36 = 3 anos

# testes de RU para determinação o número de diferenciações necessárias
# para transformar a ST em estacionária
# usarei ADF (Augmented Dickey Fuller)

adf.drift1 <- ur.df(y          = st_Vendas, 
                   type       = c("drift"),
                   lags       = 24, 
                   selectlags = "AIC")
summary(adf.drift1)

# Ao analisar a estatística de teste (tau2 = 1,367) notamos que seu valor é superior 
# ao valor crítico associado ao nível de confiança de 95% (-2,88). 
# Dessa forma, conclui-se que a ST não é estacionária (não rejeição da hipotese nula)                                                                                     hipótese nula).
# Dado que a nossa ST é não estacionária, vamos tentar torná-la estacionária fazendo 
# uma diferenciação e vamos observar o gráfico e a FAC novamente.


# tornando a serie estacionaria...tirando a tendencia
ts.plot(diff(st_Vendas, 
             lag         = 1, 
             differences = 1))

# a ST aparenta ser estacionária, a variancia tem um crescimento
# pequeno nos últimos anos, condicente com a teoria económica de crescimento
# vamos usar a funcao log para tirar o crescimento da variancia
ts.plot(diff(log(st_Vendas), 
             lag         = 1, 
             differences = 1))

# gráfico mostra agora a ST estacionária na média e na variancia, acorde
# a teoria de Box & Jenkins


#Avaliando a estacionariedade da parte sazonal

BETS.corrgram(diff(log (st_Vendas),
                            lag         = 1, 
                            differences = 1), 
              lag.max = 60)
# nota-se cortes nos lags 12, 24 e 36...

# diferença simples
diff.simples <- diff(log(st_Vendas), 
                             lag         = 1, 
                             differences = 1)
# diferença sazonal (12)
diff.sazonal <- diff(diff.simples, 
                     lag         =12, 
                     differences = 1)
ts.plot(diff.sazonal)


BETS.corrgram(diff.sazonal, 
              lag.max = 36)

BETS.corrgram(diff.sazonal, 
              lag.max = 36, 
              type    = "partial")

# Refazendo o teste de RU depois das tranformações
adf.drift2 <- ur.df(y = diff(diff(log(st_Vendas), 
                                  lag = 1), 
                            lag = 12),
                    type       = "drift", 
                    lags       = 24, 
                    selectlags = "AIC")

summary(adf.drift2)

acf(adf.drift2@res, 
    lag.max    = 36, 
    drop.lag.0 = T)

# teste de RU tau2= -2.972 é inferior ao valor crítico de -2.89,
# com 95% de probabilidade pelo que podemos concluir que a ST é estacionária 

# Modelado da ST com SARIMA
# automático para ver a sugestão de modelo
auto.arima(st_Vendas)

# Sugestão de modelo: ARIMA(0,1,1)(0,1,2)[12], AIC=611.23

fit.vendas1 <- Arima(st_Vendas,
                    order    = c(0,1,1), 
                    seasonal = c(0,1,2),
                    lambda   = 0)
summary(fit.vendas1)

# Resultados: AIC=-351.15   AICc=-350.71   BIC=-340.94

# outro modelo
fit.vendas2 <- Arima(st_Vendas, 
                     order    = c(0,1,1), 
                     seasonal = c(0,1,1),
                     lambda   = 0)
summary(fit.vendas2)

# Resultados: AIC=-347.34   AICc=-347.08   BIC=-339.68

BETS.t_test(fit.vendas1) 
BETS.t_test(fit.vendas2)

accuracy(fit.vendas1)
accuracy(fit.vendas2)# ver MAPE mean absolute percentage error 2,43% erro!!

# Diagnóstico - Verificar:
#   - Ausência de autocorrelação linear;
#   - Ausência de heterocedasticidade condicional;
#   - Normalidade.

diag1 <- tsdiag(fit.vendas1, gof.lag = 36)  
diag2 <- tsdiag(fit.vendas2, gof.lag = 36)
# analise para diag2:
# 1- os dados aparentam estar distribuídos simetricamente em torno da média zero, 
# indicação de distribuição normal. Não temos nenhuma informação discrepante 
# fora do intervalo [-3,3], apenas um dado em dez 2009.
# 2- FAC dos resíduos sem presença de lags significantes
# 3- O valor de p do teste não rejeita a H0 e diminui significativamente após a 
# desfazagem 14

# 1- Verificar a autocorrelação linear
Box.test(x = fit.vendas1$residuals, lag = 24,type = "Ljung-Box", fitdf = 2)
Box.test(x = fit.vendas2$residuals, lag = 24,type = "Ljung-Box", fitdf = 2)

# analise para fit.vendas2
# p-value = 0.04013
# a 95% de confiança não rejeitamos a hipótese nula de não existência de 
# autocorrelação serial até o lag 24.

# 2 - Ausência de heterocedasticidade condicional;
ArchTest(fit.vendas1$residuals,lags = 12)
ArchTest(fit.vendas2$residuals,lags = 12)

# analise para fit.vendas2
# Conforme mostrado pelo teste, a hipótese nula é que não há presença de efeito ARCH. 
# Dessa forma, dado o valor do p-valor = 0.7398, não rejeitamos a hipótese nula 
# a 95% de confiança, ou seja, a variância é estacionária.

#   - Normalidade dos resíduos
# o teste de Jarque-Bera para normalidade, verifica se a amostra de dados tem
# uma curtosis e simetria semelhante a uma distribuição normal.
# se os dados vem de uma dist. normal, o teste JB tem, asintóticamente, uma 
# distribuição chi-quadrado com dois graus de liberdade, e assim a estatística pode
# ser usada para verificar a hipótese H0, de que os dados provem de uma dist. normal.
# A H0 do teste diz que a assimetria e a curtosis tem de ser zero (ou 3 para curtosis,
# que é o mesmo). Qualquer diferença com essa hipótese, incrementa o valor da estatística
# JB.
# Para amostras pequenas de dados, a aproximação de chi-quadrado é muito sensível
# a variações, causando a rejeição ainda que H0 seja verdadeira e levando assim a erros
# do tipo I. 
# Para um tamanho de amostra de aprox. 100 elementos o p-valor calculado equivalente
# ao verdadeiro nível de alfa é: 0.062 (valores aprox por simulação Monte Carlo).
# Teste JB
jb.norm.test(fit.vendas2$residuals, nrepl = 2000)
# p-value = 0.002
# como se vê o teste rejeita H0 de que a amostra provem de dist. normal
# vamos verificar qual das duas condições do teste é violada:

# Curtosis
kurtosis.norm.test(fit.vendas2$residuals, nrepl=2000)
# p-value = 0.0015, a curtosis é rejeitada

# simetria
skewness.norm.test(fit.vendas2$residuals, nrepl=2000)
# p-value = 0.6495
# a simetria é aceita ou seja os resíduos tem simetria mas não tem
# a curtose de 0 (3) necessária

# Pelo número baixo de resíduos, aceita-se a hipótese de normalidade
# dos mesmos, por possívelmente estar em presença de um erro tipo I
# (rejeição quando H0 é verdadeira, ou falso positivo)

# Para amostras de tamanho pequeno (maior a 4), o pacote nortest tem a função
# lillie.test - Lilliefors (Kolmogorov-Smirnov) test for normality
# 
lillie.test(fit.vendas2$residuals)
# o valor p-value = 0.3277, estabelece que não podemos rejeitar a hipótese
# de que a amostra de resíduos é normal

# Assim sendo passamos a etapa da previsão de vendas para os próximos
# 12 meses, com nível de confiança de 95%
previsao <- forecast(fit.vendas2, 
                     h    =12,        # 12 meses
                     level=0.95)
plot(previsao)

write.csv(data.frame(previsao), "previsao_vendas.csv")
write.xlsx(data.frame(previsao), "previsao_vendas.xlsx")

# Fim do script

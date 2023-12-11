# carregar pacotes
library(readr)
library(dplyr)
library(QCA)
library(SetMethods)

# carregar base de dados
df = read.csv('preprocessed_dataset.csv')

# selecionar colunas para QCA
df = df[,c('pais','cooperou','solicit_refug_previa','concordo_ajudarefugiados', 'dias_governo_partido_esquerda')]

# nomear paises como labels das linhas
rownames(df) = df$pais


#===============================
# CALIBRAGEM
#===============================
# Calibragem e essencialmente um processo de formacao e definicao 
# de conceitos que interage com a sua teoria. Sap usados criterios conceituais e 
# teoricos para o ponto de interseção (crossoverpoint), evitabdo usar criterios 
# puramente empiricos, como estatisticas descritivas.

# alemanha
df$concordo_ajudarefugiados[df$pais== 'Germany']
df$cooperou[df$pais== 'Germany']
df$solicit_refug_previa[df$pais== 'Germany']


#========== alvo cooperacao ============

# por meio do corte de porcentagem da aleamanha
df = mutate(df, cooperou = ifelse(cooperou > 37, 1, 0))


#========== partido governo de esquerda ============

# calibrar
df$dias_governo_partido_esquerda_cal = round(calibrate(df$dias_governo_partido_esquerda, 
                                                       type = "fuzzy", 
                                                       thresholds = "e=0, c=365, i=547.5", 
                                                       logistic=TRUE), digits=2)

# visualizar grafico da calibragem
plot(df$dias_governo_partido_esquerda, df$dias_governo_partido_esquerda_cal, pch=18, col="black", main='',
     xlab='Valor Bruto',
     ylab='Valor Fuzzy')+
  abline(h=0.5, col="black")+
  abline(v= 547.5, col="black")

#========== quantidade de refugiados ============

# calibrar
df$solicit_refug_previa_cal = round(calibrate(df$solicit_refug_previa, 
                                              type = "fuzzy", 
                                              thresholds = "e=1, c=2, i=4.1", 
                                              logistic=TRUE), digits=2)

# visualizar grafico da calibragem
plot(df$solicit_refug_previa, df$solicit_refug_previa_cal, pch=18, col="black", main='',
     xlab='Valor Bruto',
     ylab='Valor Fuzzy')+
  abline(h=0.5, col="black")+
  abline(v= 4.1, col="black")


#========== concordo ajudar refugiados ============

df$concordo_ajudarefugiados_cal = round(calibrate(df$concordo_ajudarefugiados, 
                                                  type = "fuzzy", 
                                                  thresholds = "e=0.3, c=0.5, i=0.85", 
                                                  logistic=TRUE), digits=2)


plot(df$concordo_ajudarefugiados, df$concordo_ajudarefugiados_cal, pch=18, col="black", main='',
     xlab='Valor Bruto',
     ylab='Valor Fuzzy')+
  abline(h=0.5, col="black")+
  abline(v= 0.85, col="black")


#================== selecionar subset calibrado
subset = df[,c('solicit_refug_previa_cal','concordo_ajudarefugiados_cal', 'dias_governo_partido_esquerda_cal')]


#================================
# CONDICOES NECESSARIAS
#================================


# condicoes necessarias isoladamente
pof(subset, 'cooperou', df, relation = "nec")

# condicoes necessarias em combinacao
super = superSubset(df, outcome = "cooperou",
                    conditions = "solicit_refug_previa_cal, concordo_ajudarefugiados_cal, dias_governo_partido_esquerda_cal",
                    incl.cut = 0.7, 
                    ro = 0.4, 
                    cov.cut = 0.4)

super


#==============================
# CONDICOES SUFICIENTES
#=============================


# condicoes necessarias isoladamente
pof(subset, 'cooperou', df, relation = "suf")


#==========================
# TABELA DA VERDADE
#==========================


##====================== Solucao Parcimoniosa  ======================##

ttSURV <- truthTable(data=df, 
                     outcome = "cooperou", 
                     conditions = "solicit_refug_previa_cal, concordo_ajudarefugiados_cal, dias_governo_partido_esquerda_cal",
                     incl.cut=0.7, 
                     sort.by="incl, n", 
                     complete=FALSE, 
                     show.cases=TRUE) 
ttSURV 

# minimizacao da tabela da verdade
minimize(ttSURV, details=TRUE, show.cases=TRUE, row.dom=TRUE, all.sol=FALSE, use.tilde=FALSE)


##====================== Solucao Completa  ======================##

ttSURV <- truthTable(data=df, 
                     outcome = "cooperou", 
                     conditions = "solicit_refug_previa_cal, concordo_ajudarefugiados_cal",
                     incl.cut=0.7, 
                     sort.by="incl, n", 
                     complete=FALSE, 
                     show.cases=TRUE) 
ttSURV 

# minimizacao da tabela da verdade
minimize(ttSURV, details=TRUE, show.cases=TRUE, row.dom=TRUE, all.sol=FALSE, use.tilde=FALSE)

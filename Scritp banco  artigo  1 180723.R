############################Caraterísticas maternas, preditores associados ao Ganho de peso de peso gestacional: Estudo Coorte Araraquara Brasil####################################################################################
######################################################################### Banco de dados Araraquara - Tese#########################################################################
###################################################################################################################################################################################

########################################################################### defenir o directorio de trabalho#######################################################################

setwd("D:/PhD Files/Tese/Tese Artigos/Arttigo 1")


#### importando e instalando pacotes 


library(psych)    # Um pacote da area de pscometria com muitas funcoes 
library(pipeR) ## Pacote para usar a funcao pipe
require(dplyr) #pacote com diversas fun??es -> olhar help
library(tidyverse) #pacote com diversas fun??es -> olhar help
library(foreign) #importar dados
library(haven)
library(readxl)
library(readxl)



## Importando o dataset com do excel incluindo todas Sheet
q1 <- read_excel("dados tese1.xls", sheet = "Questionário 1")
q2 <- read_excel("dados tese1.xls", sheet = "Questionário 2")
q3 <- read_excel("dados tese1.xls", sheet = "Questionário 3")
q8 <- read_excel("dados tese1.xls", sheet = "Questionário 8")
q9 <- read_excel("dados tese1.xls", sheet = "Questionário 9")
q11 <- read_excel("dados tese1.xls", sheet = "Questionário 11")
q12 <- read_excel("dados tese1.xls", sheet = "Questionário 12")
q13 <- read_excel("dados tese1.xls", sheet = "Questionário 13")
q14 <- read_excel("dados tese1.xls", sheet = "Questionário 14")
q15 <- read_excel("dados tese1.xls", sheet = "Questionário 15")
q16 <- read_excel("dados tese1.xls", sheet = "Questionário 16")
q17 <- read_excel("dados tese1.xls", sheet = "Questionário 17")
q18 <- read_excel("dados tese1.xls", sheet = "Questionário 18")
q19 <- read_excel("dados tese1.xls", sheet = "Questionário 19")
q20 <- read_excel("dados tese1.xls", sheet = "Questionário 20")
q21 <- read_excel("dados tese1.xls", sheet = "Questionário 21")


dados1 <- bind_cols(q1,q2,q3,q8,q9,q11,q12,q13,q14,q15,q16,q17,q18,q19,q20,q21)

### lidando agora com os valores Omissisos

dados1[dados1 == 999] <- NA
dados1[dados1 == 9999] <- NA
dados1[dados1 == 999999] <- NA
dados1[dados1 == 99999] <- NA
dados1[dados1 == 	 99999999] <- NA
dados1[dados1 == 	 333884.3 ] <- NA
dados1[dados1 == 777] <- NA
dados1[dados1 == 7777] <- NA
dados1[dados1 == 77777] <- NA
dados1[dados1 == 777777] <- NA
dados1[dados1 == 5555] <- NA
dados1[dados1 == 555] <- NA
dados1[dados1 == 8888] <- NA
dados1[dados1 == 888] <- NA
#################################################### remover as linhas com NA na Varival Peso pregestacional e Posparto #########################################################

dados1 <- dados1[complete.cases(dados1$d_PesoParto),]
dados1 <- dados1[complete.cases(dados1$a_pesopre),]
dados1 <- dados1[complete.cases(dados1$a_estat1),]

####################################################################################################################################################################################
########################################## Base de dados com as variveis   novas ###################################################################################################
####################################################################################################################################################################################

### Criar um subset para predicacao com as features

dados <- subset(dados1,select=c( "a_idade","a_cor", "a_civil","a_rendpcr","a_escola","a_fumog","a_alcool","a_npari","a_npcomo",  
                                "a_estat1",  "a_circbracm","a_pesopre","d_PesoParto", "a_fmp", "a_vigorh","a_moderh", "d_IGUSGrn", "a_pcr",	
                                  "a_homa","a_hb" ,"a_hba1c", "a_insul","a_tg", "a_ct", "a_hdl", "a_ldl","a_agdm","b_agdm","c_agdm", "a_aghas","a_agui", "a_agsif", "a_agceva",
                                "f_FMP",  "f_FFMp"))


###################################################################################################################################################################################
############################################# As variaveis para o artigo de Influencia GWG e desfechos materno e e do concepto################################################################################
###################################################################################################################################################################################


 a_idade # Idade materna (0)  ≤19/ (1) 20-35/ (2) ≥35)
 a_cor # raca,((1) branca / (2) preta / (3) amarela / (4) indígena / (5) parda)  
 a_civil # estado civil --- Situação conjugal da gestante (1) casada / (2) solteira (com companheiro) / (3) solteira (sem companheiro) / (4) separada/viúva
 a_rendpcr    # Renda per capita
 a_escola # Escolaridade  da gestante (em anos de estudos) : (0) ≤4/ (1) 5-11 /  (2) ≥12)
 a_moradia # tipo de moradia: (0)Alugada/ (1)Própria quitada / (2) Própria não quitada/ (3) Posse/(4)Emprestada /(5)Outra
 a_materia	#Material de construção da residência: (0) Madeira, (1) Alvenaria, (2) “pau a pique”, (3) Outro
 a_npcomo	 #Número de pessoas por cômodo
 a_fumog # Fumar
 a_alcool # Uso de álcool na gestação
 a_npari  # N° de gestações anteriores
 a_npari	# Quantas vezes a pessoa já pariu
 a_estat1 # altura, 
 a_circbracm #Circunferência do braço
 a_imcga # IMC gestacional atual (Kg/m²)
 a_pesopre # peso pregestational, 
 d_PesoParto # Peso pos parto
 a_fmp # Percentual de gordura corporal (%)
 a_moderd  #ctividade fisica (Em quantos dias de uma semana normal, você realiza atividades Moderada
 a_vigord #actividade fisica (Em quantos dias de uma semana normal, você realiza atividades VIGOROSAS por pelo menos 10 minutos contínuos, como por exemplo, correr, fazer ginástica, aeróbica, jogar futebol, pedalar rápido na bicicleta, jogar basquete, fazer serviços domésticos pesados em casa, no quintal ou no jardim, carregar pesos elevados ou qualquer atividade que faça você suar BASTANTE ou aumentem MUITO sua respiração ou batimentos do coração?),
 d_IGDUMrn	#Idade Gestacional ao nascimento (DUM) em semanas e dias
 a_insul	  #Insulina de jejum (uUI/mL)
 a_homa	   #HOMA (uUI/mL)  Normal:<= 1.65 Alterado:>1.65
 a_hba1c # hemoglobina glicada, Normal: <= 6.5,Alterado:>6.5
 a_tg #	Triglicerídeos (mg/dL)
 a_ct # colestreol
 a_hdl# HDL
 a_ldl# LDL
 a_agdm # Diabetes antes e depois da gestacao 
 d_aghas # hipertensao
 gpg # ganho de peso gestacional
 

##################################################### Transformar as varariaveis em numerica #######################################################################################

dados <- dados %>%  mutate_all(as.numeric)

############################################# Peso pos parto  ##########################################################
sort(dados$d_PesoParto, decreasing = T)
summary(dados$d_PesoParto)
dados$d_PesoParto[dados$d_PesoParto == 989.50] <- 98.9

 ####################################################################################################################################################################################
 ########################################## Criando a varivel GPG de acordo com IOM###################################################################################################
 ####################################################################################################################################################################################
 
# Leitura dos dados de IMC pré-gestacional e ganho de peso gestacional

 # Cálculo do IMC pré-gestacional
dados$imc <-dados$a_pesopre/(dados$a_estat1/100)^2


 # Calcular o ganho de peso total
dados$gpg <- dados$d_PesoParto - dados$a_pesopre
summary(dados$gpg)

## O ganho de peso gestacional quantitativo 
dados$categoria1<- dados$gpg 

 # Categorização do ganho de peso gestacional
dados$gpg_cat <-  for (i in 1:nrow(dados)) {
   if (dados$imc[i] < 18.5) {
     if (dados$gpg[i] < 12.5) {
       dados$categoria[i] <- "Abaixo"
     } else if (dados$gpg[i] >= 12.5 && dados$gpg[i] <= 18) {
       dados$categoria[i] <- "Dentro"
     } else {
       dados$categoria[i] <- "Acima"
     }
   } else if (dados$imc[i] >= 18.5 && dados$imc[i] < 25) {
     if (dados$gpg[i] < 11.5) {
       dados$categoria[i] <- "Abaixo"
     } else if (dados$gpg[i] >= 11.5 && dados$gpg[i] <= 16) {
       dados$categoria[i] <- "Dentro"
     } else {
       dados$categoria[i] <- "Acima"
     }
   } else if (dados$imc[i] >= 25 && dados$imc[i] < 30) {
     if (dados$gpg[i] < 7) {
       dados$categoria[i] <- "Abaixo"
     } else if (dados$gpg[i] >= 7 && dados$gpg[i] <= 11.5) {
       dados$categoria[i] <- "Dentro"
     } else {
       dados$categoria[i] <- "Acima"
     }
   } else {
     if (dados$gpg[i] < 5) {
       dados$categoria[i] <- "Abaixo"
     } else if (dados$gpg[i] >= 5 && dados$gpg[i] <= 9) {
       dados$categoria[i] <- "Dentro"
     } else {
       dados$categoria[i] <- "Acima"
     }
   }
 }
 
 # Exibição dos resultados
 table(dados$categoria)
 prop.table(table(dados$categoria))*100
 
 
 # Reordenar a ordem das categorias  das varivaies 
 dados$categoria <- factor(dados$categoria, levels = c("Dentro", "Abaixo", "Acima"))
 
 ##################################################################################################################################################################################
 ########################################################### Criar categorias de IMC pregestacional ##############################################################################
 ##################################################################################################################################################################################
 ##################################################################################################################################################################################
 dados$cat_imc <- cut(dados$imc, 
                      breaks = c(0, 18.5, 25, 30, Inf), 
                      labels = c("Baixo peso", "Peso normal", "Sobrepeso", "Obesidade"))
 
 table(dados$cat_imc)
 
 ###################################################################################################################################################################################
 ########################################## construcao de tabelas descritivas  das variveis preditoras e o Desfecho - GPG ##########################################################
 ###################################################################################################################################################################################

 #########################################Tabela de contingencia Idade por GPG #########################################
 ############################################# Idade ##########################################################
 
 # Transformando valores negativos em positivos
 dados$a_idade <- abs(dados$a_idade)
 
 # Removendo valores iguais a 0
 dados <- dados[dados$a_idade != 0,]
 
 hist1 <- subset(dados, a_idade < 70)
 hist(hist1$a_idade,
      main = "Histograma valores da idade abaixo de 70",
      xlab = "Idade",
      ylab = "Frequência")
 
 # Categorizando as idades em três grupos: <=19, 20-35 e >=35
 dados$a_idade <- cut(dados$a_idade, breaks = c(-Inf, 19, 35, Inf), 
                      labels = c("<=19", "20-35", ">=35"))
 

 
table(dados$a_idade)
prop.table(table(dados$a_idade))*100
table(dados$a_idade,dados$categoria)
prop.table(table(dados$a_idade,dados$categoria))*100

 # Teste de Quiquadrado
Modelo_idade <- chisq.test(dados$a_idade, dados$categoria)
print(Modelo_idade)


#########################################alcular a média e o desvio padrão da  estatura por GPG#########################################
############################################################# Estatura #######################################################
sort(dados$a_estat1, decreasing = T)
summary(dados$a_estat1)

hist <- subset(dados, a_estat1 < 200)
hist(hist$a_estat1, 
     main = "Histograma de a_estat1 (valores abaixo de 200)",
     xlab = "a_estat1",
     ylab = "Frequência")

dados$a_estat1<- ifelse (dados$a_estat1 < 99, dados$a_estat1 * 100, dados$a_estat1)


summary(dados$a_estat1)

aggregate(a_estat1 ~ categoria, data = dados, FUN = function(x) c(mediana = median(x), IQR = (quantile(x, c(0.25, 0.75)))))

# Teste de normalidade
shapiro.test(dados$a_estat1)

# Teste de homogeneidade de variâncias
bartlett.test(a_estat1 ~ categoria, data = dados)

# Realizar o teste  kruskal Wallis
modelo_esta <- kruskal.test(a_estat1 ~ categoria, data = dados)
modelo_esta

# Realizando comparações múltiplas com o método de Bonferroni
comp_esta <- pairwise.wilcox.test(dados$a_estat1, dados$categoria, p.adjust.method = "bonferroni")
comp_esta
##########################################################Categoria #######################################################################################
summary(dados$a_estat1)

dados$cat_estat <- cut(dados$a_estat1, 
                       breaks = quantile(dados$a_estat1, probs = c(0, 1/3, 2/3, 1)),
                       labels = c("1º Tercil", "2º Tercil", "3º Tercil"))

table(dados$cat_estat)
prop.table(table(dados$cat_estat)) * 100
table(dados$cat_estat, dados$categoria)
prop.table(table(dados$cat_estat, dados$categoria)) * 100
Modelo_estat <- chisq.test(dados$cat_estat, dados$categoria)
Modelo_estat


########################################## calcular a média e o desvio padrão do Peso Pre-gestational por GPG#########################################
summary(dados$a_pesopre)

aggregate(a_pesopre ~ categoria, data = dados, FUN = function(x) c(mediana = median(x), IQR = (quantile(x, c(0.25, 0.75)))))

# Teste de normalidade
shapiro.test(dados$a_pesopre)

# Teste de homogeneidade de variâncias
bartlett.test(a_pesopre ~ categoria, data = dados)

# Realizar o teste  kruskal Wallis
modelo_pesopre <- kruskal.test(a_pesopre ~ categoria, data = dados)
modelo_pesopre

# Realizando comparações múltiplas com o método de Bonferroni
comp_pesopre <- pairwise.wilcox.test(dados$a_pesopre, dados$categoria, p.adjust.method = "bonferroni")
comp_pesopre

##########################################################Categoria #######################################################################################

summary(dados$a_pesopre)
dados$cat_pesopre <- cut(dados$a_pesopre, breaks = quantile(dados$a_pesopre, probs = c(0, 1/3, 2/3, 1)),
                            labels = c("1º Tercil", "2º Tercil", "3º Tercil"))

table(dados$cat_pesopre)
prop.table(table(dados$cat_pesopre)) * 100
table(dados$cat_pesopre, dados$categoria)
prop.table(table(dados$cat_pesopre, dados$categoria)) * 100
Modelo_pesopre <- chisq.test(dados$cat_pesopre, dados$categoria)
Modelo_pesopre


###########################################  calcular a médiana e o desvio padrão do IMC-Pre-gestacional por GPG########################################## 
summary(dados$imc)

aggregate(imc ~ categoria, data = dados, FUN = function(x) c(mediana = median(x), IQR = (quantile(x, c(0.25, 0.75)))))


# Teste de normalidade
shapiro.test(dados$imc)

# Teste de homogeneidade de variâncias
bartlett.test(imc ~ categoria, data = dados)
              
# Realizar o teste ANOVA do IMC
modelo_imc <- kruskal.test(imc ~ categoria, data = dados)
modelo_imc

# Realizando comparações múltiplas com o método de Bonferroni
comp_imc <- pairwise.wilcox.test(dados$imc, dados$categoria, p.adjust.method = "bonferroni")
comp_imc


############################################# Tabela de contingencia  do IMC-Pre-gestacional  categorico  por GPG ############################################ 
table(dados$cat_imc)
prop.table(table(dados$cat_imc))*100
table(dados$cat_imc,dados$categoria)
prop.table(table(dados$cat_imc,dados$categoria))*100

# Teste de Quiquadrado
Modelo_cat_imc  <- chisq.test(dados$cat_imc, dados$categoria)
Modelo_cat_imc 


###########################################  calcular a médiana  da Circunferência do braço por GPG ########################################## 
summary(dados$a_circbracm)

aggregate(a_circbracm ~ categoria, data = dados, FUN = function(x) c(mediana = median(x), IQR = (quantile(x, c(0.25, 0.75)))))

# Teste de normalidade
shapiro.test(dados$a_circbracm)

# Teste de homogeneidade de variâncias
bartlett.test(a_circbracm ~ categoria, data = dados)

# Realizar o teste ANOVA do IMC
modelo_a_circbracm <- kruskal.test(a_circbracm ~ categoria, data = dados)
modelo_a_circbracm

# Realizando comparações múltiplas com o método de Bonferroni
comp_a_circbracm <- pairwise.wilcox.test(dados$a_circbracm, dados$categoria, p.adjust.method = "bonferroni")
comp_a_circbracm

########################################################## Categoria #######################################################################################

summary(dados$a_circbracm)

dados$cat_circbr <- ifelse(dados$a_circbracm < 23, "Baixo peso",
                           ifelse(dados$a_circbracm >= 23 & dados$a_circbracm < 28, "Adequado",
                                  "Sobrepeso"))
table(dados$cat_circbr)
prop.table(table(dados$cat_circbr)) * 100
table(dados$cat_circbr, dados$categoria)
prop.table(table(dados$cat_circbr, dados$categoria)) * 100
Modelo_circbr <- chisq.test(dados$cat_circbr, dados$categoria)
Modelo_circbr



###########################################  calcular a médiana  da Percentual de gordura corporal (%) por GPG########################################## 
summary(dados$a_fmp)

aggregate(a_fmp ~ categoria, data = dados,  FUN = function(x) c(mediana = median(x), IQR = (quantile(x, c(0.25, 0.75)))))

# Teste de normalidade
shapiro.test(dados$a_fmp)

# Teste de homogeneidade de variâncias
bartlett.test(a_fmp ~ categoria, data = dados)

# Realizar o teste ANOVA do IMC
modelo_a_fmp <- kruskal.test(a_fmp ~ categoria, data = dados)
modelo_a_fmp

# Realizando comparações múltiplas com o método de Bonferroni
comp_a_fmp <- pairwise.wilcox.test(dados$a_fmp, dados$categoria, p.adjust.method = "bonferroni")
comp_a_fmp

########################################## calcular a médiaa e o IQR da Idade gestacional em semanas por GPG#########################################

############################################# Idade gestacional em semana ##########################################################
sort(dados$d_IGUSGrn , decreasing = T )
dados$d_IGUSGrn <- dados$d_IGUSGrn/ 7

summary(dados$d_IGUSGrn)

summary(dados$d_IGUSGrn)

aggregate(d_IGUSGrn ~ categoria, data = dados,FUN = function(x) c(mediana = median(x), IQR = (quantile(x, c(0.25, 0.75)))))

# Teste de normalidade
shapiro.test(dados$d_IGUSGrn)

# Teste de homogeneidade de variâncias
bartlett.test(d_IGUSGrn ~ categoria, data = dados)

# Realizar o teste  kruskal Wallis
modelo_ig <- kruskal.test(d_IGUSGrn ~ categoria, data = dados)
modelo_ig

# Realizando comparações múltiplas com o método de Bonferroni
comp_ig <- pairwise.wilcox.test(dados$d_IGUSGrn, dados$categoria, p.adjust.method = "bonferroni")
comp_ig 

###################################################### categaoria ###############################################################
summary(dados$d_IGUSGrn)

dados$cat_ig <- ifelse(dados$d_IGUSGrn <= 37 , "≤37" , ">37" )
table(dados$cat_ig )
prop.table(table(dados$cat_ig))*100
table(dados$cat_ig,dados$categoria)
prop.table(table(dados$cat_ig,dados$categoria))*100

Modelo_ig <- chisq.test(dados$cat_ig, dados$categoria)
Modelo_ig


############################### Tabela de contigencia da Escolaridade  da gestante (em anos de estudos), por GPG ############################################## 
# Categorizando a escolaridade  em três grupos: "≤4", "5-11" e "≥12".
sort(dados$a_escola, decreasing = T )

dados$a_escola[dados$a_escola == 0.11] <- 11
dados$a_escola[dados$a_escola == 0.15] <- 15
dados$a_escola[dados$a_escola == 90.09] <- 9
dados$a_escola[dados$a_escola == 0.06] <- 6

dados$a_escola <- cut(dados$a_escola, breaks = c(-Inf, 4, 11, Inf), 
                      labels = c("≤4", "5-11",  "≥12"))

table(dados$a_escola)

table(dados$a_escola)
prop.table(table(dados$a_escola))*100
table(dados$a_escola,dados$categoria)
prop.table(table(dados$a_escola,dados$categoria))*100

# Teste de Fisher
Modelo_escola<- fisher.test(dados$a_escola, dados$categoria, simulate.p.value = TRUE, B = 10000)
Modelo_escola

######################### Numero de comodos por pessoa, por GPG ##################################### 
summary(dados$a_npcomo)

aggregate(a_npcomo ~ categoria, data = dados,FUN = function(x) c(mediana = median(x), IQR = (quantile(x, c(0.25, 0.75)))))

# Teste de normalidade
shapiro.test(dados$a_npcomo)

# Teste de homogeneidade de variâncias
bartlett.test(a_npcomo ~ categoria, data = dados)

# Realizar o teste  kruskal Wallis
modelo_npcomo <- kruskal.test(a_npcomo ~ categoria, data = dados)
modelo_npcomo

# Realizando comparações múltiplas com o método de Bonferroni
comp_npcomo <- pairwise.wilcox.test(dados$a_npcomo, dados$categoria, p.adjust.method = "bonferroni")
comp_npcomo

######################################################################## categoria ######################################################################
summary(dados$a_npcomo)

dados$cat_npcomo <- cut(dados$a_npcomo, 
                        breaks = quantile(dados$a_npcomo, probs = c(0, 1/3, 2/3, 1)),
                        labels = c("1º Tercil", "2º Tercil", "3º Tercil"))

table(dados$cat_npcomo)
prop.table(table(dados$cat_npcomo)) * 100
table(dados$cat_npcomo, dados$categoria)
prop.table(table(dados$cat_npcomo, dados$categoria)) * 100
Modelo_npcomo <- chisq.test(dados$cat_npcomo, dados$categoria)
Modelo_npcomo

######################### calcular a médiana e o IQR da Renda per capita em Reais, por GPG ##################################### 
dados$a_rendpcr <- round(dados$a_rendpcr, 3)

summary(dados$a_rendpcr)

aggregate(a_rendpcr ~ categoria, data = dados,FUN = function(x) c(mediana = median(x), IQR = (quantile(x, c(0.25, 0.75)))))

# Teste de normalidade
shapiro.test(dados$a_rendpcr)

# Teste de homogeneidade de variâncias
bartlett.test(a_rendpcr ~ categoria, data = dados)

# Realizar o teste  kruskal Wallis
modelo_rendpcr <- kruskal.test(a_rendpcr ~ categoria, data = dados)
modelo_rendpcr

# Realizando comparações múltiplas com o método de Bonferroni
comp_rendpcr <- pairwise.wilcox.test(dados$a_rendpcr, dados$categoria, p.adjust.method = "bonferroni")
comp_rendpcr


##################################### Tabela de contingencia da raca, por GPG ##################################### 
dados$a_cor <- ifelse(dados$a_cor == 1, "branco", "Nbranco")

table(dados$a_cor)
prop.table(table(dados$a_cor))*100
table(dados$a_cor,dados$categoria)
prop.table(table(dados$a_cor,dados$categoria))*100

# Usando o método de simulação de Monte Carlo com 10.000 simulações
Modelo_cor <- chisq.test(dados$a_cor, dados$categoria)
Modelo_cor


##################################### Tabela de contingencia Estado civil por GPG ##################################### 
####recodificando a variável a_civil em duas categorias
dados$a_civil <- ifelse(dados$a_civil %in% c(1 , 2), 1, 2) #### recodificar 2  categorias: 1-Casada ou em união estável, 2-  Solteira, separada ou viúva 

table(dados$a_civil)
prop.table(table(dados$ a_civil))*100
table(dados$a_civil,dados$categoria)

# Teste de Quiquadrado
Modelo_ecivil  <- chisq.test(dados$a_civil, dados$categoria)
Modelo_ecivil


############################################ # calcular a média e o desvio padrão da Actividade fisica  por GPG############################################ 

dados$a_moderh <- ifelse(dados$a_moderh< 150, "inadequado", "adequado")

table(dados$a_moderh)
prop.table(table(dados$a_moderh))*100
table(dados$a_moderh, dados$categoria)
prop.table(table(dados$a_moderh,dados$categoria))*100

# Teste de Quiquadrado
Modelo_a_moderh <- chisq.test(dados$a_moderh, dados$categoria)
Modelo_a_moderh

############################################# Tabela de contingencia  do Habitio de Fumar por GPG ############################################ 

dados$a_fumog <- factor(dados$a_fumog, levels = c(0, 1), labels = c("Não", "Sim"))
table(dados$a_fumog)
prop.table(table(dados$a_fumog))*100
table(dados$a_fumog,dados$categoria)
prop.table(table(dados$a_fumog,dados$categoria))*100

# Teste de Quiquadrado
Modelo_fumo  <- chisq.test(dados$a_fumog, dados$categoria)
Modelo_fumo

############################################# Tabela de contingencia  do Acolool  por GPG ############################################ 
dados$a_alcool <- factor(dados$a_alcool, levels = c(0, 1), labels = c("Não", "Sim"))
table(dados$a_alcool)
prop.table(table(dados$a_alcool))*100
table(dados$a_alcool,dados$categoria)
prop.table(table(dados$a_alcool,dados$categoria))*100

# Teste de Quiquadrado
Modelo_alcool  <- chisq.test(dados$a_alcool, dados$categoria)
Modelo_alcool


############################################# Tabela de contingencia  do Diabetes por GPG############################################ # 
dados$a_agdm <- factor(dados$a_agdm, levels = c(0, 1), labels = c("Não", "Sim"))
table(dados$a_agdm)
prop.table(table(dados$a_agdm))*100
table(dados$a_agdm,dados$categoria)
prop.table(table(dados$a_agdm,dados$categoria))*100

# Teste de Quiquadrado
Modelo_diabete  <- chisq.test(dados$a_agdm, dados$categoria)
Modelo_diabete
  
################################################## Tabela de contingencia  do Hipertensao por GPG ############################################ 
dados$a_aghas <- factor(dados$a_aghas, levels = c(0, 1), labels = c("Não", "Sim"))
table(dados$a_aghas)
prop.table(table(dados$ a_aghas))*100
table(dados$ a_aghas,dados$categoria)
prop.table(table(dados$ a_aghas,dados$categoria))*100

# Teste de Quiquadrado
Modelo_hta  <- chisq.test(dados$a_aghas, dados$categoria)
Modelo_hta

############################################# Tabela de contingencia  Infeccao Urinaria por GPG############################################ 
dados$a_agui <- factor(dados$a_agui, levels = c(0, 1), labels = c("Não", "Sim"))
table(dados$a_agui)
prop.table(table(dados$a_agui))*100
table(dados$a_agui,dados$categoria)
prop.table(table(dados$a_agui,dados$categoria))*100

# Teste de Quiquadrado
Modelo_infuri  <- chisq.test(dados$a_agui, dados$categoria)
Modelo_infuri 

############################################# Tabela de contingencia  Sifilis por GPG############################################ 
dados$a_agsif <- factor(dados$a_agsif, levels = c(0, 1), labels = c("Não", "Sim"))
table(dados$a_agsif)
prop.table(table(dados$a_agsif))*100
table(dados$a_agsif,dados$categoria)
prop.table(table(dados$a_agsif,dados$categoria))*100

# Teste de Quiquadrado
Modelo_sifil  <- chisq.test(dados$a_agsif, dados$categoria)
Modelo_sifil

############################################# Tabela de contingencia  Cervicit/Vaginite por GPG############################################ 

dados$a_agceva <- factor(dados$a_agceva, levels = c(0, 1), labels = c("Não", "Sim"))
table(dados$a_agceva)
prop.table(table(dados$a_agceva))*100
table(dados$a_agceva,dados$categoria)
prop.table(table(dados$a_agceva,dados$categoria))*100

# Teste de Quiquadrado
Modelo_cervict  <- chisq.test(dados$a_agceva, dados$categoria)
Modelo_cervict


################################################## Tabela de contingencia do Numero de gravidezes anterior por GPG ############################################

dados$a_npari <- ifelse(dados$a_npari == 0, "0-Primigesta",
                                 ifelse(dados$a_npari == 1, "um",
                                        "2+"))


dados$a_npari <- factor(dados$a_npari, levels = c("0-Primigesta", "um", "2+"))

table(dados$a_npari)
prop.table(table(dados$a_npari))*100
table(dados$a_npari,dados$categoria)
prop.table(table(dados$a_npari,dados$categoria))*100

# Teste de Quiquadrado
Modelo_a_npari  <- chisq.test(dados$a_npari, dados$categoria)
Modelo_a_npari

################################################# Tabela de contingencia do PCR por GPG ############################################

summary(dados$a_pcr)
aggregate( a_pcr ~ categoria, data = dados, FUN = function(x) c(mediana = median(x), IQR = (quantile(x, c(0.25, 0.75)))))

# Teste de normalidade
shapiro.test(dados$a_pcr)

# Teste de homogeneidade de variâncias
bartlett.test(a_pcr ~ categoria, data = dados)

# Realizar o teste Kruskal-Wallis
modelo_pcr <- kruskal.test(a_pcr ~ categoria, data = dados)
modelo_pcr

# Realizando comparações múltiplas com o método de Bonferroni
comp_pcr<- pairwise.wilcox.test(dados$a_pcr, dados$categoria, p.adjust.method = "bonferroni")
comp_pcr


################################################## Tabela de contingencia do HOMA por GPG ############################################

summary(dados$a_homa)
aggregate( a_homa ~ categoria, data = dados, FUN = function(x) c(mediana = median(x), IQR = (quantile(x, c(0.25, 0.75)))))

# Teste de normalidade
shapiro.test(dados$a_homa)

# Teste de homogeneidade de variâncias
bartlett.test(a_homa ~ categoria, data = dados)

# Realizar o teste Kruskal-Wallis
modelo_homa <- kruskal.test(a_homa ~ categoria, data = dados)
modelo_homa

# Realizando comparações múltiplas com o método de Bonferroni
comp_homa<- pairwise.wilcox.test(dados$a_homa, dados$categoria, p.adjust.method = "bonferroni")
comp_homa
#################################################Tabela de Contingencia  da Hemoglobina por GPG ################################################
sort(dados$a_hb, decreasing = T)
summary(dados$a_hb)

aggregate( a_hb ~ categoria, data = dados, FUN = function(x) c(mediana = median(x), IQR = (quantile(x, c(0.25, 0.75)))))

# Teste de normalidade
shapiro.test(dados$a_hb)

# Teste de homogeneidade de variâncias
bartlett.test(a_hb ~ categoria, data = dados)

# Realizar o teste Kruskal-Wallis
modelo_hb <- kruskal.test(a_hb ~ categoria, data = dados)
modelo_hb 

# Realizando comparações múltiplas com o método de Bonferroni
comp_a_hb<- pairwise.wilcox.test(dados$a_hb, dados$categoria, p.adjust.method = "bonferroni")
comp_a_hb


#################################################Tabela de Contingencia  da Hemoglobina glicada por GPG ################################################
############################################# heoglobina glicada ##########################################################
sort(dados$a_hba1c, decreasing = T)
summary(dados$a_hba1c)
dados$a_hba1c [dados$a_hba1c == 55.5] <- NA
summary(dados$a_hba1c)

aggregate( a_hba1c ~ categoria, data = dados, FUN = function(x) c(mediana = median(x), IQR = (quantile(x, c(0.25, 0.75)))))

# Teste de normalidade
shapiro.test(dados$a_hba1c)

# Teste de homogeneidade de variâncias
bartlett.test(a_hba1c ~ categoria, data = dados)

# Realizar o teste Kruskal-Wallis
modelo_hba1c <- kruskal.test(a_hba1c ~ categoria, data = dados)
modelo_hba1c 

# Realizando comparações múltiplas com o método de Bonferroni
comp_hba1c<- pairwise.wilcox.test(dados$a_hba1c, dados$categoria, p.adjust.method = "bonferroni")
comp_hba1c


################################################ Tabela de contingencia da INSULINA por GPG ################################################
sort(dados$a_insul, decreasing = T)
summary(dados$a_insul)
aggregate( a_insul ~ categoria, data = dados, FUN = function(x) c(mediana = median(x), IQR = (quantile(x, c(0.25, 0.75)))))

# Teste de normalidade
shapiro.test(dados$a_insul)

# Teste de homogeneidade de variâncias
bartlett.test(a_insul ~ categoria, data = dados)

# Realizar o teste Kruskal-Wallis
modelo_insull <- kruskal.test(a_insul ~ categoria, data = dados)
modelo_insull

# Realizando comparações múltiplas com o método de Bonferroni
comp_insul<- pairwise.wilcox.test(dados$a_insul, dados$categoria, p.adjust.method = "bonferroni")
comp_insul

################################################# calcular a média e o desvio padrão da HDL por GPG################################################
summary(dados$a_hdl)
aggregate( a_hdl ~ categoria, data = dados, FUN = function(x) c(mediana = median(x), IQR = (quantile(x, c(0.25, 0.75)))))

# Teste de normalidade
shapiro.test(dados$a_hdl)

# Teste de homogeneidade de variâncias
bartlett.test(a_hdl ~ categoria, data = dados)

# Realizar o teste Kruskal-Wallis
modelo_hdl <- kruskal.test(a_hdl ~ categoria, data = dados)
modelo_hdl

# Realizando comparações múltiplas com o método de Bonferroni
comp_hdl<- pairwise.wilcox.test(dados$a_hdl, dados$categoria, p.adjust.method = "bonferroni")
comp_hdl

################################################# calcular a média e o desvio padrão da LDL por GPG################################################
summary(dados$a_ldl)
aggregate( a_ldl ~ categoria, data = dados, FUN = function(x) c(mediana = median(x), IQR = (quantile(x, c(0.25, 0.75)))))

# Teste de normalidade
shapiro.test(dados$a_ldl)

# Teste de homogeneidade de variâncias
bartlett.test(a_ldl ~ categoria, data = dados)

# Realizar o teste Kruskal-Wallis
modelo_ldl <- kruskal.test(a_ldl ~ categoria, data = dados)
modelo_ldl

# Realizando comparações múltiplas com o método de Bonferroni
comp_ldL<- pairwise.wilcox.test(dados$a_ldl, dados$categoria, p.adjust.method = "bonferroni")
comp_ldL


################################################# calcular a médiana e o IQR de Triaglicerideos  por GPG################################################
summary(dados$ a_tg)
aggregate(  a_tg ~ categoria, data = dados, FUN = function(x) c(mediana = median(x), IQR = (quantile(x, c(0.25, 0.75)))))

# Teste de normalidade
shapiro.test(dados$a_tg)

# Teste de homogeneidade de variâncias
bartlett.test( a_tg ~ categoria, data = dados)

# Realizar o teste Kruskal-Wallis
modelo_a_tg <- kruskal.test( a_tg ~ categoria, data = dados)
modelo_a_tg

# Realizando comparações múltiplas com o método de Bonferroni
comp_a_tg <- pairwise.wilcox.test(dados$ a_tg, dados$categoria, p.adjust.method = "bonferroni")
comp_a_tg

################################################# calcular a média e o desvio padrão da  Colesterol por GPG################################################
summary(dados$a_ct)
aggregate( a_ct ~ categoria, data = dados, FUN = function(x) c(mediana = median(x), IQR = (quantile(x, c(0.25, 0.75)))))
# Teste de normalidade
shapiro.test(dados$a_ct)

# Teste de homogeneidade de variâncias
bartlett.test(a_ct ~ categoria, data = dados)

# Realizar o teste ANOVA do IMC
modelo_colest <- kruskal.test(a_ct ~ categoria, data = dados)
modelo_colest

# Realizando comparações múltiplas com o método de Bonferroni
comp_colest<- pairwise.wilcox.test(dados$a_ct, dados$categoria, p.adjust.method = "bonferroni")
comp_colest


##########################################################################################################################################################################
########################### Modelo ajustado e bruto dos factores preditores ao gpg#######################################################################################
##########################################################################################################################################################################

dados$cat_imc <- relevel(dados$cat_imc, ref = "Peso normal")
dados$a_escola <- relevel(dados$a_escola, ref = "≥12")
dados$a_idade <- relevel(dados$a_idade, ref = "20-35")
dados$cat_estat <- relevel(dados$cat_estat, ref = "3º Tercil")

dados$cat_circbr <- relevel(factor(dados$cat_circbr), ref = "Adequado")
dados$cat_npcomo <- relevel(dados$cat_npcomo, ref = "1º Tercil")

table(dados$cat_npcomo)

##Instalar pacotes para rodar a regressao multinomial 
install.packages("broom")
install.packages("nnet")
install.packages("MASS")
library(nnet)   # Para ajustar o modelo de regressão logística multinomial
library(MASS)   # Para calcular a estatística de deviance do modelo ajustado
library(lme4)
########################################################################################################################################################
########################################################################################################################################################
################################################### Modelo  brutos######################################################################################
########################################################################################################################################################
########################################################################################################################################################

############################################## # Modelo para a_idade############################################## 
modelo_a_idade <- multinom(categoria ~ a_idade, data = dados)
summary(modelo_a_idade)
ORs <- exp(coef(modelo_a_idade))
ORs
intervalos_a_idade <- confint(modelo_a_idade)
intervalos_a_idade <- exp(confint(modelo_a_idade))

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo <- intervalos_a_idade[, ,1][-1,1]
upper_abaixo <- intervalos_a_idade[, ,1][-1,2]
a<-cbind(ORs[1,-1], lower_abaixo, upper_abaixo)
lower_acima <- intervalos_a_idade[, ,2][-1,1]
upper_acima <- intervalos_a_idade[, ,2][-1,2]
b<-cbind(ORs[2,-1],lower_acima, upper_acima)
joint <-rbind(a,b)
joint

# Formatar a OR e os intervalos de confiança
output <- table(paste(round(joint[,1],2),"(", round(joint[,2],2),"-", round(joint[,3],2),")"))
output <- output[reorder(names(output), c(3, 1, 2, 4))]
output <- data.frame(joint[,0], output)
view(output)



############################################## # Modelo para a_estat1############################################## 
modelo_a_estat1 <- multinom(categoria ~ cat_estat, data = dados)

# Calcular as ORs
ORs <- exp(coef(modelo_a_estat1))
ORs
# Calcular os intervalos de confiança
intervalos_a_estat1 <- confint(modelo_a_estat1)
intervalos_a_estat1 <- exp(intervalos_a_estat1)

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo <- intervalos_a_estat1[, , 1][-1, 1]
upper_abaixo <- intervalos_a_estat1[, , 1][-1, 2]
a <- cbind(ORs[1, -1], lower_abaixo, upper_abaixo)

# Formatar os resultados para a categoria abaixo
lower_acima <- intervalos_a_estat1[, , 2][-1, 1]
upper_acima <- intervalos_a_estat1[, , 2][-1, 2]
b <- cbind(ORs[2, -1], lower_acima, upper_acima)
joint <- rbind(a, b)
joint 

# Formatar a OR e os intervalos de confiança
output <- table(paste(round(joint[, 1], 2), "(", round(joint[, 2], 2), "-", round(joint[, 3], 2), ")"))
output <- output[reorder(names(output), c(3,4,1,2))]
output <- data.frame(joint[,0], output)
view(output)


##################################################################### Modelo a_pesopre #############################################################################
# Ajustar o modelo para a variável a_pesopre
modelo_a_pesopre <- multinom(categoria ~ cat_pesopre, data = dados)
summary(modelo_a_pesopre)

# Calcular as ORs
ORs <- exp(coef(modelo_a_pesopre))
ORs

# Calcular os intervalos de confiança
intervalos_a_pesopre <- exp(confint(modelo_a_pesopre))
lower_abaixo <- intervalos_a_pesopre[, , 1][-1, 1]
upper_abaixo <- intervalos_a_pesopre[, , 1][-1, 2]
a <- cbind(ORs[1, -1], lower_abaixo, upper_abaixo)
lower_acima <- intervalos_a_pesopre[, , 2][-1, 1]
upper_acima <- intervalos_a_pesopre[, , 2][-1, 2]
b <- cbind(ORs[2, -1], lower_acima, upper_acima)
joint <- rbind(a, b)

# Formatar a OR e os intervalos de confiança
output <- table(paste(round(joint[, 1], 2), "(", round(joint[, 2], 2), "-", round(joint[, 3], 2), ")"))
output <- output[reorder(names(output), c(3,4,1,2))]
output <- data.frame(joint[,0], output)
view(output)

############################################## # Modelo para cat_imc############################################## 

modelo_cat_imc <- multinom(categoria ~ cat_imc, data = dados)
summary(modelo_cat_imc)

# Calcular as ORs
ORs_imc <- exp(coef(modelo_cat_imc))
ORs_imc

# Calcular os intervalos de confiança
intervalos_cat_imc <- exp(confint(modelo_cat_imc))

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo_imc <- intervalos_cat_imc[, , 1][-1, 1]
upper_abaixo_imc <- intervalos_cat_imc[, , 1][-1, 2]
a_imc <- cbind(ORs_imc[1, -1], lower_abaixo_imc, upper_abaixo_imc)

lower_acima_imc <- intervalos_cat_imc[, , 2][-1, 1]
upper_acima_imc <- intervalos_cat_imc[, , 2][-1, 2]
b_imc <- cbind(ORs_imc[2, -1], lower_acima_imc, upper_acima_imc)

ORs <- rbind(a_imc, b_imc)

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
cat_imc <- data.frame(imc = "cat_imc", or.ci = or.ci)
view(cat_imc)


############################################## # Modelo para a_circbracm############################################## 
# Construir o modelo
modelo_a_circbracm <- multinom(categoria ~ cat_circbr, data = dados)
summary(modelo_a_circbracm)

# Calculate the odds ratios (ORs)
ORs <- exp(coef(modelo_a_circbracm))
ORs

# Calculate the confidence intervals
intervalos_a_circbracm <- exp(confint(modelo_a_circbracm))
lower_abaixo <- intervalos_a_circbracm[, , 1][-1, 1]
upper_abaixo <- intervalos_a_circbracm[, , 1][-1, 2]
a <- cbind(ORs[1, -1], lower_abaixo, upper_abaixo)
lower_acima <- intervalos_a_circbracm[, , 2][-1, 1]
upper_acima <- intervalos_a_circbracm[, , 2][-1, 2]
b <- cbind(ORs[2, -1], lower_acima, upper_acima)
joint <- rbind(a, b)

# Format the odds ratios and confidence intervals
output <- table(paste(round(joint[, 1], 2), "(", round(joint[, 2], 2), "-", round(joint[, 3], 2), ")"))
output <- output[reorder(names(output), c(3, 2, 1, 4))]
output <- data.frame(joint[,0], output)
view(output)


############################################## # Modelo para a_fmp############################################## 
modelo_a_fmp <- multinom(categoria ~ a_fmp, data = dados)
summary(modelo_a_fmp)

# Calcular as ORs
ORs_a_fmp <- exp(coef(modelo_a_fmp))
ORs_a_fmp

# Calcular os intervalos de confiança
intervalos_a_fmp <- exp(confint(modelo_a_fmp))

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo_fmp <- intervalos_a_fmp[, , 1][-1, 1]
upper_abaixo_fmp <- intervalos_a_fmp[, , 1][-1, 2]
a_fmp <- cbind(ORs_a_fmp[1, -1], lower_abaixo_fmp, upper_abaixo_fmp)

lower_acima_fmp <- intervalos_a_fmp[, , 2][-1, 1]
upper_acima_fmp <- intervalos_a_fmp[, , 2][-1, 2]
b_fmp <- cbind(ORs_a_fmp[2, -1], lower_acima_fmp, upper_acima_fmp)

ORs <- rbind(a_fmp, b_fmp)

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
a_fmp <- data.frame(fmp = "a_fmp", or.ci = or.ci)
view(a_fmp)


############################################## Modelo para  idade gestacional ############################################# 
# Construir o modelo multinomial
modelo_d_IGUSGrn <- multinom(categoria ~ d_IGUSGrn, data = dados)

# Resumo do modelo
summary(modelo_d_IGUSGrn)

# Calcular as ORs
ORs_IGUSGrn <- exp(coef(modelo_d_IGUSGrn))
ORs_IGUSGrn

# Calcular os intervalos de confiança
intervalos_IGUSGrn <- exp(confint(modelo_d_IGUSGrn))

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo_IGUSGrn <- intervalos_IGUSGrn[, , 1][-1, 1]
upper_abaixo_IGUSGrn <- intervalos_IGUSGrn[, , 1][-1, 2]
a_IGUSGrn <- cbind(ORs_IGUSGrn[1, -1], lower_abaixo_IGUSGrn, upper_abaixo_IGUSGrn)
lower_acima_IGUSGrn <- intervalos_IGUSGrn[, , 2][-1, 1]
upper_acima_IGUSGrn <- intervalos_IGUSGrn[, , 2][-1, 2]
b_IGUSGrn <- cbind(ORs_IGUSGrn[2, -1], lower_acima_IGUSGrn, upper_acima_IGUSGrn)

ORs <- rbind(a_IGUSGrn, b_IGUSGrn)

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
d_IGUSGrn <- data.frame(IGUSGrn = "d_IGUSGrn", or.ci = or.ci)
view(d_IGUSGrn)


###############################################  Modelo para a_escola############################################## 
# Construir o modelo
modelo_a_escola <- multinom(categoria ~ a_escola, data = dados)
summary(modelo_a_escola)

# Calcular as ORs
ORs_a_escola <- exp(coef(modelo_a_escola))
ORs_a_escola

# Calcular os intervalos de confiança
intervalos_a_escola <- exp(confint(modelo_a_escola))

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo_a_escola <- intervalos_a_escola[, , 1][-1, 1]
upper_abaixo_a_escola <- intervalos_a_escola[, , 1][-1, 2]
a_a_escola <- cbind(ORs_a_escola[1, -1], lower_abaixo_a_escola, upper_abaixo_a_escola)
lower_acima_a_escola <- intervalos_a_escola[, , 2][-1, 1]
upper_acima_a_escola <- intervalos_a_escola[, , 2][-1, 2]
b_a_escola <- cbind(ORs_a_escola[2, -1], lower_acima_a_escola, upper_acima_a_escola)
ORs <- rbind(a_a_escola, b_a_escola)

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
a_escola <- data.frame(a_escola = "a_escola", or.ci = or.ci)
view(a_escola)
############################################## Numero de pessoas por comodo ############################################## 
modelo_a_npcomo <- multinom(categoria ~ cat_npcomo, data = dados)
summary(modelo_a_npcomo)

# Calcular as ORs
ORs <- exp(coef(modelo_a_npcomo))
ORs

# Calcular os intervalos de confiança
intervalos_a_npcomo <- exp(confint(modelo_a_npcomo))
lower_abaixo <- intervalos_a_npcomo[, , 1][-1, 1]
upper_abaixo <- intervalos_a_npcomo[, , 1][-1, 2]
a <- cbind(ORs[1, -1], lower_abaixo, upper_abaixo)
lower_acima <- intervalos_a_npcomo[, , 2][-1, 1]
upper_acima <- intervalos_a_npcomo[, , 2][-1, 2]
b <- cbind(ORs[2, -1], lower_acima, upper_acima)
joint <- rbind(a, b)

# Formatar a OR e os intervalos de confiança
output <- table(paste(round(joint[, 1], 2), "(", round(joint[, 2], 2), "-", round(joint[, 3], 2), ")"))
output <- output[reorder(names(output), c(3,4,1,2))]
output <- data.frame(output)
view(output)


############################################## # Modelo para a_cor############################################## 
# Construir o modelo
modelo_a_cor <- multinom(categoria ~ a_cor, data = dados)

# Resumo do modelo
summary(modelo_a_cor)

# Calcular as ORs
ORs_a_cor <- exp(coef(modelo_a_cor))
ORs_a_cor

# Calcular os intervalos de confiança
intervalos_a_cor <- exp(confint(modelo_a_cor))

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo_a_cor <- intervalos_a_cor[, , 1][-1, 1]
upper_abaixo_a_cor <- intervalos_a_cor[, , 1][-1, 2]
a_a_cor <- cbind(ORs_a_cor[1, -1], lower_abaixo_a_cor, upper_abaixo_a_cor)
lower_acima_a_cor <- intervalos_a_cor[, , 2][-1, 1]
upper_acima_a_cor <- intervalos_a_cor[, , 2][-1, 2]
b_a_cor <- cbind(ORs_a_cor[2, -1], lower_acima_a_cor, upper_acima_a_cor)
ORs <- rbind(a_a_cor, b_a_cor)

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
a_cor <- data.frame(a_cor = "a_cor", or.ci = or.ci)
view(a_cor)


############################################## # Modelo para a_civil############################################## 
modelo_a_civil <- multinom(categoria ~ a_civil, data = dados)
summary(modelo_a_civil)

# Calcular as ORs
ORs_a_civil <- exp(coef(modelo_a_civil))
ORs_a_civil

# Calcular os intervalos de confiança
intervalos_a_civil <- exp(confint(modelo_a_civil))

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo_a_civil <- intervalos_a_civil[, , 1][-1, 1]
upper_abaixo_a_civil <- intervalos_a_civil[, , 1][-1, 2]
a_a_civil <- cbind(ORs_a_civil[1, -1], lower_abaixo_a_civil, upper_abaixo_a_civil)
lower_acima_a_civil <- intervalos_a_civil[, , 2][-1, 1]
upper_acima_a_civil <- intervalos_a_civil[, , 2][-1, 2]
b_a_civil <- cbind(ORs_a_civil[2, -1], lower_acima_a_civil, upper_acima_a_civil)
ORs <- rbind(a_a_civil, b_a_civil)

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
a_civil <- data.frame(a_civil = "a_civil", or.ci = or.ci)
view(a_civil)


############################################### Atividade Fisica ############################################## 
# Construir o modelo
modelo_a_moderh <- multinom(categoria ~ a_moderh, data = dados)
summary(modelo_a_moderh)

# Calcular as ORs
ORs_a_moderh <- exp(coef(modelo_a_moderh))
ORs_a_moderh

# Calcular os intervalos de confiança
intervalos_a_moderh <- exp(confint(modelo_a_moderh))

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo_moderh <- intervalos_a_moderh[, , 1][-1, 1]
upper_abaixo_moderh <- intervalos_a_moderh[, , 1][-1, 2]
a_moderh <- cbind(ORs_a_moderh[1, -1], lower_abaixo_moderh, upper_abaixo_moderh)
lower_acima_moderh <- intervalos_a_moderh[, , 2][-1, 1]
upper_acima_moderh <- intervalos_a_moderh[, , 2][-1, 2]
b_moderh <- cbind(ORs_a_moderh[2, -1], lower_acima_moderh, upper_acima_moderh)
ORs <- rbind(a_moderh, b_moderh)

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")
# Tabela de resultados
a_moderh <- data.frame(a_moderh = "a_moderh", or.ci = or.ci)
view(a_moderh)


############################################## # Modelo para Fumar ############################################## 
modelo_a_fumog <- multinom(categoria ~ a_fumog, data = dados)
summary(modelo_a_fumog)

# Calcular as ORs
ORs_a_fumog <- exp(coef(modelo_a_fumog))
ORs_a_fumog

# Calcular os intervalos de confiança
intervalos_a_fumog <- exp(confint(modelo_a_fumog))

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo_fumog <- intervalos_a_fumog[, , 1][-1, 1]
upper_abaixo_fumog <- intervalos_a_fumog[, , 1][-1, 2]
a_fumog <- cbind(ORs_a_fumog[1, -1], lower_abaixo_fumog, upper_abaixo_fumog)
lower_acima_fumog <- intervalos_a_fumog[, , 2][-1, 1]
upper_acima_fumog <- intervalos_a_fumog[, , 2][-1, 2]
b_fumog <- cbind(ORs_a_fumog[2, -1], lower_acima_fumog, upper_acima_fumog)
ORs <- rbind(a_fumog, b_fumog)

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
a_fumog <- data.frame(a_fumog = "a_fumog", or.ci = or.ci)
view(a_fumog)


############################################## # Modelo para Alcool ############################################## 

modelo_a_alcool <- multinom(categoria ~ a_alcool, data = dados)
summary(modelo_a_alcool)

# Calcular as ORs
ORs_a_alcool <- exp(coef(modelo_a_alcool))
ORs_a_alcool

# Calcular os intervalos de confiança
intervalos_a_alcool <- exp(confint(modelo_a_alcool))

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo_alcool <- intervalos_a_alcool[, , 1][-1, 1]
upper_abaixo_alcool <- intervalos_a_alcool[, , 1][-1, 2]
a_alcool <- cbind(ORs_a_alcool[1, -1], lower_abaixo_alcool, upper_abaixo_alcool)

lower_acima_alcool <- intervalos_a_alcool[, , 2][-1, 1]
upper_acima_alcool <- intervalos_a_alcool[, , 2][-1, 2]
b_alcool <- cbind(ORs_a_alcool[2, -1], lower_acima_alcool, upper_acima_alcool)

ORs <- rbind(a_alcool, b_alcool)

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
a_alcool <- data.frame(alcool = "a_alcool", or.ci = or.ci)
view(a_alcool)


############################################## # Modelo para Diabetes ############################################## 
# Construir o modelo
modelo_a_agdm <- multinom(categoria ~ a_agdm, data = dados)

# Sumário do modelo
summary(modelo_a_agdm)

# Calcular as ORs
ORs_a_agdm <- exp(coef(modelo_a_agdm))
ORs_a_agdm

# Calcular os intervalos de confiança
intervalos_a_agdm <- exp(confint(modelo_a_agdm))

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo_agdm <- intervalos_a_agdm[, , 1][-1, 1]
upper_abaixo_agdm <- intervalos_a_agdm[, , 1][-1, 2]
a_agdm <- cbind(ORs_a_agdm[1, -1], lower_abaixo_agdm, upper_abaixo_agdm)

lower_acima_agdm <- intervalos_a_agdm[, , 2][-1, 1]
upper_acima_agdm <- intervalos_a_agdm[, , 2][-1, 2]
b_agdm <- cbind(ORs_a_agdm[2, -1], lower_acima_agdm, upper_acima_agdm)

ORs <- rbind(a_agdm, b_agdm)

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
a_agdm <- data.frame(agdm = "a_agdm", or.ci = or.ci)
view(a_agdm)

############################################## # Modelo para a_aghas############################################## 
modelo_a_aghas <- multinom(categoria ~ a_aghas, data = dados)
summary(modelo_a_aghas)

# Calcular as ORs
ORs_a_aghas <- exp(coef(modelo_a_aghas))
ORs_a_aghas

# Calcular os intervalos de confiança
intervalos_a_aghas <- exp(confint(modelo_a_aghas))

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo_a_aghas <- intervalos_a_aghas[, , 1][-1, 1]
upper_abaixo_a_aghas <- intervalos_a_aghas[, , 1][-1, 2]
a_aghas <- cbind(ORs_a_aghas[1, -1], lower_abaixo_a_aghas, upper_abaixo_a_aghas)

lower_acima_a_aghas <- intervalos_a_aghas[, , 2][-1, 1]
upper_acima_a_aghas <- intervalos_a_aghas[, , 2][-1, 2]
b_a_aghas <- cbind(ORs_a_aghas[2, -1], lower_acima_a_aghas, upper_acima_a_aghas)

ORs <- rbind(a_aghas, b_a_aghas)

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
a_aghas <- data.frame(a_aghas = "a_aghas", or.ci = or.ci)
view(a_aghas)

############################################## # Modelo para Infccao Urinaria ############################################## 
modelo_a_agui <- multinom(categoria ~ a_agui, data = dados)
summary(modelo_a_agui)

# Calcular as ORs
ORs_a_agui <- exp(coef(modelo_a_agui))
ORs_a_agui

# Calcular os intervalos de confiança
intervalos_a_agui <- exp(confint(modelo_a_agui))

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo_a_agui <- intervalos_a_agui[, , 1][-1, 1]
upper_abaixo_a_agui <- intervalos_a_agui[, , 1][-1, 2]
a_a_agui <- cbind(ORs_a_agui[1, -1], lower_abaixo_a_agui, upper_abaixo_a_agui)

lower_acima_a_agui <- intervalos_a_agui[, , 2][-1, 1]
upper_acima_a_agui <- intervalos_a_agui[, , 2][-1, 2]
b_a_agui <- cbind(ORs_a_agui[2, -1], lower_acima_a_agui, upper_acima_a_agui)

ORs <- rbind(a_a_agui, b_a_agui)
ORs
# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
a_agui <- data.frame(a_agui = "a_agui", or.ci = or.ci)
view(a_agui)


############################################## # Modelo para Cervicit e Vaginite ############################################# 
modelo_a_agceva <- multinom(categoria ~ a_agceva, data = dados)
summary(modelo_a_agceva)

# Calcular as ORs
ORs_agceva <- exp(coef(modelo_a_agceva))
ORs_agceva

# Calcular os intervalos de confiança
intervalos_agceva <- exp(confint(modelo_a_agceva))

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo_agceva <- intervalos_agceva[, , 1][-1, 1]
upper_abaixo_agceva <- intervalos_agceva[, , 1][-1, 2]
a_agceva <- cbind(ORs_agceva[1, -1], lower_abaixo_agceva, upper_abaixo_agceva)

lower_acima_agceva <- intervalos_agceva[, , 2][-1, 1]
upper_acima_agceva <- intervalos_agceva[, , 2][-1, 2]
b_agceva <- cbind(ORs_agceva[2, -1], lower_acima_agceva, upper_acima_agceva)

ORs <- rbind(a_agceva, b_agceva)

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
a_agceva <- data.frame(agceva = "a_agceva", or.ci = or.ci)
view(a_agceva)

############################################## Variável a_npari############################################## 
# Construir o modelo
modelo_a_npari <- multinom(categoria ~ a_npari, data = dados)
summary(modelo_a_npari)

# Calcular as ORs
ORs_a_npari <- exp(coef(modelo_a_npari))
ORs_a_npari

# Calcular os intervalos de confiança
intervalos_a_npari <- exp(confint(modelo_a_npari))

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo_ngesta <- intervalos_a_npari[, , 1][-1, 1]
upper_abaixo_ngesta <- intervalos_a_npari[, , 1][-1, 2]
a_npari <- cbind(ORs_a_npari[1, -1], lower_abaixo_ngesta, upper_abaixo_ngesta)

lower_acima_npari <- intervalos_a_npari[, , 2][-1, 1]
upper_acima_npari <- intervalos_a_npari[, , 2][-1, 2]
b_ngesta <- cbind(ORs_a_npari[2, -1], lower_acima_npari, upper_acima_npari)

ORs <- rbind(a_npari, b_ngesta)
ORs
# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
a_npari <- data.frame(ngesta = "a_npari", or.ci = or.ci)
view(a_npari)


############################################## Variável PCR ############################################## 

# Construir o modelo utilizando a variável a_pcr
modelo_a_pcr <- multinom(categoria ~ a_pcr, data = dados)
summary(modelo_a_pcr)

# Calcular as ORs
ORs_a_pcr <- exp(coef(modelo_a_pcr))
ORs_a_pcr

# Calcular os intervalos de confiança
intervalos_a_pcr <- exp(confint(modelo_a_pcr))

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo_pcr <- intervalos_a_pcr[, , 1][-1, 1]
upper_abaixo_pcr <- intervalos_a_pcr[, , 1][-1, 2]
a_pcr <- cbind(ORs_a_pcr[1, -1], lower_abaixo_pcr, upper_abaixo_pcr)

lower_acima_pcr <- intervalos_a_pcr[, , 2][-1, 1]
upper_acima_pcr <- intervalos_a_pcr[, , 2][-1, 2]
b_pcr <- cbind(ORs_a_pcr[2, -1], lower_acima_pcr, upper_acima_pcr)

ORs <- rbind(a_pcr, b_pcr)

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
a_pcr <- data.frame(pcr = "a_pcr", or.ci = or.ci)
view(a_pcr)

############################################## Variável a_homa##############################################

modelo_a_homa <- multinom(categoria ~ a_homa, data = dados)
summary(modelo_a_homa)

# Calcular as ORs
ORs_a_homa <- exp(coef(modelo_a_homa))
ORs_a_homa

# Calcular os intervalos de confiança
intervalos_a_homa <- exp(confint(modelo_a_homa))

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo_homa <- intervalos_a_homa[, , 1][-1, 1]
upper_abaixo_homa <- intervalos_a_homa[, , 1][-1, 2]
a_homa <- cbind(ORs_a_homa[1, -1], lower_abaixo_homa, upper_abaixo_homa)

lower_acima_homa <- intervalos_a_homa[, , 2][-1, 1]
upper_acima_homa <- intervalos_a_homa[, , 2][-1, 2]
b_homa <- cbind(ORs_a_homa[2, -1], lower_acima_homa, upper_acima_homa)

ORs <- rbind(a_homa, b_homa)

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
a_homa <- data.frame(homa = "a_homa", or.ci = or.ci)

view(a_homa )

############################################## Variável Hemoglobina ############################################## 
# Ajustar o modelo
modelo_a_hb <- multinom(categoria ~ a_hb, data = dados)
summary(modelo_a_hb)

# Calcular as ORs
ORs_a_hb <- exp(coef(modelo_a_hb))
ORs_a_hb

# Calcular os intervalos de confiança
intervalos_a_hb <- exp(confint(modelo_a_hb))

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo_a_hb <- intervalos_a_hb[, , 1][-1, 1]
upper_abaixo_a_hb <- intervalos_a_hb[, , 1][-1, 2]
a_a_hb <- cbind(ORs_a_hb[1, -1], lower_abaixo_a_hb, upper_abaixo_a_hb)

lower_acima_a_hb <- intervalos_a_hb[, , 2][-1, 1]
upper_acima_a_hb <- intervalos_a_hb[, , 2][-1, 2]
b_a_hb <- cbind(ORs_a_hb[2, -1], lower_acima_a_hb, upper_acima_a_hb)

ORs <- rbind(a_a_hb, b_a_hb)

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
a_hb <- data.frame(a_hb = "a_hb", or.ci = or.ci)
view(a_hb)

############################################## Variável a_hba1c############################################## 
modelo_a_hba1c <- multinom(categoria ~ a_hba1c, data = dados)
summary(modelo_a_hba1c)

# Calcular as ORs
ORs_hba1c <- exp(coef(modelo_a_hba1c))
ORs_hba1c

# Calcular os intervalos de confiança
intervalos_a_hba1c <- exp(confint(modelo_a_hba1c))

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo_hba1c <- intervalos_a_hba1c[, , 1][-1, 1]
upper_abaixo_hba1c <- intervalos_a_hba1c[, , 1][-1, 2]
a_hba1c <- cbind(ORs_hba1c[1, -1], lower_abaixo_hba1c, upper_abaixo_hba1c)

lower_acima_hba1c <- intervalos_a_hba1c[, , 2][-1, 1]
upper_acima_hba1c <- intervalos_a_hba1c[, , 2][-1, 2]
b_hba1c <- cbind(ORs_hba1c[2, -1], lower_acima_hba1c, upper_acima_hba1c)

ORs <- rbind(a_hba1c, b_hba1c)

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
a_hba1c <- data.frame(hba1c = "a_hba1c", or.ci = or.ci)
view(a_hba1c)

############################################## Variável a_insul############################################## 
modelo_a_insul <- multinom(categoria ~ a_insul, data = dados)
summary(modelo_a_insul)

# Calcular as ORs
ORs_insul <- exp(coef(modelo_a_insul))
ORs_insul

# Calcular os intervalos de confiança
intervalos_a_insul <- exp(confint(modelo_a_insul))

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo_insul <- intervalos_a_insul[, , 1][-1, 1]
upper_abaixo_insul <- intervalos_a_insul[, , 1][-1, 2]
a_insul <- cbind(ORs_insul[1, -1], lower_abaixo_insul, upper_abaixo_insul)

lower_acima_insul <- intervalos_a_insul[, , 2][-1, 1]
upper_acima_insul <- intervalos_a_insul[, , 2][-1, 2]
b_insul <- cbind(ORs_insul[2, -1], lower_acima_insul, upper_acima_insul)

ORs <- rbind(a_insul, b_insul)

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
a_insul <- data.frame(a_insul = "a_insul", or.ci = or.ci)
view(a_insul)

##############################################Colesterol ############################################## 
# Construir modelo_a_ct
modelo_a_ct <- multinom(categoria ~ a_ct, data = dados)
summary(modelo_a_ct)

# Calcular as ORs
ORs_a_ct <- exp(coef(modelo_a_ct))
ORs_a_ct

# Calcular os intervalos de confiança
intervalos_a_ct <- exp(confint(modelo_a_ct))

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo_ct <- intervalos_a_ct[, , 1][-1, 1]
upper_abaixo_ct <- intervalos_a_ct[, , 1][-1, 2]
a_ct <- cbind(ORs_a_ct[1, -1], lower_abaixo_ct, upper_abaixo_ct)

lower_acima_ct <- intervalos_a_ct[, , 2][-1, 1]
upper_acima_ct <- intervalos_a_ct[, , 2][-1, 2]
b_ct <- cbind(ORs_a_ct[2, -1], lower_acima_ct, upper_acima_ct)

ORs <- rbind(a_ct, b_ct)

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
a_ct <- data.frame(a_ct = "a_ct", or.ci = or.ci)
view(a_ct)



############################################## Variável a_hdl############################################## 
modelo_a_hdl <- multinom(categoria ~ a_hdl, data = dados)
summary(modelo_a_hdl)

# Calcular as ORs
ORs_a_hdl <- exp(coef(modelo_a_hdl))
ORs_a_hdl

# Calcular os intervalos de confiança
intervalos_a_hdl <- exp(confint(modelo_a_hdl))

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo_hdl <- intervalos_a_hdl[, , 1][-1, 1]
upper_abaixo_hdl <- intervalos_a_hdl[, , 1][-1, 2]
a_hdl <- cbind(ORs_a_hdl[1, -1], lower_abaixo_hdl, upper_abaixo_hdl)

lower_acima_hdl <- intervalos_a_hdl[, , 2][-1, 1]
upper_acima_hdl <- intervalos_a_hdl[, , 2][-1, 2]
b_hdl <- cbind(ORs_a_hdl[2, -1], lower_acima_hdl, upper_acima_hdl)

ORs <- rbind(a_hdl, b_hdl)

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
a_hdl <- data.frame(hdl = "a_hdl", or.ci = or.ci)
view(a_hdl)


############################################## Variável a_ldl############################################## 
# Construir o modelo
modelo_a_ldl <- multinom(categoria ~ a_ldl, data = dados)
summary(modelo_a_ldl)

# Calcular as ORs
ORs_ldl <- exp(coef(modelo_a_ldl))
ORs_ldl

# Calcular os intervalos de confiança
intervalos_a_ldl <- exp(confint(modelo_a_ldl))

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo_ldl <- intervalos_a_ldl[, , 1][-1, 1]
upper_abaixo_ldl <- intervalos_a_ldl[, , 1][-1, 2]
a_ldl <- cbind(ORs_ldl[1, -1], lower_abaixo_ldl, upper_abaixo_ldl)

lower_acima_ldl <- intervalos_a_ldl[, , 2][-1, 1]
upper_acima_ldl <- intervalos_a_ldl[, , 2][-1, 2]
b_ldl <- cbind(ORs_ldl[2, -1], lower_acima_ldl, upper_acima_ldl)

ORs <- rbind(a_ldl, b_ldl)

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
a_ldl <- data.frame(ldl = "a_ldl", or.ci = or.ci)
tabela_resultados <- rbind(cat_imc, a_ldl)
view(a_ldl )




######################################################################## Modelo Ajustado  AAAAA#####################################################################################
##############################################################################################################################################################################
# Criar o modelo
modelo_adjsA <- multinom(categoria ~ a_idade + cat_estat  + cat_imc + cat_circbr + a_fmp + d_IGUSGrn + a_escola +
                           cat_npcomo + a_cor + a_civil + a_moderh + a_fumog + a_alcool + a_agdm + a_aghas + a_agui + 
                           a_agceva + a_npari + a_pcr + a_homa + a_hb + a_hba1c + a_insul + a_ct + a_hdl + a_ldl, data = dados)
summary(modelo_adjsA)

# Calcular as ORs
ORs_adjsA <- exp(coef(modelo_adjsA))
ORs_adjsA

# Calcular os intervalos de confiança
intervalos_adjsA <- exp(confint(modelo_adjsA))
intervalos_adjsA

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo_adjsA <- intervalos_adjsA[, , 1][-1,1]
upper_abaixo_adjsA <- intervalos_adjsA[, , 1][-1,2]
resultados_adjsA <- cbind(ORs_adjsA[1,-1 ], lower_abaixo_adjsA , upper_abaixo_adjsA)

lower_acima_adjsA <- intervalos_adjsA[, , 2][-1,1]
upper_acima_adjsA <- intervalos_adjsA[, , 2][-1,2]
resultados_adjsB <- cbind(ORs_adjsA[2,-1 ], lower_acima_adjsA, upper_acima_adjsA)

resultados_adjst <- rbind(resultados_adjsA, resultados_adjsB)

# Formatação dos resultados
or <- formatC(round(resultados_adjst[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(resultados_adjst[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(resultados_adjst[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
model_adjs <- data.frame(resultados_adjst[,0], or.ci = or.ci)
View(model_adjs)



######################################################################## Modelo Ajustado  Final #####################################################################################
##############################################################################################################################################################################
# Criar o modelo
modelo_adjsA <- multinom(categoria ~ a_idade + cat_estat  + cat_imc + cat_circbr + a_fmp + d_IGUSGrn + a_escola +
                           cat_npcomo + a_fumog +  a_agdm + a_aghas +  a_npari + 
                           a_hb  + a_insul +  a_hdl + a_ldl, data = dados)
summary(modelo_adjsA)

# Calcular as ORs
ORs_adjsA <- exp(coef(modelo_adjsA))
ORs_adjsA

# Calcular os intervalos de confiança
intervalos_adjsA <- exp(confint(modelo_adjsA))
intervalos_adjsA

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo_adjsA <- intervalos_adjsA[, , 1][-1,1]
upper_abaixo_adjsA <- intervalos_adjsA[, , 1][-1,2]
resultados_adjsA <- cbind(ORs_adjsA[1,-1 ], lower_abaixo_adjsA , upper_abaixo_adjsA)

lower_acima_adjsA <- intervalos_adjsA[, , 2][-1,1]
upper_acima_adjsA <- intervalos_adjsA[, , 2][-1,2]
resultados_adjsB <- cbind(ORs_adjsA[2,-1 ], lower_acima_adjsA, upper_acima_adjsA)

resultados_adjst <- rbind(resultados_adjsA, resultados_adjsB)

# Formatação dos resultados
or <- formatC(round(resultados_adjst[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(resultados_adjst[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(resultados_adjst[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
model_adjs <- data.frame(resultados_adjst[,0], or.ci = or.ci)
View(model_adjs)

############################################################################################################################################################################
############################################################################################################################################################################
#########################################################################                    ################################################################################
#########################################################################    Intergowth      ######################################################################################
#########################################################################                    #################################################################################
############################################################################################################################################################################
############################################################################################################################################################################


dados_filt <- dados[dados$a_agdm != 1, ]
dados_filt <- dados[dados$a_aghas !=1, ]
dados_filt <- dados_filt[dados_filt$imc >= 18.5 & dados_filt$imc<= 24.9, ]
dados <- dados_filt

table(dados$a_agdm)
######################################################################### Crie categorias  #################################################################################

clasif_gpg <- function(d_IGUSGrn, gpg) {
  percentis <- data.frame(
    semanas = c(23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40),
    perc10 = c(2.4, 2.8, 3.2, 3.6, 3.9, 4.3, 4.7, 5.0, 5.4, 5.7, 6.1, 6.5, 6.8, 7.2, 7.6, 7.9, 8.3, 8.7),
    perc50 = c(4.9, 5.4, 5.9, 6.5, 7.0, 7.5, 8.0, 8.5, 9.0, 9.5, 10.0, 10.5, 11.1, 11.6, 12.1, 12.6, 13.2, 13.7),
    perc90 = c(7.9, 8.6, 9.3, 10.0, 10.7, 11.4, 12.1, 12.9, 13.6, 14.3, 15.0, 15.7, 16.5, 17.2, 17.9, 18.7, 19.4, 20.1)
  )
  
  # Encontrar a linha correspondente à semana de gestação
  linha <- percentis[which.min(abs(percentis$semanas - d_IGUSGrn)), ]
  
  # Classificar o ganho de peso com base nos percentis
  if (gpg < linha$perc10) {
    categoria1 <- "Abaixo"
  } else if (gpg < linha$perc90) {
    categoria1 <- "Acima"
  } else {
    categoria1 <- "Dentro"
  } 
  return(categoria1)
}

# Classificar os dados
dados$categoria1 <- mapply(clasif_gpg, dados$d_IGUSGrn[complete.cases(dados$d_IGUSGrn)], dados$gpg[complete.cases(dados$gpg)])

dados$categoria1 <- factor(dados$categoria1, levels = c("Dentro", "Abaixo", "Acima"))
dados$categoria <-dados$categoria1
# Exibir os dados com as categorias
table(dados$categoria)
prop.table(table(dados$categoria1))*100
###################################################################################################################################################################################
########################################## construcao de tabelas descritivas  das variveis preditoras e o Desfecho - GPG ##########################################################
###################################################################################################################################################################################

#########################################Tabela de contingencia Idade por GPG #########################################
############################################# Idade ##########################################################

# Transformando valores negativos em positivos
dados$a_idade <- abs(dados$a_idade)

# Removendo valores iguais a 0
dados <- dados[dados$a_idade != 0,]

hist1 <- subset(dados, a_idade < 70)
hist(hist1$a_idade,
     main = "Histograma valores da idade abaixo de 70",
     xlab = "Idade",
     ylab = "Frequência")

# Categorizando as idades em três grupos: <=19, 20-35 e >=35
dados$a_idade <- cut(dados$a_idade, breaks = c(-Inf, 19, 35, Inf), 
                     labels = c("<=19", "20-35", ">=35"))


table(dados$a_idade)
prop.table(table(dados$a_idade))*100
table(dados$a_idade,dados$categoria)
prop.table(table(dados$a_idade,dados$categoria))*100

# Teste de Quiquadrado
Modelo_idade <- chisq.test(dados$a_idade, dados$categoria)
print(Modelo_idade)


######################################### Calcular a média e o desvio padrão da  estatura por GPG#########################################
############################################################# Estatura #######################################################
sort(dados$a_estat1, decreasing = T)
summary(dados$a_estat1)

hist <- subset(dados, a_estat1 < 200)
hist(hist$a_estat1, 
     main = "Histograma de a_estat1 (valores abaixo de 200)",
     xlab = "a_estat1",
     ylab = "Frequência")

dados$a_estat1<- ifelse (dados$a_estat1 < 99, dados$a_estat1 * 100, dados$a_estat1)


summary(dados$a_estat1)

aggregate(a_estat1 ~ categoria, data = dados, FUN = function(x) c(mediana = median(x), IQR = (quantile(x, c(0.25, 0.75)))))

# Teste de normalidade
shapiro.test(dados$a_estat1)

# Teste de homogeneidade de variâncias
bartlett.test(a_estat1 ~ categoria, data = dados)

# Realizar o teste  kruskal Wallis
modelo_esta <- kruskal.test(a_estat1 ~ categoria, data = dados)
modelo_esta

# Realizando comparações múltiplas com o método de Bonferroni
comp_esta <- pairwise.wilcox.test(dados$a_estat1, dados$categoria, p.adjust.method = "bonferroni")
comp_esta
##########################################################Categoria #######################################################################################
summary(dados$a_estat1)

dados$cat_estat <- cut(dados$a_estat1, 
                       breaks = quantile(dados$a_estat1, probs = c(0, 1/3, 2/3, 1)),
                       labels = c("1º Tercil", "2º Tercil", "3º Tercil"))

table(dados$cat_estat)
prop.table(table(dados$cat_estat)) * 100
table(dados$cat_estat, dados$categoria)
prop.table(table(dados$cat_estat, dados$categoria)) * 100
Modelo_estat <- chisq.test(dados$cat_estat, dados$categoria)
Modelo_estat



###########################################  calcular a médiana e o desvio padrão do IMC-Pre-gestacional por GPG########################################## 
summary(dados$imc)

aggregate(imc ~ categoria, data = dados, FUN = function(x) c(mediana = median(x), IQR = (quantile(x, c(0.25, 0.75)))))


# Teste de normalidade
shapiro.test(dados$imc)

# Teste de homogeneidade de variâncias
bartlett.test(imc ~ categoria, data = dados)

# Realizar o teste ANOVA do IMC
modelo_imc <- kruskal.test(imc ~ categoria, data = dados)
modelo_imc

# Realizando comparações múltiplas com o método de Bonferroni
comp_imc <- pairwise.wilcox.test(dados$imc, dados$categoria, p.adjust.method = "bonferroni")
comp_imc

###########################################  calcular a médiana  da Circunferência do braço por GPG ########################################## 
summary(dados$a_circbracm)
aggregate(a_circbracm ~ categoria, data = dados, FUN = function(x) c(mediana = median(x), IQR = (quantile(x, c(0.25, 0.75)))))

# Teste de normalidade
shapiro.test(dados$a_circbracm)

# Teste de homogeneidade de variâncias
bartlett.test(a_circbracm ~ categoria, data = dados)

# Realizar o teste ANOVA do IMC
modelo_a_circbracm <- kruskal.test(a_circbracm ~ categoria, data = dados)
modelo_a_circbracm

# Realizando comparações múltiplas com o método de Bonferroni
comp_a_circbracm <- pairwise.wilcox.test(dados$a_circbracm, dados$categoria, p.adjust.method = "bonferroni")
comp_a_circbracm

########################################################## Categoria #######################################################################################

summary(dados$a_circbracm)

dados$cat_circbr <- ifelse(dados$a_circbracm < 23, "Baixo peso",
                           ifelse(dados$a_circbracm >= 23 & dados$a_circbracm < 28, "Adequado",
                                  "Sobrepeso"))
table(dados$cat_circbr)
prop.table(table(dados$cat_circbr)) * 100
table(dados$cat_circbr, dados$categoria)
prop.table(table(dados$cat_circbr, dados$categoria)) * 100
Modelo_circbr <- chisq.test(dados$cat_circbr, dados$categoria)
Modelo_circbr



###########################################  calcular a médiana  da Percentual de gordura corporal (%) por GPG########################################## 
summary(dados$a_fmp)
aggregate(a_fmp ~ categoria, data = dados,  FUN = function(x) c(mediana = median(x), IQR = (quantile(x, c(0.25, 0.75)))))

# Teste de normalidade
shapiro.test(dados$a_fmp)

# Teste de homogeneidade de variâncias
bartlett.test(a_fmp ~ categoria, data = dados)

# Realizar o teste ANOVA do IMC
modelo_a_fmp <- kruskal.test(a_fmp ~ categoria, data = dados)
modelo_a_fmp

# Realizando comparações múltiplas com o método de Bonferroni
comp_a_fmp <- pairwise.wilcox.test(dados$a_fmp, dados$categoria, p.adjust.method = "bonferroni")
comp_a_fmp

########################################## calcular a médiaa e o IQR da Idade gestacional em semanas por GPG#########################################

############################################# Idade gestacional em semana ##########################################################
sort(dados$d_IGUSGrn , decreasing = T )
dados$d_IGUSGrn <- dados$d_IGUSGrn/ 7

summary(dados$d_IGUSGrn)

summary(dados$d_IGUSGrn)

aggregate(d_IGUSGrn ~ categoria, data = dados,FUN = function(x) c(mediana = median(x), IQR = (quantile(x, c(0.25, 0.75)))))

# Teste de normalidade
shapiro.test(dados$d_IGUSGrn)

# Teste de homogeneidade de variâncias
bartlett.test(d_IGUSGrn ~ categoria, data = dados)

# Realizar o teste  kruskal Wallis
modelo_ig <- kruskal.test(d_IGUSGrn ~ categoria, data = dados)
modelo_ig

# Realizando comparações múltiplas com o método de Bonferroni
comp_ig <- pairwise.wilcox.test(dados$d_IGUSGrn, dados$categoria, p.adjust.method = "bonferroni")
comp_ig 

###################################################### categaoria ###############################################################
summary(dados$d_IGUSGrn)

dados$cat_ig <- ifelse(dados$d_IGUSGrn <= 37 , "≤37" , ">37" )
table(dados$cat_ig )
prop.table(table(dados$cat_ig))*100
table(dados$cat_ig,dados$categoria)
prop.table(table(dados$cat_ig,dados$categoria))*100

Modelo_ig <- chisq.test(dados$cat_ig, dados$categoria)
Modelo_ig


############################### Tabela de contigencia da Escolaridade  da gestante (em anos de estudos), por GPG ############################################## 
# Categorizando a escolaridade  em três grupos: "≤4", "5-11" e "≥12".
sort(dados$a_escola, decreasing = T )

dados$a_escola[dados$a_escola == 0.11] <- 11
dados$a_escola[dados$a_escola == 0.15] <- 15
dados$a_escola[dados$a_escola == 90.09] <- 9
dados$a_escola[dados$a_escola == 0.06] <- 6

dados$a_escolat <- cut(dados$a_escola, breaks = c(-Inf, 12, Inf),
                       labels = c("<12", "≥12"))


table(dados$a_escolat)

table(dados$a_escolat)
prop.table(table(dados$a_escolat))*100
table(dados$a_escolat,dados$categoria)
prop.table(table(dados$a_escolat,dados$categoria))*100

# Teste de Fisher
Modelo_escola<-chisq.test(dados$a_escolat, dados$categoria)
Modelo_escola

######################### Numero de comodos por pessoa, por GPG ##################################### 
summary(dados$a_npcomo)

aggregate(a_npcomo ~ categoria, data = dados,FUN = function(x) c(mediana = median(x), IQR = (quantile(x, c(0.25, 0.75)))))

# Teste de normalidade
shapiro.test(dados$a_npcomo)

# Teste de homogeneidade de variâncias
bartlett.test(a_npcomo ~ categoria, data = dados)

# Realizar o teste  kruskal Wallis
modelo_npcomo <- kruskal.test(a_npcomo ~ categoria, data = dados)
modelo_npcomo

# Realizando comparações múltiplas com o método de Bonferroni
comp_npcomo <- pairwise.wilcox.test(dados$a_npcomo, dados$categoria, p.adjust.method = "bonferroni")
comp_npcomo

######################################################################## categoria ######################################################################
summary(dados$a_npcomo)

dados$cat_npcomo <- cut(dados$a_npcomo, 
                        breaks = quantile(dados$a_npcomo, probs = c(0, 1/3, 2/3, 1)),
                        labels = c("1º Tercil", "2º Tercil", "3º Tercil"))

table(dados$cat_npcomo)
prop.table(table(dados$cat_npcomo)) * 100
table(dados$cat_npcomo, dados$categoria)
prop.table(table(dados$cat_npcomo, dados$categoria)) * 100
Modelo_npcomo <- chisq.test(dados$cat_npcomo, dados$categoria)
Modelo_npcomo

######################### calcular a médiana e o IQR da Renda per capita em Reais, por GPG ##################################### 
dados$a_rendpcr <- round(dados$a_rendpcr, 3)

summary(dados$a_rendpcr)

aggregate(a_rendpcr ~ categoria, data = dados,FUN = function(x) c(mediana = median(x), IQR = (quantile(x, c(0.25, 0.75)))))

# Teste de normalidade
shapiro.test(dados$a_rendpcr)

# Teste de homogeneidade de variâncias
bartlett.test(a_rendpcr ~ categoria, data = dados)

# Realizar o teste  kruskal Wallis
modelo_rendpcr <- kruskal.test(a_rendpcr ~ categoria, data = dados)
modelo_rendpcr

# Realizando comparações múltiplas com o método de Bonferroni
comp_rendpcr <- pairwise.wilcox.test(dados$a_rendpcr, dados$categoria, p.adjust.method = "bonferroni")
comp_rendpcr


##################################### Tabela de contingencia da raca, por GPG ##################################### 
dados$a_cor <- ifelse(dados$a_cor == 1, "branco", "Nbranco")

table(dados$a_cor)
prop.table(table(dados$a_cor))*100
table(dados$a_cor,dados$categoria)
prop.table(table(dados$a_cor,dados$categoria))*100

# Usando o método de simulação de Monte Carlo com 10.000 simulações
Modelo_cor <- chisq.test(dados$a_cor, dados$categoria)
Modelo_cor


##################################### Tabela de contingencia Estado civil por GPG ##################################### 
####recodificando a variável a_civil em duas categorias
dados$a_civil <- ifelse(dados$a_civil %in% c(1 , 2), 1, 2) #### recodificar 2  categorias: 1-Casada ou em união estável, 2-  Solteira, separada ou viúva 

table(dados$a_civil)
prop.table(table(dados$ a_civil))*100
table(dados$a_civil,dados$categoria)
prop.table(table(dados$a_civil,dados$categoria))*100

# Teste de Quiquadrado
Modelo_ecivil  <- chisq.test(dados$a_civil, dados$categoria)

Modelo_ecivil


############################################ # calcular a média e o desvio padrão da Actividade fisica  por GPG############################################ 

dados$a_moderh <- ifelse(dados$a_moderh< 150, "inadequado", "adequado")

table(dados$a_moderh)
prop.table(table(dados$a_moderh))*100
table(dados$a_moderh, dados$categoria)
prop.table(table(dados$a_moderh,dados$categoria))*100

# Teste de Quiquadrado
Modelo_a_moderh <- chisq.test(dados$a_moderh, dados$categoria)
Modelo_a_moderh

############################################# Tabela de contingencia  do Habitio de Fumar por GPG ############################################ 

dados$a_fumog <- factor(dados$a_fumog, levels = c(0, 1), labels = c("Não", "Sim"))
table(dados$a_fumog)
prop.table(table(dados$a_fumog))*100
table(dados$a_fumog,dados$categoria)
prop.table(table(dados$a_fumog,dados$categoria))*100

# Teste de Quiquadrado
Modelo_fumo  <- chisq.test(dados$a_fumog, dados$categoria)
Modelo_fumo

############################################# Tabela de contingencia  do Acolool  por GPG ############################################ 
dados$a_alcool <- factor(dados$a_alcool, levels = c(0, 1), labels = c("Não", "Sim"))
table(dados$a_alcool)
prop.table(table(dados$a_alcool))*100
table(dados$a_alcool,dados$categoria)
prop.table(table(dados$a_alcool,dados$categoria))*100

# Teste de Quiquadrado
Modelo_alcool  <- chisq.test(dados$a_alcool, dados$categoria)
Modelo_alcool

################################################## Tabela de contingencia do Numero de gravidezes anterior por GPG ############################################

dados$a_npari <- ifelse(dados$a_npari == 0, "0",
                         ifelse(dados$a_npari == 1, "um",
                                "2+"))


dados$a_npari <- factor(dados$a_npari, levels = c("0", "um", "2+"))

table(dados$a_npari)
prop.table(table(dados$a_npari))*100
table(dados$a_npari,dados$categoria)
prop.table(table(dados$a_npari,dados$categoria))*100

6# Teste de Quiquadrado
Modelo_a_npari  <- chisq.test(dados$a_npari, dados$categoria)

Modelo_a_npari

################################################# Tabela de contingencia do PCR por GPG ############################################

summary(dados$a_pcr)
aggregate( a_pcr ~ categoria, data = dados, FUN = function(x) c(mediana = median(x), IQR = (quantile(x, c(0.25, 0.75)))))

# Teste de normalidade
shapiro.test(dados$a_pcr)

# Teste de homogeneidade de variâncias
bartlett.test(a_pcr ~ categoria, data = dados)

# Realizar o teste Kruskal-Wallis
modelo_pcr <- kruskal.test(a_pcr ~ categoria, data = dados)
modelo_pcr

# Realizando comparações múltiplas com o método de Bonferroni
comp_pcr<- pairwise.wilcox.test(dados$a_pcr, dados$categoria, p.adjust.method = "bonferroni")
comp_pcr


################################################## Tabela de contingencia do HOMA por GPG ############################################

summary(dados$a_homa)
aggregate( a_homa ~ categoria, data = dados, FUN = function(x) c(mediana = median(x), IQR = (quantile(x, c(0.25, 0.75)))))

# Teste de normalidade
shapiro.test(dados$a_homa)

# Teste de homogeneidade de variâncias
bartlett.test(a_homa ~ categoria, data = dados)

# Realizar o teste Kruskal-Wallis
modelo_homa <- kruskal.test(a_homa ~ categoria, data = dados)
modelo_homa

# Realizando comparações múltiplas com o método de Bonferroni
comp_homa<- pairwise.wilcox.test(dados$a_homa, dados$categoria, p.adjust.method = "bonferroni")
comp_homa
#################################################Tabela de Contingencia  da Hemoglobina por GPG ################################################
sort(dados$a_hb, decreasing = T)
summary(dados$a_hb)

aggregate( a_hb ~ categoria, data = dados, FUN = function(x) c(mediana = median(x), IQR = (quantile(x, c(0.25, 0.75)))))

# Teste de normalidade
shapiro.test(dados$a_hb)

# Teste de homogeneidade de variâncias
bartlett.test(a_hb ~ categoria, data = dados)

# Realizar o teste Kruskal-Wallis
modelo_hb <- kruskal.test(a_hb ~ categoria, data = dados)
modelo_hb 

# Realizando comparações múltiplas com o método de Bonferroni
comp_a_hb<- pairwise.wilcox.test(dados$a_hb, dados$categoria, p.adjust.method = "bonferroni")
comp_a_hb


#################################################Tabela de Contingencia  da Hemoglobina glicada por GPG ################################################
############################################# heoglobina glicada ##########################################################
sort(dados$a_hba1c, decreasing = T)
summary(dados$a_hba1c)
dados$a_hba1c [dados$a_hba1c == 55.5] <- NA
summary(dados$a_hba1c)

aggregate( a_hba1c ~ categoria, data = dados, FUN = function(x) c(mediana = median(x), IQR = (quantile(x, c(0.25, 0.75)))))

# Teste de normalidade
shapiro.test(dados$a_hba1c)

# Teste de homogeneidade de variâncias
bartlett.test(a_hba1c ~ categoria, data = dados)

# Realizar o teste Kruskal-Wallis
modelo_hba1c <- kruskal.test(a_hba1c ~ categoria, data = dados)
modelo_hba1c 

# Realizando comparações múltiplas com o método de Bonferroni
comp_hba1c<- pairwise.wilcox.test(dados$a_hba1c, dados$categoria, p.adjust.method = "bonferroni")
comp_hba1c


################################################ Tabela de contingencia da INSULINA por GPG ################################################
sort(dados$a_insul, decreasing = T)
summary(dados$a_insul)
aggregate( a_insul ~ categoria, data = dados, FUN = function(x) c(mediana = median(x), IQR = (quantile(x, c(0.25, 0.75)))))

# Teste de normalidade
shapiro.test(dados$a_insul)

# Teste de homogeneidade de variâncias
bartlett.test(a_insul ~ categoria, data = dados)

# Realizar o teste Kruskal-Wallis
modelo_insull <- kruskal.test(a_insul ~ categoria, data = dados)
modelo_insull

# Realizando comparações múltiplas com o método de Bonferroni
comp_insul<- pairwise.wilcox.test(dados$a_insul, dados$categoria, p.adjust.method = "bonferroni")
comp_insul



################################################# calcular a média e o desvio padrão da HDL por GPG################################################
summary(dados$a_hdl)
aggregate( a_hdl ~ categoria, data = dados, FUN = function(x) c(mediana = median(x), IQR = (quantile(x, c(0.25, 0.75)))))

# Teste de normalidade
shapiro.test(dados$a_hdl)

# Teste de homogeneidade de variâncias
bartlett.test(a_hdl ~ categoria, data = dados)

# Realizar o teste Kruskal-Wallis
modelo_hdl <- kruskal.test(a_hdl ~ categoria, data = dados)
modelo_hdl

# Realizando comparações múltiplas com o método de Bonferroni
comp_hdl<- pairwise.wilcox.test(dados$a_hdl, dados$categoria, p.adjust.method = "bonferroni")
comp_hdl

################################################# calcular a média e o desvio padrão da LDL por GPG################################################
summary(dados$a_ldl)
aggregate( a_ldl ~ categoria, data = dados, FUN = function(x) c(mediana = median(x), IQR = (quantile(x, c(0.25, 0.75)))))

# Teste de normalidade
shapiro.test(dados$a_ldl)

# Teste de homogeneidade de variâncias
bartlett.test(a_ldl ~ categoria, data = dados)

# Realizar o teste Kruskal-Wallis
modelo_ldl <- kruskal.test(a_ldl ~ categoria, data = dados)
modelo_ldl

# Realizando comparações múltiplas com o método de Bonferroni
comp_ldL<- pairwise.wilcox.test(dados$a_ldl, dados$categoria, p.adjust.method = "bonferroni")
comp_ldL


################################################# calcular a médiana e o IQR de Triaglicerideos  por GPG################################################
summary(dados$ a_tg)
aggregate(  a_tg ~ categoria, data = dados, FUN = function(x) c(mediana = median(x), IQR = (quantile(x, c(0.25, 0.75)))))

# Teste de normalidade
shapiro.test(dados$a_tg)

# Teste de homogeneidade de variâncias
bartlett.test( a_tg ~ categoria, data = dados)

# Realizar o teste Kruskal-Wallis
modelo_a_tg <- kruskal.test( a_tg ~ categoria, data = dados)
modelo_a_tg

# Realizando comparações múltiplas com o método de Bonferroni
comp_a_tg <- pairwise.wilcox.test(dados$ a_tg, dados$categoria, p.adjust.method = "bonferroni")
comp_a_tg



##########################################################################################################################################################################
########################### Modelo ajustado e bruto dos factores preditores ao gpg#######################################################################################
##########################################################################################################################################################################

dados$cat_imc <- relevel(dados$cat_imc, ref = "Peso normal")
dados$a_escola <- relevel(dados$a_escolat, ref = "≥12")
dados$a_idade <- relevel(dados$a_idade, ref = "20-35")

dados$cat_estat <- relevel(dados$cat_estat, ref = "3º Tercil")
dados$cat_circbr <- relevel(factor(dados$cat_circbr), ref = "Adequado")
dados$cat_npcomo <- relevel(dados$cat_npcomo, ref = "1º Tercil")



##Instalar pacotes para rodar a regressao multinomial 
install.packages("broom")
install.packages("nnet")
install.packages("MASS")
library(nnet)   # Para ajustar o modelo de regressão logística multinomial
library(MASS)   # Para calcular a estatística de deviance do modelo ajustado
library(lme4)
########################################################################################################################################################
########################################################################################################################################################
################################################### Modelo  brutos######################################################################################
########################################################################################################################################################
########################################################################################################################################################

############################################## # Modelo para a_idade############################################## 
modelo_a_idade <- multinom(categoria ~ a_idade, data = dados)
summary(modelo_a_idade)
ORs <- exp(coef(modelo_a_idade))
ORs
intervalos_a_idade <- confint(modelo_a_idade)
intervalos_a_idade <- exp(confint(modelo_a_idade))

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo <- intervalos_a_idade[, ,1][-1,1]
upper_abaixo <- intervalos_a_idade[, ,1][-1,2]
a<-cbind(ORs[1,-1], lower_abaixo, upper_abaixo)
lower_acima <- intervalos_a_idade[, ,2][-1,1]
upper_acima <- intervalos_a_idade[, ,2][-1,2]
b<-cbind(ORs[2,-1],lower_acima, upper_acima)
joint <-rbind(a,b)
joint

# Formatar a OR e os intervalos de confiança
output <- table(paste(round(joint[,1],2),"(", round(joint[,2],2),"-", round(joint[,3],2),")"))
output <- output[reorder(names(output), c(3, 1, 2, 4))]
output <- data.frame(joint[,0], output)
view(output)



############################################## # Modelo para a_estat1############################################## 
modelo_a_estat1 <- multinom(categoria ~ cat_estat, data = dados)

# Calcular as ORs
ORs <- exp(coef(modelo_a_estat1))
ORs
# Calcular os intervalos de confiança
intervalos_a_estat1 <- confint(modelo_a_estat1)
intervalos_a_estat1 <- exp(intervalos_a_estat1)

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo <- intervalos_a_estat1[, , 1][-1, 1]
upper_abaixo <- intervalos_a_estat1[, , 1][-1, 2]
a <- cbind(ORs[1, -1], lower_abaixo, upper_abaixo)

# Formatar os resultados para a categoria abaixo
lower_acima <- intervalos_a_estat1[, , 2][-1, 1]
upper_acima <- intervalos_a_estat1[, , 2][-1, 2]
b <- cbind(ORs[2, -1], lower_acima, upper_acima)
joint <- rbind(a, b)
joint 

# Formatar a OR e os intervalos de confiança
output <- table(paste(round(joint[, 1], 2), "(", round(joint[, 2], 2), "-", round(joint[, 3], 2), ")"))
output <- output[reorder(names(output), c(3,4,1,2))]
output <- data.frame(joint[,0], output)
view(output)


############################################## # Modelo para a_circbracm############################################## 
# Construir o modelo
modelo_a_circbracm <- multinom(categoria ~ cat_circbr, data = dados)
summary(modelo_a_circbracm)

# Calculate the odds ratios (ORs)
ORs <- exp(coef(modelo_a_circbracm))
ORs

# Calculate the confidence intervals
intervalos_a_circbracm <- exp(confint(modelo_a_circbracm))
lower_abaixo <- intervalos_a_circbracm[, , 1][-1, 1]
upper_abaixo <- intervalos_a_circbracm[, , 1][-1, 2]
a <- cbind(ORs[1, -1], lower_abaixo, upper_abaixo)
lower_acima <- intervalos_a_circbracm[, , 2][-1, 1]
upper_acima <- intervalos_a_circbracm[, , 2][-1, 2]
b <- cbind(ORs[2, -1], lower_acima, upper_acima)
joint <- rbind(a, b)

# Format the odds ratios and confidence intervals
output <- table(paste(round(joint[, 1], 2), "(", round(joint[, 2], 2), "-", round(joint[, 3], 2), ")"))
output <- output[reorder(names(output), c(3, 2, 1, 4))]
output <- data.frame(joint[,0], output)
view(output)


############################################## # Modelo para a_fmp############################################## 
modelo_a_fmp <- multinom(categoria ~ a_fmp, data = dados)
summary(modelo_a_fmp)

# Calcular as ORs
ORs_a_fmp <- exp(coef(modelo_a_fmp))
ORs_a_fmp

# Calcular os intervalos de confiança
intervalos_a_fmp <- exp(confint(modelo_a_fmp))

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo_fmp <- intervalos_a_fmp[, , 1][-1, 1]
upper_abaixo_fmp <- intervalos_a_fmp[, , 1][-1, 2]
a_fmp <- cbind(ORs_a_fmp[1, -1], lower_abaixo_fmp, upper_abaixo_fmp)

lower_acima_fmp <- intervalos_a_fmp[, , 2][-1, 1]
upper_acima_fmp <- intervalos_a_fmp[, , 2][-1, 2]
b_fmp <- cbind(ORs_a_fmp[2, -1], lower_acima_fmp, upper_acima_fmp)

ORs <- rbind(a_fmp, b_fmp)

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
a_fmp <- data.frame(fmp = "a_fmp", or.ci = or.ci)
view(a_fmp)


############################################## Modelo para  idade gestacional ############################################# 
# Construir o modelo multinomial
modelo_d_IGUSGrn <- multinom(categoria ~ d_IGUSGrn, data = dados)

# Resumo do modelo
summary(modelo_d_IGUSGrn)

# Calcular as ORs
ORs_IGUSGrn <- exp(coef(modelo_d_IGUSGrn))
ORs_IGUSGrn

# Calcular os intervalos de confiança
intervalos_IGUSGrn <- exp(confint(modelo_d_IGUSGrn))

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo_IGUSGrn <- intervalos_IGUSGrn[, , 1][-1, 1]
upper_abaixo_IGUSGrn <- intervalos_IGUSGrn[, , 1][-1, 2]
a_IGUSGrn <- cbind(ORs_IGUSGrn[1, -1], lower_abaixo_IGUSGrn, upper_abaixo_IGUSGrn)
lower_acima_IGUSGrn <- intervalos_IGUSGrn[, , 2][-1, 1]
upper_acima_IGUSGrn <- intervalos_IGUSGrn[, , 2][-1, 2]
b_IGUSGrn <- cbind(ORs_IGUSGrn[2, -1], lower_acima_IGUSGrn, upper_acima_IGUSGrn)

ORs <- rbind(a_IGUSGrn, b_IGUSGrn)

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
d_IGUSGrn <- data.frame(IGUSGrn = "d_IGUSGrn", or.ci = or.ci)
view(d_IGUSGrn)


###############################################  Modelo para a_escola############################################## 
# Construir o modelo
modelo_a_escola <- multinom(categoria ~ a_escola, data = dados)
summary(modelo_a_escola)

# Calcular as ORs
ORs_a_escola <- exp(coef(modelo_a_escola))
ORs_a_escola

# Calcular os intervalos de confiança
intervalos_a_escola <- exp(confint(modelo_a_escola))

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo_a_escola <- intervalos_a_escola[, , 1][-1, 1]
upper_abaixo_a_escola <- intervalos_a_escola[, , 1][-1, 2]
a_a_escola <- cbind(ORs_a_escola[1, -1], lower_abaixo_a_escola, upper_abaixo_a_escola)
lower_acima_a_escola <- intervalos_a_escola[, , 2][-1, 1]
upper_acima_a_escola <- intervalos_a_escola[, , 2][-1, 2]
b_a_escola <- cbind(ORs_a_escola[2, -1], lower_acima_a_escola, upper_acima_a_escola)
ORs <- rbind(a_a_escola, b_a_escola)

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
a_escola <- data.frame(a_escola = "a_escola", or.ci = or.ci)
view(a_escola)
############################################## Numero de pessoas por comodo ############################################## 
modelo_a_npcomo <- multinom(categoria ~ cat_npcomo, data = dados)
summary(modelo_a_npcomo)

# Calcular as ORs
ORs <- exp(coef(modelo_a_npcomo))
ORs

# Calcular os intervalos de confiança
intervalos_a_npcomo <- exp(confint(modelo_a_npcomo))
lower_abaixo <- intervalos_a_npcomo[, , 1][-1, 1]
upper_abaixo <- intervalos_a_npcomo[, , 1][-1, 2]
a <- cbind(ORs[1, -1], lower_abaixo, upper_abaixo)
lower_acima <- intervalos_a_npcomo[, , 2][-1, 1]
upper_acima <- intervalos_a_npcomo[, , 2][-1, 2]
b <- cbind(ORs[2, -1], lower_acima, upper_acima)
joint <- rbind(a, b)

# Formatar a OR e os intervalos de confiança
output <- table(paste(round(joint[, 1], 2), "(", round(joint[, 2], 2), "-", round(joint[, 3], 2), ")"))
output <- output[reorder(names(output), c(3,4,1,2))]
output <- data.frame(output)
view(output)


############################################## # Modelo para a_cor############################################## 
# Construir o modelo
modelo_a_cor <- multinom(categoria ~ a_cor, data = dados)

# Resumo do modelo
summary(modelo_a_cor)

# Calcular as ORs
ORs_a_cor <- exp(coef(modelo_a_cor))
ORs_a_cor

# Calcular os intervalos de confiança
intervalos_a_cor <- exp(confint(modelo_a_cor))

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo_a_cor <- intervalos_a_cor[, , 1][-1, 1]
upper_abaixo_a_cor <- intervalos_a_cor[, , 1][-1, 2]
a_a_cor <- cbind(ORs_a_cor[1, -1], lower_abaixo_a_cor, upper_abaixo_a_cor)
lower_acima_a_cor <- intervalos_a_cor[, , 2][-1, 1]
upper_acima_a_cor <- intervalos_a_cor[, , 2][-1, 2]
b_a_cor <- cbind(ORs_a_cor[2, -1], lower_acima_a_cor, upper_acima_a_cor)
ORs <- rbind(a_a_cor, b_a_cor)

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
a_cor <- data.frame(a_cor = "a_cor", or.ci = or.ci)
view(a_cor)


############################################## # Modelo para a_civil############################################## 
modelo_a_civil <- multinom(categoria ~ a_civil, data = dados)
summary(modelo_a_civil)

# Calcular as ORs
ORs_a_civil <- exp(coef(modelo_a_civil))
ORs_a_civil

# Calcular os intervalos de confiança
intervalos_a_civil <- exp(confint(modelo_a_civil))

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo_a_civil <- intervalos_a_civil[, , 1][-1, 1]
upper_abaixo_a_civil <- intervalos_a_civil[, , 1][-1, 2]
a_a_civil <- cbind(ORs_a_civil[1, -1], lower_abaixo_a_civil, upper_abaixo_a_civil)
lower_acima_a_civil <- intervalos_a_civil[, , 2][-1, 1]
upper_acima_a_civil <- intervalos_a_civil[, , 2][-1, 2]
b_a_civil <- cbind(ORs_a_civil[2, -1], lower_acima_a_civil, upper_acima_a_civil)
ORs <- rbind(a_a_civil, b_a_civil)

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
a_civil <- data.frame(a_civil = "a_civil", or.ci = or.ci)
view(a_civil)


############################################### Atividade Fisica ############################################## 
# Construir o modelo
modelo_a_moderh <- multinom(categoria ~ a_moderh, data = dados)
summary(modelo_a_moderh)

# Calcular as ORs
ORs_a_moderh <- exp(coef(modelo_a_moderh))
ORs_a_moderh

# Calcular os intervalos de confiança
intervalos_a_moderh <- exp(confint(modelo_a_moderh))

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo_moderh <- intervalos_a_moderh[, , 1][-1, 1]
upper_abaixo_moderh <- intervalos_a_moderh[, , 1][-1, 2]
a_moderh <- cbind(ORs_a_moderh[1, -1], lower_abaixo_moderh, upper_abaixo_moderh)
lower_acima_moderh <- intervalos_a_moderh[, , 2][-1, 1]
upper_acima_moderh <- intervalos_a_moderh[, , 2][-1, 2]
b_moderh <- cbind(ORs_a_moderh[2, -1], lower_acima_moderh, upper_acima_moderh)
ORs <- rbind(a_moderh, b_moderh)

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")
# Tabela de resultados
a_moderh <- data.frame(a_moderh = "a_moderh", or.ci = or.ci)
view(a_moderh)


############################################## # Modelo para Fumar ############################################## 
modelo_a_fumog <- multinom(categoria ~ a_fumog, data = dados)
summary(modelo_a_fumog)

# Calcular as ORs
ORs_a_fumog <- exp(coef(modelo_a_fumog))
ORs_a_fumog

# Calcular os intervalos de confiança
intervalos_a_fumog <- exp(confint(modelo_a_fumog))

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo_fumog <- intervalos_a_fumog[, , 1][-1, 1]
upper_abaixo_fumog <- intervalos_a_fumog[, , 1][-1, 2]
a_fumog <- cbind(ORs_a_fumog[1, -1], lower_abaixo_fumog, upper_abaixo_fumog)
lower_acima_fumog <- intervalos_a_fumog[, , 2][-1, 1]
upper_acima_fumog <- intervalos_a_fumog[, , 2][-1, 2]
b_fumog <- cbind(ORs_a_fumog[2, -1], lower_acima_fumog, upper_acima_fumog)
ORs <- rbind(a_fumog, b_fumog)

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
a_fumog <- data.frame(a_fumog = "a_fumog", or.ci = or.ci)
view(a_fumog)


############################################## # Modelo para Alcool ############################################## 

modelo_a_alcool <- multinom(categoria ~ a_alcool, data = dados)
summary(modelo_a_alcool)

# Calcular as ORs
ORs_a_alcool <- exp(coef(modelo_a_alcool))
ORs_a_alcool

# Calcular os intervalos de confiança
intervalos_a_alcool <- exp(confint(modelo_a_alcool))

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo_alcool <- intervalos_a_alcool[, , 1][-1, 1]
upper_abaixo_alcool <- intervalos_a_alcool[, , 1][-1, 2]
a_alcool <- cbind(ORs_a_alcool[1, -1], lower_abaixo_alcool, upper_abaixo_alcool)

lower_acima_alcool <- intervalos_a_alcool[, , 2][-1, 1]
upper_acima_alcool <- intervalos_a_alcool[, , 2][-1, 2]
b_alcool <- cbind(ORs_a_alcool[2, -1], lower_acima_alcool, upper_acima_alcool)

ORs <- rbind(a_alcool, b_alcool)

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
a_alcool <- data.frame(alcool = "a_alcool", or.ci = or.ci)
view(a_alcool)


############################################## Variável a_npari############################################## 
# Construir o modelo
modelo_a_npari <- multinom(categoria ~ a_npari, data = dados)
summary(modelo_a_npari)

# Calcular as ORs
ORs_a_npari <- exp(coef(modelo_a_npari))
ORs_a_npari

# Calcular os intervalos de confiança
intervalos_a_npari <- exp(confint(modelo_a_npari))

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo_ngesta <- intervalos_a_npari[, , 1][-1, 1]
upper_abaixo_ngesta <- intervalos_a_npari[, , 1][-1, 2]
a_npari <- cbind(ORs_a_npari[1, -1], lower_abaixo_ngesta, upper_abaixo_ngesta)

lower_acima_npari <- intervalos_a_npari[, , 2][-1, 1]
upper_acima_npari <- intervalos_a_npari[, , 2][-1, 2]
b_ngesta <- cbind(ORs_a_npari[2, -1], lower_acima_npari, upper_acima_npari)

ORs <- rbind(a_npari, b_ngesta)
ORs
# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
a_npari <- data.frame(ngesta = "a_npari", or.ci = or.ci)
view(a_npari)


############################################## Variável PCR ############################################## 

# Construir o modelo utilizando a variável a_pcr
modelo_a_pcr <- multinom(categoria ~ a_pcr, data = dados)
summary(modelo_a_pcr)

# Calcular as ORs
ORs_a_pcr <- exp(coef(modelo_a_pcr))
ORs_a_pcr

# Calcular os intervalos de confiança
intervalos_a_pcr <- exp(confint(modelo_a_pcr))

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo_pcr <- intervalos_a_pcr[, , 1][-1, 1]
upper_abaixo_pcr <- intervalos_a_pcr[, , 1][-1, 2]
a_pcr <- cbind(ORs_a_pcr[1, -1], lower_abaixo_pcr, upper_abaixo_pcr)

lower_acima_pcr <- intervalos_a_pcr[, , 2][-1, 1]
upper_acima_pcr <- intervalos_a_pcr[, , 2][-1, 2]
b_pcr <- cbind(ORs_a_pcr[2, -1], lower_acima_pcr, upper_acima_pcr)

ORs <- rbind(a_pcr, b_pcr)

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
a_pcr <- data.frame(pcr = "a_pcr", or.ci = or.ci)
view(a_pcr)


############################################## Variável a_homa##############################################

modelo_a_homa <- multinom(categoria ~ a_homa, data = dados)
summary(modelo_a_homa)

# Calcular as ORs
ORs_a_homa <- exp(coef(modelo_a_homa))
ORs_a_homa

# Calcular os intervalos de confiança
intervalos_a_homa <- exp(confint(modelo_a_homa))

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo_homa <- intervalos_a_homa[, , 1][-1, 1]
upper_abaixo_homa <- intervalos_a_homa[, , 1][-1, 2]
a_homa <- cbind(ORs_a_homa[1, -1], lower_abaixo_homa, upper_abaixo_homa)

lower_acima_homa <- intervalos_a_homa[, , 2][-1, 1]
upper_acima_homa <- intervalos_a_homa[, , 2][-1, 2]
b_homa <- cbind(ORs_a_homa[2, -1], lower_acima_homa, upper_acima_homa)

ORs <- rbind(a_homa, b_homa)

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
a_homa <- data.frame(homa = "a_homa", or.ci = or.ci)

view(a_homa )

############################################## Variável Hemoglobina ############################################## 
# Ajustar o modelo
modelo_a_hb <- multinom(categoria ~ a_hb, data = dados)
summary(modelo_a_hb)

# Calcular as ORs
ORs_a_hb <- exp(coef(modelo_a_hb))
ORs_a_hb

# Calcular os intervalos de confiança
intervalos_a_hb <- exp(confint(modelo_a_hb))

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo_a_hb <- intervalos_a_hb[, , 1][-1, 1]
upper_abaixo_a_hb <- intervalos_a_hb[, , 1][-1, 2]
a_a_hb <- cbind(ORs_a_hb[1, -1], lower_abaixo_a_hb, upper_abaixo_a_hb)

lower_acima_a_hb <- intervalos_a_hb[, , 2][-1, 1]
upper_acima_a_hb <- intervalos_a_hb[, , 2][-1, 2]
b_a_hb <- cbind(ORs_a_hb[2, -1], lower_acima_a_hb, upper_acima_a_hb)

ORs <- rbind(a_a_hb, b_a_hb)

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
a_hb <- data.frame(a_hb = "a_hb", or.ci = or.ci)
view(a_hb)

############################################## Variável a_hba1c############################################## 
modelo_a_hba1c <- multinom(categoria ~ a_hba1c, data = dados)
summary(modelo_a_hba1c)

# Calcular as ORs
ORs_hba1c <- exp(coef(modelo_a_hba1c))
ORs_hba1c

# Calcular os intervalos de confiança
intervalos_a_hba1c <- exp(confint(modelo_a_hba1c))

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo_hba1c <- intervalos_a_hba1c[, , 1][-1, 1]
upper_abaixo_hba1c <- intervalos_a_hba1c[, , 1][-1, 2]
a_hba1c <- cbind(ORs_hba1c[1, -1], lower_abaixo_hba1c, upper_abaixo_hba1c)

lower_acima_hba1c <- intervalos_a_hba1c[, , 2][-1, 1]
upper_acima_hba1c <- intervalos_a_hba1c[, , 2][-1, 2]
b_hba1c <- cbind(ORs_hba1c[2, -1], lower_acima_hba1c, upper_acima_hba1c)

ORs <- rbind(a_hba1c, b_hba1c)

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
a_hba1c <- data.frame(hba1c = "a_hba1c", or.ci = or.ci)
view(a_hba1c)

############################################## Variável a_insul############################################## 
modelo_a_insul <- multinom(categoria ~ a_insul, data = dados)
summary(modelo_a_insul)

# Calcular as ORs
ORs_insul <- exp(coef(modelo_a_insul))
ORs_insul

# Calcular os intervalos de confiança
intervalos_a_insul <- exp(confint(modelo_a_insul))

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo_insul <- intervalos_a_insul[, , 1][-1, 1]
upper_abaixo_insul <- intervalos_a_insul[, , 1][-1, 2]
a_insul <- cbind(ORs_insul[1, -1], lower_abaixo_insul, upper_abaixo_insul)

lower_acima_insul <- intervalos_a_insul[, , 2][-1, 1]
upper_acima_insul <- intervalos_a_insul[, , 2][-1, 2]
b_insul <- cbind(ORs_insul[2, -1], lower_acima_insul, upper_acima_insul)

ORs <- rbind(a_insul, b_insul)

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
a_insul <- data.frame(a_insul = "a_insul", or.ci = or.ci)
view(a_insul)

##############################################Colesterol ############################################## 
# Construir modelo_a_ct
modelo_a_ct <- multinom(categoria ~ a_ct, data = dados)
summary(modelo_a_ct)

# Calcular as ORs
ORs_a_ct <- exp(coef(modelo_a_ct))
ORs_a_ct

# Calcular os intervalos de confiança
intervalos_a_ct <- exp(confint(modelo_a_ct))

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo_ct <- intervalos_a_ct[, , 1][-1, 1]
upper_abaixo_ct <- intervalos_a_ct[, , 1][-1, 2]
a_ct <- cbind(ORs_a_ct[1, -1], lower_abaixo_ct, upper_abaixo_ct)

lower_acima_ct <- intervalos_a_ct[, , 2][-1, 1]
upper_acima_ct <- intervalos_a_ct[, , 2][-1, 2]
b_ct <- cbind(ORs_a_ct[2, -1], lower_acima_ct, upper_acima_ct)

ORs <- rbind(a_ct, b_ct)

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
a_ct <- data.frame(a_ct = "a_ct", or.ci = or.ci)
view(a_ct)



############################################## Variável a_hdl############################################## 
modelo_a_hdl <- multinom(categoria ~ a_hdl, data = dados)
summary(modelo_a_hdl)

# Calcular as ORs
ORs_a_hdl <- exp(coef(modelo_a_hdl))
ORs_a_hdl

# Calcular os intervalos de confiança
intervalos_a_hdl <- exp(confint(modelo_a_hdl))

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo_hdl <- intervalos_a_hdl[, , 1][-1, 1]
upper_abaixo_hdl <- intervalos_a_hdl[, , 1][-1, 2]
a_hdl <- cbind(ORs_a_hdl[1, -1], lower_abaixo_hdl, upper_abaixo_hdl)

lower_acima_hdl <- intervalos_a_hdl[, , 2][-1, 1]
upper_acima_hdl <- intervalos_a_hdl[, , 2][-1, 2]
b_hdl <- cbind(ORs_a_hdl[2, -1], lower_acima_hdl, upper_acima_hdl)

ORs <- rbind(a_hdl, b_hdl)

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
a_hdl <- data.frame(hdl = "a_hdl", or.ci = or.ci)
view(a_hdl)


############################################## Variável a_ldl############################################## 
# Construir o modelo
modelo_a_ldl <- multinom(categoria ~ a_ldl, data = dados)
summary(modelo_a_ldl)

# Calcular as ORs
ORs_ldl <- exp(coef(modelo_a_ldl))
ORs_ldl

# Calcular os intervalos de confiança
intervalos_a_ldl <- exp(confint(modelo_a_ldl))

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo_ldl <- intervalos_a_ldl[, , 1][-1, 1]
upper_abaixo_ldl <- intervalos_a_ldl[, , 1][-1, 2]
a_ldl <- cbind(ORs_ldl[1, -1], lower_abaixo_ldl, upper_abaixo_ldl)

lower_acima_ldl <- intervalos_a_ldl[, , 2][-1, 1]
upper_acima_ldl <- intervalos_a_ldl[, , 2][-1, 2]
b_ldl <- cbind(ORs_ldl[2, -1], lower_acima_ldl, upper_acima_ldl)

ORs <- rbind(a_ldl, b_ldl)

# Formatação dos resultados
or <- formatC(round(ORs[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(ORs[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(ORs[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
a_ldl <- data.frame(ldl = "a_ldl", or.ci = or.ci)
tabela_resultados <- rbind(cat_imc, a_ldl)
view(a_ldl )


######################################################################## Modelo Ajustado  AAAAA#####################################################################################
##############################################################################################################################################################################
# Criar o modelo
modelo_adjsA <- multinom(categoria ~ a_idade + cat_estat +  cat_circbr + a_fmp + d_IGUSGrn + a_escola +
                           cat_npcomo + a_cor + a_civil + a_moderh + a_fumog + a_alcool + a_npari + a_pcr + a_homa +
                           a_hb + a_hba1c + a_insul + a_ct + a_hdl + a_ldl, data = dados)
summary(modelo_adjsA)

# Calcular as ORs
ORs_adjsA <- exp(coef(modelo_adjsA))
ORs_adjsA

# Calcular os intervalos de confiança
intervalos_adjsA <- exp(confint(modelo_adjsA))
intervalos_adjsA

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo_adjsA <- intervalos_adjsA[, , 1][-1,1]
upper_abaixo_adjsA <- intervalos_adjsA[, , 1][-1,2]
resultados_adjsA <- cbind(ORs_adjsA[1,-1 ], lower_abaixo_adjsA , upper_abaixo_adjsA)

lower_acima_adjsA <- intervalos_adjsA[, , 2][-1,1]
upper_acima_adjsA <- intervalos_adjsA[, , 2][-1,2]
resultados_adjsB <- cbind(ORs_adjsA[2,-1 ], lower_acima_adjsA, upper_acima_adjsA)

resultados_adjst <- rbind(resultados_adjsA, resultados_adjsB)

# Formatação dos resultados
or <- formatC(round(resultados_adjst[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(resultados_adjst[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(resultados_adjst[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
model_adjs <- data.frame(resultados_adjst[,0], or.ci = or.ci)
View(model_adjs)


######################################################################## Modelo Ajustado  Final #####################################################################################

# Criar o modelo
modelo_adjsA <- multinom(categoria ~ a_idade + cat_estat +  cat_circbr + a_fmp + d_IGUSGrn + a_escola +
                           cat_npcomo + a_moderh + a_fumog + a_npari + 
                           a_hb  + a_insul +  a_hdl + a_ldl, data = dados)
summary(modelo_adjsA)

# Calcular as ORs
ORs_adjsA <- exp(coef(modelo_adjsA))
ORs_adjsA

# Calcular os intervalos de confiança
intervalos_adjsA <- exp(confint(modelo_adjsA))
intervalos_adjsA

# Extrair os limites inferiores e superiores dos intervalos de confiança
lower_abaixo_adjsA <- intervalos_adjsA[, , 1][-1,1]
upper_abaixo_adjsA <- intervalos_adjsA[, , 1][-1,2]
resultados_adjsA <- cbind(ORs_adjsA[1,-1 ], lower_abaixo_adjsA , upper_abaixo_adjsA)

lower_acima_adjsA <- intervalos_adjsA[, , 2][-1,1]
upper_acima_adjsA <- intervalos_adjsA[, , 2][-1,2]
resultados_adjsB <- cbind(ORs_adjsA[2,-1 ], lower_acima_adjsA, upper_acima_adjsA)

resultados_adjst <- rbind(resultados_adjsA, resultados_adjsB)

# Formatação dos resultados
or <- formatC(round(resultados_adjst[, 1], 2), format = 'f', digits = 2)
or.lb <- formatC(round(resultados_adjst[, 2], 2), format = 'f', digits = 2)
or.ub <- formatC(round(resultados_adjst[, 3], 2), format = 'f', digits = 2)
or.ci <- paste0("", or, " (", or.lb, "-", or.ub, ")")

# Tabela de resultados
model_adjs <- data.frame(resultados_adjst[,0], or.ci = or.ci)
View(model_adjs)
table(dados)


# libraries ---------------------------------------------------------------
library(tidyverse)

#RESEARCH QUESTION
#Relation between trust in institutions, political values and media consumption
#European Survey Dataset with 44000+ respondents


# importing data ----------------------------------------------------------

variables <- read.csv("original_data/original_variables.csv")
variables <- as_tibble(variables)

data <- read.csv("original_data/ESS8e02.1_F1.csv")
data <- as_tibble(data)

#keeping the original for comparison
original <- read.csv("original_data/ESS8e02.1_F1.csv")

View(variables)
dim(variables)
#there are 534 variables

dim(data)
#44387 respondents on the survey

colnames(variables)
#the 9th column refers to the question applied on the survey

colnames(data)


# Selecting the relevant variables ----------------------------------------

#cntry - country of respondent
#Group Media and social trust
#Group politics - variables related to trust
#trstprl, trstlgl, trstplc, trstplt, trstprt, trstep, trstun

#filtering the variables on group media and social trust
news <- filter(variables, Group=="Group Media and social trust")

#filtering the variables on group politics
politics <- filter(variables, Group=="Group Politics")

#vizualizing the variables on the groups
View(politics)
View(news)

colnames(news)
colnames(politics)
#removing every country specific variable

politics <- filter(politics, Country_specific=="no")
politics <- as_tibble(politics)
View(politics)

#in politics I want the questions related to trust and ideological position
#trstprl  trstlgl  trstplc  trstplt  trstprt trstep trstun lrscale
#todas as outras vou excluir

politics <- filter(politics, Name=="trstprl" | Name=="trstlgl" | Name=="trstplc"
                   | Name=="trstplt" | Name=="trstep" | Name =="trstun" | Name =="lrscale")

View(politics)

#in media group, there are only 3 questions related to news consumption:
#nwspol   netusoft netustm
colnames(news)
news$Name

news <- filter(news, Name=="nwspol" | Name =="netusoft" | Name=="netustm")
View(news)


# Cleaning the dataset ----------------------------------------------------

View(data)
#valores na
dim(data[!complete.cases(data),])

#aparentemente tem NA em todas as linhas. Vamos filtrar só as colunas que importam primeiro

#Selecionando apenas as colunas que importam
#-----------------------
colnames(data)
#manter as colunas name, proddate, idno, cntry e as variáveis
#vamos manter também as colunas com informações demográficas
#yrbrn, gndr, agea, edulvlb level of education, eduyrs education years
#select() columns

data <- select(data, idno, cntry, gndr, agea, edulvlb,
               eduyrs, nwspol, netusoft, netustm, trstprl,  trstlgl, trstplc, trstplt, trstprt, trstep, trstun, lrscale,
               yrbrn)
View(data)
colnames(data)
#------------------------------

#Checando os valores na
#---------------------------
dim(data[!complete.cases(data),])

na <-data[!complete.cases(data),]
View(na)
#não há valores na!
remove(na)
#------------------------------


# Saving the cleaned data -------------------------------------------------

#saving only revelant variables
write.csv(news, "working_data/variables_news.csv")
write.csv(politics, "working_data/variables_politics.csv")

#saving data as only the part I am working with
write.csv(data, "working_data/cleaning_data_2.csv") 
#--------------------

#reloarding environment
#-----------------
data <- read.csv("working_data/cleaning_data_2.csv")
news <- read.csv("working_data/variables_news.csv")
politics <- read.csv("working_data/variables_politics.csv")
variables <- read.csv("original_data/original_variables.csv")


colnames(data)
#vizualizando todas a variavés
summary(data)
View(data)
#-------------------------------

#LIMPANDO
#------------------------------------
#a coluna edition é inútil e a name também e a proddate também
#quais os domínios de cada valor? tem muitos valores do tipo 9999, vou ter que ir checando como o PDF ao lado

#idades não podem estar acima de 100 anos
dim(data[data$agea > 100,])
#ano de nascimento não pode ser antes de 1940 nem depois de 2010
dim(data[data$yrbrn<1940,])
dim(data[data$yrbrn > 2010,])
#mais de 18 anos
dim(data[data$agea < 18,])

data2 <- data %>%
  filter(agea < 100) %>%
  filter(agea > 18) %>%
  filter(yrbrn > 1940) %>%
  filter(yrbrn < 2010)

#checar se deu certo
dim(data2[data2$agea < 18,])
dim(data2[data2$agea > 100,])
dim(data2[data2$yrbrn < 1940,])
dim(data2[data2$yrbrn > 2010,])

#genero  - gndr - no arquvio gndr = 9 é no answer
#só valores 1 ou 2
colnames(data2)

data2 <- data2 %>%
  filter(gndr == 2 | gndr == 1)
summary(data2$gndr)

summary(data2)

#edulvlb - remover 5555, 7777, 9999, 8888
data2 <- data2 %>%
  filter(edulvlb < 900)

summary(data2$edulvlb)

#eduyrs range 0 a 54
data2 <- data2 %>% filter(eduyrs < 55)
summary(data2$eduyrs)

#nwspol range 0 a 1428
data2 <- data2 %>% filter(nwspol < 1429)
summary(data2$nwspol)

#netusoft range 1 a 5
data2 <- data2 %>% filter(netusoft < 6)
summary(data2$netusoft)

#netustm range 0 a 1440
data2 <- data2 %>% filter(netustm < 1441)
summary(data2$netustm)

#trstprl range 0 a 10
#trstlgl range 0 a 10
#"trstplc range 0 a 10
#trstplt range 0 a 10
#trstprt range 0 a 10
#trstep range 0 a 10
#trstun range 0 a 10
#lrscale range 0 a 10

data2 <- data2 %>% 
  filter(trstprl < 11) %>%
  filter(trstlgl < 11) %>%
  filter(trstplc < 11) %>%
  filter(trstplt < 11) %>%
  filter(trstprt < 11) %>%
  filter(trstep < 11) %>%
  filter(trstun < 11) %>%
  filter(lrscale < 11)

summary(data2)
#-------------------------------------

#SALVANDO!!!
write.csv(data2, "clean_data.csv")

#######################################################################################
#template_secondary_data_analysis.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License. see full license at the end of this file.
#######################################################################################

#####################################################################################
#SETTING ENVIRONMENT
#####################################################################################
rm(list = ls())
ls()
detach()

library("ggplot2")
library("car")
library("RMySQL")
#####################################################################################
#IMPORTING DATA AND RECODING
#####################################################################################


######### if you are using MYSQL DATA BASE

# 1. Create a Connection

con <- dbConnect(MySQL(), user="tiedu_1", password="saopaulo12", dbname="tiedu_1", host="dbmy0059.whservidor.com")

dbListTables(con)

#Import and export data.frames:

templateData <- dbReadTable(con, "arthroplasty")

dbDisconnect(con)

View(templateData)

str(templateData)
names(templateData)
attach(templateData)


#######################################################################################
# My issues
#######################################################################################

## Criando um subset baseado no filtro do grupo e a primeira linha da internacao 

# There are 2 ways

# 1.Logical 
 # aih<-subset(templateData, (grupo=="Revisado" || grupo =="1CirurgiaAtual" ) & ID_SERVICO ==1)

# 2.Using Clausule 'in'
aih<-subset(templateData, grupo %in% c("Revisado" , "1CirurgiaAtual" ) & id_servico ==1)

qplot(droplevels(aih)$grupo)

aih_faixa<-data.frame(grupo=aih$grupo, faixa=cut(aih$IDADE, breaks=c(1,20,40,60,70,300), labels=c("1-20","20-40","40-60","60-70",">70"), right=TRUE) )

table(aih_faixa)

## Comparando a frequencia por faixa etaria
qplot(faixa, data=aih_faixa, geom="freqpoly", group=grupo, colour=grupo, position="identity")


levels( aih_faixa$grupo )


#---


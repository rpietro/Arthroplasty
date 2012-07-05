#######################################################################################
#template_secondary_data_analysis.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License. see full license at the end of this file.
#######################################################################################
#this script follows a combination of the guidelines proposed by Hadley Wickham http://goo.gl/c04kq as well as using the formatR package http://goo.gl/ri6ky
#if this is the first time you are conducting an analysis using this protocol, please watch http://goo.gl/DajIN while following step by step

#link to manuscript

#####################################################################################
#SETTING ENVIRONMENT
#####################################################################################
#remove all objects and then check
rm(list = ls())
ls()
#dettach all packages
detach()

#command below will install individual and is only run once. remove the hash tag if this is the first time you are running the code on RStudio, and then you can add the hash tag again
#install.packages("car", repos="http://cran.r-project.org")
#install.packages("ggplot2", repos="http://cran.r-project.org")

#command below will install each package. if you run this script from the beginning you need to run every single one again
library("ggplot2")
library("car")
library("RMySQL")
#####################################################################################
#IMPORTING DATA AND RECODING
#####################################################################################

#if you are using a file that is local to your computer, then replace path below with path to the data file in your computer. command will send all the data into the templateData object. replace the word template.data by a name that might easier for you to remember and that represents your data. If you don't know where to get the path to your file, please watch http://goo.gl/i0cPh
<<<<<<< HEAD

# Pietro's machine
#db_dir = "/Users/rpietro/Google Drive/R/nonpublicdata_publications/arthroplasty/"

# Jacson's machine
#db_dir = "/Users/Administrator/Documents/Projects/Data/Arthroplasty/"

#db_name = "arthroplasty.csv"

#templateData <- read.csv( paste(db_dir , db_name , sep ="") , header = TRUE, sep = ";", dec=",")

######### if you are using MYSQL DATA BASE

# 1. Create a Connection

con <- dbConnect(MySQL(), user="tiedu_1", password="saopaulo12", dbname="tiedu_1", host="dbmy0059.whservidor.com")

dbListTables(con)

#Import and export data.frames:

templateData <- dbReadTable(con, "arthroplasty")

dbDisconnect(con)
=======
templateData <- read.csv("/Users/rpietro/Google Drive/R/nonpublicdata_publications/templateData.csv")
>>>>>>> just some additions

#below will view data in a spreadsheet format. notice that in this all subsequent commands you have to replace templateData with whatever name you chose for your data object in the previous command

View(templateData)

#below will list variable names, classes (integer, factor, etc), alternative responses
str(templateData)
#list variable names so that they can be used later
names(templateData)
#below will attach your data so that when you execute a command you don't have to write the name of your data object over and over again
attach(templateData)

#function below is used to recode variables. things to notice: replace old.var with the variable you are recoding, replace new.var with the variable you want to create. the whole recoding happens within " ". all character and factor variables will be within '', numbers will be displayed with digits (not inside '') or NA (also without ''). see video at http://goo.gl/aDgo4 for more details

new.var  <- car::recode(old.var, " 1:2 = 'A'; 3 = 'C'; '' = NA; else = 'B' ")

###########################################################################################
#TABLE 1: DEMOGRAPHICS
###########################################################################################
#describes your entire dataset
describe(templateData)

summary(templateData$IDADE)
qplot(templateData$IDADE)

#t.test, where outcome is a continuous variable and predictor is a dichotomous variable
t.test(outcome~predictor)

#chi square test where both outcome and predictor are categorical variables
CrossTable(outcome, predictor, chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)


########################################################################################
# TABLE WITH MODELS
########################################################################################

logisticmodel1  <- glm(outcome ~ predictor + confounder,family=binomial(link="logit"))
summary(logisticmodel1) #gives you model results
coefficients(logisticmodel1) # model coefficients
confint(logisticmodel1, level=0.95) # CIs for model parameters 
fitted(logisticmodel1) # predicted values
residuals(logisticmodel1) # residuals
influence(logisticmodel1) # regression diagnostics
layout(matrix(c(1,2,3,4),2,2)) # creates the white space for 4 graphs/page 
plot(logisticmodel1) #generates 4 graphs/page

survivalmodel1 <- coxph(Surv(time_to_event, event_yes_no) ~ predictor + confounder1 + confounder2, ties="efron")
#below will test for proportional hazards assumption
prop.assump1 <- cox.zph(survivalmodel1) 
print(prop.assump1) #display results for assumption 
plot(prop.assump1)  #plot curves -- from the help page: "If the proportional hazards assumption is true, beta(t) will be a horizontal line. The printout gives a test for slope=0."
#summary results for the model
summary(survivalmodel1)

#######################################################################################
# My issues
#######################################################################################

## Criando um subset baseado no filtro do grupo e a primeira linha da internacao 

# There are 2 ways

# 1.Logical 
 # aih<-subset(templateData, (grupo=="Revisado" || grupo =="1CirurgiaAtual" ) & ID_SERVICO ==1)

# 2.Using Clausule 'in'
aih<-subset(templateData, grupo %in% c("Revisado" , "1CirurgiaAtual" ) & ID_SERVICO ==1)

qplot(droplevels(aih)$grupo)

aih_faixa<-data.frame(grupo=aih$grupo, faixa=cut(aih$IDADE, breaks=c(1,20,40,60,70,300), labels=c("1-20","20-40","40-60","60-70",">70"), right=TRUE) )

table(aih_faixa)

## Comparando a frequencia por faixa etaria
qplot(faixa, data=aih_faixa, geom="freqpoly", group=grupo, colour=grupo, position="identity")


levels( aih_faixa$grupo )


#---


############################
# Load necessary libraries #
############################
library(readr)
library(ggplot2)




##############################
# Download and data cleaning #
##############################
dados_unsort <- read_csv("healthcare-dataset-stroke-data.csv", na = c("","NA","N/A", "Unknown"))
dados_unsort$id = as.numeric(dados_unsort$id)
dados = dados_unsort[order(dados_unsort$id), ]

#Data structure

dados$bmi = as.numeric(dados$bmi)

unique(dados$gender)
dados$gender = factor(dados$gender, levels = c("Female", "Male", "Other"))
unique(dados$hypertension)
dados$hypertension = factor(dados$hypertension, levels = c(0, 1), labels = c("No", "Yes"))
unique(dados$heart_disease)
dados$heart_disease = factor(dados$heart_disease, levels = c(0, 1), labels = c("No", "Yes"))
unique(dados$ever_married)
dados$ever_married = factor(dados$ever_married, levels = c("No", "Yes"))
unique(dados$work_type)
dados$work_type = factor(dados$work_type, levels = c("Private", "Self-employed", "Govt_job", "children", "Never_worked"))
unique(dados$Residence_type)
dados$Residence_type = factor(dados$Residence_type, levels = c("Urban", "Rural"))
unique(dados$smoking_status)
dados$smoking_status = factor(dados$smoking_status, levels = c("never smoked", "formerly smoked", "smokes"))# Unknown means that the information is unavailable for this patient
unique(dados$stroke)
dados$stroke = factor(dados$stroke, levels = c(0, 1), labels = c("No", "Yes"))


str(dados)


############################
#Exploratory Data Analysis #
############################










########################
#Statistical inference #
########################


########################
#Possible Correlations #
########################



########################################
#Linear and Logistic Regression Models #
########################################









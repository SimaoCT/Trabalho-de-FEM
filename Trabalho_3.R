############################
# Load necessary libraries #
############################
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gt)



##############################
# Download and data cleaning #
##############################
setwd("~/Tomás 2025-26/1º Semestre/Fundamentos de Estatística Médica/Trabalho")
dados_unsort <- read_csv("healthcare-dataset-stroke-data.csv", na = c("","NA","N/A", "Unknown"))
dados_unsort$id = as.numeric(dados_unsort$id)
dados = dados_unsort[order(dados_unsort$id), ]
#at this point, all the data is ordered according to the id number

#Data structure

dados$bmi=as.numeric(dados$bmi)
dados$age=as.integer(dados$age)
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


##Stroke~Gender

ggplot(dados, aes(x=gender, fill=stroke)) +
  geom_bar(position="fill") +
  labs(title="Stroke status across gender", x="Gender", y="Count") +
  theme_classic()+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values=c("red3", "forestgreen"), name="Stroke Status")


# Summary table Stroke~Gender
tbl1 <- dados %>%
  count(stroke, gender) %>%           
  pivot_wider(names_from = gender, values_from = n, values_fill = 0) %>%
  select(stroke, Female, Male, Other) %>%
  mutate(Total = rowSums(across(where(is.numeric)))) %>% 
  bind_rows(
    summarise(.,
              stroke = "Total",
              across(where(is.numeric), sum))
  )

gt(tbl1) %>%
  
  # ---- STROKE ROW GROUP (like the example) ----
tab_row_group(
  group = md("**Stroke**"),    ### <-- Stroke title appears ABOVE "No" and "Yes"
  rows = stroke %in% c("No", "Yes")
) %>%
  
  tab_header(title = " ") %>% 
  
  cols_label(
    stroke = "",                 # keep empty: stroke values appear in stub
    Female = "Female",
    Male   = "Male",
    Other = "Other",
    Total  = "Total"
  ) %>%
  
  tab_spanner(
    label = md("**Gender**"),
    columns = c(Female, Male,Other, Total)
  ) %>%
  
  # ---- ONLY BOLD THE TOTAL COLUMN LABEL ----
  tab_style(
   style = cell_text(weight = "bold"),
   locations = cells_column_labels(columns = Total)
  ) %>%
  
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = stroke,   # só a coluna do stub
      rows = stroke == "Total"
    )
  ) %>%
  
  fmt_number(columns = where(is.numeric), decimals = 0)




##Stroke~Hypertension

ggplot(dados, aes(x=hypertension, fill=stroke)) +
  geom_bar(position="fill") +
  labs(title="Stroke status and hypertension", x="Hypertension", y="Count") +
  theme_classic()+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values=c("red3", "forestgreen"), name="Stroke Status")

# Summary table Stroke~Hypertension

tbl2 <- dados %>%
  count(stroke, hypertension) %>%           
  pivot_wider(names_from = hypertension, values_from = n, values_fill = 0) %>%
  mutate(Total = rowSums(across(where(is.numeric)))) %>% 
  bind_rows(
    summarise(.,
              stroke = "Total",
              across(where(is.numeric), sum))
  )
gt(tbl2) %>%
  tab_row_group(
    group = md("**Hypertension**"),    
    rows = stroke %in% c("No", "Yes")
  ) %>%
  tab_header(title = " ") %>% 
  cols_label(
    stroke = "",
    No = "No",
    Yes = "Yes",
    Total = "Total"
  ) %>%
  tab_spanner(
    label = md("**Hypertension**"),
    columns = c(No, Yes, Total)
  ) %>%
  # BOLD TOTAL COLUMN
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = Total)
  ) %>%
  # BOLD ONLY THE WORD "Total" IN THE STUB COLUMN
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = stroke,
      rows = stroke == "Total"
    )
  ) %>%
  # Aumentar espaçamento entre palavras nos cabeçalhos
  tab_style(
    style = cell_text(word_spacing = px(8)),  # ajusta o valor conforme gostes
    locations = cells_column_labels(columns = everything())
  ) %>%
  # Aumentar espaçamento entre palavras na coluna de rótulos
  tab_style(
    style = cell_text(word_spacing = px(8)),
    locations = cells_body(columns = stroke)
  ) %>%
  fmt_number(columns = where(is.numeric), decimals = 0)

#Stroke~heart_disease
ggplot(dados, aes(x=heart_disease, fill=stroke)) +
  geom_bar(position="fill") +
  labs(title="Stroke status and heart disease", x="Heart Disease", y="Count") +
  theme_classic()+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values=c("red3", "forestgreen"), name="Stroke Status")

# Summary table Stroke~Heart disease
gt(tbl) %>%
  tab_row_group(
    group = md("**Heart Disease**"),    
    rows = stroke %in% c("No", "Yes")
  ) %>%
  tab_header(title = " ") %>% 
  cols_label(
    stroke = "",
    No = "No",
    Yes = "Yes",
    Total = "Total"
  ) %>%
  tab_spanner(
    label = md("**Heart Disease**"),
    columns = c(No, Yes, Total)
  ) %>%
  # BOLD TOTAL COLUMN
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = Total)
  ) %>%
  # BOLD ONLY THE WORD "Total" IN THE STUB COLUMN
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = stroke,
      rows = stroke == "Total"
    )
  ) %>%
  # Aumentar espaçamento entre palavras nos cabeçalhos
  tab_style(
    style = cell_text(word_spacing = px(8)),  # ajusta o valor conforme gostes
    locations = cells_column_labels(columns = everything())
  ) %>%
  # Aumentar espaçamento entre palavras na coluna de rótulos
  tab_style(
    style = cell_text(word_spacing = px(8)),
    locations = cells_body(columns = stroke)
  ) %>%
  fmt_number(columns = where(is.numeric), decimals = 0)

#Stroke~ever married
ggplot(dados, aes(x=ever_married, fill=stroke)) +
  geom_bar(position="fill") +
  labs(title="Stroke status and marriage", x="The patient ever married", y="Count") +
  theme_classic()+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values=c("red3", "forestgreen"), name="Stroke Status")

#Stroke~work type
ggplot(dados, aes(x=work_type, fill=stroke)) +
  geom_bar(position="fill") +
  labs(title="Stroke status and Work Type", x="Work Type", y="Count") +
  theme_classic()+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values=c("red3", "forestgreen"), name="Stroke Status")+
  scale_x_discrete(labels = c("Private", "Self-employed", "Government\nJob", "Children", "Never\nWorked"))


#Stroke~Residence type
ggplot(dados, aes(x=Residence_type, fill=stroke)) +
  geom_bar(position="fill") +
  labs(title="Stroke status and Residence type", x="Residence Type", y="Count") +
  theme_classic()+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values=c("red3", "forestgreen"), name="Stroke Status")

#Stroke~average glucose level
a1=ggplot(dados, aes(x = avg_glucose_level, fill = stroke))+
  geom_histogram(position = "identity", bins = nclass.Sturges(dados$avg_glucose_level))+
  labs(title="Stroke status and Average Glucose Level", x="Average Glucose Level", y="Count") +
  theme_classic()+
  scale_fill_manual(values=c("red3", "forestgreen"), name="Stroke Status")

a2=ggplot(subset(dados,stroke=="Yes"),aes(x=avg_glucose_level))+
  geom_histogram(fill="forestgreen",bins=nclass.Sturges(dados$avg_glucose_level))+
  labs(title="Stroke's distribution across Average Glucose Levels intervals", x="Average Glucose Level", y="Count")+
  theme_classic()

(a1+a2)+plot_layout(nrow=2) #isto permite-nos ver se as distribuições são minimamente semelhantes



#Stroke~

########################
#Statistical inference #
########################
#teste do qui-quadrado (H1: Existe associação entre as variáveis)
table(dados$hypertension, dados$stroke)
chisq.test(dados$hypertension, dados$stroke)
table(dados$gender, dados$stroke)
chisq.test(dados$gender, dados$stroke)#aqui acho que devemos usar o fisher Test
table(dados$heart_disease, dados$stroke)
chisq.test(dados$heart_disease, dados$stroke)
table(dados$ever_married, dados$stroke)
chisq.test(dados$ever_married, dados$stroke)
table(dados$work_type, dados$stroke)
chisq.test(dados$work_type, dados$stroke)#aqui acho que devemos usar o fisher Test
table(dados$Residence_type, dados$stroke)
chisq.test(dados$Residence_type, dados$stroke)
table(dados$smoking_status, dados$stroke)
chisq.test(dados$smoking_status, dados$stroke)

#Testar normalidade
shapiro.test(dados$age[dados$stroke == "Yes"])
shapiro.test(dados$age[dados$stroke == "No"])

shapiro.test(dados$bmi[dados$stroke == "Yes"])
shapiro.test(dados$bmi[dados$stroke == "No"])

shapiro.test(dados$avg_glucose_level[dados$stroke == "Yes"])
shapiro.test(dados$avg_glucose_level[dados$stroke == "No"])
#nao ha nenhuma normalidade--> fazer wilcox

wilcox.test(age ~ stroke, data = dados)
wilcox.test(bmi ~ stroke, data = dados)
wilcox.test(avg_glucose_level ~ stroke, data = dados)
#para todos deu: diferença estatisticamente significativa entre os grupos (com e sem AVC).


modelo = glm(stroke ~ age + bmi + avg_glucose_level +
                hypertension + heart_disease + gender +
                ever_married + work_type + Residence_type +
                smoking_status,
              data = dados, family = binomial)

summary(modelo)
exp(coef(modelo)) #da os OR (oddRatio)
exp(confint(modelo)) #da o IC
#a idade, o Bmi, e a glucose,hipertensão e o heartDisease (no limite) aumenta a probabilidade de AVC.
#no caso da hypertensionYEs, Pessoas com hipertensão têm 1.77 vezes mais odds 
#(quase o dobro) de ter stroke comparado com quem não tem hipertensão, mantendo as outras variáveis constantes.


########################
#Possible Correlations #
########################



########################################
#Linear and Logistic Regression Models #
########################################










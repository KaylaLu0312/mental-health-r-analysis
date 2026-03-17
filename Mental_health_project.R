rm(list = ls())
rm(data_1)
rm(data)
data$age_group <- NULL
#=====================================
#Set working Directory $ Read Data
#=====================================
getwd()
setwd("/Users/kaylalu/Desktop/R project")

library(tidyverse)
library(ggplot2)
data <- read_csv("mental_health_dataset.csv")

#======================================
#Inspect data
#======================================
head(data)
str(data)
summary(data)
colSums(is.na(data))

#=====================================
#Convert daat types
#======================================
data$gender <- as.factor(data$gender)
data$employment_status <- as.factor(data$employment_status)
data$work_environment <- as.factor(data$work_environment)
data$mental_health_history <- as.factor(data$mental_health_history)
data$seeks_treatment <- as.factor(data$seeks_treatment)
data$mental_health_risk <- factor(
  data$mental_health_risk,
  levels = c("Low", "Medium", "High")
)

str(data)

#====================================
#Descriptive Statistics
#====================================
numeric_var <- data %>%
  select(where(is.numeric))
summary (numeric_var)

cat_vars <- data %>% 
  select(where(is.factor))
summary (cat_vars)


#=====================================
#Visualisations
#=====================================

#=============Sample Overview=============#
#Age
ggplot(data, aes(x = age)) +
  geom_histogram(binwidth = 5, fill="skyblue", color="white") +
  labs (title="Age Distribusion",
        x= "Age", y = "Count") +
  theme_minimal()

#Gender
ggplot(data, aes(x=gender)) +
         geom_bar() +
         theme_minimal() +
         labs (title="Gender")

#=============Core Analysis================#
#Define custom colors 
risk_colors <- c(
  "Low" = "lightgreen",
  "Medium" = "khaki",
  "High" = "lightcoral"
)
scale_fill_manual(values = risk_colors)

#1. Mental health Risk Distribution
ggplot(data, aes(x = mental_health_risk, fill= mental_health_risk)) + 
  geom_bar() +
  scale_fill_manual(values = risk_colors) +   
  geom_text(stat = "count",
    aes(label = ..count..),
    position = position_stack(vjust = 0.5)) +
  labs(title = "Distribution of Mental Health Risk",
    x = "Mental helath Risk", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none") 

#2. Gender vs Mental Health Risk
Gendaer_mental_porportion <- data %>%
  count(mental_health_risk, gender) %>%
  group_by(gender) %>%
  mutate(prop = n / sum(n))

ggplot(Gendaer_mental_porportion, aes(x = gender, y = prop, fill = mental_health_risk)) +
  geom_col() +
  geom_text(aes(label = scales::percent(prop)),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = risk_colors) +  
  labs(title = "Gender vs Mental Health Risk",
       x = "Gender", y="Percent") +
  theme_minimal()


##Mental Health Measures##
#3. Depression vs Mental Health Risk
ggplot(data, aes(x = mental_health_risk, y = depression_score, fill = mental_health_risk)) +
  geom_boxplot() +
  scale_fill_manual(values = risk_colors) +  
  labs(title = "Depresion score vs Mental Helath Risk",
       x = "Mental Health Risk", y = "Depresion Score") +
  theme_minimal() +
  theme(legend.position = "none") 

#4. Anxiety vs Mental Health Risk
ggplot(data, aes(x = mental_health_risk, y= anxiety_score, fill = mental_health_risk)) +
  geom_boxplot() +
  scale_fill_manual(values = risk_colors) +  
  labs(title = "Anxiety score vs Mental Health Risk",
       x = "Mental Health Risk", y ="Anxiety Score") +
  theme_minimal() +
  theme(legend.position = "none") 

#5. Seek treatment vs Mental health Risk
Seek_treatment_mental_porportion <- mental_data %>%
  count(seeks_treatment, mental_health_risk) %>%
  group_by(mental_health_risk) %>%
  mutate(prop = n / sum(n))

ggplot(Seek_treatment_mental_porportion, aes(x = mental_health_risk, y = prop, fill = seeks_treatment)) +
  geom_col() +
  geom_text(aes(label = scales::percent(prop)),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_manual(values = c("No" = "lightgreen", "Yes" = "lightcoral")) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Percantage of Seeking Treatment by Mental Health Risk",
       x = "Mental Health History", y = "Percentage", fill = "Seek Treatment") +
  theme_minimal()

#6. Mental Health History vs Mental health Risk
History_proportion <- mental_data %>%
  count(mental_health_history, mental_health_risk) %>%
  group_by(mental_health_risk) %>%
  mutate (prop = n / sum(n))

ggplot(History_proportion, aes(x = mental_health_risk, y = prop, fill = mental_health_history)) +
  geom_col() +
  geom_text(aes(label = scales::percent(prop)),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_manual(values = c("No" = "lightgreen", "Yes" = "lightcoral")) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Percentage of Mental Health History by Mental health Risk",
       x  ="Mental Helath Risk", y = "Percentage" , fill = "Mental Health History") +
  theme_minimal()


##Health and Lifestyle Factors##
#7. Sleep vs Mental Health Risk
ggplot(data, aes(x = mental_health_risk, y = sleep_hours, fill = mental_health_risk)) +
  geom_boxplot() +
  scale_fill_manual(values = risk_colors) +
  labs(title = "Sleep hours vs Mental Health Risk",
       x = "Mental Health Risk", y = "Sleep Hours") +
  theme_minimal() +
  theme(legend.position = "none")

#8. Stress Level
ggplot(data, aes(x = mental_health_risk, y = stress_level, fill = mental_health_risk)) +
  geom_boxplot() +
  scale_fill_manual(values = risk_colors) +  
  labs(title = "Stress Level vs Mental Health Risk",
       x = "Mental Health Risk", y = "Stress Level") +
  theme_minimal() +
  theme(legend.position = "none")

#9. Social Support vs Mental Health Risk
ggplot(data, aes(x = mental_health_risk, y = social_support_score, fill = mental_health_risk)) +
  geom_boxplot() +
  scale_fill_manual(values = risk_colors) +
  labs(title = "Social Support Score vs Mental Health Risk",
       x = "Mental Health Risk", y ="Social Support Score") +
  theme_minimal() +
  theme(legend.position = "none")


###Work-Related Factors##
#10. Employment Status vs Menatl Health Risk
Employment_mental_propotion <- mental_data %>%
  count(employment_status, mental_health_risk) %>%
  group_by(mental_health_risk) %>%
  mutate(prop = n / sum(n))

ggplot(Employment_mental_propotion, aes(x = mental_health_risk, y= prop, fill = employment_status)) +
  geom_col() +
  geom_text(aes(label = scales::percent(prop)),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Percentage of Emploment Status by Mental Health Risk",
       x = "Mental Health Risk", y = "Percentage") +
  theme_minimal() 

#11. Work Environment vs Mental Health Risk
Environment_mental_proportion <- mental_data %>%
  count(work_environment, mental_health_risk) %>%
  group_by(mental_health_risk) %>%
  mutate(prop = n / sum(n))

ggplot(Environment_mental_proportion, aes(x = mental_health_risk, y = prop, fill= work_environment))+
  geom_col() +
  geom_text(aes(label = scales::percent(prop)),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Percentage of Work Environment by Mental Health Risk",
       x = "Mental Health Risk", y = "Percentage") +
  theme_minimal()

#12. Productivity vs Mental Health Risk
ggplot(data, aes(x = mental_health_risk, y = productivity_score, fill = mental_health_risk)) +
  geom_boxplot() +
  scale_fill_manual(values = risk_colors) +
  labs(title = "Productivity Score vs Mental Health Risk",
       x = "Mental Health Risk", y ="Productivity Score") +
  theme_minimal() +
  theme(legend.position = "none")







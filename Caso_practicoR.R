library(tidyverse)

caso1 <- read_csv("Titanicv2.csv")
View(caso1)



#Calcular el numero total de pasajeros
str(caso1)
summary(caso1)

#desglozarlos por sexo y edad

Sex_masc <- caso1 %>% 
  filter(Sex == "male")

Sex_fem <- caso1 %>% 
  filter(Sex == "female")

print(Sex_masc)
print(Sex_fem)

Sex_masc2 <- caso1 %>% 
  filter(Sex == "male") %>% 
  summarise(total= n())

print(Sex_masc2)

Sex_fem2 <- caso1 %>% 
  filter(Sex == "female") %>% 
  summarise(total= n())

print(Sex_fem2)

# Análisis por edad

analisis_edad <- caso1 %>%
  summarise(
    conteo = n(),                            
    edad_promedio = mean(Age, na.rm = TRUE),
    edad_maxima = max(Age, na.rm = TRUE),    
    edad_minima = min(Age, na.rm = TRUE),    
  )

print(analisis_edad)

#Cuantos sujetos hay por rango de edad
rango_minimo1 <- 0
rango_maximo1 <- 10

conteo_rango <- caso1 %>%
  filter(Age >= rango_minimo1 & Age <= rango_maximo1) %>%
  summarise(conteo = n())

print(conteo_rango)


rango_minimo2 <- 11
rango_maximo2 <- 20

conteo_rango2 <- caso1 %>%
  filter(Age >= rango_minimo2 & Age <= rango_maximo2) %>%
  summarise(conteo = n())

print(conteo_rango2)


rango_minimo2 <- 21
rango_maximo2 <- 35

conteo_rango2 <- caso1 %>%
  filter(Age >= rango_minimo2 & Age <= rango_maximo2) %>%
  summarise(conteo = n())

print(conteo_rango2)

rango_minimo2 <- 36
rango_maximo2 <- 45

conteo_rango2 <- caso1 %>%
  filter(Age >= rango_minimo2 & Age <= rango_maximo2) %>%
  summarise(conteo = n())

print(conteo_rango2)

rango_minimo2 <- 46
rango_maximo2 <- 77

conteo_rango2 <- caso1 %>%
  filter(Age >= rango_minimo2 & Age <= rango_maximo2) %>%
  summarise(conteo = n())

print(conteo_rango2)


# Contar el número de sobrevivientes
conteo_supervivientes <- caso1 %>%
  filter(Survived == "Yes") %>%    
  summarise(conteo = n())          

print(conteo_supervivientes)

sobrevivientes<- caso1%>%
  filter(Survived == "Yes") %>% 
  summarise(edad_promedio = mean(Age, na.rm = TRUE))

print(sobrevivientes)

#calcular cuangtos eran hombres y mujeres
sobrevivientes_sex<- caso1%>%
  filter(Survived == "Yes") %>% 
  group_by(Sex) %>%                      
  summarise(conteo = n())

print (sobrevivientes_sex)

#de que clase erab las sobrevivientes?
sobrevivientes_class<- caso1%>%
  filter(Survived == "Yes") %>% 
  group_by(Pclass) %>%                      
  summarise(conteo = n())

print (sobrevivientes_class)

#Ganancia de los tickets

Ganancia <- caso1%>%
  summarise(total = sum(Fare, na.rm = TRUE))

print(Ganancia)

#De que puerto salieron la mayor cantidad de pasajeros:

Puerto <- caso1 %>%
group_by(Embarked) %>%
summarise(conteo = n() , .groups = 'drop')

print(Puerto)

sobrevivientes_port<- caso1%>%
  filter(Survived == "Yes") %>% 
  group_by(Embarked) %>%                      
  summarise(conteo = n())

print(sobrevivientes_port)

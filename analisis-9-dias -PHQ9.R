#Análisis de 9 días de encuestas a pacientes para determinar síntomas 
#de depresión utilizando el cuestionario PHQ9
#dataset:Dataset_14-day_AA_depression_symptoms_mood_and_PHQ9.csv
#descargado de:https://www.kaggle.com/datasets/thedevastator/phq-9-depression-assessment
#Autores del datastet: Sebastian Burchert; André Kerber; Johannes Zimmermann; Christine Knaevelsrud
#Autor del presente análisis: Nora Leticia Cuevas-Cuevas

library(ggplot2)
library(dplyr)


#se carga el archivo con los datos y se coloca en un dataframe llamado df
df<-read.csv("/Users/noracuevas/Desktop/Proyecto_9dias_phq9/Dataset_9-day_depression_symptoms_mood_and_PHQ-9.csv")
View(df)

#Construimos 3 datasets para visualizar: uno para depresión
#otro para felicidad y un tercero con ambas

#comenzamos con summarize, por paciente, con el promedio de sus scores
#usamos %>% para concatenar instrucciones
#se guarda en el dataframe: df_mtc
df_mtc<-df%>%group_by(user_id)%>%summarise(promediod=mean(Prom_PHQ9), medianad=median(Prom_PHQ9),minimod=min(Prom_PHQ9),maximod=max(Prom_PHQ9))
View(df_mtc)

#realizamos lo mismo pero ahora con la variable score de felicidad
df_hs<-df%>%group_by(user_id)%>%summarise(promedioh=mean(happiness.score), medianah=median(happiness.score),minimoh=min(happiness.score),maximoh=max(happiness.score))
View(df_hs)

#unimos los dos dataframes
df_todo<-merge(x=df_mtc,y=df_hs,by=c("user_id"))
View(df_todo)

#algunos indicadores para conocer la población encuestada

#edad
promedio_edad<-df%>%summarise(promedioe=mean(age,na.rm=TRUE))
min_edad<-df%>%summarise(minimo=min(age,na.rm=TRUE))
max_edad<-df%>%summarise(maximo=max(age,na.rm=TRUE))
promedio_edad
min_edad
max_edad
#Sexo
porgen<-df%>%group_by(user_id,sex)%>%summarize(distinct_points = n_distinct(sex))
por_sexo<-porgen%>%group_by(sex) %>%summarize(distinct_points2 = sum(distinct_points))
por_sexo

#graficamos por sexo
ggplot(porgen,aes(sex))+geom_bar(color="green", fill=rgb(0.1,0.4,0.5,0.7))

#graficamos los promedios de depresión y los promedios de felicidad
ggplot(data=grupos, aes(x=sex, y=Prom_PHQ9, fill=Prom_PHQ9))+geom_boxplot(notch=TRUE)+stat_summary(fun.y=mean, geom="point", fill="red", shape=21, size=2.5)+ylab("Depression")+facet_wrap(~sex)
ggplot(data=grupos, aes(x=sex, y=happiness.score, fill=happiness.score))+geom_boxplot(notch=TRUE)+stat_summary(fun.y=mean, geom="point", fill="blue", shape=21, size=2.5)+ylab("Happiness")+facet_wrap(~sex)


#graficamos depression y happiness by sex
ggplot(data=grupos)+
  geom_point(aes(x=Prom_PHQ9, y=happiness.score,colour=sex,shape=sex))+
  facet_wrap(~sex)

#se calcula la correlación de los promedios de depresión y felicidad, 
#pero ahora agrupados por paciente, los cuales
#se encuentra en el datast df_todo
cor(df_todo$promediod, df_todo$promedioh) 

#se gráfica la relación entre los promedios de 
#depresión y felicidad. Notando una tendencia lineal negativa

ggplot(data=df_todo,aes(x=promediod, y=promedioh))+
  geom_point(color="blue")+
  geom_smooth(method=lm , color="red", se=FALSE)



### Práctico 4 Felipe Silva 02/04

#Librerías
pacman::p_load(sjlabelled, dplyr, stargazer, sjmisc, summarytools, kableExtra, sjPlot, corrplot, sessioninfo, ggplot2)

#Vamos a llamar la base de datos directo del sitio web:
load(url("https://github.com/Kevin-carrasco/R-data-analisis/raw/main/files/data/latinobarometro_total.RData"))
#Pero en otro caso, deberiamos haber abierto nuestra propia base de datos.

names(proc_data)
dim(proc_data)

#Tabla descriptiva de variables para sección metodológica

#Tabla stargazer
stargazer(proc_data, type = "text")

#Tabla descriptiva con descr
sjmisc::descr(proc_data)

sjmisc::descr(proc_data,
              show = c("label", "range", "mean", "sd", "NA.prc", "n"))%>%
              kable(.,"markdown")
summarytools::dfSummary(proc_data, plain.ascii = FALSE)
view(dfSummary(proc_data, headings = FALSE))

#Vamos a eliminar los casos perdidos, pero vamos a guardar la base original
proc_data_original <-proc_data
dim(proc_data_original)
#Contamos el n° de datos perdidos
sum(is.na(proc_data))
# 4005 casos perdidos de 20204
#Ahora borramos los casos perdidos
proc_data <-na.omit(proc_data)
dim(proc_data)
#Ahora tenemos un total de 18765 casos y 9 variables.
#Durante este proceso, se borran las etiquetas de nuestras categorías, por ello, las vamos a recuperar
proc_data <-sjlabelled::copy_labels(proc_data, proc_data_original)

#3.2 visualización de las variables
ggplot()
ggplot(proc_data, aes(x = conf_inst))
proc_data %>% ggplot(aes(x = conf_inst)) +
  geom_bar()
proc_data %>% ggplot(aes(x = conf_inst)) +
  geom_bar(fill = "coral")
#Con este ultimo codigo cambiamos el color
proc_data %>% ggplot(aes(x = conf_inst)) +
  geom_bar(fill = "green")+
  labs(title = "confianza en instituciones",
       x = "confianza en instituciones",
       y = "frecuencia")
#Creamos el gráfico 1
graph1 <- proc_data %>% ggplot(aes(x = conf_inst))+
  geom_bar(fill = "green")+
  labs(title = "Confianza en instituciones",
       x = "Confianza en instituciones",
       y = "Frecuencia")+
  theme_bw()
#theme_bw() lo que hace es que la de una "forma" al gráfico, le da lineas en el fondo, bordes, etc.

#Ahora lo guardamos como imágen.
ggsave(graph1, file="C:/Users/Alumno/Documents/GitHub/Practico3/output/graph1.png")
graph1

## 3.1 Exploración de asociación entre variables

#Tables de contigencia para variables categóricas
sjt.xtab(proc_data$educacion, proc_data$sexo)
sjt.xtab(proc_data$educacion, proc_data$sexo,
         show.col.prc=TRUE,
         show.summary=TRUE,
         encoding = "UTF-8"
         )
#Tablas de promedio de una variable continua por una categóricas
tapply(proc_data$conf_inst, proc_data$educacion, mean)

proc_data %>% #Se especifica la base de datos
  select(conf_inst, educacion) %>% #se seleccionan las variables
  dplyr::group_by(Educación=sjlabelled::as_label(educacion)) %>% #se agrupan por la variable categórica y se usan etiquetas con as_label
  dplyr::summarise(Obs.=n(), Promedio=mean(conf_inst), SD=sd(conf_inst)) %>% #se agregan las operaciones a presentar en la tabla
  kable(, format = "markdown")
#Cuando utilizamos la función "kable" solamente se ve cuando estamos usando Quarto.

graph <- ggplot(proc_data, aes(x =educacion, y = conf_inst)) +
  geom_boxplot() +
  labs(x = "Educación", y = "Confianza en instituciones") +
  theme_minimal()
graph
#Guardamos el codigo de cajas
ggsave(graph, file="C:/Users/Alumno/Documents/GitHub/Practico3/output/graph.png")

#Bastante feo el gráfico la verdad asi que hacemos otro
ggplot(proc_data, aes(x =educacion, y = conf_inst)) +
  geom_point() +
  labs(x = "Educación", y = "Confianza en instituciones") +
  theme_minimal()
#Pero por las características de la tabla tampoco nos sirve mucho, asi que usaremos en este caso los promedios

#haremos un objeto nuevo en función de esto
datos <- proc_data %>% group_by(educacion) %>%
  summarise(promedio = mean(conf_inst))

#Ahora si hacemos el gráfico
ggplot(datos, aes(x =educacion, y =promedio))+
  geom_point()+
  labs(x = "Educación", y = "Confianza en instituciones")+
  theme_minimal()+
  ylim(0,12)
# La info es más precisa en este caso pero como los promedios son similares no tiene tanto sentido
proc_data$idenpa <- factor(proc_data$idenpa,
                           labels=c("Argentina",
                                    "Bolivia",
                                    "Brasil",
                                    "Chile",
                                    "Colombia",
                                    "Costa Rica",
                                    "Cuba",
                                    "República Dominicana",
                                    "Ecuador",
                                    "El Salvador",
                                    "Guatemala",
                                    "Honduras",
                                    "México",
                                    "Nicaragua",
                                    "Panamá",
                                    "Paraguay",
                                    "Uruguay",
                                    "Venezuela"),
                           levels=c("32",
                                    "68",
                                    "76",
                                    "152",
                                    "170",
                                    "188",
                                    "214",
                                    "218",
                                    "222",
                                    "320",
                                    "340",
                                    "484",
                                    "558",
                                    "591",
                                    "600",
                                    "604",
                                    "858",
                                    "862"))
graph_box <- ggplot(proc_data, aes(x =idenpa, y = conf_inst))+
  geom_boxplot()+
  labs(x = "País", y = "Confianza en instituciones") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



library(tidyverse)
library(cluster)
library(factoextra)
library(janitor)
library(ggdendro)
library(tidyverse)
library(datasets)

getwd()

setwd("D:/Universidad/Minería de datos 2/Proyecto2")


load("D:/Universidad/Minería de datos 2/Proyecto2/beats.RData")

data= beats[,c(1,8,9, 10, 11, 12, 13, 14, 15, 16, 17, 18,19, 23, 25,27)]


summary(data)
head(data)




# Para las observaciones que tengan datos faltantes, le asignamos el valor NA para eliminarlos en el siguiente paso
data[data == ""] <- NA




# Verificamos donde hay valores NAs
data %>% 
  summarise_all(funs(sum(is.na(.))))

# De existir eliminamos todas las observaciones que presenten estos datos
data_pre <- data %>% filter(!(is.na(duration_ms)|is.na(track_name)))

# Corroboramos que no queden datos NA
data_pre %>%  summarise_all(funs(sum(is.na(.))))

#filtro y saco los datos duplicados

data_pre <- data_pre[!duplicated(data_pre$track_id),]


# Ahora corroboraremos si existen canciones que esten duplicadas
data_pre %>% count(duplicated(data_pre$track_name))


# Como existen canciones repetidas realizamos la consulta para obtener los valores distintos, pero este hecho obvia que hayan canciones con el mismo nombre pero de distinto autos  
data_pre %>% distinct(track_name, .keep_all = TRUE, )

# Por lo que creamos una variables que almacene si existe duplicidad en la cancion y/o en el artista
data_pre$duplicate <- duplicated(data_pre[,c("track_name","artist_name")])

# Generamos un sub data frame que almacenara solo los valores que haya obtenido el valor TRUE a la consulta anterior y los ordenamos por energía
data_dupli <- data_pre %>% 
  filter(data_pre$duplicate == TRUE) %>% 
  arrange("track_name", "energy", desc(energy))



# Seleciono las filas que sean distintas, borro todas las canciones que se repiten y me quedo con la mayor energía
data_dupli <- data_dupli %>%  distinct(track_name, artist_name, .keep_all = TRUE)

# Elimino de mi data pre procesada los datos que dieron positivo a la duplicidad, para que al momento de re insertar los datos sobrevivieron a la limpieza de duplicidad no se genere la duplicidad que se estaba evitando
data_pre <- data_pre[!(data_pre$duplicate == TRUE),]

# Junto la data pre procesada con los datos que sobrevivieron a la limpieza de duplicidad
data_pre <- rbind(data_pre, data_dupli)

# Elimino la columna que me indicaba duplicidad ya que no sera util mas adelante
data_pre$duplicate <- NULL





#######################################################################################################################

# Como generamos nuevos valores NA dentro de nuestra BBDD, debemos volver a ejecutar el paso uno de la limpieza de datos


# Verificamos si aún quedan valores NAs
data %>% 
  summarise_all(funs(sum(is.na(.))))

#como no quedan pasamos al siguiente paso

# Ahora corroboraremos si existen canciones que esten duplicadas
data_pre %>% count(duplicated(data_pre$track_name))

# Como existen canciones repetidas realizamos la consulta para obtener los valores distintos, pero este hecho obvia que hayan canciones con el mismo nombre pero de distinto autos  
data_pre %>% distinct(track_name, .keep_all = TRUE, )

# Por lo que creamos una variables que almacene si existe duplicidad en la cacion y/o en el artista
data_pre$duplicate <- duplicated(data_pre[,c("track_name", "artist_name")])



# Seleciono las filas que sean distintas, borro todas las canciones que se repiten y me quedo con la mayor track popularity
data_dupli <- data_dupli %>% 
  distinct(track_name, artist_name, .keep_all = TRUE)

# Elimino de mi data pre procesada los datos que dieron positivo a la duplicidad, para que al momento de re insertar los datos sobrevivieron a la limpieza de duplicidad no se genere la duplicidad que se estaba evitando
data_pre <- data_pre[!(data_pre$duplicate == TRUE),]

# Junto la data pre procesada con los datos que sobrevivieron a la limpieza de duplicidad
data_pre <- rbind(data_pre, data_dupli)

# Elimino la columna que me indicaba duplicidad ya que no sera util mas adelante
data_pre$duplicate <- NULL


set.seed(123)
n=nrow(data_pre)
L=round(0.01*n)

filas=sample(1:n, size = L, replace = F)

data_final=data_pre[filas,]

# transformacion de milisegundos a minutos
data_final <- data_final %>% mutate(duration_min = data_final$duration_ms/60000)

# Character
data_char <- c("artist_name", "track_id", "track_href", "track_name")

# Double
data_dou <- c("danceability", "energy", "key", "loudness", "mode", "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "tempo", "duration_ms", "duration_min")





summary(data_final)

str(data_final)

#separo los datos


datanum <- data_final %>% 
  select(data_dou)

datachar <- data_final %>% 
  select(data_char)

# escalo los datos


data_sca <- sapply(datanum, scale)

#min_max_norm <- function(x) {
#    return((x - mean(x))/(max(x) - min(x)))    
#  }

#div_norm <- function(y) {
#    y/100
#  }

#des_norm <- function(z) {
#    return((z+min(z))*(max(z) - min(z)))
#  }

#data_scalmin <- min_max_norm(datanum)




############################## aplico metodo jerarquico


d = dist(data_sca, method = "euclidean")

#Distancia Manhattan
d1 = dist(data_sca, method = "manhattan")

#Distancia Minkowski
d2 = dist(data_sca, method = "minkowski")

hist(d, main = "Histograma Distancia Euclideana")

hist(d1, main = "Histograma Distancia Manhattan")
hist(d2, main = "Histograma Distancia Minkowski")


##complete model
set.seed(123)

model_complete <- hclust(d, method = "complete")

summary(model_complete)

## ward model
set.seed(123)

model_ward <- hclust(d, method = "ward.D")

summary(model_ward)

##comparacion entre modelos

models <- c("complete", "ward")
names(models) <- c("complete", "ward")

agcoef <- function(x) {
  agnes(data_sca, method = x)$ac
}

##dendrograma

ggdendrogram(model_complete, rotate = FALSE, theme_dendro = TRUE) 
plot(model_complete)
rect.hclust(model_complete, k=7, border= "yellow")




# Determinamos un valor para h lo que nos entregara un valor distinto de k para cada h que escogamos, tambien podemos definir el k desde un inicio
groups <- cutree(model_complete, h = 7)

# Se imprimen los tamaños de cada cluster
table(groups)

# Generamos una nueva columna para almacenar a que cluster pertenece cada observacion (tanto en data_pre y datanum)
data_final$clust <- as.factor(groups)
datanum$clust <- as.factor(groups)

# Graficamos las observaciones agrupadas por su cluster
fviz_cluster(list(data = data_sca, cluster = groups))



#caracteristicas de los clusters


datanum$clust <- as.numeric(as.character(datanum$clust))


##PROMEDIOS 
# Generamos una tabla que almacenara los valores PROMEDIOS para cada uno de los clusters encontrados lo que nos permitira caracterizar a cada uno de ellos

infoclusters <- aggregate(datanum, by=list(cluster=datanum$clust), mean)

# Borramos la columna clust ya que se repite esa informacion en la tabla generada
infoclusters$clust <- NULL

# Borramos la columna de la duracion en milisegundoss
infoclusters$duration_ms <- NULL

infoclusters

infoclters2 <- aggregate (datanum, bylist(cluster=datanum$clust), sum(datanum$duration_min))


## ordeno la base de datos 

data_ordenada= datanum %>% arrange(clust, desc(energy))

data_ordenada

playlist = data_ordenada %>% group_by(clust) %>%
            summarise( duracion_cada_cluster= sum(duration_min)) %>%
            arrange(desc(duracion_cada_cluster))

playlist


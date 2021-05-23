######### k-means-clustering als numerisches Feature ###########
library(tidyverse)
# daten 
circulo <- function(x, R, centroX=0, centroY=0){
  r = R*sqrt(runif(x))
  theta = runif(x)*2*pi
  x = centroX + r*cos(theta)
  y = centroY + r*sin(theta)
  
  z = data.frame(x=x, y=y)
  return(z)
}

set.seed(123)

# We create points around three different points.
datos_1 <- circulo(x = 20, R = 10, centroX = 5, centroY = 30)
datos_2 <- circulo(x = 20, R = 10, centroX = 20, centroY = 10)
datos_3 <- circulo(x = 20, R = 10, centroX = 50, centroY = 50)


datos = data.frame(
  rbind(datos_1,datos_2, datos_3),
  numero_punto = 1:60
)

rm(datos_1,datos_2,datos_3, circulo)

# distanzmessung zw datenpunkten und darstellung
crear_centroides <- function(n_centroides=3, datos=datos, columns_skip){
  datos_limpios = datos[ , -which(names(datos) %in% columns_skip)]
  columnas <- c(letters[24:26],letters[1:23])
  
  x = matrix(ncol = ncol(datos_limpios) +1, nrow = n_centroides)
  x[1:n_centroides,1:ncol(datos_limpios)] = replicate(ncol(datos_limpios), runif(n_centroides,min(datos_limpios), max(datos_limpios))) 
  x[,ncol(datos_limpios) +1] = 1:n_centroides
  x = data.frame(x, stringsAsFactors = FALSE)
  colnames(x) <- c(columnas[1:ncol(x)-1], "n_centroide")
  return(x)
}

centroides = crear_centroides(n_centroides=3, datos=datos, columns_skip="numero_punto")

ggplot() +
  geom_text(aes(x, y, label=numero_punto), data=datos) + 
  geom_point(aes(x,y), col = "blue",alpha = 0.2, data = datos) + 
  geom_point(aes(x,y), data = centroides, size = 10, shape=21) +
  geom_text(aes(x,y, label = n_centroide), data = centroides, col = "black") +
  
  geom_segment(aes(x = datos[1,"x"], y =  datos[1,"y"],
                   xend =   centroides[1,"x"], yend =  centroides[1,"y"]), linetype = "dashed") +
  
  geom_segment(aes(x = datos[1,"x"], y =  datos[1,"y"],
                   xend =   centroides[2,"x"], yend =  centroides[2,"y"]), linetype = "dashed") +
  geom_segment(aes(x = datos[1,"x"], y =  datos[1,"y"],
                   xend =   centroides[3,"x"], yend =  centroides[3,"y"]), linetype = "dashed") +
  
  theme_minimal() +
  labs(title = "Error del punto 1 respecto a los 3 centroides")

# distanzen
# Distance 1
(centroides[1,1] - datos[1,1])^2 + (centroides[1,2] - datos[1,2])^2 
(centroides[2,1] - datos[2,1])^2 + (centroides[2,2] - datos[2,2])^2  
(centroides[3,1] - datos[3,1])^2 + (centroides[3,2] - datos[3,2])^2 

prueba <- c(
  (centroides[1,1] - datos[1,1])^2 + (centroides[1,2] - datos[1,2])^2,
  (centroides[2,1] - datos[2,1])^2 + (centroides[2,2] - datos[2,2])^2,
  (centroides[3,1] - datos[3,1])^2 + (centroides[3,2] - datos[3,2])^2  
)

which(prueba == min(prueba))

datos$error = NA
datos$cluster = NA

for(posicion in 1:length(datos$x)){
  x <- c(
    (centroides[1,1] - datos[posicion,1])^2 + (centroides[1,2] - datos[posicion,2])^2,  
    (centroides[2,1] - datos[posicion,1])^2 + (centroides[2,2] - datos[posicion,2])^2, 
    (centroides[3,1] - datos[posicion,1])^2 + (centroides[3,2] - datos[posicion,2])^2  
  )
  
  datos$error[posicion] = min(x)
  datos$cluster[posicion] = which(x == min(x))
  
}
error = sum(datos$error)

rm(x, posicion)

# plotten
ggplot() + 
  geom_point(aes(x,y, col = as.factor(cluster), size = 7), data = datos) + 
  geom_point(aes(x,y), data = centroides, size = 10, shape=21) +
  geom_text(aes(x,y, label = n_centroide), data = centroides, col = "black") +
  theme_minimal() + theme(legend.position = "bottom") + guides(size = FALSE) 

# klassifikation der k-means-cluster
centroides <- datos %>%
  group_by(cluster) %>%
  summarize(
    x = mean(x), 
    y = mean(y)
  ) %>%
  mutate(n_centroide = cluster) %>%
  select(-cluster) %>%
  ungroup() %>%
  as.data.frame(.)


ggplot() + 
  geom_point(aes(x,y, col = as.factor(cluster), size = 7), data = datos) + 
  geom_point(aes(x,y), data = centroides, size = 10, shape=21) +
  geom_text(aes(x,y, label = n_centroide), data = centroides, col = "black") +
  theme_minimal() + theme(legend.position = "bottom") + guides(size = FALSE) 

# iterative verbesserung der cluster-findung
# We create an initial error to calculate the total error difference in the second iteration
error = c(0,error)

i = 2
while(round(error[i],2) != round(error[i-1],2) ) {
  
  # We calculate the distance
  for(posicion in 1:length(datos$x)) {
    x <- unlist( c(
      (centroides[1,1] - datos[posicion,1])^2 + (centroides[1,2] - datos[posicion,2])^2,  
      (centroides[2,1] - datos[posicion,1])^2 + (centroides[2,2] - datos[posicion,2])^2, 
      (centroides[3,1] - datos[posicion,1])^2 + (centroides[3,2] - datos[posicion,2])^2  
    ))
    
    # We updrade the error and the cluster
    datos$error[posicion] = min(x)
    datos$cluster[posicion] = which(x == min(x))
  }
  
  # We append the total erro to the vector of errors
  error = c(error, sum(datos$error))
  
  centroides <- datos %>%
    group_by(cluster) %>%
    summarize(
      x = mean(x), 
      y = mean(y)
    ) %>%
    mutate(n_centroide = cluster) %>%
    select(-cluster) %>%
    ungroup() %>%
    as.data.frame(.)
  
  
  i = i + 1
  
}
rm(i,x, posicion, error)

# plotten
ggplot() + 
  geom_point(aes(x,y, col = as.factor(cluster), size = 7), data = datos) + 
  geom_point(aes(x,y), data = centroides, size = 10, shape=21) +
  geom_text(aes(x,y, label = n_centroide), data = centroides, col = "black") +
  theme_minimal() + theme(legend.position = "bottom") + guides(size = FALSE) 

# standardisierung
# Standardization of X
datos$x <- datos$x - mean(datos$x) 
datos$x <- datos$x / sd(datos$x)

# Standardization of y
datos$y <- datos$y - mean(datos$y) 
datos$y <- datos$y / sd(datos$y)

# plotten
ggplot() + 
  geom_point(aes(x,y, col = as.factor(cluster), size = 7), data = datos) + 
  theme_minimal() + theme(legend.position = "bottom") + guides(size = FALSE) 

# keine 'random' cluster-initialpunkte am anfang
elevar <- function(x) {
  x**2
}


kmeans_personalizada <- function(numero_iteraciones, datos, n_centroides, columns_skip) {
  # We initialize the list where we will store the results
  resultados = list()
  
  for(iteracion in 1:numero_iteraciones){
    
    set.seed(iteracion)
    # 1. We create the centroids
    centroides = crear_centroides(n_centroides = n_centroides, datos = datos, 
                                  columns_skip = columns_skip)
    
    centroides <- as.matrix(centroides)
    # centroides <- as.matrix(centroides[,1:ncol(centroides)-1])
    
    # 2. We convert the centroids into a matrix
    datos_limpios = datos[ , -which(names(datos) %in% columns_skip)]
    datos_limpios = as.matrix(datos_limpios)
    
    error = c(-10,0)
    
    i = 2
    centroides_que_no_estan = c(1)
    while(round(error[i],2) != round(error[i-1],2) | length(centroides_que_no_estan)> 0 ) {
      
      for(posicion in 1:length(datos$x)) {
        
        #3. We calculate the squared error to each centroid
        repeticiones = rep(datos_limpios[posicion,], times = n_centroides)
        centroide_posicion = matrix(repeticiones, nrow = n_centroides, byrow = TRUE)
        
        errores = centroides[,1:ncol(centroides)-1]  - centroide_posicion
        errores = sapply(errores, elevar)
        errores = matrix(errores, nrow = n_centroides)
        errores = rowSums(errores)
        
        #4. We find the error that minimizes the error
        datos$error[posicion] = min(errores)
        datos$cluster[posicion] = which(errores == min(errores))
        
      }
      
      #5. We calculate the total error
      error = c(error, sum(datos$error))
      
      #6. We recalculate the position of the centroids
      centroides <- datos %>%
        group_by(cluster) %>%
        summarize(
          x = mean(x), 
          y = mean(y)
        ) %>%
        mutate(n_centroide = cluster) %>%
        select(-cluster) %>%
        ungroup() %>%
        as.matrix(.)
      
      centroides_que_no_estan = setdiff(seq(1:n_centroides), unique(datos$cluster))
      
      # We check that we don't have lost any cluster. If we have, then we add it
      if(length(centroides_que_no_estan)> 0) {
        centroides_que_no_estan = setdiff(seq(1:n_centroides), unique(datos$cluster))
        
        centroides_nuevos = crear_centroides(n_centroides = length(centroides_que_no_estan), datos = datos, 
                                             columns_skip = columns_skip)
        centroides_nuevos <- as.matrix(centroides_nuevos[,1:ncol(centroides_nuevos)-1])
        
        centroides_nuevos <- cbind(centroides_nuevos, centroides_que_no_estan)
        colnames(centroides_nuevos)[colnames(centroides_nuevos) == "centroides_que_no_estan"] <- "n_centroide"
        centroides <- rbind(centroides, centroides_nuevos)
        
      }
      
      i = i + 1
      
    }
    
    #7.We return the centrooids, the errors and the number of cluster of each observation
    lista <- list(datos[,c("error","cluster")], centroides)
    resultados[[iteracion]] = lista 
    iteracion = iteracion + 1
    
  }
  
  return(resultados)
}

resultados <- kmeans_personalizada(numero_iteraciones = 5,
                                   n_centroides = 3, datos = datos, 
                                   columns_skip = c("numero_punto","error", "cluster"))

# 5 k-means-funktionen checken auf niedrigste verlust-funktion
iteraciones = c()

for(i in 1:length(resultados)) {
  x = sum(resultados[[i]][[1]]$error)
  iteraciones = c(iteraciones,x)
}

iteraciones

# plotten
centroides <- as.data.frame(resultados[[5]][[2]])
datos[,c("error","cluster")] = resultados[[5]][[1]]

ggplot() + 
  geom_point(aes(x,y, col = as.factor(cluster), size = 7), data = datos) + 
  geom_point(aes(x,y), data = centroides, size = 10, shape=21) +
  geom_text(aes(x,y, label = n_centroide), data = centroides, col = "black") +
  theme_minimal() + theme(legend.position = "bottom") + guides(size = FALSE) 

# anzahl optimaler cluster?
errores = data.frame(
  numero_clusters = seq(1:(nrow(datos)/2)),
  error = NA
)

for(i in 1:length(errores$numero_clusters)) {
  cluster <- kmeans_personalizada(numero_iteraciones = 1,
                                  n_centroides = i, datos = datos, 
                                  columns_skip = c("numero_punto","error", "cluster"))
  
  errores$error[i] = sum(cluster[[1]][[1]]$error)
  
}

ggplot(errores, aes(numero_clusters, error)) +
  geom_line() +
  geom_point() + 
  theme_minimal() +
  labs(title = "Error Total según el número de clusters") +
  scale_x_continuous(breaks = seq(1,30))




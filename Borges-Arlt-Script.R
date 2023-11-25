#### Importamos librerías necesarias

library(readr)
library(tidytext)
library(Matrix)
library(ggplot2)
library(tm)
library(dplyr)
library(tidyr)
library(stringr)
library(gridExtra)
library(scales)




### Descargamos archivos obtenidos de https://github.com/karen-pal/borges
### Les asignamos etiquetas con el nombre de los autores

archivoborges <- read_csv("C:/Users/ian bounos/OneDrive/Escritorio/DISCURSOS CRISTINA/archivoborges.csv")
archivoborges$autor = rep("Borges",nrow(archivoborges))
archivoarlt <- read_csv("C:/Users/ian bounos/OneDrive/Escritorio/DISCURSOS CRISTINA/archivoarlt.csv")
archivoarlt$autor = rep("Arlt",nrow(archivoarlt))
archivocortazar <- read_csv("C:/Users/ian bounos/OneDrive/Escritorio/DISCURSOS CRISTINA/archivocortazar.csv")
archivocortazar$autor = rep("Cortazar",nrow(archivocortazar))

### Para ilustrar alguan


textos = rbind(archivoborges,archivoarlt)
textos$link <- str_extract(textos$link, "([^/]+)/$")

tidy_books = textos %>% unnest_tokens(word, text)
tidy_books_borges = tidy_books %>% filter(autor=="Borges")
tidy_books_arlt = tidy_books %>% filter(autor=="Arlt")
tidy_books_cortazar = tidy_books %>% filter(autor=="Cortazar")
tidy_books_limpio = tidy_books%>%  filter(!word %in% stopwords("es") )    



### Grafiquemos las frecuencias de palabras sin excluir stopwords para ilustrar el problema que representan.

# Gráfico 1
plot1 <- tidy_books_borges %>%
  count(word, sort = TRUE) %>%
  top_n(40) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)+
  ggtitle("Borges")



# Gráfico 2
plot2 <- tidy_books_arlt %>%
  count(word, sort = TRUE) %>%
  top_n(40) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)+
  ggtitle("Arlt")

# Combina los gráficos uno al lado del otro
grid.arrange(plot1, plot2, ncol = 2)



#### Graficamos frecuencia de autores sin stopwords 
plot1_1 <-  tidy_books_borges %>%
  filter(!word %in% stopwords("es") )     %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL) +
  ggtitle("Borges")



plot2_1  <- tidy_books_arlt %>%
 filter(!word %in% stopwords("es") )     %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)+
  ggtitle("Arlt")

grid.arrange(plot1_1, plot2_1, ncol = 2)


##### Hacemos scatterplot de frecuencia relativa palabras

frequency <- tidy_books_limpio%>% 
  count(autor, word) %>%
  group_by(autor) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = autor, values_from = proportion)



ggplot(frequency, aes(x = Borges, y = Arlt)) +
  geom_abline(color = "gray40", lty = 2) +
  geom_point(alpha = 0.1, size = 2.5) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  theme(legend.position="none")





### Filtramos palabras que aparecen menos de 5 veces
tidy_limpio_para_matriz = subset(tidy_books_limpio,!grepl("\\d", word))%>%count(word)
filtropalabras= tidy_limpio_para_matriz%>%filter(n>5)
tidy_books_limpio2 = tidy_books_limpio%>%filter(word %in% filtropalabras$word)%>%mutate(link = paste(autor,link))





# Crear la matriz TF
tf_matrix <-tidy_books_limpio2 %>%
  unnest_tokens(word, word) %>%  # Tokenizar las palabras
  count(link, word) %>%  # Contar la frecuencia de cada palabra en cada libro
  cast_sparse(link, word, n)  # Crear la matriz TF esparsa

# Visualizar la matriz TF haciendo reducción de dimensión con PCA
res.pca = prcomp(tf_matrix)#,scale=TRUE)

autor = substr( rownames(res.pca$x),1,1)=="B"
autor
autor = ifelse(autor,"Borges","Arlt"     )

### En df.pca guardamos las dos primeras componentes
df.pca = data.frame( PC1 = res.pca$x[,1],PC2=res.pca$x[,2],Autor= autor   )
ggplot(data=df.pca,aes(x=PC1,y=PC2,col=Autor))+
  geom_point(size=2)+
  ggtitle("PCA textos Borges y Arlt")


## Hacemo histograma de las dos primeras componentes principales


ggplot(df.pca, aes(x = PC1, fill = Autor)) +
  geom_histogram(binwidth = 2, position = "identity", alpha = 0.7) +
  labs(x = "PC2", y = "Frecuencia") +
  scale_fill_discrete(name = "Autor")+
  ggtitle("Histograma PCA Borges y Arlt")



ggplot(df.pca, aes(x = PC2, fill = Autor)) +
  geom_histogram(binwidth = 2, position = "identity", alpha = 0.7) +
  labs(x = "PC2", y = "Frecuencia") +
  scale_fill_discrete(name = "Autor")+
  ggtitle("Histograma PCA Borges y Arlt")

### Queremos las palabras de mayor valor absoluto en los scores.
res.pca$rotation[order(-abs(res.pca$rotation[,2])),2][1:20]



#################################################################
### Implementamos KNN #########################################


library(class)


# Preparar los datos
features <- df.pca[, 1:(ncol(df.pca)-1)]  
# Dividir los datos en conjunto de entrenamiento y prueba
set.seed(38803087)  # Establecer semilla para reproducibilidad
indices_entrenamiento <- sample(1:nrow(df.pca), 0.7 * nrow(df.pca))  # 70% entrenamiento
conjunto_entrenamiento <- df.pca[indices_entrenamiento, ]
conjunto_prueba <- df.pca[-indices_entrenamiento, ]

# Entrenar el modelo k-NN
k <- 5  # Puedes ajustar este valor según tus necesidades
modelo_knn <- knn(train = features[indices_entrenamiento, ],
                  test = features[-indices_entrenamiento, ],
                  cl = df.pca$Autor[indices_entrenamiento],
                  k = k)

# Evaluar el modelo
precision <- sum(modelo_knn == df.pca$Autor[-indices_entrenamiento]) / length(modelo_knn)
cat("Precisión del modelo k-NN:", precision, "\n")


# Construir la matriz de confusión
confusion_matrix <- table(modelo_knn, df.pca$Autor[-indices_entrenamiento])
print(confusion_matrix)

## Observemos que la clasificación tiene una tasa de acierto del 100%

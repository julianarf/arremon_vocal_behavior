### Evaluate similarity among individuals in song repertoire (song types and frequency of use)
### and relation between acoustic and geographic distances

## By Juliana Rodriguez Fuentes
## Last updated: 23/02/2021 

# Load libraries
library(vegan)
library(gplots)
library(RColorBrewer)
library(geosphere)
library(ade4)
library(dplyr)


# Analyses of all individuals using song frequencies -----------------------------------------

# This analysis excludes the male Pippin 

###                   ###
### Acoustic distance ###
###                   ###


# Matrix data must have song types in the columns and the individuals in the rows
# Species (or Song type) vs Cuadrant (or Individual)  - col vs row -

# Getting the data
setwd("C://Users//Juliana//OneDrive - Universidad de los andes//Tesis Biologia//Analysis//DatosR")
# Song data
song_all <- read.delim("DataSongsR.txt", h=T)
song <- song_all %>% filter(Individuo != "Pippin")

tCanto <- levels(factor(song$TipoCanto)) #Song types
ind <- levels(factor(song$Individuo))    #Individuals
mConteo <- matrix(ncol = length(tCanto), nrow = length(ind))
rownames(mConteo) <- ind
colnames(mConteo) <- tCanto

# Fill the matrix with the number of songs sung by each individual per song type
for (i in 1:length(ind)){
  
  sigInd <- subset(song, Individuo == ind[i])
  
  for(j in 1:length(tCanto)){
    sigTip <- subset(sigInd, TipoCanto == tCanto[j])
    sumTip <- sum(sigTip$Cantidad)
    mConteo[i,j] <- sumTip
  }
}


###                  ###
### Heatmap absoluta ###
###                  ###

absoluta <- vegdist(mConteo, method = "morisita")
abs_dend <- as.dendrogram(hclust(absoluta, method = 'ward.D2'))

tiff("Heatmap_TodosInd_Abs_Mori_Ward.tiff", units="in", width=10, height=10, res=300)
heatmap.2(x = as.matrix(absoluta), Rowv=abs_dend, Colv=abs_dend, trace="none", scale="none", density.info="none")
dev.off()

# Do the same using the relative frequency, not the absolute number of songs - Gives the same result

#mFreq <- matrix(ncol = length(tCanto), nrow = length(ind))
#rownames(mFreq) <- ind
#colnames(mFreq) <- tCanto

# Song-type relative frequency
#for (i in 1:length(ind)){
#  sigSum <- sum(mConteo[i,])
#  for(j in 1:length(tCanto)){
#    sig <- mConteo[i,j]
#    freqSig <- sig/sigSum
#    mFreq[i,j] <- freqSig
#  }
#}

# Heat map relative frequency
#relativa <- vegdist(mFreq, method = "horn")
#rel_dend <- as.dendrogram(hclust(relativa, method = 'ward.D2'))

#tiff("Heatmap_TodosInd_Rel_Horn_Ward.tiff", units="in", width=10, height=10, res=300)
#heatmap.2(x = as.matrix(relativa), Rowv=rel_dend, Colv=rel_dend, scale="none", density.info="none", trace="none", keysize = 1)
#dev.off()


###                     ###
### Geographic distance ###
###                     ###

# Coordinates from each individual  
coordinates_all <- read.delim("indCoordinates.txt", h = T)
coordinates <- coordinates_all %>% filter(ID != "Pippin")
ind <- as.character(coordinates$ID)

# Build an empty distance matrix 
matrix_distance <- matrix (ncol = length(ind), nrow = length(ind))
colnames(matrix_distance) <- ind
rownames(matrix_distance) <- ind

# For loop to fill the matrix
for(i in 1:length(ind)){
  for(j in 1:length(ind)){
    rowInd <- subset(coordinates, ID == ind[i])
    colInd <- subset(coordinates, ID == ind[j])
    matrix_distance[i,j] <- distm(c(rowInd$lon,rowInd$lat), c(colInd$lon,colInd$lat), fun = distHaversine)
  }
}

distance <- as.dist(matrix_distance)

distanceAll <- data.frame(matrix_distance)
indMeanAll <- colMeans(distanceAll)
mean(indMeanAll)
sd(indMeanAll)
max(distanceAll)


###             ###         
### Mantel test ###
###             ###

mantel.rtest(absoluta, distance, nrepet = 10000)


###         ###         
### ANOSIM  ###
###         ###

attach(coordinates)
song.anosim <- anosim(absoluta, Sector)
summary(song.anosim)

tiff("ANOSIM_TodosInd_Abs_Morisita.tiff", units="in", width=10, height=10, res=300)
plot(song.anosim)
dev.off()



# Analyses of individuals sector A using song frequencies ------------------------------------------------------
        
###                   ###
### Acoustic distance ###
###                   ###

# Getting the data
setwd("C://Users//Juliana//OneDrive - Universidad de los andes//Tesis Biologia//Analysis//DatosR")

# Song data
song_all <- read.delim("DataSongsR.txt", h=T)
song <- song_all %>% filter(Individuo != "Pippin")

# Sector A
indN <- subset(song, Sector == "N")
indP <- subset(song, Sector == "P")
indArriba <- rbind(indN, indP)

indArriba$TipoCanto <- as.character(indArriba$TipoCanto)
indArriba$TipoCanto <- as.factor(indArriba$TipoCanto)

indArriba$Individuo <- as.character(indArriba$Individuo)
indArriba$Individuo <- as.factor(indArriba$Individuo)

tCanto <- levels(indArriba$TipoCanto)
ind <- levels(indArriba$Individuo)
mConteoArriba <- matrix(ncol = length(tCanto), nrow = length(ind))
rownames(mConteoArriba) <- ind
colnames(mConteoArriba) <- tCanto


# Fill the matrix with the number of songs sung by each individual per song type
for (i in 1:length(ind)){
  
  sigInd <- subset(indArriba, Individuo == ind[i])
  
  for(j in 1:length(tCanto)){
    sigTip <- subset(sigInd, TipoCanto == tCanto[j])
    sumTip <- sum(sigTip$Cantidad)
    mConteoArriba[i,j] <- sumTip
  }
}

# Song-type distance matrix
absolutaArriba <- vegdist(mConteoArriba, method = "morisita")

###                     ###
### Geographic distance ###
###                     ###

# Load geosphere package


# Coordinates from each individual  
coordinates <- read.delim("indCoordinates.txt", h = T)

# Arriba individuals
coorA <- subset(coordinates, Sector =="A")
coorArriba <- coorA %>% filter (ID != "Pippin")
ind <- as.character(coorArriba$ID)

# Build an empty distance matrix 
matrix_distance <- matrix (ncol = length(ind), nrow = length(ind))
colnames(matrix_distance) <- ind
rownames(matrix_distance) <- ind

# For loop to fill the matrix
for(i in 1:length(ind)){
  for(j in 1:length(ind)){
    rowInd <- subset(coordinates, ID == ind[i])
    colInd <- subset(coordinates, ID == ind[j])
    matrix_distance[i,j] <- distm(c(rowInd$lon,rowInd$lat), c(colInd$lon,colInd$lat), fun = distHaversine)
  }
}

distanceArriba <- as.dist(matrix_distance)

# Getting the mean distance of each male and then mean distance in the A sector
distanceA <- data.frame(matrix_distance)
indMeanA <- colMeans(distanceA)
mean(indMeanA)
sd(indMeanA)
max(distanceA)

###             ###         
### Mantel test ###
###             ###

mantel.rtest(absolutaArriba, distanceArriba, nrepet = 10000)


# Analyses of individuals sector B using song frequencies -----------------------------------------------------

###                   ###
### Acoustic distance ###
###                   ###

### Los datos de la matriz deben contar con los tipos de canto en las columnas
### y los individuos en las filas. Especies vs parcelas (col vs fila)

# Song data
song <- read.delim("DataSongsR", h=T)

# Sector B 
indR <- subset(song, Sector == "R")
indG <- subset(song, Sector == "G")
indAbajo <- rbind(indR, indG)

indAbajo$TipoCanto <- as.character(indAbajo$TipoCanto)
indAbajo$TipoCanto <- as.factor(indAbajo$TipoCanto)

indAbajo$Individuo <- as.character(indAbajo$Individuo)
indAbajo$Individuo <- as.factor(indAbajo$Individuo)

tCanto <- levels(indAbajo$TipoCanto)
ind <- levels(indAbajo$Individuo)
mConteoAbajo <- matrix(ncol = length(tCanto), nrow = length(ind))
rownames(mConteoAbajo) <- ind
colnames(mConteoAbajo) <- tCanto


# Fill the matrix with the number of songs sung by each individual per song type
for (i in 1:length(ind)){
  
  sigInd <- subset(indAbajo, Individuo == ind[i])
  
  for(j in 1:length(tCanto)){
    sigTip <- subset(sigInd, TipoCanto == tCanto[j])
    sumTip <- sum(sigTip$Cantidad)
    mConteoAbajo[i,j] <- sumTip
  }
}

# Song type distance matrix
absolutaAbajo <- vegdist(mConteoAbajo, method = "morisita")


###                     ###
### Geographic distance ###
###                     ###

# Coordinates from each individual  
coordinates <- read.delim("indCoordinates.txt", h = T)

# Abajo individuals
coorAbajo <- subset(coordinates, Sector =="B")

# Build an empty distance matrix 
matrix_distance <- matrix (ncol = length(ind), nrow = length(ind))
colnames(matrix_distance) <- ind
rownames(matrix_distance) <- ind

# For loop to fill the matrix
for(i in 1:length(ind)){
  for(j in 1:length(ind)){
    rowInd <- subset(coordinates, ID == ind[i])
    colInd <- subset(coordinates, ID == ind[j])
    matrix_distance[i,j] <- distm(c(rowInd$lon,rowInd$lat), c(colInd$lon,colInd$lat), fun = distHaversine)
  }
}

distanceAbajo <- as.dist(matrix_distance)

# Getting the mean distance of each male and then mean distance in the B sector
distanceB <- data.frame(matrix_distance)
indMeanB <- colMeans(distanceB)
mean(indMeanB)
sd(indMeanB)
max(distanceB)


###             ###         
### Mantel test ###
###             ###

mantel.rtest(absolutaAbajo, distanceAbajo, nrepet = 10000)

#p.adjust(0.00029997, method ="bonferroni", n = 3)
#p.adjust(0.00009999, method ="bonferroni", n = 3) 


# Geographic distance between sectors -------------------------------------

# Coordinates from each individual  
coordinates <- read.delim("indCoordinates.txt", h = T)

# Abajo individuals
coorAbajo <- subset(coordinates, Sector =="B")
ind.SectorA <- coordinates %>% filter(Sector == "A") %>% filter (ID != "Pippin")
distances <- vector()
matrix_sector <- matrix (ncol = nrow(ind.SectorA), nrow = nrow(coorAbajo))


for (k in 1:nrow(ind.SectorA)){
  sig.SectorA <- coordinates %>% filter(ID == ind.SectorA$ID[k])
  ind <- rbind (coorAbajo, sig.SectorA)
  
  # Build an empty distance matrix 
  matrix_distance <- matrix (ncol = nrow(ind), nrow = nrow(ind))
  colnames(matrix_distance) <- ind$ID
  rownames(matrix_distance) <- ind$ID
  
  # For loop to fill the matrix
  for(i in 1:nrow(ind)){
    for(j in 1:nrow(ind)){
      rowInd <- subset(coordinates, ID == ind$ID[i])
      colInd <- subset(coordinates, ID == ind$ID[j])
      matrix_distance[i,j] <- distm(c(rowInd$lon,rowInd$lat), c(colInd$lon,colInd$lat), fun = distHaversine)
    }
  }
  
  # Getting the mean distance of each male and then mean distance in the B sector
  distanceB <- data.frame(matrix_distance)
  matrix_sector[,k] <-distanceB[c(1:18),19] 
  distances[k] <- mean(distanceB[c(1:18),19])
}

min(matrix_sector)
max(matrix_sector)
mean(matrix_sector)
sd(matrix_sector)

# ANOSIM ------------------------------------------------------------------

# Getting the data
setwd("C:/Users/Juliana/Desktop")
# Song data
song_all <- read.delim("DatosCantosRCompleto.txt", h=T)
song <- song_all %>% filter(Individuo != "Pippin")


tCanto <- levels(factor(song$TipoCanto)) #Song types
ind <- levels(factor(song$Individuo))    #Individuals
mConteo <- matrix(ncol = length(tCanto), nrow = length(ind))
rownames(mConteo) <- ind
colnames(mConteo) <- tCanto

# Fill the matrix with the number of songs sung by each individual per song type
for (i in 1:length(ind)){
  
  sigInd <- subset(song, Individuo == ind[i])
  
  for(j in 1:length(tCanto)){
    sigTip <- subset(sigInd, TipoCanto == tCanto[j])
    sumTip <- sum(sigTip$Cantidad)
    mConteo[i,j] <- sumTip
  }
}

absoluta <- vegdist(mConteo, method = "morisita")
coordinates_all <- read.delim("indCoordinates.txt", h = T)
coordinates <- coordinates_all %>% filter (ID != "Pippin")
attach(coordinates)
song.anosim <- anosim(absoluta, Sector)
summary(song.anosim)


# Manuscript plot -----------------------------------------------------
names <- coordinates$Code

d <- vegdist(mConteo, method="morisita")
d <- as.matrix(d, labels = TRUE)
colnames(d) <- rownames (d) <- names
d <- as.dist(d)

h <- hclust(d, method = 'average')
plot(h, axes = FALSE, hang = -1, xlab="", ylab="Similarity", sub = "")
axis(side=2, at=seq(0, max(h$height), length=6), labels=seq(1,0,-0.2))

tiff("Dendrogram_Todos_Morisita.tiff", units="in", width=10, height=7, res=300)
plot(h, axes = FALSE, hang = -1, xlab="", ylab="Similarity", sub = "")
axis(side=2, at=seq(0, max(h$height), length=6), labels=seq(1,0,-0.2))
dev.off()


# DO NOT RUN --- Analyses of all individuals using a Presence - Absence Matrix ----------------------------------------------------

# Getting the data
setwd("C://Users//Juliana//OneDrive - Universidad de los andes//Tesis Biologia//Analysis//DatosR")

# Song data
song_all <- read.delim("DataSongsR.txt", h=T)
song <- song_all %>% filter(Individuo != "Pippin")

# Name of each individual
ind <- levels(song$Individuo)

# Song types
songTypes <- levels(song$TipoCanto)

# Presence-Absence matrix
matrixST <- matrix(ncol = length(songTypes), nrow = length(ind))
rownames(matrixST) <- ind
colnames(matrixST) <- songTypes

# For loop to fill the matrix
for(i in 1:length(ind)){
  sig <- subset(song, Individuo == ind[i])
  sigSongType <- levels(factor(sig$TipoCanto))
  
  for(j in 1:length(songTypes)){
    if(is.element(songTypes[j],sigSongType)){
      matrixST[i,j] <- 1 
    } else {
      matrixST[i,j] <- 0
    }
  }
}

presencia <- vegdist(matrixST, method="horn", binary = T)
pre_dend <- as.dendrogram(hclust(presencia, method = 'ward.D2'))
heatmap.2(x = as.matrix(presencia), Rowv=pre_dend, Colv=pre_dend, trace="none", scale="none", density.info="none")

d <- vegdist(matrixST, method="horn")
coordinates <- read.delim("indCoordinates.txt", h = T)
attach(coordinates)
song.anosim <- anosim(d, Sector)
summary(song.anosim)
plot(song.anosim)
dev.off()

# Mantel graph ------------------------------------------------------------

absArriba <- 1- absolutaArriba

dfA <- data.frame(Similitud=absArriba[lower.tri(absArriba)], Distancia=distanceArriba[lower.tri(distanceArriba)])
dfArriba <- data.frame(na.omit(dfA), Sector = "A")

absAbajo <- 1- absolutaAbajo
dfB <- data.frame(Similitud=absAbajo[lower.tri(absAbajo)], Distancia=distanceAbajo[lower.tri(distanceAbajo)])
dfAbajo <- data.frame(na.omit(dfB), Sector = "B")

df <- rbind(dfArriba, dfAbajo)

library(ggplot2)
plot <- ggplot(df, aes(x=Distancia, y=Similitud, colour=Sector)) + geom_point(size=2,aes(shape=Sector)) + 
      theme_test(base_size = 14, base_family = "Arial") +
      labs(x="Geographic distance (m)", y="Repertoire-use similarity") +
      theme(axis.text = element_text(colour="black"), text = element_text(size=20)) +
      geom_smooth(method=lm, aes(fill=Sector)) + scale_color_manual(values=c('#A6A6A6', '#D0A402')) +
      scale_fill_manual(values = c('#A6A6A6', '#D0A402'))

tiff("MantelPlotBothWithoutRegression.tiff", units="in", width=10, height=7, res=300)
plot


# DO NOT RUN --- Analyses of only some individuals ------------------------------------------------------
par(mfcol=c(1,3))
## Analysis of individuals restricting to x number of songs (either 150 or 200)##
#In this case instead I'll truncating the sampling per individual such as all males 
#have the same number of songs

#Get data
raw.song.data <- read.delim("DataSongsR.txt", h=T)
#Song types in the file
songs <- raw.song.data %>% group_by(Individuo) %>% summarise(sum = sum(Cantidad))
#Threshold in number of songs classified per individual
n.songs <- 200
#ID of males to be analyzed
ind.Analisis <- data.frame(songs %>% filter(sum > n.songs))
ind.names <- ind.Analisis$Individuo
#Filter original data to only include the individuals with more than x songs classified
filtered.data <- filter(raw.song.data, Individuo %in% ind.names)


#Create matrix to include the first x songs per individual
song.150 <- matrix(ncol = length(ind.names), nrow = n.songs)
colnames(song.150) <- c(as.character(ind.names))
filtered.data$TipoCanto <- as.character(filtered.data$TipoCanto)

#fill the matrix
for (i in 1:length(ind.names)){
  values.sig <- subset(filtered.data, Individuo == ind.names[i])
  song.vector <- vector()
  for (j in 1:nrow(values.sig)){
    sig.song <- rep(values.sig[j,3], values.sig[j,4])
    song.vector <- c(song.vector, sig.song)
  }
  song.150[(1:(n.songs)),i] <- song.vector[1:n.songs]
}


tCanto <- levels(factor(filtered.data$TipoCanto))
mConteo <- matrix(ncol = length(tCanto), nrow = length(ind.names))
rownames(mConteo) <- ind.names
colnames(mConteo) <- tCanto

## Fill the matrix with the number of songs per male for all song types
for (i in 1:length(ind.names)){
  for(j in 1:length(tCanto)){
    sigTip <- song.150[,i] == tCanto[j]
    sumTip <- sum(sigTip, na.rm = TRUE)
    mConteo[i,j] <- sumTip
  }
}

d <- vegdist(mConteo, method="morisita")
d <- as.matrix(d, labels = TRUE)
colnames(d) <- rownames (d) <- ind.names
d <- as.dist(d)

h <- hclust(d, method = 'average')
plot(h, axes = FALSE, hang = -1, xlab="", ylab="Similarity", sub = "", main="Only first 200 songs")



## Analysis of individuals that have more than x number of songs (150/200) or complete repertoire ##
#In this case instead of truncating the songs per individual such as all males have the same
#number of songs, I'll use the complete sample

#Get data
raw.song.data <- read.delim("DataSongsR.txt", h=T)
#Song types in the file
songs <- raw.song.data %>% group_by(Individuo) %>% summarise(sum = sum(Cantidad))
#Threshold in number of songs classified per individual
n.songs <- 200
#ID of males to be analyzed
ind.Analisis <- data.frame(songs %>% filter(sum > n.songs))
ind.names <- ind.Analisis$Individuo
#Filter original data to only include the individuals with more than x songs classified
filtered.data <- filter(raw.song.data, Individuo %in% ind.names)

tCanto <- levels(factor(filtered.data$TipoCanto))
mConteo <- matrix(ncol = length(tCanto), nrow = length(ind.names))
rownames(mConteo) <- ind.names
colnames(mConteo) <- tCanto

## Fill the matrix with the number of songs per male for all song types
for (i in 1:length(ind.names)){
  
  sigInd <- subset(filtered.data, Individuo == ind.names[i])
  tot <- sum(sigInd$Cantidad)
  
  for(j in 1:length(tCanto)){
    sigTip <- subset(sigInd, TipoCanto == tCanto[j])
    sumTip <- sum(sigTip$Cantidad)
    mConteo[i,j] <- sumTip
  }
}

d <- vegdist(mConteo, method="morisita")
d <- as.matrix(d, labels = TRUE)
colnames(d) <- rownames (d) <- ind.names
d <- as.dist(d)

h <- hclust(d, method = 'average')
plot(h, axes = FALSE, hang = -1, xlab="", ylab="Similarity", sub = "", main = ">200 songs")



## Analysis of individuals that have more than x number of songs (150/200) or complete repertoire ##
#In this case instead of truncating the songs per individual such as all males have the same
#number of songs, I'll use the complete sample

#Get data
raw.song.data <- read.delim("DataSongsR.txt.txt", h=T)
#Song types in the file
songs <- raw.song.data %>% group_by(Individuo) %>% summarise(sum = sum(Cantidad))
#Threshold in number of songs classified per individual
n.songs <- 200
#ID of males to be analyzed
ind.Analisis <- data.frame(songs %>% filter(sum > n.songs))
ind.names <- ind.Analisis$Individuo
#Filter original data to only include the individuals with more than x songs classified
filtered.data <- filter(raw.song.data, Individuo %in% ind.names)
filtered.data <- filtered.data %>% filter(Individuo !="Optimus") %>% filter (Individuo !="Antonio") %>% filter (Individuo !="Ringo") %>% filter (Individuo !="Seiya")

ind.names <- levels(as.factor(as.character(filtered.data$Individuo)))
tCanto <- levels(factor(filtered.data$TipoCanto))
mConteo <- matrix(ncol = length(tCanto), nrow = length(ind.names))
rownames(mConteo) <- ind.names
colnames(mConteo) <- tCanto

## Fill the matrix with the number of songs per male for all song types
for (i in 1:length(ind.names)){
  
  sigInd <- subset(filtered.data, Individuo == ind.names[i])
  tot <- sum(sigInd$Cantidad)
  
  for(j in 1:length(tCanto)){
    sigTip <- subset(sigInd, TipoCanto == tCanto[j])
    sumTip <- sum(sigTip$Cantidad)
    mConteo[i,j] <- sumTip
  }
}

d <- vegdist(mConteo, method="morisita")
d <- as.matrix(d, labels = TRUE)
colnames(d) <- rownames (d) <- ind.names
d <- as.dist(d)

h <- hclust(d, method = 'average')
plot(h, axes = FALSE, hang = -1, xlab="", ylab="Similarity", sub = "", main = "ind reached asymptote")




### THIS MADE ME CRY :'( Threshold analysis for individuals in sector B ------------------------------------------

#Get data
raw.song.data <- read.delim("DataSongsR.txt", h=T)
#Song types in the file
songs <- raw.song.data %>% group_by(Individuo) %>% summarise(sum = sum(Cantidad))
#Threshold in number of songs classified per individual
n.songs <- 200
#ID of males to be analyzed
ind.Analisis <- data.frame(songs %>% filter(sum > n.songs))
ind.names <- ind.Analisis$Individuo
#Filter original data to only include the individuals with more than x songs classified
filtered.data <- filter(raw.song.data, Individuo %in% ind.names)


# Sector B 
indR <- subset(filtered.data, Sector == "R")
indG <- subset(filtered.data, Sector == "G")
indAbajo <- rbind(indR, indG)

indAbajo$TipoCanto <- as.character(indAbajo$TipoCanto)
indAbajo$TipoCanto <- as.factor(indAbajo$TipoCanto)

indAbajo$Individuo <- as.character(indAbajo$Individuo)
indAbajo$Individuo <- as.factor(indAbajo$Individuo)

tCanto <- levels(indAbajo$TipoCanto)
ind.names <- levels(indAbajo$Individuo)

mConteo <- matrix(ncol = length(tCanto), nrow = length(ind.names))
rownames(mConteo) <- ind.names
colnames(mConteo) <- tCanto

## Fill the matrix with the number of songs per male for all song types
for (i in 1:length(ind.names)){
  
  sigInd <- subset(filtered.data, Individuo == ind.names[i])
  tot <- sum(sigInd$Cantidad)
  
  for(j in 1:length(tCanto)){
    sigTip <- subset(sigInd, TipoCanto == tCanto[j])
    sumTip <- sum(sigTip$Cantidad)
    mConteo[i,j] <- sumTip
  }
}

d <- vegdist(mConteo, method="morisita")
d <- as.matrix(d, labels = TRUE)
colnames(d) <- rownames (d) <- ind.names
d <- as.dist(d)

h <- hclust(d, method = 'average')
plot(h, axes = FALSE, hang = -1, xlab="", ylab="Similarity", sub = "")


## Coordinates from each individual  
coordinates <- read.delim("indCoordinates.txt", h = T)

## Build an empty distance matrix 
matrix_distance <- matrix (ncol = length(ind.names), nrow = length(ind.names))
colnames(matrix_distance) <- ind.names
rownames(matrix_distance) <- ind.names

## For loop to fill the matrix
for(i in 1:length(ind.names)){
  for(j in 1:length(ind.names)){
    rowInd <- subset(coordinates, ID == ind.names[i])
    colInd <- subset(coordinates, ID == ind.names[j])
    matrix_distance[i,j] <- distm(c(rowInd$lon,rowInd$lat), c(colInd$lon,colInd$lat), fun = distHaversine)
  }
}

dist.filtered <- as.dist(matrix_distance)
mantel.rtest(dist.filtered, d, nrepet = 10000)

### Truncated analysis for individuals in sector B

#Get data
raw.song.data <- read.delim("DataSongsR.txt", h=T)
#Song types in the file
songs <- raw.song.data %>% group_by(Individuo) %>% summarise(sum = sum(Cantidad))
#Threshold in number of songs classified per individual
n.songs <- 200
#ID of males to be analyzed
ind.Analisis <- data.frame(songs %>% filter(sum > n.songs))
ind.names <- ind.Analisis$Individuo
#Filter original data to only include the individuals with more than x songs classified
filtered.data <- filter(raw.song.data, Individuo %in% ind.names)

# Sector B 
indR <- subset(filtered.data, Sector == "R")
indG <- subset(filtered.data, Sector == "G")
indAbajo <- rbind(indR, indG)

indAbajo$TipoCanto <- as.character(indAbajo$TipoCanto)
indAbajo$TipoCanto <- as.factor(indAbajo$TipoCanto)

indAbajo$Individuo <- as.character(indAbajo$Individuo)
indAbajo$Individuo <- as.factor(indAbajo$Individuo)

tCanto <- levels(indAbajo$TipoCanto)
ind.names <- levels(indAbajo$Individuo)

#Create matrix to include the first x songs per individual
song.150 <- matrix(ncol = length(ind.names), nrow = n.songs)
colnames(song.150) <- c(as.character(ind.names))
indAbajo$TipoCanto <- as.character(indAbajo$TipoCanto)

#fill the matrix
for (i in 1:length(ind.names)){
  values.sig <- subset(indAbajo, Individuo == ind.names[i])
  song.vector <- vector()
  for (j in 1:nrow(values.sig)){
    sig.song <- rep(values.sig[j,3], values.sig[j,4])
    song.vector <- c(song.vector, sig.song)
  }
  song.150[(1:(n.songs)),i] <- song.vector[1:n.songs]
}


mConteo <- matrix(ncol = length(tCanto), nrow = length(ind.names))
rownames(mConteo) <- ind.names
colnames(mConteo) <- tCanto

## Fill the matrix with the number of songs per male for all song types
for (i in 1:length(ind.names)){
  for(j in 1:length(tCanto)){
    sigTip <- song.150[,i] == tCanto[j]
    sumTip <- sum(sigTip, na.rm = TRUE)
    mConteo[i,j] <- sumTip
  }
}

d <- vegdist(mConteo, method="morisita")
d <- as.matrix(d, labels = TRUE)
colnames(d) <- rownames (d) <- ind.names
d <- as.dist(d)

h <- hclust(d, method = 'average')
plot(h, axes = FALSE, hang = -1, xlab="", ylab="Similarity", sub = "")


## Coordinates from each individual  
coordinates <- read.delim("indCoordinates.txt", h = T)

## Build an empty distance matrix 
matrix_distance <- matrix (ncol = length(ind.names), nrow = length(ind.names))
colnames(matrix_distance) <- ind.names
rownames(matrix_distance) <- ind.names

## For loop to fill the matrix
for(i in 1:length(ind.names)){
  for(j in 1:length(ind.names)){
    rowInd <- subset(coordinates, ID == ind.names[i])
    colInd <- subset(coordinates, ID == ind.names[j])
    matrix_distance[i,j] <- distm(c(rowInd$lon,rowInd$lat), c(colInd$lon,colInd$lat), fun = distHaversine)
  }
}

dist.filtered <- as.dist(matrix_distance)
mantel.rtest(dist.filtered, d, nrepet = 10000)
?p.adjust
p.adjust(0.00059994, method ="bonferroni", n = 4)


# Jaccard Index Analysis --------------------------------------------------

# All individuals, but excluding the male Pippin

###                   ###
### Acoustic distance ###
###                   ###


# Matrix data must have song types in the columns and the individuals in the rows
# Species (or Song type) vs Cuadrant (or Individual)  - col vs row -

# Getting the data
setwd("C://Users//Juliana//OneDrive - Universidad de los andes//Tesis Biologia//Analysis//DatosR")
# Song data
song_all <- read.delim("DataSongsR.txt", h=T)
song <- song_all %>% filter(Individuo != "Pippin")

tCanto <- levels(factor(song$TipoCanto)) #Song types
ind <- levels(factor(song$Individuo))    #Individuals
mConteo <- matrix(ncol = length(tCanto), nrow = length(ind))
rownames(mConteo) <- ind
colnames(mConteo) <- tCanto

# Fill the matrix with the number of songs sung by each individual per song type
for (i in 1:length(ind)){
  
  sigInd <- subset(song, Individuo == ind[i])
  
  for(j in 1:length(tCanto)){
    sigTip <- subset(sigInd, TipoCanto == tCanto[j])
    sumTip <- sum(sigTip$Cantidad)
    mConteo[i,j] <- sumTip
  }
}

absoluta <- vegdist(mConteo, method = "jaccard")

###                     ###
### Geographic distance ###
###                     ###

# Coordinates from each individual  
coordinates_all <- read.delim("indCoordinates.txt", h = T)
coordinates <- coordinates_all %>% filter(ID != "Pippin")
ind <- as.character(coordinates$ID)

# Build an empty distance matrix 
matrix_distance <- matrix (ncol = length(ind), nrow = length(ind))
colnames(matrix_distance) <- ind
rownames(matrix_distance) <- ind

# For loop to fill the matrix
for(i in 1:length(ind)){
  for(j in 1:length(ind)){
    rowInd <- subset(coordinates, ID == ind[i])
    colInd <- subset(coordinates, ID == ind[j])
    matrix_distance[i,j] <- distm(c(rowInd$lon,rowInd$lat), c(colInd$lon,colInd$lat), fun = distHaversine)
  }
}

distance <- as.dist(matrix_distance)

distanceAll <- data.frame(matrix_distance)
indMeanAll <- colMeans(distanceAll)
mean(indMeanAll)
sd(indMeanAll)
max(distanceAll)


###             ###         
### Mantel test ###
###             ###

mantel.rtest(absoluta, distance, nrepet = 10000)


###         ###         
### ANOSIM  ###
###         ###

attach(coordinates)
song.anosim <- anosim(absoluta, Sector)
summary(song.anosim)

tiff("ANOSIM_TodosInd_Abs_Jaccard.tiff", units="in", width=10, height=10, res=300)
plot(song.anosim)
dev.off()

###             ###         
### Dendrogram  ###
###             ###

names <- coordinates$Code

d <- vegdist(mConteo, method="jaccard")
d <- as.matrix(d, labels = TRUE)
colnames(d) <- rownames (d) <- names
d <- as.dist(d)

h <- hclust(d, method = 'average')
plot(h, axes = FALSE, hang = -1, xlab="", ylab="Similarity", sub = "")
axis(side=2, at=seq(0, max(h$height), length=6), labels=seq(1,0,-0.2))

tiff("Dendrogram_Todos_Jaccard.tiff", units="in", width=10, height=7, res=300)
plot(h, axes = FALSE, hang = -1, xlab="", ylab="Similarity", sub = "")
axis(side=2, at=seq(0, max(h$height), length=6), labels=seq(1,0,-0.2))
dev.off()


# Analyses of individuals sector A 

###                   ###
### Acoustic distance ###
###                   ###

# Getting the data
setwd("C://Users//Juliana//OneDrive - Universidad de los andes//Tesis Biologia//Analysis//DatosR")

# Song data
song_all <- read.delim("DataSongsR.txt", h=T)
song <- song_all %>% filter(Individuo != "Pippin")

# Sector A
indN <- subset(song, Sector == "N")
indP <- subset(song, Sector == "P")
indArriba <- rbind(indN, indP)
#indA <- rbind(indN, indP)
#indArriba <- indA %>% filter(Individuo != "Pippin")

indArriba$TipoCanto <- as.character(indArriba$TipoCanto)
indArriba$TipoCanto <- as.factor(indArriba$TipoCanto)

indArriba$Individuo <- as.character(indArriba$Individuo)
indArriba$Individuo <- as.factor(indArriba$Individuo)

tCanto <- levels(indArriba$TipoCanto)
ind <- levels(indArriba$Individuo)
mConteoArriba <- matrix(ncol = length(tCanto), nrow = length(ind))
rownames(mConteoArriba) <- ind
colnames(mConteoArriba) <- tCanto


# Fill the matrix with the number of songs sung by each individual per song type
for (i in 1:length(ind)){
  
  sigInd <- subset(indArriba, Individuo == ind[i])
  
  for(j in 1:length(tCanto)){
    sigTip <- subset(sigInd, TipoCanto == tCanto[j])
    sumTip <- sum(sigTip$Cantidad)
    mConteoArriba[i,j] <- sumTip
  }
}

# Song-type distance matrix
absolutaArriba <- vegdist(mConteoArriba, method = "jaccard")

###                     ###
### Geographic distance ###
###                     ###

# Coordinates from each individual  
coordinates <- read.delim("indCoordinates.txt", h = T)

# Arriba individuals
coorA <- subset(coordinates, Sector =="A")
coorArriba <- coorA %>% filter (ID != "Pippin") 
ind <- as.character(coorArriba$ID)

# Build an empty distance matrix 
matrix_distance <- matrix (ncol = length(ind), nrow = length(ind))
colnames(matrix_distance) <- ind
rownames(matrix_distance) <- ind

# For loop to fill the matrix
for(i in 1:length(ind)){
  for(j in 1:length(ind)){
    rowInd <- subset(coordinates, ID == ind[i])
    colInd <- subset(coordinates, ID == ind[j])
    matrix_distance[i,j] <- distm(c(rowInd$lon,rowInd$lat), c(colInd$lon,colInd$lat), fun = distHaversine)
  }
}

distanceArriba <- as.dist(matrix_distance)

# Getting the mean distance of each male and then mean distance in the A sector
distanceA <- data.frame(matrix_distance)
indMeanA <- colMeans(distanceA)
mean(indMeanA)
sd(indMeanA)
max(distanceA)

###             ###         
### Mantel test ###
###             ###

mantel.rtest(absolutaArriba, distanceArriba, nrepet = 10000)


# Analyses of individuals sector B using song frequencies 

###                   ###
### Acoustic distance ###
###                   ###

### Los datos de la matriz deben contar con los tipos de canto en las columnas
### y los individuos en las filas. Especies vs parcelas (col vs fila)

# Song data
song <- read.delim("DataSongsR.txt", h=T)

# Sector B 
indR <- subset(song, Sector == "R")
indG <- subset(song, Sector == "G")
indAbajo <- rbind(indR, indG)

indAbajo$TipoCanto <- as.character(indAbajo$TipoCanto)
indAbajo$TipoCanto <- as.factor(indAbajo$TipoCanto)

indAbajo$Individuo <- as.character(indAbajo$Individuo)
indAbajo$Individuo <- as.factor(indAbajo$Individuo)

tCanto <- levels(indAbajo$TipoCanto)
ind <- levels(indAbajo$Individuo)
mConteoAbajo <- matrix(ncol = length(tCanto), nrow = length(ind))
rownames(mConteoAbajo) <- ind
colnames(mConteoAbajo) <- tCanto


# Fill the matrix with the number of songs sung by each individual per song type
for (i in 1:length(ind)){
  
  sigInd <- subset(indAbajo, Individuo == ind[i])
  
  for(j in 1:length(tCanto)){
    sigTip <- subset(sigInd, TipoCanto == tCanto[j])
    sumTip <- sum(sigTip$Cantidad)
    mConteoAbajo[i,j] <- sumTip
  }
}

# Song type distance matrix
absolutaAbajo <- vegdist(mConteoAbajo, method = "jaccard")


###                     ###
### Geographic distance ###
###                     ###

# Coordinates from each individual  
coordinates <- read.delim("indCoordinates.txt", h = T)

# Abajo individuals
coorAbajo <- subset(coordinates, Sector =="B")

# Build an empty distance matrix 
matrix_distance <- matrix (ncol = length(ind), nrow = length(ind))
colnames(matrix_distance) <- ind
rownames(matrix_distance) <- ind

# For loop to fill the matrix
for(i in 1:length(ind)){
  for(j in 1:length(ind)){
    rowInd <- subset(coordinates, ID == ind[i])
    colInd <- subset(coordinates, ID == ind[j])
    matrix_distance[i,j] <- distm(c(rowInd$lon,rowInd$lat), c(colInd$lon,colInd$lat), fun = distHaversine)
  }
}

distanceAbajo <- as.dist(matrix_distance)

# Getting the mean distance of each male and then mean distance in the B sector
distanceB <- data.frame(matrix_distance)
indMeanB <- colMeans(distanceB)
mean(indMeanB)
sd(indMeanB)
max(distanceB)


###             ###         
### Mantel test ###
###             ###

mantel.rtest(absolutaAbajo, distanceAbajo, nrepet = 10000)
#Number of song-type per individual

## Load ggplot
library(ggplot2)
library(dplyr)

## Getting the data
setwd("C:/Users/Juliana/Desktop")
### Song data
song <- read.delim("DatosCantosRCompleto.txt", h=T)

### Name of each individual
ind <- levels(song$Individuo)
## Empty matrix for the number of song-type per individual
songTypesInd <- matrix(ncol = 2, nrow = length(ind))
## Fill the matrix with a for loop with the number of song types
for(i in 1:length(ind)){
  sig <- subset(song, Individuo == ind[i])
  sig$TipoCanto <- factor(sig$TipoCanto)    ###Re-factorize the values for each subset, R is sooooo weird
  numSongTypes <- length(levels(sig$TipoCanto))
  songTypesInd[i,]<-c(ind[i],numSongTypes)
}

songTypesIndDF <- data.frame(songTypesInd)
songTypesIndDF$X1 <- factor(songTypesIndDF$X1)
songTypesIndDF$X2 <- factor(songTypesIndDF$X2, levels = c("5","6","7","8","9","10","11","12","13","14"))



### plot
songTypesPlot <- ggplot(data = songTypesIndDF, aes(x=X1, y=X2)) +
                  geom_bar(stat="identity", fill="grey92") +
                  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank()) +
                  labs( x = "Individuos", y = "# de tipos de canto")
songTypesPlot

write.csv(songTypesIndDF,"CantidadTiposIndividuos.csv")

vectorSongTypes <- function(name, df){
  sig <- subset(df, Individuo == name)
  sig$TipoCanto <- as.character(sig$TipoCanto)
  sig$TipoCanto <- as.factor(sig$TipoCanto)
  songTypes <- levels(sig$TipoCanto)
  return(songTypes)
}

########### How many song types share each individual within its sector NOOOO ########


### Song-types for the entire population
All <- levels(song$TipoCanto)

### Getting the song types for the higher elevation sector
Arriba <- subset(song, Sector == "N" | Sector == "P")
Arriba$Individuo <- as.character(Arriba$Individuo)
Arriba$Individuo <- as.factor(Arriba$Individuo)
Arriba$TipoCanto <- as.character(Arriba$TipoCanto)
Arriba$TipoCanto <- as.factor(Arriba$TipoCanto)
SongTypesArriba <- levels(Arriba$TipoCanto)
namesArriba <- levels(Arriba$Individuo)

## Get a vector of song-types per individual
A <- vectorSongTypes("Tesla",song)

numSharedSongs <- vector()

for (i in 1:length(namesArriba)){
  sig <- vectorSongTypes(namesArriba[i], song)
  numSig <- length(intersect(SongTypesArriba, sig))
  numSharedSongs[i] <- numSig
}

### Getting the song types for the lower elevation sector
Abajo <- subset(song, Sector == "R")
Abajo$TipoCanto <- as.character(Abajo$TipoCanto)
Abajo$TipoCanto <- as.factor(Abajo$TipoCanto)
SongTypesAbajo <- levels(Abajo$TipoCanto)

## Get a vector of song-types per individual


P <- vectorSongTypes("Pijao",song)
Z <- vectorSongTypes("Zim",song)
G <- vectorSongTypes("Guecha",song)
Y <- vectorSongTypes("Yarigui",song)
R <- vectorSongTypes("Ringo",song)




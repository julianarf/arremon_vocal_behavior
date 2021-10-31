# Does repertoire size correlates with territory size, body condition or number of neighbors?

## Getting the repetoire data
setwd("/Users/imacandes/Desktop/Juliana_Rodriguez")
## Song data
song <- read.delim("DatosCantosR.txt", h=T)


## Name of each individual
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

## Repertoire size per individual
songTypesIndDF <- data.frame(songTypesInd)


## Getting the additional data
TerrBodyNeigh <- read.delim("Body_data_32_males_phase_I_Juliana.txt", h=T)

## Only get the data for the individuals with analyzed repertoire in an empty dataframe
tbdInd <- data.frame()
## Fill the data.frame with the individual information
for(i in 1:length(TerrBodyNeigh$ID)){
  for(j in 1:length(ind)){
    sig <- TerrBodyNeigh$ID[i]
    if(sig == ind[j]){
      sigInd <- subset(TerrBodyNeigh, ID == ind[j])
      tbdInd <- rbind(sigInd,tbdInd)
    }
  }
}

## Order the dataframe by ID
tbdIndOrder <- tbdInd[order(tbdInd$ID),]

## New dataframe with the  repertoire size and addiitional measurements
corAnaInd <- data.frame(ID = songTypesIndDF$X1, repSize = songTypesIndDF$X2, RMI_peso_ala = tbdIndOrder$RMI_peso_ala,
                        RMI_peso_tarso = tbdIndOrder$RMI_peso_tarso, terSize = tbdIndOrder$Kernel1, numNeighbors = tbdIndOrder$Number_neighbors)


##################### Statistics #######################

a <- corAnaInd[,]

fit <- lm(repSize ~ RMI_peso_ala+RMI_peso_tarso+numNeighbors+terSize,data = a)
summary(fit)



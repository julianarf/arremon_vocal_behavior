# Temporal and espectral parameters of A. assimilis vocalizations 

## By Juliana Rodriguez Fuentes
## Last updated 13/06/2020 18:10

## Cargar los paquetes requeridos
#library(warbleR)
#library(Rraven)
#library(data.table)
library(dplyr)
library(ggplot2)

# Temporal and spectral parameters of songs -----------------------------------------------------------------------

# Go to the directory containing the selection files 
setwd("C:/Users/Juliana/OneDrive - Universidad de los andes/Tesis Biologia/Analysis/SeleccionesRaven")

# Get the name of the files within the directory that have the pattern ".txt" 
fileNames <- list.files(path = "C:/Users/Juliana/OneDrive - Universidad de los andes/Tesis Biologia/Analysis/SeleccionesRaven", pattern ="\\.txt$")
length(fileNames)

# Create an empty data frame to bind all of the selection files
selRav <- data.frame()

# Loop through the files binding each file to the data frame 
for (i in 1:length(fileNames)){
  sig <- read.delim(fileNames[i], h = T, stringsAsFactors=F)
  selRav <- rbind(selRav, sig)
}

#change values
selRav[selRav == TRUE] <- "T"
selRav[selRav == FALSE] <- "F"
selRav[selRav == "AC6"] <- "C"

duets <- subset(selRav, Dueto == "S")

# Filter the selections to only analize those with good quality
# and that are not part of a duet
goodQuality <- subset(selRav, BuenaCalidad == "S")
soloGoodQ <- subset(goodQuality, Dueto == "N")

#Read file that contains filenames for good solo songs
goodSoloSongsID <- read.delim("C://Users//Juliana//OneDrive - Universidad de los andes//Tesis Biologia//Analysis//DatosR//GoodMaleSolo.txt", h =T)
IDvector <- vector()

for (i in 1:nrow(soloGoodQ)){
  sigFile <- soloGoodQ$Begin.File[i]
  sigID <- (subset(goodSoloSongsID, File == sigFile))$ID
  IDvector[i] <- sigID
}

soloGoodQ$ID <- IDvector
soloGoodQ$Bandwidth <- soloGoodQ$High.Freq..Hz. - soloGoodQ$Low.Freq..Hz.
soloGoodQ$Duration <- soloGoodQ$End.Time..s. - as.numeric(soloGoodQ$Begin.Time..s.)

## Low frequency descriptors
lowFreqSolo <- soloGoodQ %>% group_by(ID) %>% summarise(mean=mean(Low.Freq..Hz.),
                                         sd=sd(Low.Freq..Hz.), var = sd^2)
mean(lowFreqSolo$mean)
sqrt(mean(lowFreqSolo$var))

## High frequency descriptors
highFreqSolo <- soloGoodQ %>% group_by(ID) %>% summarise(mean=mean(High.Freq..Hz.),
                                                         sd=sd(High.Freq..Hz.), var = sd^2)
mean(highFreqSolo$mean)
sqrt(mean(highFreqSolo$var))

## Peak frequency descriptors
peakFreqSolo <- soloGoodQ %>% 
                group_by(ID) %>% summarise(mean=mean(Peak.Freq..Hz.),
                                          sd=sd(Peak.Freq..Hz.), var = sd^2)
mean(peakFreqSolo$mean)
sqrt(mean(peakFreqSolo$var))

## Bandwidth descriptors
bandwidthSolo <- soloGoodQ %>% 
                group_by(ID) %>% summarise(mean=mean(Bandwidth),
                              sd=sd(Bandwidth), var = sd^2)
mean(bandwidthSolo$mean)
sqrt(mean(bandwidthSolo$var))


## Duration descriptors
durationSolo <- soloGoodQ %>% 
                group_by(ID) %>% summarise(mean=mean(Duration),
                             sd=sd(Duration), var = sd^2)
mean(durationSolo$mean)
sqrt(mean(durationSolo$var))


## Descriptive measures of good duets

duetGoodQ <- subset(goodQuality, Dueto == "S")

#Read file that contains file names for good solo songs
goodDuetsID <- read.delim("C://Users//Juliana//OneDrive - Universidad de los andes//Tesis Biologia//Analysis//DatosR//GoodDuets.txt", h =T)
IDvector <- vector()

for (i in 1:nrow(duetGoodQ)){
  sigFile <- duetGoodQ$Begin.File[i]
  sigID <- (subset(goodDuetsID, Begin.File == sigFile))$ID
  IDvector[i] <- sigID
}

duetGoodQ$ID <- IDvector
duetGoodQ$Bandwidth <- (duetGoodQ$High.Freq..Hz.) - (duetGoodQ$Low.Freq..Hz.) 
duetGoodQ$Duration <- as.numeric(duetGoodQ$End.Time..s.) - as.numeric(duetGoodQ$Begin.Time..s.)

## Low frequency descriptors
lowFreqduet <- duetGoodQ %>% group_by(ID) %>% summarise(mean=mean(Low.Freq..Hz.),
                                                        sd=sd(Low.Freq..Hz.), var = sd^2)
mean(lowFreqduet$mean)
sqrt(mean(lowFreqduet$var))

## High frequency descriptors
highFreqduet <- duetGoodQ %>% group_by(ID) %>% summarise(mean=mean(High.Freq..Hz.),
                                                         sd=sd(High.Freq..Hz.), var = sd^2)
mean(highFreqduet$mean)
sqrt(mean(highFreqduet$var))

## Peak frequency descriptors
peakFreqduet <- duetGoodQ %>% 
  group_by(ID) %>% summarise(mean=mean(Peak.Freq..Hz.),
                             sd=sd(Peak.Freq..Hz.), var = sd^2)
mean(peakFreqduet$mean)
sqrt(mean(peakFreqduet$var))

## Bandwidth descriptors
bandwidthduet <- duetGoodQ %>% 
  group_by(ID) %>% summarise(mean=mean(Bandwidth),
                             sd=sd(Bandwidth), var = sd^2)
mean(bandwidthduet$mean)
sqrt(mean(bandwidthduet$var))


## Duration descriptors
durationduet <- duetGoodQ %>% 
  group_by(ID) %>% summarise(mean=mean(Duration),
                             sd=sd(Duration), var = sd^2)
mean(durationduet$mean)
sqrt(mean(durationduet$var))


# Temporal and spectral parameters of calls -------------------------------------------------

# Go to the directory containing the selection files 
setwd("C://Users//Juliana//OneDrive - Universidad de los andes//Tesis Biologia//Analysis//Llamados")

# Get the name of the files within the directory that have the pattern ".txt" 
fileNamesC <- list.files(path = "C://Users//Juliana//OneDrive - Universidad de los andes//Tesis Biologia//Analysis//Llamados", pattern ="\\.txt$")
length(fileNamesC)

# Create an empty data frame to bind all of the selection files
selRav.C <- data.frame()

# Loop through the files binding each file to the data frame 
for (i in 1:length(fileNamesC)){
  sig <- read.delim(fileNamesC[i], h = T, stringsAsFactors=F)
  selRav.C <- rbind(selRav.C, sig)
}


#Read file that contains file names for calls
setwd("C:/Users/Juliana/OneDrive - Universidad de los andes/Tesis Biologia/Analysis/DatosR")
callsID <- read.delim("DataSelectionCalls.txt", h =T)
IDvector <- vector()

for (i in 1:nrow(selRav.C)){
  sigFile <- selRav.C$Begin.File[i]
  sigID <- (subset(callsID, Archivo == sigFile))$ID.Individuo[1]
  IDvector[i] <- sigID
}

selRav.C$ID <- IDvector
selRav.C$Duration <- selRav.C$End.Time..s. - selRav.C$Begin.Time..s.
selRav.C$Bandwidth <- selRav.C$High.Freq..Hz. - selRav.C$Low.Freq..Hz.

#### Chip Calls ###
Chip <- subset(selRav.C, CallType == "C")

## Low frequency descriptors
lowFreqChip <- Chip %>% group_by(ID) %>% summarise(mean=mean(Low.Freq..Hz.),
                                                        sd=sd(Low.Freq..Hz.), var = sd^2)
mean(lowFreqChip$mean)
sqrt(mean(lowFreqChip$var))

## High frequency descriptors
highFreqChip <- Chip %>% group_by(ID) %>% summarise(mean=mean(High.Freq..Hz.),
                                                         sd=sd(High.Freq..Hz.), var = sd^2)
mean(highFreqChip$mean)
sqrt(mean(highFreqChip$var))

## Peak frequency descriptors
peakFreqChip <- Chip %>% 
  group_by(ID) %>% summarise(mean=mean(Peak.Freq..Hz.),
                             sd=sd(Peak.Freq..Hz.), var = sd^2)
mean(peakFreqChip$mean)
sqrt(mean(peakFreqChip$var))

## Bandwidth descriptors
bandwidthChip <- Chip %>% 
  group_by(ID) %>% summarise(mean=mean(Bandwidth),
                             sd=sd(Bandwidth), var = sd^2)
mean(bandwidthChip$mean)
sqrt(mean(bandwidthChip$var))

## Duration descriptors
durationChip <- Chip %>% 
  group_by(ID) %>% summarise(mean=mean(Duration),
                             sd=sd(Duration), var = sd^2)
mean(durationChip$mean)
sqrt(mean(durationChip$var))



#### Peep Calls ###

Peep <- subset(selRav.C, CallType == "P")

## Low frequency descriptors
lowFreqPeep <- Peep %>% group_by(ID) %>% summarise(mean=mean(Low.Freq..Hz.),
                                                   sd=sd(Low.Freq..Hz.), var = sd^2)
lowFreqPeep$var[is.na(lowFreqPeep$var)] <- 0
mean(lowFreqPeep$mean)
sqrt(mean(lowFreqPeep$var))

## High frequency descriptors
highFreqPeep <- Peep %>% group_by(ID) %>% summarise(mean=mean(High.Freq..Hz.),
                                                    sd=sd(High.Freq..Hz.), var = sd^2)
highFreqPeep$var[is.na(highFreqPeep$var)] <- 0
mean(highFreqPeep$mean)
sqrt(mean(highFreqPeep$var))

## Peak frequency descriptors
peakFreqPeep <- Peep %>% 
  group_by(ID) %>% summarise(mean=mean(Peak.Freq..Hz.),
                             sd=sd(Peak.Freq..Hz.), var = sd^2)
peakFreqPeep$var[is.na(peakFreqPeep$var)] <- 0
mean(peakFreqPeep$mean)
sqrt(mean(peakFreqPeep$var))

## Bandwidth descriptors
bandwidthPeep <- Peep %>% 
  group_by(ID) %>% summarise(mean=mean(Bandwidth),
                             sd=sd(Bandwidth), var = sd^2)
bandwidthPeep$var[is.na(bandwidthPeep$var)] <- 0
mean(bandwidthPeep$mean)
sqrt(mean(bandwidthPeep$var))

## Duration descriptors
durationPeep <- Peep %>% 
  group_by(ID) %>% summarise(mean=mean(Duration),
                             sd=sd(Duration), var = sd^2)
durationPeep$var[is.na(durationPeep$var)] <- 0
mean(durationPeep$mean)
sqrt(mean(durationPeep$var))



#### Long Peep Calls ###
LongPeep <- subset(selRav.C, CallType == "PL")

## Low frequency descriptors
lowFreqLongPeep <- LongPeep %>% group_by(ID) %>% summarise(mean=mean(Low.Freq..Hz.),
                                                           sd=sd(Low.Freq..Hz.), var = sd^2)
lowFreqLongPeep$var[is.na(lowFreqLongPeep$var)] <- 0
mean(lowFreqLongPeep$mean)
sqrt(mean(lowFreqLongPeep$var))

## High frequency descriptors
highFreqLongPeep <- LongPeep %>% group_by(ID) %>% summarise(mean=mean(High.Freq..Hz.),
                                                            sd=sd(High.Freq..Hz.), var = sd^2)
highFreqLongPeep$var[is.na(highFreqLongPeep$var)] <- 0
mean(highFreqLongPeep$mean)
sqrt(mean(highFreqLongPeep$var))

## Peak frequency descriptors
peakFreqLongPeep <- LongPeep %>% 
  group_by(ID) %>% summarise(mean=mean(Peak.Freq..Hz.),
                             sd=sd(Peak.Freq..Hz.), var = sd^2)
peakFreqLongPeep$var[is.na(peakFreqLongPeep$var)] <- 0
mean(peakFreqLongPeep$mean)
sqrt(mean(peakFreqLongPeep$var))

## Bandwidth descriptors
bandwidthLongPeep <- LongPeep %>% 
  group_by(ID) %>% summarise(mean=mean(Bandwidth),
                             sd=sd(Bandwidth), var = sd^2)
bandwidthLongPeep$var[is.na(bandwidthLongPeep$var)] <- 0
mean(bandwidthLongPeep$mean)
sqrt(mean(bandwidthLongPeep$var))

## Duration descriptors
durationLongPeep <- LongPeep %>% 
  group_by(ID) %>% summarise(mean=mean(Duration),
                             sd=sd(Duration), var = sd^2)
durationLongPeep$var[is.na(durationLongPeep$var)] <- 0
mean(durationLongPeep$mean)
sqrt(mean(durationLongPeep$var))



#### Juvenile Calls ###

Juvenile  <- subset(selRav.C, CallType == "J")
Juvenile.S <- subset(selRav.C, CallType == "JE")

## Low frequency descriptors
lowFreqJuvenile <- Juvenile %>% group_by(ID) %>% summarise(mean=mean(Low.Freq..Hz.),
                                                           sd=sd(Low.Freq..Hz.), var = sd^2)
lowFreqJuvenile$var[is.na(lowFreqJuvenile$var)] <- 0
mean(lowFreqJuvenile$mean)
sqrt(mean(lowFreqJuvenile$var))

## High frequency descriptors
highFreqJuvenile <- Juvenile %>% group_by(ID) %>% summarise(mean=mean(High.Freq..Hz.),
                                                            sd=sd(High.Freq..Hz.), var = sd^2)
highFreqJuvenile$var[is.na(highFreqJuvenile$var)] <- 0
mean(highFreqJuvenile$mean)
sqrt(mean(highFreqJuvenile$var))

## Peak frequency descriptors
peakFreqJuvenile <- Juvenile %>% 
  group_by(ID) %>% summarise(mean=mean(Peak.Freq..Hz.),
                             sd=sd(Peak.Freq..Hz.), var = sd^2)
peakFreqJuvenile$var[is.na(peakFreqJuvenile$var)] <- 0
mean(peakFreqJuvenile$mean)
sqrt(mean(peakFreqJuvenile$var))

## Bandwidth descriptors
bandwidthJuvenile <- Juvenile %>% 
  group_by(ID) %>% summarise(mean=mean(Bandwidth),
                             sd=sd(Bandwidth), var = sd^2)
bandwidthJuvenile$var[is.na(bandwidthJuvenile$var)] <- 0
mean(bandwidthJuvenile$mean)
sqrt(mean(bandwidthJuvenile$var))

## Duration descriptors
durationJuvenile <- Juvenile %>% 
  group_by(ID) %>% summarise(mean=mean(Duration),
                             sd=sd(Duration), var = sd^2)
durationJuvenile$var[is.na(durationJuvenile$var)] <- 0
mean(durationJuvenile$mean)
sqrt(mean(durationJuvenile$var))

# Duration Juvenile Space
durationJuvenileS <- Juvenile.S %>% 
  group_by(ID) %>% summarise(mean=mean(Duration),
                             sd=sd(Duration), var = sd^2)
durationJuvenileS$var[is.na(durationJuvenileS$var)] <- 0
mean(durationJuvenileS$mean)
sqrt(mean(durationJuvenileS$var))

setwd("C:/Users/Juliana/OneDrive - Universidad de los andes/Tesis Biologia/Analysis/DatosR")

callSel <- read.delim("DataSelectionCalls.txt", h =T)
levels(as.factor(callSel$ID.Individuo))
chip <- callSel %>% filter(Tipo.llamado == "Chip") %>% group_by(ID.Individuo) %>% summarise(n=sum(Cantidad))
length(levels(as.factor(chip$ID.Individuo)))
mean(chip$n)
sd(chip$n)

peep <- callSel %>% filter(Tipo.llamado == "Peep") %>% group_by(ID.Individuo) %>% summarise(n=sum(Cantidad))
mean(peep$n)
sd(peep$n)

lp <- callSel %>% filter(Tipo.llamado == "LongPeep") %>% group_by(ID.Individuo) %>% summarise(n=sum(Cantidad))
mean(lp$n)
sd(lp$n)

juve <- callSel %>% filter(Tipo.llamado == "Juvenile") %>% group_by(ID.Individuo) %>% summarise(n=sum(Cantidad))
mean(juve$n)
sd(juve$n)

# Analyzed songs ----------------------------------------------------------

#a <- soloGoodQ %>% group_by(Begin.File) %>% summarise(n=n())
#write.csv(a, file ="Good solo songs.csv")

setwd("C:/Users/Juliana/OneDrive - Universidad de los andes/Tesis Biologia/Analysis/DatosR")
songSel <- read.delim("GoodMaleSolo.txt", h=T)
maleSolo <- songSel %>% group_by(ID) %>% summarise(n = sum(GoodMaleSolo))
mean(maleSolo$n)
sd(maleSolo$n)

#b <- duetGoodQ %>% group_by(Begin.File) %>% summarise(n=n())
#write.csv(b, file ="Good duets.csv")

duetSel <- read.delim("GoodDuets.txt", h=T)
duets <- duetSel %>% group_by(ID) %>% summarise(n = sum(n))
mean(duets$n)
sd(duets$n)

data <- goodQuality %>% filter (NumSilabas != "N") %>% filter (NumNotas !="N")
data$NumSilabas <- as.numeric(data$NumSilabas)
data$NumNotas <- as.numeric(data$NumNotas)
mean(data$NumNotas)

badDuets <- selRav %>% filter (BuenaCalidad != "S") %>% filter (Dueto == "S")
###Revisar porque bad+good no suma la totalidad de cantos analizados
correct <- selRav %>% filter(is.na(BuenaCalidad))
levels(as.factor(correct$Begin.File))

# Intra-song variation -----------------------------------------------------
library(dplyr)
library(tidyverse)
soloAnalysis <- selRav %>% filter(Dueto == "N") %>% filter(NumSilabas != "N") %>% filter(NumNotas != "N")
songTypes <- levels(as.factor(soloAnalysis$Tipo.de.Vocalizacion))
intraSongDescriptors <- matrix(ncol=11, nrow=length(songTypes))
intraSongDescriptors[,1] <- songTypes
colnames(intraSongDescriptors) <- c("SongType","Min.S","Max.S","Mean.S","SD.S","CV.S","Min.N","Max.N","Mean.N","SD.N","CV.N")
soloAnalysis$NumSilabas <- as.numeric(soloAnalysis$NumSilabas)
soloAnalysis$NumNotas <- as.numeric(soloAnalysis$NumNotas)

for (i in 1:length(songTypes)){
  sigType <- soloAnalysis %>% filter(Tipo.de.Vocalizacion == songTypes[i])
  
  if(ncol(sigType)>1){
    summary <- sigType %>% 
      group_by(Begin.File) %>% 
      summarize(min=min(NumSilabas), max=max(NumSilabas), 
                mean=mean(NumSilabas), sd=sd(NumSilabas),
                cv=sd(NumSilabas)/mean(NumSilabas)*100)
    summary[is.na(summary)] <- 0
    intraSongDescriptors[i,2] <- mean(summary$min)
    intraSongDescriptors[i,3] <- mean(summary$max)
    intraSongDescriptors[i,4] <- mean(summary$mean)
    intraSongDescriptors[i,5] <- mean(summary$sd)
    intraSongDescriptors[i,6] <- mean(summary$cv)
    
    summary <- sigType %>% 
      group_by(Begin.File) %>% 
      summarize(min=min(NumNotas), max=max(NumNotas), 
                mean=mean(NumNotas), sd=sd(NumNotas),
                cv=sd(NumNotas)/mean(NumNotas)*100)
    summary[is.na(summary)] <- 0
    intraSongDescriptors[i,7] <- mean(summary$min)
    intraSongDescriptors[i,8] <- mean(summary$max)
    intraSongDescriptors[i,9] <- mean(summary$mean)
    intraSongDescriptors[i,10] <- mean(summary$sd)
    intraSongDescriptors[i,11] <- mean(summary$cv)
  }
}

min(goodQuality$NumSilabas)
max(goodQuality$NumSilabas)

mean(as.numeric(intraSongDescriptors[,2])) #Num syllables
mean(as.numeric(intraSongDescriptors[,9])) #Num notes


# Female notes ------------------------------------------------------------
# Go to the directory containing the selection files 
setwd("C:/Users/Juliana/OneDrive - Universidad de los andes/Tesis Biologia/Analysis/SelectionTableFemales")

# Get the name of the files within the directory that have the pattern ".txt" 
fileNamesF <- list.files(path = "C:/Users/Juliana/OneDrive - Universidad de los andes/Tesis Biologia/Analysis/SelectionTableFemales", pattern ="\\.txt$")
length(fileNamesF)

# Create an empty data frame to bind all of the selection files
selRavF <- data.frame()

# Loop through the files binding each file to the data frame 
for (i in 1:length(fileNamesF)){
  sig <- read.delim(fileNamesF[i], h = T, stringsAsFactors=F)
  selRavF <- rbind(selRavF, sig)
}

length(levels(as.factor(selRavF$MaleID)))
selRavF %>% group_by(MaleID) %>% summarise(n=n()) %>% summarise(mean = mean(n), sd = sd(n))

selRavF$Bandwidth <- selRavF$High.Freq..Hz. - selRavF$Low.Freq..Hz.
selRavF$Duration <- selRavF$End.Time..s. - selRavF$Begin.Time..s.


## Low frequency descriptors
lowFreqF <- selRavF %>% group_by(MaleID) %>% summarise(mean=mean(Low.Freq..Hz.),
                                                        sd=sd(Low.Freq..Hz.), var = sd^2)
lowFreqF[is.na(lowFreqF)] <- 0
mean(lowFreqF$mean)
sqrt(mean(lowFreqF$var))

## High frequency descriptors
highFreqF <- selRavF %>% group_by(MaleID) %>% summarise(mean=mean(High.Freq..Hz.),
                                                         sd=sd(High.Freq..Hz.), var = sd^2)
highFreqF[is.na(highFreqF)] <- 0
mean(highFreqF$mean)
sqrt(mean(highFreqF$var))

## Peak frequency descriptors
peakFreqF <- selRavF %>% 
  group_by(MaleID) %>% summarise(mean=mean(Peak.Freq..Hz.),
                             sd=sd(Peak.Freq..Hz.), var = sd^2)
peakFreqF[is.na(peakFreqF)] <- 0
mean(peakFreqF$mean)
sqrt(mean(peakFreqF$var))

## Bandwidth descriptors
bandwidthF <- selRavF %>% 
  group_by(MaleID) %>% summarise(mean=mean(Bandwidth),
                             sd=sd(Bandwidth), var = sd^2)
bandwidthF[is.na(bandwidthF)] <- 0
mean(bandwidthF$mean)
sqrt(mean(bandwidthF$var))


## Duration descriptors
durationF <- selRavF %>% 
  group_by(MaleID) %>% summarise(mean=mean(Duration),
                             sd=sd(Duration), var = sd^2)
durationF[is.na(durationF)] <- 0
mean(durationF$mean)
sqrt(mean(durationF$var))



# Number of ind analyzed --------------------------------------------------

goodSolo <- read.delim("C://Users//Juliana//OneDrive - Universidad de los andes//Tesis Biologia//Analysis//DatosR//GoodMaleSolo.txt", h = T)
length(levels(as.factor(goodSolo$ID)))
goodDuets <- read.delim("C://Users//Juliana//OneDrive - Universidad de los andes//Tesis Biologia//Analysis//DatosR//GoodDuets.txt", h = T)
length(levels(as.factor(goodDuets$ID)))
songs <- read.delim("C://Users//Juliana//OneDrive - Universidad de los andes//Tesis Biologia//Analysis//DatosR//DataSongsR.txt", h = T)
length(levels(as.factor(songs$Individuo)))
calls <- read.delim("C://Users//Juliana//OneDrive - Universidad de los andes//Tesis Biologia//Analysis//DatosR//DataSelectionCalls.txt", h = T)
length(levels(as.factor(calls$ID.Individuo)))

chip <- calls %>% filter(Tipo.llamado == "Juvenile")
length(levels(as.factor(chip$ID.Individuo)))


# Other stuff -------------------------------------------------------------
library(tuneR)
setwd("C:/Users/Juliana/OneDrive - Universidad de los andes/Tesis Biologia/Recordings")

recordingType <- read.delim("C://Users//Juliana//OneDrive - Universidad de los andes//Tesis Biologia//Analysis//DatosR//RecordingTypeFilenames.txt", h=T)
RT <- vector()

for(i in 1:nrow(selRav)){
  sigFile <- selRav$Begin.File[i]
  sigRT <- (subset(recordingType, File == sigFile))$RecordingType
  RT[i] <- sigRT
}

selRav <- data.frame(selRav, RT)
selRav <- selRav %>% filter(RT == "Focal")

all_audiofiles <- levels(as.factor(selRav$Begin.File))
allTime <- 0

for(i in 1:length(all_audiofiles)){
  audioSig <- readWave(all_audiofiles[i], header = T)
  allTime <- allTime + round(audioSig$samples / audioSig$sample.rate, 2)
}

femaleFiles <- (read.delim("C://Users//Juliana//OneDrive - Universidad de los andes//Tesis Biologia//Analysis//DatosR//FilesFemaleSong.txt")[,1])
femaleTime <- 0

for(i in 1:length(femaleFiles)){
  audioSig <- readWave(femaleFiles[i], header = T)
  femaleTime <- femaleTime + round(audioSig$samples / audioSig$sample.rate, 2)
}



duets <- selRav %>% filter(Dueto == "S")
duet_audiofiles <- levels(as.factor(duets$Begin.File))
duetTime <- 0

for(i in 1:length(duet_audiofiles)){
  audioSig <- readWave(duet_audiofiles[i], header = T)
  duetTime <- duetTime + round(audioSig$samples / audioSig$sample.rate, 2)
}


maleTime <- allTime-duetTime
maleTime/(allTime+femaleTime)*100 
maleTime/60

femaleTime/(allTime+femaleTime)*100
femaleTime/60

duetTime/(allTime+femaleTime)*100
duetTime/60

(allTime+femaleTime)/60

# Individual variation ----------------------------------------------------
library(dplyr)
goodSongsID <- read.delim("C://Users//Juliana//OneDrive - Universidad de los andes//Tesis Biologia//Analysis//DatosR//GoodMaleSolo.txt", h =T)
IDvector <- vector()

for (i in 1:nrow(soloGoodQ)){
  sigFile <- soloGoodQ$Begin.File[i]
  sigID <- (subset(goodSongsID, File == sigFile))$ID
  IDvector[i] <- sigID
}

soloGoodQ<- data.frame(soloGoodQ, ID = IDvector)

obsSongtypes <- soloGoodQ %>% group_by(Tipo.de.Vocalizacion) %>% summarise(n=n())
indA <- soloGoodQ %>% 
                  filter(Tipo.de.Vocalizacion == "A") %>% 
                  group_by(ID) %>% 
                  summarise(n=n())
A <- soloGoodQ %>% filter(Tipo.de.Vocalizacion == "A")
A$ID <- as.factor(A$ID)
A$Duration <- as.numeric(A$End.Time..s.) - as.numeric(A$Begin.Time..s.)
A <- A[,-c(1:5, 8:10,13:18)]
A <- A[,c(1,2,3,4,6,5)]

library(MASS)
model.lda <- lda(ID~., data = A)
algo <- data.frame(A, predict(model.lda)$x)
ggplot(algo, aes(LD1, LD2)) +
  geom_point(aes(color = ID))

BE <- soloGoodQ %>% filter(Tipo.de.Vocalizacion == "BE")

BE$ID <- as.factor(BE$ID)
BE$Duration <- as.numeric(BE$End.Time..s.) - as.numeric(BE$Begin.Time..s.)
BE <- BE[,-c(1:5, 8:10,13:18)]
BE <- BE[,c(1,2,3,4,6,5)]

model.lda <- lda(ID~., data = BE)
algo <- data.frame(BE, predict(model.lda)$x)
ggplot(algo, aes(LD1, LD2)) +
  geom_point(aes(color = ID))


BK <- soloGoodQ %>% filter(Tipo.de.Vocalizacion == "BK") %>% group_by(ID) %>% summarise(n=n())







shapiro.test(durationSolos)
shapiro.test(durationDuets)
wilcox.test(durationSolos, durationDuets, alternative = "two.sided")
hist(durationDuets)
summary(durationDuets)
hist(durationSolos)
summary(durationSolos)

durationVal <- data.frame(duration = c(durationDuets, durationSolos), 
                          type = c(rep("Duet",length(durationDuets)), rep("Solo",length(durationSolos))))
boxplot(duration ~ type, data = durationVal)
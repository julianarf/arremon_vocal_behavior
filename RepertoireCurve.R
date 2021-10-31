# Curva de acumulacion de cantos
## Modified 23.02.2021

## Getting the data
setwd("C:/Users/Juliana/OneDrive - Universidad de los andes/Tesis Biologia/Analysis/DatosR")
### Song data
song <- read.delim("DataSongsR_PlusDateDuet.txt", h=T)

###This function only works if the database contains the columns Individuo, TipoCanto and Cantidad 
## df is the data and indName refers to the name given to a specific individual


valuesCurve <- function (df, indName){
  
  #Get only the values for a particular individual
  ind <- subset(df, Individuo == indName)
  #Un-factorize the TipoCanto variables
  ind$TipoCanto<- as.character(ind$TipoCanto)
  #Create an empty vector for songTypes which will include the types of song and 
  #and additional empty vector that will get the values for the curve
  songTypes <- vector()
  vectorCurve <- vector()
  
  #For loop that assing values to the vector vectorCurve based in the current name of TipoCanto
  #if TipoCanto is not included in the vector songTypes it will be included increasing the size 
  #of the vector in one element. In the opposite case the songTypes vector will keep the same length 
  #and values curve will take the previous vector length for the repetition of this number based on 
  #the Cantidad value asigned to each TipoCanto in the data
  
  for (i in 1:length(ind$TipoCanto)){
    sigType <- ind$TipoCanto[i]
    if (is.element(sigType,songTypes)){
      valuesCurve <- c(rep(length(songTypes),ind$Cantidad[i]))
    } else {
      songTypes <- c(songTypes, sigType)
      valuesCurve<- c(rep(length(songTypes),ind$Cantidad[i]))
    }
    vectorCurve <- c(vectorCurve, valuesCurve)
  }
  
  return(vectorCurve)
}

ID <- levels(as.factor(song$Individuo))

valuesInd <- list()

for(i in 1:length(ID)){
  valuesSig <- valuesCurve(song, ID[i])
  valuesInd[[i]] <- valuesSig
}

tiff("Repertoire_Curve.tiff", units="in", width=13, height=7, res=400)

plot(NULL, xlab = "Number of song types analysed", 
     ylab = "Cumulative count of song-types",xlim = c(0,550), 
     ylim =c(0,14), family = "A", cex.axis=1.1, cex.lab=1.15)

for(i in 1:11){
  lines(x = seq(1, length(valuesInd[[i]])), y = valuesInd[[i]], col = "gray55", lty = 1, lwd = 1.5)
}
for(i in 12:22){
  lines(x = seq(1, length(valuesInd[[i]])), y = valuesInd[[i]], col = "black", lty = 3, lwd = 1.5)
}
for(i in 23:33){
  lines(x = seq(1, length(valuesInd[[i]])), y = valuesInd[[i]], col = "gray30", lty = 5, lwd = 1.5)
}

dev.off()



## Get the curve values for each individual in the song database
AntonioCurve <- valuesInd[[1]]
AsdrubalCurve <- valuesInd[[2]]
CamusCurve <- valuesInd[[3]]
ChainsCurve <- valuesInd[[4]]
CharlieCurve <- valuesInd[[5]]
ChesterCurve <- valuesInd[[6]]
plot(x = seq(1, length(valuesInd[[6]])), y = valuesInd[[6]], col = "gray55", type = "l", lty = 1, lwd = 1.5)

DanyCurve <- valuesInd[[7]]
DumoulinCurve <- valuesInd[[8]]
FrankCurve <- valuesInd[[9]]
GaryCurve <- valuesInd[[10]]
GuechaCurve <- valuesInd[[11]]
HamiltonCurve <- valuesInd[[12]]
JarvisCurve <- valuesInd[[13]]
JimmyCurve <- valuesInd[[14]]
JuanguiCurve <- valuesInd[[15]]
MarleyCurve <- valuesInd[[16]]
MaynardCurve <- valuesInd[[17]]
MorrisonCurve <- valuesInd[[18]]
NiceforoCurve <- valuesInd[[19]]
OptimusCurve <- valuesInd[[20]]
plot(x = seq(1, length(valuesInd[[20]])), y = valuesInd[[20]], col = "gray55", type = "l", lty = 1, lwd = 1.5)

PijaoCurve <- valuesInd[[21]]
PippinCurve <- valuesInd[[22]]
RingoCurve <- valuesInd[[23]]
plot(x = seq(1, length(valuesInd[[23]])), y = valuesInd[[23]], col = "gray55", type = "l", lty = 1, lwd = 1.5)

RonaldoCurve <- valuesInd[[24]]
SeiyaCurve <- valuesInd[[25]]
plot(x = seq(1, length(valuesInd[[25]])), y = valuesInd[[25]], col = "gray55", type = "l", lty = 1, lwd = 1.5)

SenorXCurve <- valuesInd[[26]]
SospeCurve <- valuesInd[[27]]
plot(x = seq(1, length(valuesInd[[27]])), y = valuesInd[[27]], col = "gray55", type = "l", lty = 1, lwd = 1.5)

TeslaCurve <- valuesInd[[28]]
ThanosCurve <- valuesInd[[29]]
plot(x = seq(1, length(valuesInd[[29]])), y = valuesInd[[29]], col = "gray55", type = "l", lty = 1, lwd = 1.5)

ThorinCurve <- valuesInd[[30]]
plot(x = seq(1, length(valuesInd[[30]])), y = valuesInd[[30]], col = "gray55", type = "l", lty = 1, lwd = 1.5)

TiggerCurve <- valuesInd[[31]]
YariguiCurve <- valuesInd[[32]]
ZimCurve <- valuesInd[[33]]
plot(x = seq(1, length(valuesInd[[33]])), y = valuesInd[[33]], col = "gray55", type = "l", lty = 1, lwd = 1.5)

n <- subset(song, Sector == "P")
n$Individuo <- as.character(n$Individuo)
n$Individuo <- as.factor(n$Individuo)
levels(n$Individuo)


## Plot the curves
windowsFonts(A = windowsFont("Times New Roman"))

plot(NULL, xlab = "Number of song types analysed", 
     ylab = "Cumulative count of song-types",xlim = c(0,415), 
     ylim =c(0,14), family = "A", cex.axis=1.1, cex.lab=1.15)

### Individuos del R
lines(x = seq(1, length(PijaoCurve)), y = PijaoCurve, col = "#003366", lty = 2)
lines(x = seq(1, length(GuechaCurve)), y = GuechaCurve, col = "#003366", lty = 3)
lines(x = seq(1, length(GuechaCurve)), y = GuechaCurve, col = "#003366", lty = 3)
lines(x = seq(1, length(ZimCurve)), y = ZimCurve, col = "#2457d1", lty = 3)
lines(x = seq(1, length(ZimCurve)), y = ZimCurve, col = "#2457d1", lty = 3)
lines(x = seq(1, length(YariguiCurve)), y = YariguiCurve, col = "#2457d1", lty = 2)
lines(x = seq(1, length(RingoCurve)), y = RingoCurve, col = "#1097ea", lty = 3)
lines(x = seq(1, length(RingoCurve)), y = RingoCurve, col = "#1097ea", lty = 3)
lines(x = seq(1, length(NiceforoCurve)), y = NiceforoCurve, col = "#1097ea", lty = 2)
lines(x = seq(1, length(TeslaCurve)), y = TeslaCurve, col = "#89bced", lty = 3)
lines(x = seq(1, length(TeslaCurve)), y = TeslaCurve, col = "#89bced", lty = 3)
lines(x = seq(1, length(MorrisonCurve)), y = MorrisonCurve, col = "#89bced", lty = 2)
lines(x = seq(1, length(TeslaCurve)), y = TeslaCurve, col = "#89bced", lty = 3)
lines(x = seq(1, length(TeslaCurve)), y = TeslaCurve, col = "#89bced", lty = 3)
lines(x = seq(1, length(MorrisonCurve)), y = MorrisonCurve, col = "#89bced", lty = 2)
lines(x = seq(1, length(MarleyCurve)), y = MarleyCurve, col = "#00ebf5", lty = 2)

## Individuos del N
lines(x = seq(1, length(DumoulinCurve)), y = DumoulinCurve, col = "#006666", lty = 4)
lines(x = seq(1, length(DumoulinCurve)), y = DumoulinCurve, col = "#006666", lty = 4)
lines(x = seq(1, length(FrankCurve)), y = FrankCurve, col = "#6ac671", lty = 4)
lines(x = seq(1, length(FrankCurve)), y = FrankCurve, col = "#6ac671", lty = 4)
lines(x = seq(1, length(JarvisCurve)), y = JarvisCurve, col = "#006666", lty = 1)
lines(x = seq(1, length(JimmyCurve)), y = JimmyCurve, col = "#6ac671", lty = 1)
lines(x = seq(1, length(SeiyaCurve)), y = SeiyaCurve, col = "#97e083", lty = 4)
lines(x = seq(1, length(SeiyaCurve)), y = SeiyaCurve, col = "#97e083", lty = 4)
lines(x = seq(1, length(SospeCurve)), y = SospeCurve, col = "#97e083", lty = 1)
lines(x = seq(1, length(ThanosCurve)), y = ThanosCurve, col = "#a0e53a", lty = 4)
lines(x = seq(1, length(ThanosCurve)), y = ThanosCurve, col = "#a0e53a", lty = 4)
lines(x = seq(1, length(TiggerCurve)), y = TiggerCurve, col = "#a0e53a", lty = 1)


####### Individuos del P
lines(x = seq(1, length(AntonioCurve)), y = AntonioCurve, col = "#660066", lty = 6)
lines(x = seq(1, length(AntonioCurve)), y = AntonioCurve, col = "#660066", lty = 6)
lines(x = seq(1, length(CamusCurve)), y = CamusCurve, col = "#57176e", lty = 5)
lines(x = seq(1, length(JuanguiCurve)), y = JuanguiCurve, col = "#6600c2", lty = 6)
lines(x = seq(1, length(JuanguiCurve)), y = JuanguiCurve, col = "#6600c2", lty = 6)
lines(x = seq(1, length(SenorXCurve)), y = SenorXCurve, col = "#6600c2", lty = 5)
lines(x = seq(1, length(ThorinCurve)), y = ThorinCurve, col = "#8fa3f5", lty = 6)
lines(x = seq(1, length(ThorinCurve)), y = ThorinCurve, col = "#8fa3f5", lty = 6)


legend ("topleft", legend = c ("N","N","R","R","P","P"), 
        title = "Sector", lty = c(1,4,2,3,5,6), col = c("#a0e53a","#a0e53a","#1097ea","#1097ea","#6600c2","#6600c2"), cex = 0.7)



####### Manuscript plot #########
AntonioCurve <- valuesCurve(song, "Antonio")
SenorXCurve <- valuesCurve(song, "SenorX")
DanyCurve <- valuesCurve(song, "Dany")
DumoulinCurve <- valuesCurve(song, "Dumoulin")
GaryCurve <- valuesCurve(song, "Gary")
Niceforo <- valuesCurve(song, "Niceforo")
SeiyaCurve <- valuesCurve(song, "Seiya")
SospeCurve <- valuesCurve(song, "Sospe") 
TeslaCurve <- valuesCurve(song, "Tesla")
ThanosCurve <- valuesCurve(song, "Thanos")
ZimCurve <- valuesCurve(song, "Zim")
AsdrubalCurve <- valuesCurve(song, "Asdrubal")


windowsFonts(A = windowsFont("Arial"))

plot(NULL, xlab = "Number of song types analysed", 
     ylab = "Cumulative count of song-types",xlim = c(0,415), 
     ylim =c(0,14), family = "A", cex.axis=1.1, cex.lab=1.15)

lines(x = seq(1, length(AntonioCurve)), y = AntonioCurve, col = "gray30", lty = 4)
lines(x = seq(1, length(SenorXCurve)), y = SenorXCurve, col = "gray50", lty = 1)
lines(x = seq(1, length(MarleyCurve)), y = MarleyCurve, col = "black", lty = 6)
lines(x = seq(1, length(JuanguiCurve)), y = JuanguiCurve, col = "gray30", lty = 5)
lines(x = seq(1, length(GuechaCurve)), y = GuechaCurve, col = "black", lty = 3)
lines(x = seq(1, length(SospeCurve)), y = SospeCurve, col = "gray40", lty = 2)


####### Manuscript plot #########
AntonioCurve <- valuesCurve(song, "Antonio")
SenorXCurve <- valuesCurve(song, "SenorX")
DanyCurve <- valuesCurve(song, "Dany")
DumoulinCurve <- valuesCurve(song, "Dumoulin")
GaryCurve <- valuesCurve(song, "Gary")
ChainsCurve <- valuesCurve(song, "Chains")
SeiyaCurve <- valuesCurve(song, "Seiya")
SospeCurve <- valuesCurve(song, "Sospe") 
TeslaCurve <- valuesCurve(song, "Tesla")
ThanosCurve <- valuesCurve(song, "Thanos")
ZimCurve <- valuesCurve(song, "Zim")
MarleyCurve <- valuesCurve(song, "Marley")
AsdrubalCurve <- valuesCurve(song, "Asdrubal")
PippinCurve <- valuesCurve(song, "Pippin")


tiff("Repertoire_Curve.tiff", units="in", width=13, height=7, res=400)

plot(NULL, xlab = "Number of song types analysed", 
     ylab = "Cumulative count of song-types",xlim = c(1,500), 
     ylim =c(1,14), family = "A", cex.axis=1.1, cex.lab=1.15)

lines(x = seq(1, length(AntonioCurve)), y = AntonioCurve, col = "#8c510a", lty = 1, lwd = 2)
lines(x = seq(1, length(SenorXCurve)), y = SenorXCurve, col = "#d8b365", lty = 1, lwd = 2)
lines(x = seq(1, length(DanyCurve)), y = DanyCurve, col = "#8c510a", lty = 3, lwd = 2)
lines(x = seq(1, length(DumoulinCurve)), y = DumoulinCurve, col = "#f6e8c3", lty = 1, lwd = 2)
lines(x = seq(1, length(GaryCurve)), y = GaryCurve, col = "#bf812d", lty = 3, lwd = 2)
lines(x = seq(1, length(ChainsCurve)), y = ChainsCurve, col = "#dfc27d", lty = 3, lwd = 2)
lines(x = seq(1, length(SeiyaCurve)), y = SeiyaCurve, col = "#c7eae5", lty = 1, lwd = 2)
lines(x = seq(1, length(SospeCurve)), y = SospeCurve, col = "#5ab4ac", lty = 1, lwd = 2)
lines(x = seq(1, length(MarleyCurve)), y = MarleyCurve, col = "#f6e8c3", lty = 3, lwd = 2)
lines(x = seq(1, length(TeslaCurve)), y = TeslaCurve, col = "#80cdc1", lty = 3, lwd = 2)
lines(x = seq(1, length(ThanosCurve)), y = ThanosCurve, col = "#01665e", lty = 1, lwd = 2)
lines(x = seq(1, length(ZimCurve)), y = ZimCurve, col = "#35978f", lty = 3, lwd = 2)
lines(x = seq(1, length(AsdrubalCurve)), y = AsdrubalCurve, col = "#01665e", lty = 3, lwd = 2)

legend("bottomright",
       c("Sector A","Sector B"), 
       lty =c(1,3))

dev.off()


library(dplyr)


song$TipoCanto <- as.character(song$TipoCanto)
ind <- levels(song$Individuo)
songTypes <- list()

for(i in 1:length(ind)){
  songTypes[[i]] <- levels(as.factor((song %>% filter(Individuo == ind[i]))[,3]))
}


a <- read.delim("numSharedSongs.txt", h=T)
library(dplyr)

mean(a$NumSharedSongs)
sd(a$NumSharedSongs)

A <- a %>% filter(Sector == "A") %>% group_by(ID) %>% summarise (mean = mean(NumSharedSongs), sd = sd(NumSharedSongs))
B <- a %>% filter(Sector == "B") %>% group_by(ID) %>% summarise (mean = mean(NumSharedSongs), sd = sd(NumSharedSongs))

mean(A$mean)
mean(A$sd[!is.na(A$sd)])
A$ID <- as.factor(as.character(A$ID))
length(levels(A$ID))

mean(B$mean)
mean(B$sd)
B$ID <- as.factor(as.character(B$ID))
length(levels(B$ID))

ind.A <- song %>% filter(Sector == "N" | Sector == "P") %>% filter(Individuo != "Pippin")
ind.P <- song %>% filter(Sector == "N" | Sector == "P")

#Add an analysis comparing neighbors vs non-neigbors within the same sector


library(dplyr)
song %>% group_by(TipoGrabacion) %>% summarise(n=sum(Cantidad))

datetxt <- as.Date(song$Fecha)

df <- data.frame(date = datetxt,
                 year = as.numeric(format(datetxt, format = "%Y")),
                 month = as.numeric(format(datetxt, format = "%m")),
                 day = as.numeric(format(datetxt, format = "%d")))
df %>% group_by(month) %>% summarise(n=n())

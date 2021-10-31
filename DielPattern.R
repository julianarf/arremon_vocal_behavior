# Diel pattern of vocal output

############### Graphs  ################

## Load ggplot2 to create the plots
library(ggplot2)
## Load dplyr to apply functions and do weird things with the dataframes >%>
library(dplyr)

## Getting the data
setwd("C:/Users/Juliana/OneDrive - Universidad de los andes/Tesis Biologia/Analysis/DatosR")
### Song data
song <- read.delim("DataSongsR.txt", h=T)
### Call data
calls <- read.delim("DatosLlamadosR.txt", h=T)

## Filter the song data for automatic recordings only
autoSong <- song %>% filter(TipoGrabacion == "Automatica") %>% filter(Dueto =="N")

## Change the hours with :30 to :00 for the song data
### Un-factorize 
autoSong$Hora <- as.character(autoSong$Hora)
for (i in 5:18){
  hourOld <- capture.output(cat(i,":30",sep =""))
  hourNew <- capture.output(cat(i,":00",sep =""))
  autoSong[autoSong$Hora == hourOld, "Hora"] <- hourNew
}
###Re-factorize
autoSong$Hora <- as.factor(autoSong$Hora)


## Change the hours with :30 to :00 for the call data
### Un-factorize 
calls$Hora <- as.character(calls$Hora)
for (i in 5:18){
  hourOld <- capture.output(cat(i,":30",sep =""))
  hourNew <- capture.output(cat(i,":00",sep =""))
  calls[calls$Hora == hourOld, "Hora"] <- hourNew
}
###Re-factorize
calls$Hora <- as.factor(calls$Hora)



############### Pattern of diel variation for songs ######################
## Sum the number of vocalizations per hour per individual and then join all the individuals in a single dataframe

### Names of each individuals wich will be used in the for loop
autoSong$Individuo <- as.factor(autoSong$Individuo)
ind <- levels(autoSong$Individuo)
### Create a empty dataframe
autoSongDf <- data.frame()

### Loop through the names aggregating the values for each hour between 5:00 to 18:00 
for (i in 1:length(ind)){
  sig <- subset(autoSong, Individuo == ind [i])
  a <- aggregate(sig$Cantidad, by = list(Hour=sig$Hora), FUN = sum)
  name <- rep(ind[i],length(a$Hour))
  type <- rep("Song",length(a$Hour))
  b <- data.frame(name, type, a)
  autoSongDf <- rbind(autoSongDf, b)
}

### Get the sum and standar deviation for each hour

autoSongDfPlot <- autoSongDf[,-c(1,2)]

dielSong <- autoSongDfPlot %>% group_by(Hour) %>% summarise(sum = sum(x), sd = sd(x))


### Re-organize the factors starting at 5:00 and ending at 18:00
dielSong$Hour <- factor(dielSong$Hour, levels = c("5:00", "6:00", "7:00", "8:00", "9:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00"))

### plot
songPlot <- ggplot(dielSong) + 
  ggtitle ("Cantos")+
  geom_bar( aes(x=Hour, y=sum), colour = "grey92", stat="identity", fill="grey80", alpha=0.7) + 
  geom_errorbar( aes(x=Hour, ymin=sum-sd, ymax=sum+sd), width=0.35, colour="darkgoldenrod3", alpha=0.9, size=0.5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  labs( x = "Hora", y = "# de vocalizaciones")

songPlot




############### Pattern of diel variation for calls  ####################
## Sum the number of vocalizations per hour per individual and then join all the individuals in a single dataframe

### Names of each individuals wich will be used in the for loop
calls$Individuo <- as.factor(calls$Individuo)
ind <- levels(calls$Individuo)
### Create a empty dataframe
callsDf <- data.frame()

### Loop through the names aggregating the values for each hour between 5:00 to 18:00 
for (i in 1:length(ind)){
  sig <- subset(calls, Individuo == ind [i])
  a <- aggregate(sig$Total, by = list(Hour=sig$Hora), FUN = sum)
  name <- rep(ind[i],length(a$Hour))
  type <- rep("Calls",length(a$Hour))
  b <- data.frame(name, type, a)
  callsDf <- rbind(callsDf, b)
}


### Get the sum and standar deviation for each hour
callsDfPlot <- callsDf[,-c(1,2)]
dielCalls <- callsDfPlot %>% group_by(Hour) %>% summarise(sum = sum(x), sd = sd(x))

### Re-organize the factors starting at 5:00 and ending at 18:00
dielCalls$Hour <- factor(dielCalls$Hour, levels = c("5:00", "6:00", "7:00", "8:00", "9:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00"))

### plot
callsPlot <- ggplot(dielCalls) + 
  ggtitle ("Llamados")+
  geom_bar( aes(x=Hour, y=sum), colour = "grey92", stat="identity", fill="grey80", alpha=0.7) + 
  geom_errorbar( aes(x=Hour, ymin=sum-sd, ymax=sum+sd), width=0.35, colour="darkgoldenrod3", alpha=0.9, size=0.5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  labs( x = "Hora", y = "# de vocalizaciones")


callsPlot




############### Pattern of diel variation for songs and calls ###############

###Bind both dataframes
both <- rbind(autoSongDf,callsDf)
bothDf <- both[,-c(1,2)]

### Get the sum and standar deviation for each hour
dielBoth <- bothDf %>% group_by(Hour) %>% summarise_each(funs(sum,sd))

### Re-organize the factors starting at 5:00 and ending at 18:00
dielBoth$Hour <- factor(dielBoth$Hour, levels = c("5:00", "6:00", "7:00", "8:00", "9:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00"))

### plot
bothPlot <- ggplot(dielBoth) + 
  ggtitle ("Todas las vocalizaciones")+
  geom_bar( aes(x=Hour, y=sum), colour = "grey92", stat="identity", fill="grey92", alpha=0.7) + 
  geom_errorbar( aes(x=Hour, ymin=sum-sd, ymax=sum+sd), width=0.3, colour="darkgoldenrod3", alpha=1, size=0.5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  labs( x = "Hora", y = "# de vocalizaciones")


#### Plot both calls and songs

dielCallsPlot <- data.frame(dielCalls, Vocalization = rep("Call",length(dielCalls$Hour)))
dielSongsPlot <- data.frame(dielSong, Vocalization = rep("Song",length(dielSong$Hour)))
callSongPlot <- rbind(dielCallsPlot, dielSongsPlot)

callAndSong <- ggplot(data=callSongPlot, aes(x=Hour, y=sum, fill=Vocalization)) +
  geom_bar(stat="identity", position=position_dodge(), alpha=0.7) + 
  geom_errorbar( aes(x=Hour, ymin=sum-sd, ymax=sum+sd), position=position_dodge(0.9), 
                 width=0.3, colour="black", alpha=1, size=0.7) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), 
      text = element_text(size = 13), axis.title.x = element_text(vjust=-0.5),
      axis.title.y = element_text(vjust=+0.8)) +
  labs(x = "Hour", y = "Number of vocalizations")
  

bothPlot <- callAndSong + scale_fill_manual(values=c("gray40","gray75")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))

tiff("Dial_Song_Calls.tiff", units="in", width=12, height=8, res=300)
bothPlot
dev.off()


############### Is there a statistic difference in the vocalization production among hours? ############################


### Bind complete dataframes
allVocalizations <- rbind(autoSongDf, callsDf)

### Test for all vocalizations
library(nlme)

m1.nlme <- lme(x ~ Hour,
             random = ~ 1|name,
             data = allVocalizations)
summary(m1.nlme)
anova(m1.nlme)

library(emmeans)
emmeans(m1.nlme, list(pairwise ~ Hour), adjust = "tukey")

### Test for songs
m2.lmer <- lmer(x ~ Hour + (1|name),data = autoSongDf)
summary(m2.lmer)
anova(m2.lmer)

emmeans(m2.lmer, list(pairwise ~ Hour), adjust = "tukey")


### Test for calls
m3.lmer <- lmer(x ~ Hour + (1|name), data = callsDf) 
summary(m3.lmer)
anova(m3.lmer)

emmeans(m3.lmer, list(pairwise ~ Hour), adjust = "tukey")





############### Mean and sd in the number of calls #######################


### Long calls ###

#No tengo muy claro que es lo que estoy haciendo, luego entenderé, espero

### Names of each individuals wich will be used in the for loop
ind <- levels(calls$Individuo)
calls$Hora <- factor(calls$Hora, levels = c("5:00", "6:00", "7:00", "8:00", "9:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00"))
hours <- levels(calls$Hora)

### Create a empty matrix for long calls. In the columns wil be the pair and in the rows the hour
longCall <- matrix (ncol = length(ind), nrow = length(hours))

for (i in 1:length(ind)){
  sig <- subset(calls, Individuo == ind [i])

    ### Loop through the names and hours doing something that I clearly do not understand yet, 
    ### but who cares? not me
    
  for (j in 1:length(hours)){
  valSig <- subset(sig, Hora == hours[j]) 
  sumSig <- sum(valSig$Largo)
  longCall[j,i] <- sumSig 
  }
}

longMean <- vector()

for(i in 1:length(ind)){
  longMean[i]<- mean(longCall[,i])
}
mean(longMean)
sd(longMean)




### Short calls ###

### Create a empty matrix for short calls. In the columns wil be the pair and in the rows the hour
shortCall <- matrix (ncol = length(ind), nrow = length(hours))

### Loop through the names and hours doing something that I do not clearly understand yet, but how cares? not me
for (i in 1:length(ind)){
  sig <- subset(calls, Individuo == ind [i])
  for (j in 1:length(hours)){
    valSig <- subset(sig, Hora == hours[j]) 
    sumSig <- sum(valSig$Corto)
    shortCall[j,i] <- sumSig 
  }
}

d <- as.data.frame(shortCall)
d[is.na(d)] <- 0
shortCall <- as.matrix(d)

shortMean <- vector()

for(i in 1:length(hours)){
  shortMean[i]<- mean(shortCall[i,])
}
mean(shortMean)
sd(shortMean)


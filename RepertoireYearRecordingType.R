library(dplyr)
library(lmerTest)
library(emmeans)
library(ggplot2)

setwd("C:/Users/Juliana/OneDrive - Universidad de los andes/Tesis Biologia/Analysis/DatosR")

data <- read.delim("DataSongsR.txt", h=T)
data$Year <- format(as.Date(data$Fecha,"%d/%m/%Y"),"%Y")

all <- data %>% group_by(Individuo) %>% summarise(repertoireSize = n_distinct(TipoCanto))

auto <- data %>% filter(TipoGrabacion == "Automatica") 
a2019_auto <- auto %>% filter(Year == 2019) %>% group_by(Individuo) %>% summarise(repertoire = n_distinct(TipoCanto), recordings = n())
#a2019_auto <- data.frame(a2019_auto, typeRecording = rep(ncol(a2019_auto)))
a2018 <- data %>% filter(Year == 2018) %>% group_by(Individuo) %>% summarise(repertoire = n_distinct(TipoCanto), recordings = n())

typeRecording <- c(rep("Automatic",nrow(a2019_auto)), rep("Focal",nrow(a2018)))
auto2019_all2018 <- rbind(a2019_auto, a2018)
auto2019_all2018 <- data.frame(auto2019_all2018, typeRecording)

auto2019_all2018_excludingIDs <- auto2019_all2018[-c(20,26,30,32,42,52),]

lm1 <- lmer(repertoire ~ typeRecording + recordings + (1|Individuo), data = auto2019_all2018) 
summary(lm1)
anova(lm1)
rand(lm1)


a2019 <- data %>% filter(Year == 2019) %>% group_by(Individuo) %>% summarise(repertoire = n_distinct(TipoCanto), recordings = n())
year <- c(rep("2019a",nrow(a2019)), rep("2018a",nrow(a2018)))
all2019_all2018 <- rbind(a2019, a2018)
all2019_all2018 <- data.frame(all2019_all2018, year)

#auto2019_all2018_excludingIDs <- auto2019_all2018[-c(20,26,30,32,42,52),] No entiendo por qué hago esto ...

lm1 <- lmer(repertoire ~ year + recordings + (1|Individuo), data = all2019_all2018) 
summary(lm1)
anova(lm1)
rand(lm1)


all <- data %>% 
            group_by(Individuo) %>% 
            summarise(repertoire = n_distinct(TipoCanto), recordings = n()) 
#            %>% filter(repertoire > 5)

pm1 <- glm(repertoire ~ recordings, family="poisson", data = all) 
summary(pm1)

## Descriptive analysis ---------------------------------------------------------

a2019 <- data %>% filter(Year == 2019) %>% group_by(Individuo) %>% summarise(repertoire2019 = n_distinct(TipoCanto), recordings2019 = n())
a2018 <- data %>% filter(Year != 2019) %>% group_by(Individuo) %>% summarise(repertoire2018 = n_distinct(TipoCanto), recordings2018 = n())
a2019_8 <- data.frame(a2019,repertorio2018 = a2018$repertoire2018, recordings2018 = a2018$recordings2018)

auto <- data %>% filter(TipoGrabacion == "Automatica")
focal <- data %>% filter(TipoGrabacion == "Focal")  %>% group_by(Individuo) %>% summarise(repertoireFocal = n_distinct(TipoCanto), recordingsFocal = n())
auto_focal <- data.frame(auto, focal[,c(2,3)])


data2019 <- data %>% filter(Year == 2019)
data2019$TipoCanto <- as.character(data2019$TipoCanto)

data2018 <- data %>% filter(Year != 2019)
data2018$TipoCanto <- as.character(data2018$TipoCanto)

dataAuto <- data %>% filter(TipoGrabacion == "Automatica")
dataAuto$TipoCanto <- as.character(dataAuto$TipoCanto)

dataFocal <- data %>% filter(TipoGrabacion == "Focal")
dataFocal$TipoCanto <- as.character(dataFocal$TipoCanto)

data2019Focal <- data2019 %>% filter(TipoGrabacion == "Focal")
data2019Focal$TipoCanto <- as.character(data2019Focal$TipoCanto)


repertoire <- matrix(ncol = 6, nrow = length(levels(as.factor(data$Individuo))))
repertoire[,1] <- levels(as.factor(data$Individuo))
colnames(repertoire) <- c("ID","2019","2018","Auto","Focal", "2019_Focal")

for(i in 1:nrow(repertoire)){
  sigInd <- levels(as.factor(data$Individuo))[i]
  
  rep2019 <- capture.output(cat(levels(as.factor((subset(data2019, Individuo == sigInd))$TipoCanto)), sep="/"))
  if(identical(rep2019,character(0))){
    rep2019 <- 0
  }
  
  rep2018 <- capture.output(cat(levels(as.factor((subset(data2018, Individuo == sigInd))$TipoCanto)), sep="/"))
  if(identical(rep2018,character(0))){
    rep2018 <- 0
  }
  
  repAuto <- capture.output(cat(levels(as.factor((subset(dataAuto, Individuo == sigInd))$TipoCanto)), sep="/"))
  if(identical(repAuto,character(0))){
    repAuto <- 0
  }
  
  repFocal <- capture.output(cat(levels(as.factor((subset(dataFocal, Individuo == sigInd))$TipoCanto)), sep="/"))
  if(identical(repFocal,character(0))){
    repFocal <- 0
  }
  
  rep2019Focal <- capture.output(cat(levels(as.factor((subset(data2019Focal, Individuo == sigInd))$TipoCanto)), sep="/"))
  if(identical(rep2019Focal,character(0))){
    rep2019Focal <- 0
  }
  
  repertoire[i,2] <- rep2019
  repertoire[i,3] <- rep2018
  repertoire[i,4] <- repAuto
  repertoire[i,5] <- repFocal
  repertoire[i,6] <- rep2019Focal
  
}

write.csv(repertoire, file="Repertorios.csv")


## Random stuff -----------
a <- data %>% filter(Tipo.de.respuesta != "")
a <- na.omit(a)
a$Tipo.de.respuesta <- as.character(a$Tipo.de.respuesta)
a$Tipo.de.respuesta <- factor(a$Tipo.de.respuesta)
a$Dueto[a$Dueto == "S "] <- 1
a$Dueto[a$Dueto == "S"] <- 1
a$Dueto[a$Dueto == "N"] <- 0
a$Dueto <- as.numeric(a$Dueto)

glm1 <- glmer(Dueto~TipoGrabacion+(1|Individuo), 
              family = "binomial"(link = "logit"), data = a)
summary(glm1)
anova(glm1)
emmeans(glm1, list(pairwise ~ TipoGrabacion), adjust = "tukey")

library(ggmosaic)
b <- a %>% group_by(TipoGrabacion) %>% summarise (Yes = sum(Dueto)/n()) 
No <- 1-(b$Yes)
responseTo <- rep(b$TipoGrabacion,2)
Female <- c(rep("Yes",2),rep("No",2))

c <- data.frame(responseTo, Female, value = c(b$Yes,No))


ggplot(data = c, aes(x = responseTo, y = value, fill = Female))+
  geom_bar(stat="identity")






data$Month <- format(as.Date(data$Fecha,"%d/%m/%Y"),"%m")
a <- data %>% group_by(Month) %>% summarise(n=n())

# Repertoire Size Estimation

## Modified by: Juliana Rodríguez Fuentes
## Original script from - Estimating repertoire size in a songbird: A comparison of three techniques -
## Last updated: 10/06/2020 17:57

setwd("C:/Users/Juliana/OneDrive - Universidad de los andes/Tesis Biologia/Analysis/DatosR")
raw.song.data <- read.delim("DataSongsR_PlusDateDuet.txt", h=T)

# ---------------------------------
#ind <- levels(raw.song.data$Individuo)
#test.data <- matrix(nrow = 500, )
#a <- subset(raw.song.data, Individuo == "Chains")
#sum(a$Cantidad)
#aVector <- as.character(rep(a$TipoCanto,a$Cantidad))[1:460]
#aST <- as.factor(aVector)

#session <- raw.song.data %>% group_by(Individuo) %>% summarise(mean = mean(Cantidad), sd = sd(Cantidad))
#mean(session$mean)
#mean(session$sd)

#sessionsize <- 20 ## preliminar
#subsetsize <- length(aST)
#numsessions <- subsetsize/sessionsize # calculate number of capture occasions
#capturehistory <- matrix(nrow =length(levels(aST)), ncol = numsessions) #Tenia una coma ....
#length(levels(aST))

#rownames(capturehistory) <- levels(aST)

#for (session in 1:numsessions) {  # run one loop for each capture occasion and populate matrix 'capturehistory'
 # capturehistory[, session] <- as.numeric(rownames(capturehistory) %in% aST[(session*sessionsize+2-sessionsize):(session*sessionsize+1)])
#}

# use Darroch's Mh model to estimate repertoire size; assumes closed population and heterogeneity among song types
#closedp.0(capturehistory)$results[4,1]

#mean(raw.song.data$Cantidad)


# ------------------------------------------
library(dplyr)

song.types.all <- raw.song.data %>% group_by(Individuo) %>% summarise(st = n_distinct(TipoCanto))

songs <- raw.song.data %>% group_by(Individuo) %>% summarise(sum = sum(Cantidad))
mean(songs$sum)
sd(songs$sum)


n.songs <- 200
ind.Analisis <- data.frame(songs %>% filter(sum > n.songs))
n.ind <- nrow(ind.Analisis)

test.raw.data <- matrix(ncol = n.ind+1, nrow = n.songs+1)
test.raw.data[,1] <- c(0:n.songs)
colnames(test.raw.data) <- c("Switches",as.character(ind.Analisis$Individuo))


for (i in 1:n.ind){
  sig.ind <- ind.Analisis[i,]
  values.sig <- subset(raw.song.data, Individuo == sig.ind$Individuo)
  song.vector <- vector()
  for (j in 1:nrow(values.sig)){
    sig.song <- rep(values.sig[j,3], values.sig[j,4])
    song.vector <- c(song.vector, sig.song)
  }
  test.raw.data[(2:(n.songs+1)),i+1] <- song.vector[1:n.songs]
}




Nmales <- ncol(test.raw.data)-1  # count number of males in sample (ie. 40)
Nmax <- 30  # set to a value that is greater than the maximum number of unique song types that could be in a repertoire
Nswitches <- nrow(test.raw.data)-1  # count total number of song type switches in the sample (ie. 150)
Niterations <- 100	# set to the number of iterations to be used in Monte Carlo simulations; analysis in manuscript
# used 100,000 iterations, but that took several hours to run
sessionsize <- 10	# set the number of song type switches that constitute a capture occasion for the
#  capture-recapture analysis; the number should divide evenly into all subset sizes included in analysis


cumulative.data <- matrix(, nrow=nrow(test.raw.data)-1, ncol=ncol(test.raw.data))
cumulative.data[,1] <- test.raw.data[2:nrow(test.raw.data),1]
colnames(cumulative.data) <- colnames(test.raw.data)
colnames(cumulative.data)[1] <- "Switches"
for (c in 1:(ncol(cumulative.data)-1)) {
  for(r in 1:nrow(cumulative.data)) {
    cumulative.data[r,c+1] <- length(unique(test.raw.data[2:(r+1),c+1]))
  }
}

test.raw.data[1,2:(Nmales+1)] <- cumulative.data[150,2:(Nmales+1)]

# create blank matrices that will show the number of song types expected by Coupon Collector and Curve Fitting
# techniques for each possible repertoire size and each possible number of song type switches sampled
Nexpect.coupon.100 <- Nexpect.coupon.125 <- Nexpect.coupon.150 <- matrix(, nrow=Nswitches, ncol=Nmax)  
Nexpect.curve <- matrix(, nrow=Nswitches, ncol=Nmax)

# create results file in long format; include columns for male ID, estimation technique, subset size,
# subset size centered on zero, true repertoire size, predicted repertoire size, accuracy, and precision;
data.long <- data.frame(matrix(, nrow=Nmales*9, ncol=8))
colnames(data.long) <- c("male", "technique", "subset", "subset.c", "true", "predicted", "accuracy", "precision")
# populate columns male, technique, subset, and subset.c
data.long[, "male"]<-rep(colnames(test.raw.data)[2:(Nmales+1)],9)
data.long[,"technique"]<-c(rep("curve",Nmales*3), rep("coupon.unequal",Nmales*3), rep("capture",Nmales*3))
data.long[,"subset"]<-rep(c(rep(150,Nmales), rep(175,Nmales), rep(200,Nmales)),3)
data.long[,"subset.c"]<-data.long[,3]-mean(data.long[,3])
# populate true repertoire size column using data from first row of object 'test.raw.data'
data.long[,"true"]<-rep(t(test.raw.data[1,2:(Nmales+1)]),9)
data.long$male <- factor(data.long$male)
data.long$technique <- factor(data.long$technique)


# PART 2: CALCULATE PREDICTED NUMBER OF SONG TYPES FOR EACH POSSIBLE REPERTOIRE SIZE AND NUMBER OF SONG TYPE
# SWITCHES USING THE CURVE FITTING AND COUPON COLLECTOR TECHNIQUES

# Calculate Zipf coefficient (s) for each subset size (100, 125, 150 song type switches)
# the coefficient is the average log(frequency) x log(rank) regression slope among males
s <- vector(length=3)
rank.data <- as.data.frame(matrix(ncol=Nmales, nrow=max(cumulative.data[,2:Nmales+1])))
for (w in 1:length(s)) {
  w.new <- w+4
  avg.slope <- 0
  for (x in 1:Nmales) {
    rank.list <- as.vector(sort(table(test.raw.data[2:(w.new*25+1),(x+1)]), decreasing=T))
    rank.data[1:max(cumulative.data[1:(w.new*25),x+1]), x] <- t(t(rank.list))
    log.log <- lm(log(rank.list) ~ log(1:length(rank.list)))
    avg.slope <- avg.slope + log.log$coefficients[2]
  }
  avg.slope <- avg.slope / Nmales
  s[w] <- abs(avg.slope)
}



for (t in 1:Nmax) {  # run one loop for each possible repertoire size (i.e. 1 to Nmax)
  
  # calculate song type selection probabilities for Coupon Collector technique using Zipf's Law
  # create empty vector to store selection probabilities for each song type in repertoire
  zipf.probs.100 <- zipf.probs.125 <- zipf.probs.150 <- vector(length=t)
  for (m in 1:t) {  # run loop for each song type (i.e., each rank in repertoire of t song types; rank 1=most common)
    # create term for denominator of Zipf formula
    zipf.denominator100 <- zipf.denominator125 <- zipf.denominator150 <- 0
    for (n in 1:t) {  # run one loop for each song type in repertoire
      zipf.denominator100 <- zipf.denominator100 + (1/n^s[1]) # use summation to calculate denominator in Zipf formula
      zipf.denominator125 <- zipf.denominator125 + (1/n^s[2])
      zipf.denominator150 <- zipf.denominator150 + (1/n^s[3])
    }
    zipf.probs.100[m] <- (1/m^s[1]) / zipf.denominator100   # calculate Zipf probability for current song type in current repertoire size
    zipf.probs.125[m] <- (1/m^s[2]) / zipf.denominator125
    zipf.probs.150[m] <- (1/m^s[3]) / zipf.denominator150
  }
  
  for (k in 1:Nswitches) {  # run one loop for each song type switch included in sample (i.e. 1 to Nswitches)
    # create a cumulative count of the number of unique song types in each Monte Carlo simulation, for each subset size
    counter.coupon.probs.100 <- counter.coupon.probs.125 <- counter.coupon.probs.150 <- 0
    
    for (f in 1:Niterations) {  # run loop for each iteration in Monte Carlo simulation
      # draw sample of k songs at random from repertoire of t song types, with replacement and with selection
      # probabilities determined from Zipf distribution
      counter.coupon.probs.100 <- counter.coupon.probs.100 + length(unique(sample(1:t, size = k, replace=TRUE, prob=zipf.probs.100)))
      counter.coupon.probs.125 <- counter.coupon.probs.125 + length(unique(sample(1:t, size = k, replace=TRUE, prob=zipf.probs.125)))
      counter.coupon.probs.150 <- counter.coupon.probs.150 + length(unique(sample(1:t, size = k, replace=TRUE, prob=zipf.probs.150)))
    }
    
    Nexpect.coupon.100[k, t] <- counter.coupon.probs.100/Niterations # populate matrices 'Nexpect.coupon'
    Nexpect.coupon.125[k, t] <- counter.coupon.probs.125/Niterations
    Nexpect.coupon.150[k, t] <- counter.coupon.probs.150/Niterations
    
    # use formula in Wildenthal (1965) to populate matrix 'Nexpect.curve' with estimates from Curve fitting technique
    Nexpect.curve[k,t] <- t*(1-exp(1)^((-1)*k/t))	
  }
}


##########################################################################################################################

# PART 3: USE LEAST SQUARES APPROACH TO ESTIMATE REPERTOIRE SIZE FOR EACH MALE AND SUBSET SIZE
# BASED ON DIFFERENCE BETWEEN OBSERVED VALUES AND EXPECTED VALUES GENERATED FROM COUPON COLLECTOR AND
# CURVE FITTING TECHNIQUES

for (i in 1:Nmales) {  # run one loop for each male in dataset
  # create matrix showing absolute differences between expected and observed number
  # of song types for each Nmax for male i and for each subset and each technique
  ms150curve <- abs(Nexpect.curve[1:150,]-cumulative.data[1:150,i+1])
  ms150coupon.100 <- abs(Nexpect.coupon.100[1:150,]-cumulative.data[1:150,i+1])
  ms150coupon.125 <- abs(Nexpect.coupon.125[1:150,]-cumulative.data[1:150,i+1])
  ms150coupon.150 <- abs(Nexpect.coupon.150[1:150,]-cumulative.data[1:150,i+1])
  
  # create vector containing sums of absolute differences for each Nmax, male, and subset size
  sms100curve <- colSums(ms150curve[1:100,])
  sms125curve <- colSums(ms150curve[1:125,])
  sms150curve <- colSums(ms150curve[1:150,])
  
  sms100coupon <- colSums(ms150coupon.100[1:100,])
  sms125coupon <- colSums(ms150coupon.125[1:125,])
  sms150coupon <- colSums(ms150coupon.150[1:150,])
  
  # enter Nmax corresponding to the minimum sum of absolute differences for each male and subset size
  data.long[(Nmales*1-Nmales+i),6] <- which(sms100curve == min(sms100curve))
  data.long[(Nmales*2-Nmales+i),6] <- which(sms125curve == min(sms125curve))
  data.long[(Nmales*3-Nmales+i),6] <- which(sms150curve == min(sms150curve))
  
  data.long[(Nmales*4-Nmales+i),6] <- which(sms100coupon == min(sms100coupon))
  data.long[(Nmales*5-Nmales+i),6] <- which(sms125coupon == min(sms125coupon))
  data.long[(Nmales*6-Nmales+i),6] <- which(sms150coupon == min(sms150coupon))
}




##########################################################################################################################

# PART 4: RUN CAPTURE-RECAPTURE ANALYSIS

library(Rcapture)  # see Rivest & Baillargeon (2014)

# create matrix of capture histories for each male; for a given male, the matrix will have one row for each unique song type
# and one column for each capture occasion; the cells contain zero if the song type was not observed and one if it was
for (numsubsets in 1:3) { # run one loop for each subset (100, 125, 150 song type switches)
  numsubsets.corrected <- numsubsets+3
  subsetsize <- numsubsets.corrected*25  # calculate subset size
  numsessions <- subsetsize/sessionsize  # calculate number of capture occasions
  
  for (male in 1:Nmales) {  # run one loop for each male
    # create a blank capture history for male
    capturehistory <- matrix(, nrow=length(unique(test.raw.data[2:(subsetsize + 1),male + 1])), ncol=numsessions)
    rownames(capturehistory) <- t(unique(test.raw.data[2:(subsetsize + 1), male + 1]))
    
    for (session in 1:numsessions) {  # run one loop for each capture occasion and populate matrix 'capturehistory'
      capturehistory[, session] <- t(as.numeric(rownames(capturehistory) %in% test.raw.data[(session*sessionsize+2-sessionsize):(session*sessionsize+1),male+1]))
    }
    
    # use Darroch's Mh model to estimate repertoire size; assumes closed population and heterogeneity among song types
    data.long[(Nmales*6 + numsubsets*Nmales - Nmales + male),6] <- closedp.0(capturehistory)$results[4,1]
  }
}

data.long %>% filter(technique == "capture") %>% filter(subset == 200) %>% summarise(mean = mean(predicted), sd = sd(predicted))
capure.recapture <- data.long %>% filter(technique == "capture") %>% filter(subset == 200)


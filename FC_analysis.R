#This reads in the *.dat data from a forced-choice
#experiment (TVW at the moment!)

#NOTE: Data cleaning & participant selection is lengthy and messy (See FCW_participants for details on fussout/other exclusion)
#Descriptives/analysis starts on line 314

#setwd("C:\\Users\\mekline\\Documents\\My Dropbox\\_Projects\\TransitiveVerbsWugging-FCW 2010\\Forced Choice\\TVW-twoquestions\\Analysis - Final Cogsci Submit")
setwd("/Users/mekline/Dropbox/_Projects/Wugging - Finished Experiments/2010 Forced Choice Wugging - TouchNoTouch/TVW-twoquestions-ANALYSIS/Analysis - Final Cogsci Submit")
#DATA LOADING & SHAPING
#libraries
library(reshape)

####TVW -from before automated touch trials
#Load in the data from the TVW files
mydata <- data.frame(NULL)

participants <- c(1,2,3,4,5,6,7,9,10,11,13,14,15,16,17) #fussed out & didn'trecord: 8, 12 

for(f in participants) {
	tryCatch({filename = paste('Data_full/TVW_', f, '.dat', sep='')}, finally="")
	tmp <- read.table(filename, header=FALSE, sep=" ")
	names(tmp) <- c("Subject", "Trial.Number", "Item.Number", "Item",
			 "First.Side", "Causal.Side", "Response.Causal", "Choice.Causal", "Response.NC", "Choice.NC", "Yesfirst")

	mydata <- rbind(mydata, tmp)
}

#Load in the data about participants and pretest/touch performance
pdata <- data.frame(NULL)
pdata <- read.table('Participants_FCW.csv', header=TRUE, sep=',')
mydata <- merge(mydata, pdata, by=c("Subject"))

#Add correct 'touch' data onto the mydata dataframe
trial.numbers <- c(1,2,3,4)

#loop over trial numbers and add the touch data correctly
touchdata <- data.frame(NULL)
for(i in trial.numbers) {

	tmp <- pdata[,c("Subject",paste("Item", i, sep="."),paste("Touching", i, sep="."))]
	names(tmp) <- c("Subject", "Item", "Touching")
	tmp$Trial.Number <- i
	touchdata <- rbind(touchdata,tmp)
	
}

mydata <- merge(mydata,touchdata, by=c("Subject", "Item", "Trial.Number"), all.x = TRUE)

####TVW_new -from with automated touch trials
mydata2 <- data.frame(NULL)
participants <- c(19,21,23,24,25,26,27,28,29,35,36,37,38,39) #fussed out, bilingual, pilot: 18, 20, 22, 30-34, 

for(f in participants) {
	tryCatch({filename = paste('Data_full/TVW_', f, '.dat', sep='')}, finally="")
	tmp <- read.table(filename, header=FALSE, sep=" ")
	names(tmp) <- c("Subject", "Trial.Number", "Item.Number", "Item",
			 "First.Side", "Causal.Side", "Response.Causal", "Choice.Causal", "Response.NC", "Choice.NC", "Yesfirst", "Touch.Key", "Choice.T")

	mydata2 <- rbind(mydata2, tmp)
}

##Relabel "Touching" data to match above
mydata2$Touching <- NA
mydata2[mydata2$Touch.Key=="z",]$Touching <- "left"
mydata2[mydata2$Touch.Key=="c",]$Touching <- "right"
mydata2$Touching <- as.factor(mydata2$Touching)


#Add the relevant participant data
mydata2 <- merge(mydata2, pdata, by=c("Subject"))


#Merge the two sets of data together
inter <- intersect(names(mydata), names(mydata2))

mydata <- subset(mydata, select=inter)
mydata2 <- subset(mydata2, select=inter)

mydata <- rbind(mydata, mydata2) #Probably doesn't work if some have extra columns - matches by first dataframe column names!


######
#SUBJECT REMOVAL - FUSS OUT/AGE

#Remove subjects who are too young or fussed out of study
mydata <- mydata[(mydata$Age.Years > 2),]

#Remove fuss-outs (double check)
mydata <- mydata[(mydata$Subject != 5),] #Fuss outs
mydata <- mydata[(mydata$Subject != 8),]
mydata <- mydata[(mydata$Subject != 12),]
mydata <- mydata[(mydata$Subject != 18),] #Fuss outs
mydata <- mydata[(mydata$Subject != 20),]
mydata <- mydata[(mydata$Subject != 22),]
mydata <- mydata[(mydata$Subject != 30),]
mydata <- mydata[(mydata$Subject != 31),]
mydata <- mydata[(mydata$Subject != 32),]
mydata <- mydata[(mydata$Subject != 33),]
mydata <- mydata[(mydata$Subject != 34),]


######
#SUBJECT REMOVAL - PRETEST
#Calculate pretest scores and find out who should be removed for failure to pass

##Find causal/non side for Pilk pretest trial

mydata$Pilk.CausalSide <- NULL
for (i in 1:nrow(mydata)) {
	if (mydata$Trial.A.1[i] == "Pilk.C") {
		mydata$Pilk.CausalSide[i] <- as.character(mydata$Side.A.1[i])
	} else if (mydata$Trial.A.1[i] == "Pilk.NC"){
		mydata$Pilk.CausalSide[i] <- as.character(mydata$Side.A.2[i]) }
}
mydata$Pilk.CausalSide <- as.factor(mydata$Pilk.CausalSide)
#levels(mydata$Pilk.CausalSide) <- c("right", "left")


mydata$Pilk.NonSide <- NULL
for (i in 1:nrow(mydata)) {
	if (mydata$Pilk.CausalSide[i] == "left") {
		mydata$Pilk.NonSide[i] <- "right"
	} else {
		mydata$Pilk.NonSide[i] <- "left" }
}
#levels(mydata$Pilk.NonSide) <- c("right", "left")
mydata$Pilk.NonSide <- as.factor(mydata$Pilk.NonSide)

##Find causal/non side for Gorp pretest trial

mydata$Gorp.CausalSide <- NA
for (i in 1:nrow(mydata)) {
	if (mydata$Trial.B.1[i] == "Gorp.C") {
		mydata$Gorp.CausalSide[i] <- as.character(mydata$Side.B.1[i])
	} else if (mydata$Trial.B.1[i] == "Gorp.NC"){
		mydata$Gorp.CausalSide[i] <- as.character(mydata$Side.B.2[i]) }
}
mydata$Gorp.CausalSide <- as.factor(mydata$Gorp.CausalSide)
#levels(mydata$Gorp.CausalSide) <- c("left", "right")

mydata$Gorp.NonSide <- NULL
for (i in 1:nrow(mydata)) {
	if (mydata$Gorp.CausalSide[i] == "left") {
		mydata$Gorp.NonSide[i] <- "right"
	} else {
		mydata$Gorp.NonSide[i] <- "left" }
}
mydata$Gorp.NonSide <- as.factor(mydata$Gorp.NonSide)
#levels(mydata$Gorp.NonSide) <- c("left", "right")

###
#HAND OUT PRETEST SCORES
###

##Try to give a score for Pilk.Touch.Score

mydata$Pilk.Touch.Score <- NA
for (i in 1:nrow(mydata)) {
	if (is.na(mydata$ForceTouch.A.Ans[i])){
		##Didn't do forced choice, check free response
		mydata$Pilk.Touch.Score[i] <- NA
		s = 0
		if (mydata$Trial.A.1[i] == "Pilk.C"){
			s = s + mydata$Touch.A.1[i] + (1-mydata$Touch.A.2[i])
		} else if (mydata$Trial.A.1[i] == "Pilk.NC"){
			s = s + mydata$Touch.A.2[i] + (1-mydata$Touch.A.1[i])
		}
		##Did they get both right?
		if (s == 2){
		mydata$Pilk.Touch.Score[i] <- 1
		} else { 
		mydata$Pilk.Touch.Score[i] <- 0
		}
		
 	} else if (mydata$ForceTouch.A.Q[i] == "Touching"){
		##Did Forced choice
		if (mydata$ForceTouch.A.Ans[i] == mydata$Pilk.CausalSide[i]){
			mydata$Pilk.Touch.Score[i] <- 1
		} else if (mydata$ForceTouch.A.Ans[i] == mydata$Pilk.NonSide[i]){
			mydata$Pilk.Touch.Score[i] <- 0
		}
	} else if (mydata$ForceTouch.A.Q[i] == "NotTouching"){
		if (mydata$ForceTouch.A.Ans[i] == mydata$Pilk.NonSide[i]){
			mydata$Pilk.Touch.Score[i] <- 1
		} else if (mydata$ForceTouch.A.Ans[i] == mydata$Pilk.CausalSide[i]){
			mydata$Pilk.Touch.Score[i] <- 0
		}
	}
}


##Try to give a score for Pilk.Causal.Score

mydata$Pilk.Cause.Score <- NA
for (i in 1:nrow(mydata)) {
	if (is.na(mydata$ForceCause.A.Ans[i])){
		mydata$Pilk.Cause.Score[i] <- NA
		##Didn't do force choice, check free response
		s = 0
		if (mydata$Trial.A.1[i] == "Pilk.C"){
			s = s + mydata$Cause.A.1[i] + (1-mydata$Cause.A.2[i])
		} else if (mydata$Trial.A.1[i] == "Pilk.NC"){
			s = s + mydata$Cause.A.2[i] + (1-mydata$Cause.A.1[i])
		}
		##Did they get both right?
		if (s == 2){
		mydata$Pilk.Cause.Score[i] <- 1
		} else { 
		mydata$Pilk.Cause.Score[i] <- 0
		}
		
		
	} else if (mydata$ForceCause.A.Q[i] == "SarahDid"){
		if (mydata$ForceCause.A.Ans[i] == mydata$Pilk.CausalSide[i]){
			mydata$Pilk.Cause.Score[i] <- 1
		} else if (mydata$ForceCause.A.Ans[i] == mydata$Pilk.NonSide[i]){
			mydata$Pilk.Cause.Score[i] <- 0
		}
	} else if (mydata$ForceCause.A.Q[i] == "SarahDidnt"){
		if (mydata$ForceCause.A.Ans[i] == mydata$Pilk.NonSide[i]){
			mydata$Pilk.Cause.Score[i] <- 1
		} else if (mydata$ForceCause.A.Ans[i] == mydata$Pilk.CausalSide[i]){
			mydata$Pilk.Cause.Score[i] <- 0
		}
	}
}

##Try to give a score for Gorp.Touch.Score

mydata$Gorp.Touch.Score <- NA
for (i in 1:nrow(mydata)) {
	if (is.na(mydata$ForceTouch.B.Ans[i])){
	##Didn't do forced choice, check free response
		mydata$Gorp.Touch.Score[i] <- NA
		s = 0
		if (mydata$Trial.B.1[i] == "Gorp.C"){
			s = mydata$Touch.B.1[i] + (1-mydata$Touch.B.2[i])
		} else if (mydata$Trial.B.1[i] == "Gorp.NC"){
			s = mydata$Touch.B.2[i] + (1-mydata$Touch.B.1[i])
		}
		##Did they get both right?
		if (s == 2){
		mydata$Gorp.Touch.Score[i] <- 1
		} else { 
		mydata$Gorp.Touch.Score[i] <- 0
		}
	} else if (mydata$ForceTouch.B.Q[i] == "Touching"){
		if (mydata$ForceTouch.B.Ans[i] == mydata$Gorp.CausalSide[i]){
			mydata$Gorp.Touch.Score[i] <- 1
		} else if (mydata$ForceTouch.B.Ans[i] == mydata$Gorp.NonSide[i]){
			mydata$Gorp.Touch.Score[i] <- 0
		}
	} else if (mydata$ForceTouch.B.Q[i] == "NotTouching"){
		if (mydata$ForceTouch.B.Ans[i] == mydata$Gorp.NonSide[i]){
			mydata$Gorp.Touch.Score[i] <- 1
		} else if (mydata$ForceTouch.B.Ans[i] == mydata$Gorp.CausalSide[i]){
			mydata$Gorp.Touch.Score[i] <- 0
		}
	}
}



##Try to give a score for Gorp.Causal.Score

mydata$Gorp.Cause.Score <- NA
for (i in 1:nrow(mydata)) {
	if (is.na(mydata$ForceCause.B.Ans[i])){
		mydata$Gorp.Cause.Score[i] <- NA
		##Didn't do force choice, check free response
		s = 0
		if (mydata$Trial.B.1[i] == "Gorp.C"){
			s = s + mydata$Cause.B.1[i] + (1-mydata$Cause.B.2[i])
		} else if (mydata$Trial.B.1[i] == "Gorp.NC"){
			s = s + mydata$Cause.B.2[i] + (1-mydata$Cause.B.1[i])
		}
		##Did they get both right?
		if (s == 2){
		mydata$Gorp.Cause.Score[i] <- 1
		} else { 
		mydata$Gorp.Cause.Score[i] <- 0
		}
		
		
	} else if (mydata$ForceCause.B.Q[i] == "SarahDid"){
		if (mydata$ForceCause.B.Ans[i] == mydata$Gorp.CausalSide[i]){
			mydata$Gorp.Cause.Score[i] <- 1
		} else if (mydata$ForceCause.B.Ans[i] == mydata$Gorp.NonSide[i]){
			mydata$Gorp.Cause.Score[i] <- 0
		}
	} else if (mydata$ForceCause.B.Q[i] == "SarahDidnt"){
		if (mydata$ForceCause.B.Ans[i] == mydata$Gorp.NonSide[i]){
			mydata$Gorp.Cause.Score[i] <- 1
		} else if (mydata$ForceCause.B.Ans[i] == mydata$Gorp.CausalSide[i]){
			mydata$Gorp.Cause.Score[i] <- 0
		}
	}
}

##Calculate whether they pass pretest
for (i in 1:nrow(mydata)) {
	mydata$Pass.Score[i] <- sum(mydata$Pilk.Touch.Score[i], mydata$Pilk.Cause.Score[i], mydata$Gorp.Touch.Score[i], mydata$Gorp.Cause.Score[i], na.rm=TRUE)
}

##Pass 1 and 6 manually because they got 3/4 of Gorp free questions correct,
##This was only for early pilot kids who didn't get forced choice Qs

mydata[mydata$Subject==1,]$Pass.Score <- 3
mydata[mydata$Subject==6,]$Pass.Score <- 3

##Keep passers only
mydata <- mydata[(mydata$Pass.Score > 2),]

################################################
#####DEMOGRAPHICS

#means days old - Note, make sure it counts subjects, not trials!
collapsed <- mydata[!duplicated(mydata$Subject),]
mean(aggregate(collapsed$Days.Old, by=list(collapsed$Subject), mean)) 
foo <- subset(collapsed, select=c("Subject", "Days.Old"))
min(foo$Days.Old)
max(foo$Days.Old)

#Number of girls - Note, make sure it counts subjects, not trials!
foo <- subset(collapsed, select=c("Subject", "Gender"))
nrow(foo[foo$Gender=="F",])
nrow(foo[foo$Gender=="M",])




################################################
#######ANALYSIS!

#aggregate the "pilking"/"notpilking" choices in a new dataframe
sum.na.rm <- function(x) { sum(x,na.rm=T) }

NotPilkingScores <- aggregate(mydata$Choice.NC, by=list(mydata$Subject), sum.na.rm)
PilkingScores <- aggregate(mydata$Choice.C, by=list(mydata$Subject), sum.na.rm)

#NotPilkingScores, divided by when you got asked that question! For Review 4/30/15
NotPilkingFirst <- aggregate(mydata[mydata$Yesfirst==0,]$Choice.NC, by=list(mydata[mydata$Yesfirst==0,]$Subject), sum.na.rm)
NotPilkingSecond  <- aggregate(mydata[mydata$Yesfirst==1,]$Choice.NC, by=list(mydata[mydata$Yesfirst==1,]$Subject), sum.na.rm)

mean(NotPilkingFirst$x)
mean(NotPilkingSecond$x)

wilcox.test(NotPilkingFirst$x, NotPilkingSecond$x, exact=FALSE, conf.int=TRUE)

#Means & SEM for graphs!
mean(NotPilkingScores$x)
sd(NotPilkingScores$x)/sqrt(length(NotPilkingScores$x))
mean(PilkingScores$x)
sd(PilkingScores$x)/sqrt(length(PilkingScores$x))

wilcox.test(PilkingScores$x, mu=2, exact=FALSE, conf.int=TRUE)
mean(PilkingScores$x)
wilcox.test(NotPilkingScores$x, mu=2, exact=FALSE, conf.int=TRUE)
mean(NotPilkingScores$x)

#Hm, those confidence intervals don't make a ton of sense!  See bootstrapped version below!

#Did they do okay on the manipulation?  Check touch responses

mydata$IsCausalTouch <- 0
mydata[!is.na(mydata$Touching) & (mydata$Causal.Side=="left") & (mydata$Touching=="left"),]$IsCausalTouch <- 1
mydata[!is.na(mydata$Touching) & (mydata$Causal.Side=="right") & (mydata$Touching=="right"),]$IsCausalTouch <- 1

TouchingScores <- aggregate(mydata$IsCausalTouch, by=list(mydata$Subject), sum)
wilcox.test(TouchingScores$x, mu=2, exact=FALSE, conf.int=TRUE)
mean(TouchingScores$x)
sd(TouchingScores$x)/sqrt(length(TouchingScores$x))

#And condition comparisons
wilcox.test(PilkingScores$x, NotPilkingScores$x, exact=FALSE, paired=TRUE)
wilcox.test(PilkingScores$x, TouchingScores$x, exact=FALSE, paired=TRUE)


#Time for bootstrapped confidence intervals around the means of the 3 conditions!
library(bootstrap)
pilk.boot.mean = bootstrap(PilkingScores$x, 1000, mean)
quantile(pilk.boot.mean$thetastar, c(0.025, 0.975))
nopilk.boot.mean = bootstrap(NotPilkingScores$x, 1000, mean)
quantile(nopilk.boot.mean$thetastar, c(0.025, 0.975))
touching.boot.mean = bootstrap(TouchingScores$x, 1000, mean)
quantile(touching.boot.mean$thetastar, c(0.025, 0.975))





#T tests for paper
t.test(PilkingScores$x, mu=2, exact=FALSE)
t.test(NotPilkingScores$x, mu=2, exact=FALSE)
t.test(TouchingScores$x, mu=2, exact=FALSE)
t.test(PilkingScores$x, NotPilkingScores$x, exact=FALSE)
t.test(PilkingScores$x, TouchingScores$x, exact=FALSE)



#Compare 3s and 4s

threes <- mydata[mydata$Age.Years == 3,]
fours <- mydata[mydata$Age.Years == 4,]

PScore3 <- aggregate(threes$Choice.C, by=list(threes$Subject), sum.na.rm)
NPScore3 <- aggregate(threes$Choice.NC, by=list(threes$Subject), sum.na.rm)
TScore3 <- aggregate(threes$IsCausalTouch, by=list(threes$Subject), sum.na.rm)


PScore4 <- aggregate(fours$Choice.C, by=list(fours$Subject), sum.na.rm)
NPScore4 <- aggregate(fours$Choice.NC, by=list(fours$Subject), sum.na.rm)
TScore4 <- aggregate(fours$IsCausalTouch, by=list(fours$Subject), sum.na.rm)

wilcox.test(PScore3$x, PScore4$x, exact=FALSE)
mean(PScore3$x)
mean(PScore4$x)
wilcox.test(NPScore3$x, NPScore4$x, exact=FALSE)
mean(NPScore3$x)
mean(NPScore4$x)
wilcox.test(TScore3$x, TScore4$x, exact=FALSE)
mean(TScore3$x)
mean(TScore4$x)

#Check manipulation on both age groups!!

wilcox.test(PScore3$x, mu=2, exact=FALSE)
mean(PScore3$x)
sd(PScore3$x)/sqrt(length(PScore3$x))

wilcox.test(NPScore3$x, mu=2, exact=FALSE)
mean(NPScore3$x)
sd(NPScore3$x)/sqrt(length(NPScore3$x))

wilcox.test(PScore4$x, mu=2, exact=FALSE)
mean(PScore4$x)
sd(PScore4$x)/sqrt(length(PScore4$x))

wilcox.test(NPScore4$x, mu=2, exact=FALSE)
mean(NPScore4$x)
sd(NPScore4$x)/sqrt(length(NPScore4$x))






























################################################
#Old ANALYSIS
##ANALYSIS - DESCRIPTIVES
#How did they do on causal questions?

trials.CQ <- nrow(mydata)
corr.CQ <- nrow(mydata[mydata$Choice.Causal == 1,])
corr.CQ/trials.CQ

#How about noncausal?

trials.NCQ <- nrow(mydata)
corr.NCQ <- nrow(mydata[mydata$Choice.NC == 0,])
corr.NCQ/trials.NCQ

#Overall?

(corr.CQ + corr.NCQ)/(trials.CQ + trials.NCQ)

#What about 1st vs. 2nd question?

yesfirst <- mydata[mydata$Yesfirst == 1,]
nofirst <- mydata[mydata$Yesfirst == 0,]

#First questions are yesfirst causal and nofirst NC
trials.CQ <- nrow(yesfirst)
corr.CQ <- nrow(yesfirst[yesfirst$Choice.Causal == 1,])
corr.CQ/trials.CQ

trials.NCQ <- nrow(nofirst)
corr.NCQ <- nrow(nofirst[nofirst$Choice.NC == 0,])
corr.NCQ/trials.NCQ

(corr.CQ + corr.NCQ)/(trials.CQ + trials.NCQ)

#Second questions are yesfirst NC, nofirst C

trials.NCQ <- nrow(yesfirst)
corr.NCQ <- nrow(yesfirst[yesfirst$Choice.NC == 0,])
corr.NCQ/trials.NCQ

trials.CQ <- nrow(nofirst)
corr.CQ <- nrow(nofirst[nofirst$Choice.Causal == 1,])
corr.CQ/trials.CQ

(corr.CQ + corr.NCQ)/(trials.CQ + trials.NCQ)










#################################

mydata$Causal.First <- mydata$First.Side==mydata$Causal.Side

trials.cfirst <- nrow(oldsters[oldsters$Causal.First == TRUE,])
trials.cfirst
trials.clast <- nrow(oldsters[oldsters$Causal.First == FALSE,])
trials.clast

corr.cfirst <- sum(oldsters[oldsters$Causal.First == TRUE,]$Causal.Choice)
corr.cfirst/trials.cfirst

corr.clast <- sum(oldsters[oldsters$Causal.First == FALSE,]$Causal.Choice)
corr.clast/trials.clast

(corr.cfirst+corr.clast)/(trials.cfirst+trials.clast)

unique(oldsters$Subject)








trials.cfirst <- nrow(mydata[mydata$Causal.First == TRUE,])
trials.cfirst
trials.clast <- nrow(mydata[mydata$Causal.First == FALSE,])
trials.clast

corr.cfirst <- sum(mydata[mydata$Causal.First == TRUE,]$Causal.Choice)
corr.cfirst/trials.cfirst

corr.clast <- sum(mydata[mydata$Causal.First == FALSE,]$Causal.Choice)
corr.clast/trials.clast

(corr.cfirst+corr.clast)/(trials.cfirst+trials.clast)




library(readxl)
library(car)
library(ggpubr)
library(yarrr)
library(agricolae)
library(ggstatsplot)

getwd()
setwd("C:/Users/eliza/OneDrive/Desktop/JL Lab")

RData <- read_excel('C:\\Users\\eliza\\OneDrive\\Desktop\\JL Lab\\Juv. Barley\\RollStats.xlsx')
RData
summary(RData)

#Remove Notes
NNote <- subset (RData, select = -Notes)
NNote

#Remove rows with N/A
RollData <- na.omit(NNote) 
RollData

#Set a Categorical
RollData$Dose <- as.factor(RollData$Dose)
RollData$Roll <- as.factor(RollData$Roll)
RollData$Month <- as.factor(RollData$Month)
RollData$GrowthLength <- as.factor(RollData$GrowthLength)

#####Anova#####
#Make Linear Model
Model1 <- lm(Length ~ Dose + Month + Month:Roll, data = RollData)

anova_two_way <- Anova(Model1)
anova_two_way

#####Tukey's HSD#####
Comp1 <- HSD.test(Model1, "Dose", group = TRUE)
Comp1

#####Day 7 Separation#####
Day7 <- RollData [RollData$GrowthLength =="7",]

Model7 <- lm(Length ~ Dose + Month + Month:Roll, data = Day7)
ATW7 <- Anova(Model7)
ATW7

Comp7 <- HSD.test(Model7, "Dose", group = TRUE)
Comp7

#####Day 9 Separation##### 
Day9 <- RollData [RollData$GrowthLength =="9",]

Model9 <- lm(Length ~ Dose + Roll, data = Day9)
ATW9 <- Anova(Model9)
ATW9

Comp9 <- HSD.test(Model9, "Dose", group = TRUE)
Comp9

#####Day 8 Separation#####
Day8 <- RollData [RollData$GrowthLength =="8",]

Model8 <- lm(Length ~ Dose + Roll, data = Day8)
ATW8 <- Anova(Model8)
ATW8

Comp8 <- HSD.test(Model8, "Dose", group = TRUE)
Comp8

#####Boxplot#####
Boxplot(Length ~ Dose, RollData, col="green")
Boxplot(Length ~ Dose, Day7, col="blue")
Boxplot(Length ~ Dose, Day8, col="red")
Boxplot(Length ~ Dose, Day9, col="yellow")

#####Outliers Removal#####
boxplot(Day7$Length, plot=T)$out
Outliers7 <- boxplot(Day7$Length, plot=T)$out
Out7 <- Day7[-which(Day7$Length %in% Outliers7),]
Out7
Out7 <- as.data.frame(Out7)

boxplot(Day8$Length, plot=T)$out
Outliers8 <- boxplot(Day8$Length, plot=T)$out
Out8 <- Day8[-which(Day8$Length %in% Outliers8),]
Out8

boxplot(Day9$Length, plot=T)$out
Outliers9 <- boxplot(Day9$Length, plot=T)$out
Out9 <- Day9[-which(Day9$Length %in% Outliers9),]
Out9

#####Boxplot Outliers#####
Boxplot(Length ~ Dose, RollData, col="green")
Boxplot(Length ~ Dose, Out7, col="blue")
Boxplot(Length ~ Dose, Out8, col="red")
Boxplot(Length ~ Dose, Out9, col="yellow")

#####Day 7 Outliers#####
OModel2 <- lm(Length ~ Dose + Roll + Month + Month:Roll + Month:Dose, data = Out7)
OATW2 <- Anova(OModel2)
OATW2

OComp2 <- HSD.test(OModel2, "Dose", group = TRUE)
OComp2

#####Day 8 Outliers#####
OModel4 <- lm(Length ~ Dose + Roll, data = Out8)
OATW4 <- Anova(OModel4)
OATW4

OComp4 <- HSD.test(OModel4, "Dose", group = TRUE)
OComp4

#####Day 9 Outliers#####
OModel3 <- lm(Length ~ Dose + Roll, data = Out9)
OATW3 <- Anova(OModel3)
OATW3

OComp3 <- HSD.test(OModel3, "Dose", group = TRUE)
OComp3

plot(OComp2)
plot(OComp4)
plot(OComp3)

#####Plot Tukey's as Line Graph#####
Out7$Dose <- order(as.numeric(Out7$Dose, levels = c("0", "0.02", "0.1", "0.2", "0.66", "1"))
means7 <- aggregate(Out7$Length,
  by = list(Out7$Dose), FUN = mean)
plot(as.numeric(as.character(means7$Group.1)), means7$x, type = "b", pch = 16, ylim = c(5,14))

Out7$Dose <- order(as.numeric(Out7$Dose))

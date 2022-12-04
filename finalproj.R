rm(list=ls())

#import packages
library(randomForest)
library(ggplot2)
library(pROC)
library(dplyr)
library(tidyr)
library(stringr)


####### READ IN DATA-------
pokemon <-  read.csv("~/Desktop/archive/pokemon-data.csv", sep=";", stringsAsFactors=F)
head(pokemon)
str(pokemon)
#String - Name, Tier
#List of strings - Types, Abilities, Next Evolution(s), Moves (movenames)
#Integers - HP, Attack, Defense, Special.Attack, Special.Defense, Speed

moves <- read.csv("~/Desktop/archive/move-data.csv", sep=",", stringsAsFactors=F)
head(moves)
str(moves)
summary(moves$Power)
subset(moves, moves$Power == 'None')
#Strings - Name, Type, Category, Contest
#Integers - PP, Power, Accuracy, Gen
#Int or None - Power, Accuracy

######### CLEANING DATA-----
#### column names -----
colnames(pokemon) <- c('Poke_name','Poke_type','Abilities','Tier','Hit_points',
                       'Attack','Defense','Special_attack','Speed','Special_defense',
                       'Next_evolution','Moves')

colnames(moves) <- c('Index','Move_name','Move_type','Category','Contest','Power_points','Power',
                     'Accuracy','Generation')
head(pokemon)
head(moves)

#### missing values ----
#Find missing values in pokemon 
summary(pokemon)
sum(is.na(moves))
#no missing values
#Find missing values in moves 
summary(moves)
#'None' in Power and Accuracy 
#lets replace None in power with 0 and None in Accuracy with 100 
moves$Power[moves$Power == 'None'] <- 0 
moves$Accuracy[moves$Accuracy == 'None'] <- 100
#check if it worked 
moves$Power
moves$Accuracy
sum(moves$Accurary =='None')
sum(moves$Power == 'None')

#### binary Y ----
pokemon$tier_bin <- ifelse(pokemon$Tier =="OU", 1,0)
head(pokemon)
summary(pokemon$Tier)

#Abilities binary ----
#type binary ---- 
#Next evolution as yes or no ----
#Power into 4 categories -----
moves$Power <- as.numeric(moves$Power)
summary(moves$Power)
#High Power, Moderate Power, Low Power, No Power
moves$Power <- as.factor(ifelse(moves$Power > 200,'High Power',
                                ifelse(moves$Power >= 100, 'Moderate Power',
                                ifelse(moves$Power > 0, 'Low Power','No Power'))))
summary(moves$Power)
HighPower <- subset(moves, moves$Power == 'High Power')
ModeratePower <- subset(moves, moves$Power == 'Moderate Power')
LowPower <- subset(moves, moves$Power == 'Low Power')
NoPower <- subset(moves, moves$Power == 'No Power')

HighPower <- unlist(HighPower$Move_name)
ModeratePower <- unlist(ModeratePower$Move_name)
LowPower <- unlist(LowPower$Move_name)
NoPower <- unlist(NoPower$Move_name)

#testing 
test <- moveone[3]
test %in% ModeratePower

#Clean pokemon moves ----
#counting number of each type of move in the moveslist for each pokemon

pokemon$HighPowerCount <- pokemon$ModeratePowerCount <- pokemon$LowPowerCount <-
  pokemon$NoPowerCount <- pokemon$UnknownPowerCount <- pokemon$MoveCount <- 0

head(pokemon)
for(i in 1:100){
  ModP = 0
  HighP = 0 
  LowP = 0 
  NoP = 0
  Unknown = 0
  movelist <- pokemon$Moves[i]
  movelist <- unlist(strsplit(gsub("[\\[\\]']", "", movelist, perl = TRUE), ", "))
  pokemon$MoveCount[i] <- length(movelist)
  moveCount <- length(movelist)
  for(element in movelist){
    if(element %in% HighPower){
      HighP = HighP + 1
      pokemon$HighPowerCount[i]<- HighP
    }
    if(element %in% ModeratePower){
      ModP = ModP+1
      pokemon$ModeratePowerCount[i]<- ModP
    }
    if(element %in% LowPower){
      LowP = LowP + 1
      pokemon$LowPowerCount[i] <- LowP
    }
    if(element %in% NoPower){
      NoP = NoP + 1
      pokemon$NoPowerCount[i] <- NoP
    }
    total = NoP + LowP + ModP + HighP
    pokemon$UnknownPowerCount[i] <- moveCount - total
  }
}
#check if it worked
pokemon[1:50,]
#this works, some pokemon have the same exact moves 


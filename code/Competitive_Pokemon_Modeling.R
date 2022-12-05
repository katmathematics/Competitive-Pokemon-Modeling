# PROJECT DESCRIPTION ----
# This analysis is a final project for STAT 172
# Contributors: Katja Mathesius, Amanda Perrira, Ellie Reece
####
# INITIAL ----
# Clear workspace
rm(list = ls())

# Import packages
library(randomForest) # Used in creation of Random Forests
library(ggplot2) # Used for visualizations
library(pROC) # Metric for analysis of models
library(dplyr)
library(tidyr)
library(stringr) # Used for str_contains() used in data cleaninng

# CHANGE THESE BEFORE RUNNING ----
# Location of the Pokemon data on your device
pokeDataLoc <- "data/pokemon-data.csv"
# Location of the move data on your device
moveDataLoc <- "data/move-data.csv"

# Location of the abilities list on your device
abilitiesLoc <- "data/lists/abilities_list.txt"
# Location of the abilites names list on your device
abilitiesNamesLoc <- "data/lists/abilities_names_list.txt"

# DATA READ-IN & EXPLORATION ----
# Read in the Pokemon and Moves data
pokemon = read.csv(pokeDataLoc, sep=";", header=T, stringsAsFactors = TRUE)
moves = read.csv(moveDataLoc, sep=',', header = T)

# Initial view of the pokemon data frame
# This is the primary df for this project
head(pokemon) 
str(pokemon) 
dim(pokemon) 

## Variables in Pokemon ##
#String: Name, Tier
#List of strings: Types, Abilities, Next Evolution(s), Moves (movenames)
#Integers: HP, Attack, Defense, Special.Attack, Special.Defense, Speed

# Initial view of the moves data frame
# This is a supplementary df for this project to assist in cleaning moves
head(moves)
str(moves)
summary(moves$Power)
subset(moves, moves$Power == 'None')

## Variables in Moves ##
#Strings: Name, Type, Category, Contest
#Integers: PP, Power, Accuracy, Gen
#Int or None: Power, Accuracy

# DATA CLEANING ----

## Column Header Cleaning ##

# New column headers for the Pokemon df
colnames(pokemon) <- c('Poke_name','Poke_type','Abilities','Tier','Hit_points',
                       'Attack','Defense','Special_attack','Speed','Special_defense',
                       'Next_evolution','Moves')

# New column headers for the Move df
colnames(moves) <- c('Index','Move_name','Move_type','Category','Contest','Power_points','Power',
                     'Accuracy','Generation')

# Check that these changes worked
head(pokemon)
head(moves)

## Evolution Binary Variable ##
# Turn evolution into a binary variable based on if a Pokemon 
# does or doesn't have an evolution. Does = 1, Does Not = 0.

# Convert Evo to Character Type
pokemon$Next_evolution = as.character(pokemon$Next_evolution)
# If string length > 3, then yes evolution... 3 because of '[]' entries
pokemon$evol_bin <- ifelse(nchar(pokemon$Next_evolution) >= 3, 1, 0)
# Checking Result
unique(pokemon$evol_bin)




# MISSING VALUES
#Find missing values in pokemon 
summary(pokemon)
sum(is.na(moves))
#no missing values


#'None' in Power and Accuracy 
#lets replace None in power with 0 and None in Accuracy with 100 
moves$Power[moves$Power == 'None'] <- 0 
moves$Accuracy[moves$Accuracy == 'None'] <- 100
#check if it worked 
moves$Power
moves$Accuracy
sum(moves$Accurary =='None')
sum(moves$Power == 'None')

#BINARY Y (Tier)----
pokemon$tier_bin <- ifelse(pokemon$Tier =="OU", 1,0)
head(pokemon)
summary(pokemon$Tier)

# BINARY VARIABLE - ABILITIES ----
#type binary
#Next evolution as yes or no
#Power into 4 categories 
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

# VARIABLE CLEANING - MOVES  ----
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

# DUMMY VARIABLES - TYPES & ABILITIES ----

elemental_types <- c("Normal", "Fire", "Water", "Grass", "Electric", "Ice", 
                     "Fighting", "Poison", "Ground", "Flying", "Psychic", "Bug", 
                     "Rock", "Ghost", "Dark", "Dragon", "Steel", "Fairy")

# Read in the data
abilities_list <- scan(abilitiesLoc, character(), quote = "", sep=",")
abilities_names_list <- scan(abilitiesNamesLoc, character(), quote = "", sep=",")

str(pokemon)
summary(pokemon)
head(pokemon)
describe(pokemon)

# Create a binary variable for typings
for(i in seq_along(elemental_types)){
  pokemon[elemental_types[i]] <- ifelse(str_detect(pokemon$Poke_type, elemental_types[i]), 1, 0)
}

# Create a binary variable for abilities
for(i in seq_along(abilities_list)){
  pokemon[abilities_names_list[i]] <- ifelse(str_detect(pokemon$Abilities, abilities_list[i]), 1, 0)
}

# Drop the variables that have been cleaned
drop <- c('Poke_name','Poke_type','Abilities','Tier','Next_evolution','Moves')
pokemon = pokemon[,!(names(pokemon) %in% drop)]

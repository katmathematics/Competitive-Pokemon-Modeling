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

## Missing Values ##

# Check for missing values in Pokemon 
sapply(pokemon, function(x) sum(is.na(x)))
# No missing values

# Check for missing values in Moves 
sapply(moves, function(x) sum(is.na(x)))
# No missing values

## Missing Moves ##

# There are moves missing from the moves dataset due to their names
# in the source they've been scrapped from containing ' and/or -. 
# These moves are added by hand here.
moves[nrow(moves) + 1,] <- c(nrow(moves) + 1, 'XScissor', 'Bug', 'Physical', 'Cool', 15, 80, 100, 4)
moves[nrow(moves) + 1,] <- c(nrow(moves) + 1, 'Uturn', 'Bug', 'Physical', 'Cute', 20, 70, 100, 4)
moves[nrow(moves) + 1,] <- c(nrow(moves) + 1, 'MudSlap', 'Ground', 'Special', 'Cute', 10, 20, 100, 2)
moves[nrow(moves) + 1,] <- c(nrow(moves) + 1, 'DoubleEdge', 'Normal', 'Physical', 'Tough', 15, 120, 100, 1)
moves[nrow(moves) + 1,] <- c(nrow(moves) + 1, 'WillOWisp', 'Ghost', 'Status', 'Beauty', 15, 'None', 85, 3)
moves[nrow(moves) + 1,] <- c(nrow(moves) + 1, 'PowerUp Punch', 'Fighting', 'Physical', 'Tough', 20, 40, 100, 6)
moves[nrow(moves) + 1,] <- c(nrow(moves) + 1, 'Lands Wrath', 'Ground', 'Physical', 'Cute', 10, 90, 100, 6)
moves[nrow(moves) + 1,] <- c(nrow(moves) + 1, 'BabyDoll Eyes', 'Fairy', 'Status', 'Cute', 30, 'None', 100, 6)
moves[nrow(moves) + 1,] <- c(nrow(moves) + 1, 'WakeUp Slap', 'Fighting', 'Physical', 'Tough', 10, 70, 100, 6)
moves[nrow(moves) + 1,] <- c(nrow(moves) + 1, 'SelfDestruct', 'Normal', 'Physical', 'Tough', 5, 200, 100, 1)
moves[nrow(moves) + 1,] <- c(nrow(moves) + 1, 'FreezeDry', 'Ice', 'Special', 'Beautiful', 20, 70, 100, 6)
moves[nrow(moves) + 1,] <- c(nrow(moves) + 1, 'Natures Madness', 'Fairy', 'Special', '???', 10, 'None', 90, 7)
moves[nrow(moves) + 1,] <- c(nrow(moves) + 1, 'MultiAttack', 'Normal', 'Physical', '???', 10, 120, 100, 7)
moves[nrow(moves) + 1,] <- c(nrow(moves) + 1, 'LockOn', 'Normal', 'Status', 'Smart', 5, 'None', 'None', 2)
moves[nrow(moves) + 1,] <- c(nrow(moves) + 1, 'TrickorTreat', 'Ghost', 'Status', 'Cute', 20, 'None', 100, 6)
moves[nrow(moves) + 1,] <- c(nrow(moves) + 1, 'Forests Curse', 'Grass', 'Status', 'Clever', 20, 'None', 100, 6)
moves[nrow(moves) + 1,] <- c(nrow(moves) + 1, 'SoftBoiled', 'Normal', 'Status', 'Beauty', 5, 'None', 'None', 1)
moves[nrow(moves) + 1,] <- c(nrow(moves) + 1, 'TopsyTurvy', 'Dark', 'Status', 'Clever', 20, 'None', 'None', 6)
moves[nrow(moves) + 1,] <- c(nrow(moves) + 1, 'Kings Shield', 'Steel', 'Status', 'Cool', 10, 'None', 'None', 6)

## Creating the Binary Y-Variable: Tier ##
# Turn tier into a binary variable based on if a Pokemon's tier ranking
# in competitive play. Overused (OU) = 1, Anything Else = 0.
pokemon$tier_bin <- ifelse(pokemon$Tier =="OU", 1,0)
# Check that it worked
unique(pokemon$tier_bin)
sum(pokemon$tier_bin)

## Evolution Binary Variable ##
# Turn evolution into a binary variable based on if a Pokemon 
# does or doesn't have an evolution. Does = 1, Does Not = 0.

# Convert Evo to Character Type
pokemon$Next_evolution = as.character(pokemon$Next_evolution)
# If string length > 3, then yes evolution... 3 because of '[]' entries
pokemon$evol_bin <- ifelse(nchar(pokemon$Next_evolution) >= 3, 1, 0)
# Checking Result
unique(pokemon$evol_bin)

## Parsing the Move Variable via the Moves DF ##

# Handling 'None' values in Power and Accuracy 
# Replace 'None' in power with 0
moves$Power[moves$Power == 'None'] <- 0 
# Replace 'None' in Accuracy with 100 
moves$Accuracy[moves$Accuracy == 'None'] <- 100

# Check if it worked 
unique(moves$Power)
unique(moves$Accuracy)

sum(moves$Accurary =='None')
sum(moves$Power == 'None')


## Move Power Variable Cleaning ##
# Divide power into 4 categories 
# Categories: High Power, Moderate Power, Low Power, No Power
moves$Power <- as.numeric(moves$Power)

# Using if else statements convert power back into a factor data type
moves$Power <- as.factor(ifelse(moves$Power > 200,'High Power',
                                ifelse(moves$Power >= 100, 'Moderate Power',
                                       ifelse(moves$Power > 0, 'Low Power','No Power'))))

# Create subsets for each variable
HighPower <- subset(moves, moves$Power == 'High Power')
ModeratePower <- subset(moves, moves$Power == 'Moderate Power')
LowPower <- subset(moves, moves$Power == 'Low Power')
NoPower <- subset(moves, moves$Power == 'No Power')

# Flatten the subsets so they can be used for string detection 
HighPower <- unlist(HighPower$Move_name)
ModeratePower <- unlist(ModeratePower$Move_name)
LowPower <- unlist(LowPower$Move_name)
NoPower <- unlist(NoPower$Move_name)

# Test if the method works
#test <- moveone[3]
#test %in% ModeratePower

## Moves Power Variable Cleaning ##

# Create empty columns for the assorted counts of varying powered moves
pokemon$HighPowerCount <- pokemon$ModeratePowerCount <- pokemon$LowPowerCount <-
  pokemon$NoPowerCount <- pokemon$UnknownPowerCount <- pokemon$MoveCount <- 0
pokemon.shape[0]

# Loop for all Pokemon in the dataframe
for(i in 1:nrow(pokemon)){
  # Set all the sum storage vals to 0
  ModP = 0
  HighP = 0 
  LowP = 0 
  NoP = 0
  Unknown = 0
  # Get the moves of the ith pokemon
  movelist <- pokemon$Moves[i]
  # Convert the moves string into a list
  movelist <- unlist(strsplit(gsub("[\\[\\]']", "", movelist, perl = TRUE), ", "))
  # Get the number of total moves in the moves list
  pokemon$MoveCount[i] <- length(movelist)
  # Store the length of the move list
  moveCount <- length(movelist)
  
  # For each move in the list of moves
  for(element in movelist){
    # If the element is contained in the list of HighPower moves add 1
    # to the total high power moves and write the value to the dataframe
    if(element %in% HighPower){
      HighP = HighP + 1
      pokemon$HighPowerCount[i]<- HighP
    }
    # If the element is contained in the list of ModeratePower moves add 1
    # to the total moderate power moves and write the value to the dataframe
    else if(element %in% ModeratePower){
      ModP = ModP+1
      pokemon$ModeratePowerCount[i]<- ModP
    }
    # If the element is contained in the list of LowPower moves add 1
    # to the total low power moves and write the value to the dataframe
    else if(element %in% LowPower){
      LowP = LowP + 1
      pokemon$LowPowerCount[i] <- LowP
    }
    # If the element is contained in the list of NoPower moves add 1
    # to the total no power moves and write the value to the dataframe
    else if(element %in% NoPower){
      NoP = NoP + 1
      pokemon$NoPowerCount[i] <- NoP
    }
    else {
      print(element)
    }
    # Sum the counts of each move type to create a total moves successfully classified
    total = NoP + LowP + ModP + HighP
    # Subtract the total successfully classified moves from the overall total moves as
    # a check columns for any moves that might have missed classification
    pokemon$UnknownPowerCount[i] <- moveCount - total
  }
}

## One-Hot Encode Types and Abilities ##

elemental_types <- c("Normal", "Fire", "Water", "Grass", "Electric", "Ice", 
                     "Fighting", "Poison", "Ground", "Flying", "Psychic", "Bug", 
                     "Rock", "Ghost", "Dark", "Dragon", "Steel", "Fairy")

# Read in the list of abilities (for detection) and list of R-friendy abilities names
# (for column headers) from text files
abilities_list <- scan(abilitiesLoc, character(), quote = "", sep=",")
abilities_names_list <- scan(abilitiesNamesLoc, character(), quote = "", sep=",")

# Loop for all of the elemental types in the list
for(i in seq_along(elemental_types)){
  # Create a binary variable for each type
  # 1 - The Pokemon is that type, 0 - The Pokemon does not possess that type
  pokemon[elemental_types[i]] <- ifelse(str_detect(pokemon$Poke_type, elemental_types[i]), 1, 0)
}


# Loop for all of the abilities in the list
for(i in seq_along(abilities_list)){
  # Create a binary variable for each type
  # 1 - The Pokemon can possess the ability, 0 - The Pokemon can not possess the ability
  pokemon[abilities_names_list[i]] <- ifelse(str_detect(pokemon$Abilities, abilities_list[i]), 1, 0)
}

# Drop the variables that have been cleaned and are not needed for analysis
pokemon = subset(pokemon, select = -c(Poke_name,Poke_type,Abilities,Tier,Next_evolution,Moves) )
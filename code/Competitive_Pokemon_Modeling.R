# PROJECT DESCRIPTION ----
#
# Contributors (Listed Alphabetical by Last Name): Katja Mathesius, Amanda Perrira, Ellie Reece
#
# Description:
# This analysis of competitive Pokemon is a final project for STAT 172
# The project serves to demonstrate ability with the use of Random Forest and GLM models
# in a real world application setting.
#
# Data:
# Data for this project is taken from Kaggle user Nicholas Vadivelu: https://www.kaggle.com/datasets/n2cholas/competitive-pokemon-dataset?select=pokemon-data.csv
#
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
# Location of the abilities names list on your device
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

# RANDOM FOREST -----
# Set a seed so results are repeatable
RNGkind(sample.kind="default")
set.seed(123)

# Divide the Pokemon data into a training and testing dataset
train.idx <- sample(x=1:nrow(pokemon),size = .8*nrow(pokemon))
train.df <- pokemon[train.idx,]
test.df <- pokemon[-train.idx,]

# Check the training dataset
#View(train.df)

# Ensure tier_bin is set to a factor as weirdness occurs when its read numeric
train.df$tier_bin = as.factor(train.df$tier_bin)
test.df$tier_bin = as.factor(test.df$tier_bin)

# Check the number of variables in the dataset in order to calculate mtry
length(colnames(pokemon))

# Create a baseline forest
myforest <- randomForest(tier_bin ~ ., #remember dangers here
                         data = train.df, #training data
                         ntree = 1000, #B
                         mtry = 17, #choose m - sqrt(297) ~17 
                         importance = TRUE,
                         method = 'class')

myforest
# OOB error rate estimate is 5.59% 

# Create a selective list of mtry values to run as the data has 298 columns
mtry = c(8,17,34,68,100,150)

# Creates an empty keeps data frame to store mtry 
# and its corresponding out of bounds error
keeps <- data.frame(m=rep(NA, length(mtry)),
                    OOB_error_rate = rep(NA, length(mtry)))

# Run tests of how changing mtry affects the random forest
for(idx in 1:length(mtry)){
  print(paste0("Fitting m = ", mtry[idx]))
  tempforest <- randomForest(tier_bin ~.,
                             data = train.df,
                             ntree = 1000,
                             mtry = mtry[idx]) #mtry is varying
  keeps[idx,"m"] <- mtry[idx]
  keeps[idx, "OOB_error_rate"] <- mean(predict(tempforest) != train.df$tier_bin)
}

keeps

# Plot OOB error rate for different mtry vals
ggplot(data = keeps)+
  geom_line(aes(x = m, y = OOB_error_rate))+
  theme_bw() + labs(x = "m (mtry) value", y = "OOB error rate")+
  scale_x_continuous(breaks = c(8,17,34,68,100))

# Currently it looks like 34 is the best value for mtry based on OOB

# Build a random forest using the optimal mtry
final_forest <- randomForest(tier_bin ~ ., #remember dangers here
                             data = train.df, #training data
                             ntree = 1000, #B
                             mtry = 34, #try 2* initial mtry value 
                             class_wt = c(.01,.99),
                             importance = TRUE,
                             method = 'class')
final_forest


## Create ROC Curve ##
# Positive event is 1 -"OU"

# Extract a vector of probabilities of a positive event
# and assign it to pi_hat
pi_hat <- predict(final_forest,test.df, type="prob")[,"1"]

# Create ROC Curve
rocCurve <- roc(response = test.df$tier_bin, 
                predictor = pi_hat,
                levels = c("0","1")) #Order matters here

# Visualize the ROC Curve
plot(rocCurve, print.thres=TRUE, print.auc=TRUE)

# Save the optimal to pi_star
pi_star <- coords(rocCurve, "best", ret = "threshold")$threshold[1]
# Interpretation: If the forest predicts an overuse probability greater than 
# 0.0165 (pi_star), we predict that the Pokemon is overused. Otherwise, we predict a loss. 

# Overwrite the old predictions with the improved ones that use pi_star
test.df$forest_pred <- as.factor(ifelse(pi_hat > pi_star,"1","0"))

## INTERPRETATION USING FOREST ##
# Using random forests as a tool to understand relationships between the 
# x-variables and tier (y-variable). Random forests will give us 
# a ranked list of variable importance, i.e. how much would the 
# out of sample error suffer if the variable was removed from 
# the model. 

varImpPlot(final_forest, type=1) #must specify importance = TRUE in forest 
# Plot contains too many variables to be elegant, but 
# does convey which variables are the most important

## Random Forests vs GLMs ##
# RF pro: automatic variable selection 
# RF con: no directional effects 
# Logistic regression pro: very good interpretations/ directional effects 
# Logistic regression con: no automatic variable selection 

# As these two techniques compliment each others weaknesses, 
# we will pair random forest and logistic regression.
# Random forest will be used both as a prediction tool 
# AND to help inform what variables go into a logistic regression. 

# LOGISTIC REGRESSION -----

## Create a Bernoulli RV ##

# As the logistic regression is not a predictive tool, pass
# it the entire data set.

# Convert tier_bin from factor to numeric
pokemon$tier_bin <- as.numeric(pokemon$tier_bin) 

# Make a copy of the data
data <- pokemon
# Check the dimensions of the data
dim(data)

quartiles <- quantile(data$MoveCount, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(data$MoveCount)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier <- subset(data, data$MoveCount > Lower & data$MoveCount < Upper)

dim(data_no_outlier)
plot(data_no_outlier$MoveCount)

#This is with mtry = 68
m1 <- glm(tier_bin ~ Attack + Special_attack + MoveCount, data = pokemon, 
          family = binomial(link="logit"))

# Warning message: fitted probabilities numerically 0 or 1 occurred  
# Can ignore, increase sample size, remove outliers
AIC(m1) #375.6081

plot(pokemon$ModeratePowerCount)
m2 <- glm(tier_bin ~ Attack + Special_attack + MoveCount + ModeratePowerCount, data = pokemon, 
          family = binomial(link="logit"))
AIC(m2) #376.5345
# Forest implied importance and using AIC to gauge whether they should be kept. 

# This is with mtry = 34 
m3 <- glm(tier_bin ~ Attack + LowPowerCount + MoveCount + Special_attack, data = pokemon, 
          family = binomial(link="logit"))
AIC(m3) #374.9328

m4 <- glm(tier_bin ~ Attack + LowPowerCount + MoveCount + Special_attack + UnknownPowerCount, data = pokemon, 
          family = binomial(link="logit"))
AIC(m4) #376.5889

# Final Model with lowest AIC: Attack, low power, move count, special attack 
final_glm <- glm(tier_bin ~ Attack + LowPowerCount + MoveCount + Special_attack, data = pokemon, 
                 family = binomial(link="logit"))
AIC(final_glm) #AIC = 374.9328

summary(final_glm)

# Coefficients for the finalized glm
coef_final <- coef(final_glm)
coef_final

# Odds Ratios
exp(coef_final)

# INTERPRETATIONS/PREDICTIONS-----


# VISUALIZATIONS---- 

# First variable: Attack 
# Univariate - Attack & Tier_Bin
ggplot(pokemon,  aes(y = tier_bin, x = Attack, col = tier_bin)) +
  geom_jitter()

# Multivariate - Attack & MoveCount 
ggplot(data = pokemon, mapping = aes(x = Attack, y = MoveCount)) +
  geom_point(aes(color = tier_bin))
# Doesn't appear to be a relationship between Attack and MoveCount 
# or either variable and being in the Overused tier

# Multivariate - MoveCount and LowPower count
ggplot(data = pokemon, mapping = aes(x = LowPowerCount, y = MoveCount)) +
  geom_point(aes(color = tier_bin))
# Positive Relationship between MoveCount and LowPowerCount. This is logical as
# the more moves a Pokemon has, the more low power moves they will have

# Multivariate - Special_Attack and Attack
ggplot(data = pokemon, mapping = aes(x = Special_attack, y = Attack)) +
  geom_point(aes(color = tier_bin))

# Next step: add line/box to highlight lack of light blue in lower values for
# each observation
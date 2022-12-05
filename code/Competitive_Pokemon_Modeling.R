# PROJECT DESCRIPTION ----
# This analysis is a final project for STAT 172
# Contributors: Katja Mathesius, Amanda Perrira, Ellie Reece
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

# DATA EXPLORATION ----
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
abilities_list <- c("Adaptability","Aerilate","Aftermath","Air Lock","Analytic",
                    "Anger Point","Anticipation","Arena Trap","Aroma Veil",
                    "As One","Aura Break","Bad Dreams","Ball Fetch","Battery",
                    "Battle Armor","Battle Bond","Beast Boost","Berserk",
                    "Big Pecks","Blaze","Bulletproof","Cheek Pouch","Chilling 
                    Neigh","Chlorophyll","Clear Body","Cloud Nine","Color Change",
                    "Comatose","Competitive","Compound Eyes","Contrary",
                    "Corrosion","Cotton Down","Curious Medicine","Cursed Body",
                    "Cute Charm","Damp","Dancer","Dark Aura","Dauntless Shield",
                    "Dazzling","Defeatist","Defiant","Delta Stream","Desolate 
                    Land","Disguise","Download","Dragon's Maw","Drizzle",
                    "Drought","Dry Skin","Early Bird","Effect Spore","Electric 
                    Surge","Emergency Exit","Fairy Aura","Filter","Flame Body",
                    "Flare Boost","Flash Fire","Flower Gift","Flower Veil",
                    "Fluffy","Forecast","Forewarn","Friend Guard","Frisk","Full 
                    Metal Body","Fur Coat","Gale Wings","Galvanize","Gluttony",
                    "Gooey","Gorilla Tactics","Grass Pelt","Grassy Surge","Grim
                    Neigh","Gulp Missile","Guts","Harvest","Healer","Heatproof",
                    "Heavy Metal","Honey Gather","Huge Power","Hunger Switch",
                    "Hustle","Hydration","Hyper Cutter","Ice Body","Ice Face",
                    "Ice Scales","Illuminate","Illusion","Immunity","Imposter",
                    "Infiltrator","Innards Out","Inner Focus","Insomnia","Intimidate",
                    "Intrepid Sword","Iron Barbs","Iron Fist","Justified","Keen Eye",
                    "Klutz","Leaf Guard","Levitate","Libero","Light Metal",
                    "Lightning Rod","Limber","Liquid Ooze","Liquid Voice","Long 
                    Reach","Magic Bounce","Magic Guard","Magician","Magma Armor",
                    "Magnet Pull","Marvel Scale","Mega Launcher","Merciless",
                    "Mimicry","Minus","Mirror Armor","Misty Surge","Mold Breaker",
                    "Moody","Motor Drive","Moxie","Multiscale","Multitype","Mummy",
                    "Natural Cure","Neuroforce","Neutralizing Gas","No Guard",
                    "Normalize","Oblivious","Overcoat","Overgrow","Own Tempo",
                    "Parental Bond","Pastel Veil","Perish Body","Pickpocket",
                    "Pickup","Pixilate","Plus","Poison Heal","Poison Point",
                    "Poison Touch","Power Construct","Power of Alchemy","Power 
                    Spot","Prankster","Pressure","Primordial Sea","Prism Armor",
                    "Propeller Tail","Protean","Psychic Surge","Punk Rock","Pure
                    Power","Queenly Majesty","Quick Draw","Quick Feet","Rain 
                    Dish","Rattled","Receiver","Reckless","Refrigerate","Regenerator",
                    "Ripen","Rivalry","RKS System","Rock Head","Rough Skin",
                    "Run Away","Sand Force","Sand Rush","Sand Spit","Sand Stream",
                    "Sand Veil","Sap Sipper","Schooling","Scrappy","Screen Cleaner",
                    "Serene Grace","Shadow Shield","Shadow Tag","Shed Skin",
                    "Sheer Force","Shell Armor","Shield Dust","Shields Down",
                    "Simple","Skill Link","Slow Start","Slush Rush","Sniper",
                    "Snow Cloak","Snow Warning","Solar Power","Solid Rock",
                    "Soul-Heart","Soundproof","Speed Boost","Stakeout","Stall",
                    "Stalwart","Stamina","Stance Change","Static","Steadfast",
                    "Steam Engine","Steelworker","Steely Spirit","Stench",
                    "Sticky Hold","Storm Drain","Strong Jaw","Sturdy","Suction 
                    Cups","Super Luck","Surge Surfer","Swarm","Sweet Veil",
                    "Swift Swim","Symbiosis","Synchronize","Tangled Feet",
                    "Tangling Hair","Technician","Telepathy","Teravolt","Thick 
                    Fat","Tinted Lens","Torrent","Tough Claws","Toxic Boost",
                    "Trace","Transistor","Triage","Truant","Turboblaze","Unaware",
                    "Unburden","Unnerve","Unseen Fist","Victory Star","Vital 
                    Spirit","Wandering Spirit","Water Absorb","Water Bubble",
                    "Water Compaction","Water Veil","Weak Armor","White Smoke",
                    "Wimp Out","Wonder Guard","Wonder Skin","Zen Mode")

abilities_names_list = c("Adaptability","Aerilate","Aftermath","Air_Lock","Analytic",
                    "Anger_Point","Anticipation","Arena_Trap","Aroma_Veil",
                    "As_One","Aura_Break","Bad_Dreams","Ball_Fetch","Battery",
                    "Battle_Armor","Battle_Bond","Beast_Boost","Berserk",
                    "Big_Pecks","Blaze","Bulletproof","Cheek_Pouch","Chilling_
Neigh","Chlorophyll","Clear_Body","Cloud_Nine","Color_Change",
                    "Comatose","Competitive","Compound_Eyes","Contrary",
                    "Corrosion","Cotton_Down","Curious_Medicine","Cursed_Body",
                    "Cute_Charm","Damp","Dancer","Dark_Aura","Dauntless_Shield",
                    "Dazzling","Defeatist","Defiant","Delta_Stream","Desolate_
Land","Disguise","Download","Dragon's_Maw","Drizzle",
                    "Drought","Dry_Skin","Early_Bird","Effect_Spore","Electric_
Surge","Emergency_Exit","Fairy_Aura","Filter","Flame_Body",
                    "Flare_Boost","Flash_Fire","Flower_Gift","Flower_Veil",
                    "Fluffy","Forecast","Forewarn","Friend_Guard","Frisk","Full_
Metal_Body","Fur_Coat","Gale_Wings","Galvanize","Gluttony",
                    "Gooey","Gorilla_Tactics","Grass_Pelt","Grassy_Surge","Grim
Neigh","Gulp_Missile","Guts","Harvest","Healer","Heatproof",
                    "Heavy_Metal","Honey_Gather","Huge_Power","Hunger_Switch",
                    "Hustle","Hydration","Hyper_Cutter","Ice_Body","Ice_Face",
                    "Ice_Scales","Illuminate","Illusion","Immunity","Imposter",
                    "Infiltrator","Innards_Out","Inner_Focus","Insomnia","Intimidate",
                    "Intrepid_Sword","Iron_Barbs","Iron_Fist","Justified","Keen_Eye",
                    "Klutz","Leaf_Guard","Levitate","Libero","Light_Metal",
                    "Lightning_Rod","Limber","Liquid_Ooze","Liquid_Voice","Long_
Reach","Magic_Bounce","Magic_Guard","Magician","Magma_Armor",
                    "Magnet_Pull","Marvel_Scale","Mega_Launcher","Merciless",
                    "Mimicry","Minus","Mirror_Armor","Misty_Surge","Mold_Breaker",
                    "Moody","Motor_Drive","Moxie","Multiscale","Multitype","Mummy",
                    "Natural_Cure","Neuroforce","Neutralizing_Gas","No_Guard",
                    "Normalize","Oblivious","Overcoat","Overgrow","Own_Tempo",
                    "Parental_Bond","Pastel_Veil","Perish_Body","Pickpocket",
                    "Pickup","Pixilate","Plus","Poison_Heal","Poison_Point",
                    "Poison_Touch","Power_Construct","Power_of_Alchemy","Power_
Spot","Prankster","Pressure","Primordial_Sea","Prism_Armor",
                    "Propeller_Tail","Protean","Psychic_Surge","Punk_Rock","Pure
Power","Queenly_Majesty","Quick_Draw","Quick_Feet","Rain_
Dish","Rattled","Receiver","Reckless","Refrigerate","Regenerator",
                    "Ripen","Rivalry","RKS_System","Rock_Head","Rough_Skin",
                    "Run_Away","Sand_Force","Sand_Rush","Sand_Spit","Sand_Stream",
                    "Sand_Veil","Sap_Sipper","Schooling","Scrappy","Screen_Cleaner",
                    "Serene_Grace","Shadow_Shield","Shadow_Tag","Shed_Skin",
                    "Sheer_Force","Shell_Armor","Shield_Dust","Shields_Down",
                    "Simple","Skill_Link","Slow_Start","Slush_Rush","Sniper",
                    "Snow_Cloak","Snow_Warning","Solar_Power","Solid_Rock",
                    "Soul-Heart","Soundproof","Speed_Boost","Stakeout","Stall",
                    "Stalwart","Stamina","Stance_Change","Static","Steadfast",
                    "Steam_Engine","Steelworker","Steely_Spirit","Stench",
                    "Sticky_Hold","Storm_Drain","Strong_Jaw","Sturdy","Suction_
Cups","Super_Luck","Surge_Surfer","Swarm","Sweet_Veil",
                    "Swift_Swim","Symbiosis","Synchronize","Tangled_Feet",
                    "Tangling_Hair","Technician","Telepathy","Teravolt","Thick_
Fat","Tinted_Lens","Torrent","Tough_Claws","Toxic_Boost",
                    "Trace","Transistor","Triage","Truant","Turboblaze","Unaware",
                    "Unburden","Unnerve","Unseen_Fist","Victory_Star","Vital_
Spirit","Wandering_Spirit","Water_Absorb","Water_Bubble",
                    "Water_Compaction","Water_Veil","Weak_Armor","White_Smoke",
                    "Wimp_Out","Wonder_Guard","Wonder_Skin","Zen_Mode")

str(pokemon)
summary(pokemon)
head(pokemon)
describe(pokemon)

# Checks the number of overused pokemon in the data set
length(which(pokemon$Comp_Bin==1))

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

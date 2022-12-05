#GETTING STARTED ----
rm(list = ls())
pokemon = read.csv("pokemon-data.csv", sep=";", header=T, stringsAsFactors = TRUE)
head(pokemon) 
str(pokemon) 
dim(pokemon) 
#String - Name, Tier
#List of strings - Types, Abilities, Next Evolution(s), Moves (movenames)
#Integers - HP, Attack, Defense, Special.Attack, Special.Defense, Speed
View(pokemon)

moves = read.csv("move-data.csv", sep=',', header = T)
View(moves)
head(moves)
str(moves)
summary(moves$Power)
subset(moves, moves$Power == 'None')
#Strings - Name, Type, Category, Contest
#Integers - PP, Power, Accuracy, Gen
#Int or None - Power, Accuracy

#########CLEANING DATA-----
# BINARY VARIABLE - EVOLUTION ----
#Make Character Type
pokemon$Next.Evolution.s.= as.character(pokemon$Next.Evolution.s.)
#If string length > 3, then yes evolution... 3 because of '[]' entries
pokemon$evol_bin <- ifelse(nchar(pokemon$Next.Evolution.s.) >= 3, 1, 0)
#Checking Result
unique(pokemon$evol_bin)

#import packages
library(randomForest)
library(ggplot2)
library(pROC)
library(dplyr)
library(tidyr)
library(stringr)

#### column names 
colnames(pokemon) <- c('Poke_name','Poke_type','Abilities','Tier','Hit_points',
                       'Attack','Defense','Special_attack','Speed','Special_defense',
                       'Next_evolution','Moves')

colnames(moves) <- c('Index','Move_name','Move_type','Category','Contest','Power_points','Power',
                     'Accuracy','Generation')
head(pokemon)
head(moves)

#MISSING VALUES ----
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

pokemon$Comp_Bin <- ifelse(pokemon$Tier == "OU", 1, 0)

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

str(pokemon)
summary(pokemon)
head(pokemon)
describe(pokemon)

# Checks the number of overused pokemon in the data set
length(which(pokemon$Comp_Bin==1))

# Create a binary variable for typings
for(i in seq_along(elemental_types)){
  pokemon[elemental_types[i]] <- ifelse(str_detect(pokemon$Types, elemental_types[i]), 1, 0)
}

# Create a binary variable for abilities
for(i in seq_along(abilities_list)){
  pokemon[abilities_list[i]] <- ifelse(str_detect(pokemon$Abilities, abilities_list[i]), 1, 0)
}

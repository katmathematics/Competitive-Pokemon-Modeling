#GETTING STARTED ----
rm(list = ls())
#import packages
library(randomForest)
library(ggplot2)
library(pROC)
library(dplyr)
library(tidyr)
library(stringr)
library(RColorBrewer)


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

#### column names 
colnames(pokemon) <- c('Poke_name','Poke_type','Abilities','Tier','Hit_points',
                       'Attack','Defense','Special_attack','Speed','Special_defense',
                       'Next_evolution','Moves')

colnames(moves) <- c('Index','Move_name','Move_type','Category','Contest','Power_points','Power',
                     'Accuracy','Generation')
head(pokemon)
head(moves)
moves$Power
#########CLEANING DATA-----
# BINARY VARIABLE - EVOLUTION ----
#Make Character Type
pokemon$Next_evolution= as.character(pokemon$Next_evolution)
#If string length > 3, then yes evolution... 3 because of '[]' entries
pokemon$evol_bin <- ifelse(nchar(pokemon$Next_evolution) >= 3, 1, 0)
#Checking Result
unique(pokemon$evol_bin)

View(pokemon)
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
sum(moves$Accurary =='None')
sum(moves$Power == 'None')

summary(moves$Power)
unique(moves$Power)


#BINARY Y (Tier)----
pokemon$tier_bin <- ifelse(pokemon$Tier =="OU", 1,0)
head(pokemon)
summary(pokemon$Tier)
summary(pokemon$tier_bin)

View(moves)
# BINARY VARIABLE - ABILITIES ----

# VARIABLE CLEANING - MOVES  ----
#counting number of each type of move in the moveslist for each pokemon
#moves$Power into 4 categories 
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

pokemon$HighPowerCount <- pokemon$ModeratePowerCount <- pokemon$LowPowerCount <-
  pokemon$NoPowerCount <- pokemon$UnknownPowerCount <- pokemon$MoveCount <- 0

for(i in 1:918){
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
head(pokemon)
#this works, some pokemon have the same exact moves
View(pokemon)
# DUMMY VARIABLES - TYPES & ABILITIES ----

elemental_types <- c("Normal", "Fire", "Water", "Grass", "Electric", "Ice", 
                     "Fighting", "Poison", "Ground", "Flying", "Psychic", "Bug", 
                     "Rock", "Ghost", "Dark", "Dragon", "Steel", "Fairy")
abilities_names_list = c("Adaptability","Aerilate","Aftermath","Air_Lock","Analytic",
                         "Anger_Point","Anticipation","Arena_Trap","Aroma_Veil",
                         "As_One","Aura_Break","Bad_Dreams","Ball_Fetch","Battery",
                         "Battle_Armor","Battle_Bond","Beast_Boost","Berserk",
                         "Big_Pecks","Blaze","Bulletproof","Cheek_Pouch","Chilling_Neigh",
                         "Chlorophyll","Clear_Body","Cloud_Nine","Color_Change",
                         "Comatose","Competitive","Compound_Eyes","Contrary",
                         "Corrosion","Cotton_Down","Curious_Medicine","Cursed_Body",
                         "Cute_Charm","Damp","Dancer","Dark_Aura","Dauntless_Shield",
                         "Dazzling","Defeatist","Defiant","Delta_Stream","Desolate_Land",
                         "Disguise","Download","Dragons_Maw","Drizzle",
                         "Drought","Dry_Skin","Early_Bird","Effect_Spore","Electric_Surge",
                         "Emergency_Exit","Fairy_Aura","Filter","Flame_Body",
                         "Flare_Boost","Flash_Fire","Flower_Gift","Flower_Veil",
                         "Fluffy","Forecast","Forewarn","Friend_Guard","Frisk","Full_Metal_Body",
                         "Fur_Coat","Gale_Wings","Galvanize","Gluttony",
                         "Gooey","Gorilla_Tactics","Grass_Pelt","Grassy_Surge","GrimNeigh",
                         "Gulp_Missile","Guts","Harvest","Healer","Heatproof",
                         "Heavy_Metal","Honey_Gather","Huge_Power","Hunger_Switch",
                         "Hustle","Hydration","Hyper_Cutter","Ice_Body","Ice_Face",
                         "Ice_Scales","Illuminate","Illusion","Immunity","Imposter",
                         "Infiltrator","Innards_Out","Inner_Focus","Insomnia","Intimidate",
                         "Intrepid_Sword","Iron_Barbs","Iron_Fist","Justified","Keen_Eye",
                         "Klutz","Leaf_Guard","Levitate","Libero","Light_Metal",
                         "Lightning_Rod","Limber","Liquid_Ooze","Liquid_Voice","Long_Reach",
                         "Magic_Bounce","Magic_Guard","Magician","Magma_Armor",
                         "Magnet_Pull","Marvel_Scale","Mega_Launcher","Merciless",
                         "Mimicry","Minus","Mirror_Armor","Misty_Surge","Mold_Breaker",
                         "Moody","Motor_Drive","Moxie","Multiscale","Multitype","Mummy",
                         "Natural_Cure","Neuroforce","Neutralizing_Gas","No_Guard",
                         "Normalize","Oblivious","Overcoat","Overgrow","Own_Tempo",
                         "Parental_Bond","Pastel_Veil","Perish_Body","Pickpocket",
                         "Pickup","Pixilate","Plus","Poison_Heal","Poison_Point",
                         "Poison_Touch","Power_Construct","Power_of_Alchemy","Power_Spot",
                         "Prankster","Pressure","Primordial_Sea","Prism_Armor",
                         "Propeller_Tail","Protean","Psychic_Surge","Punk_Rock","PurePower",
                         "Queenly_Majesty","Quick_Draw","Quick_Feet","Rain_Dish","Rattled",
                         "Receiver","Reckless","Refrigerate","Regenerator",
                         "Ripen","Rivalry","RKS_System","Rock_Head","Rough_Skin",
                         "Run_Away","Sand_Force","Sand_Rush","Sand_Spit","Sand_Stream",
                         "Sand_Veil","Sap_Sipper","Schooling","Scrappy","Screen_Cleaner",
                         "Serene_Grace","Shadow_Shield","Shadow_Tag","Shed_Skin",
                         "Sheer_Force","Shell_Armor","Shield_Dust","Shields_Down",
                         "Simple","Skill_Link","Slow_Start","Slush_Rush","Sniper",
                         "Snow_Cloak","Snow_Warning","Solar_Power","Solid_Rock",
                         "Soul_Heart","Soundproof","Speed_Boost","Stakeout","Stall",
                         "Stalwart","Stamina","Stance_Change","Static","Steadfast",
                         "Steam_Engine","Steelworker","Steely_Spirit","Stench",
                         "Sticky_Hold","Storm_Drain","Strong_Jaw","Sturdy","Suction_Cups",
                         "Super_Luck","Surge_Surfer","Swarm","Sweet_Veil",
                         "Swift_Swim","Symbiosis","Synchronize","Tangled_Feet",
                         "Tangling_Hair","Technician","Telepathy","Teravolt","Thick_Fat",
                         "Tinted_Lens","Torrent","Tough_Claws","Toxic_Boost",
                         "Trace","Transistor","Triage","Truant","Turboblaze","Unaware",
                         "Unburden","Unnerve","Unseen_Fist","Victory_Star","Vital_Spirit",
                         "Wandering_Spirit","Water_Absorb","Water_Bubble",
                         "Water_Compaction","Water_Veil","Weak_Armor","White_Smoke",
                         "Wimp_Out","Wonder_Guard","Wonder_Skin","Zen_Mode")


str(pokemon)
summary(pokemon)
head(pokemon)
describe(pokemon)

# Create a binary variable for typings
for(i in seq_along(elemental_types)){
  pokemon[elemental_types[i]] <- ifelse(str_detect(pokemon$Poke_type, elemental_types[i]), 1, 0)
}

# Create a binary variable for abilities
for(i in seq_along(abilities_names_list)){
  pokemon[abilities_names_list[i]] <- ifelse(str_detect(pokemon$Abilities, abilities_names_list[i]), 1, 0)
}
View(pokemon)
##### RANDOM FOREST -----
#X variable selection
#drop old variables
pokemon = subset(pokemon, select = -c(Poke_name,Poke_type, Abilities, Tier, Next_evolution, Moves))
View(pokemon)

RNGkind(sample.kind="default")
set.seed(123)
train.idx <- sample(x=1:nrow(pokemon),size = .8*nrow(pokemon))
train.df <- pokemon[train.idx,]
test.df <- pokemon[-train.idx,]

View(train.df)

train.df$tier_bin = as.factor(train.df$tier_bin)
test.df$tier_bin = as.factor(test.df$tier_bin)

length(colnames(pokemon))
#baseline forest
myforest <- randomForest(tier_bin ~ ., #remember dangers here
                         data = train.df, #training data
                         ntree = 1000, #B
                         mtry = 17, #choose m - sqrt(297) ~17 
                         importance = TRUE,
                         method = 'class')

myforest
#OOB error rate estimate is 5.59% 

mtry = c(8,17,34,68,100,150)
keeps <- data.frame(m=rep(NA, length(mtry)),
                    OOB_error_rate = rep(NA, length(mtry)))

for(idx in 1:length(mtry)){
  print(paste0("Fitting m = ", mtry[idx]))
  tempforest <- randomForest(tier_bin ~.,
                             data = train.df,
                             ntree = 1000,
                             mtry = mtry[idx]) #mtry is varying
  #record ))B error, corresponding mtry for each forest fit
  keeps[idx,"m"] <- mtry[idx]
  keeps[idx, "OOB_error_rate"] <- mean(predict(tempforest) != train.df$tier_bin)
}
#str(train.df)

keeps

#plot OOB error rate for different mtry vals
ggplot(data = keeps)+
  geom_line(aes(x = m, y = OOB_error_rate))+
  theme_bw() + labs(x = "m (mtry) value", y = "OOB error rate")+
  scale_x_continuous(breaks = c(8,17,34,68,100))


final_forest <- randomForest(tier_bin ~ ., #remember dangers here
                                 data = train.df, #training data
                                 ntree = 1000, #B
                                 mtry = 34, #try 2* initial mtry value 
                                 importance = TRUE,
                                 method = 'class')
final_forest


#Create ROC Curve 
#assuming positive event is 1 -"OU"
#extract prob. of positive event 
pi_hat <- predict(final_forest,test.df, type="prob")[,"1"]
#pi hat is a vector of predicted probabilities

rocCurve <- roc(response = test.df$tier_bin, 
                predictor = pi_hat,
                levels = c("0","1")) #Order matters here

plot(rocCurve, print.thres=TRUE, print.auc=TRUE)


pi_star <- coords(rocCurve, "best", ret = "threshold")$threshold[1]
#Interpreation: If the forest predicts an overuse probabilty greater than 
#0.0165 (pi_star), we predict that the Pokemon is overused. Otherwise, we predict a loss. 

#Let's overwrite our old predictions with these better ones 
test.df$forest_pred <- as.factor(ifelse(pi_hat > pi_star,"1","0"))
test.df$forest_pred



#This is more acceptable than the default predict() function 
#because it uses pi_star, rather than the default 0.5.

####### INTERPRETATION USING FOREST ----
#Using random forests as a tool to understand relationships between x and y 
#random forests will give us a ranked list of variable importance
#"variable importance": How much would our out of sample error suffer if the 
#variable was removed from the model. (reflects how well they will help predict on NEW data)

varImpPlot(final_forest, type=1) #must specify importance = TRUE in forest 
#too many variables to include in this plot, but a few seemingly more important 
#ones for sure 

### RF pro: automatic variable selection 
### RF con: no directional effects 
### logistic regression con: no automatic variable selection 
### logistic regression pro: very good interpreatations/ directional effects 

#Pair random forest and logistic regression 
#Specifically, use random forest both as a prediction tool 
#AND to help inform what variables go into a logistic regression. 

#### LOGISTIC REGRESSION-----
#First create a bernoulli RV 
#pokemon$tier_bin <- ifelse(pokemon$tier_bin == "W",1,0)
pokemon$tier_bin <- as.numeric(pokemon$tier_bin)

data <- pokemon
dim(data)
## [1] 150   4


#This is with mtry = 68
m0 <- glm(tier_bin ~ Attack + UnknownPowerCount + 
            ModeratePowerCount + LowPowerCount + Special_attack + NoPowerCount, data = pokemon, 
          family = binomial(link="logit"))
AIC(m0) #377.5363

m1 <- glm(tier_bin ~ Attack + Special_attack + MoveCount, data = pokemon, 
          family = binomial(link="logit"))
#warning message: fitted probabilities numerically 0 or 1 occured  
#can ignore, increase sample size, remove outliers
AIC(m1) #375.6081

plot(pokemon$ModeratePowerCount)
m2 <- glm(tier_bin ~ Attack + Special_attack + MoveCount + ModeratePowerCount, data = pokemon, 
          family = binomial(link="logit"))
AIC(m2) #376.5345
#forest implied importance and using AIC to gauge whether they should be kept. 

#This is with mtry = 34 
m3 <- glm(tier_bin ~ Attack + LowPowerCount + MoveCount + Special_attack, data = pokemon, 
          family = binomial(link="logit"))
AIC(m3)#374.9328

m4 <- glm(tier_bin ~ Attack + LowPowerCount + MoveCount + Special_attack + UnknownPowerCount, data = pokemon, 
          family = binomial(link="logit"))
AIC(m4)#376.5889

#Final Model with lowest AIC
#My thoughts with move count variable - I think we should leave it in because 
#AIC comparisson suggets that the model with move count is better 
#Though total move count is a sum of each individual power count, it is representing 
#something different in a way. like it is looking at the sheer number of moves that a pokemon has 
#and whether it effects OU rather than looking at how the power of the moves effect it. 
#So it is the sum of other variables but it represents something different than the variables do
final_glm <- glm(tier_bin ~ Attack + LowPowerCount + MoveCount + Special_attack, data = pokemon, 
          family = binomial(link="logit"))

AIC(final_glm)#AIC = 374.9328
summary(final_glm)
###### INTERPRETATIONS/PREDICTIONS-----

###### VISUALIZATIONS---- 
#Univariate distributions - 
#tier
data$tier_bin = as.factor(data$tier_bin)
ggplot(data, aes(x=tier_bin))+
  geom_bar(fill = "cornflowerblue",color="black") + 
  geom_text(aes(label=..count..), stat = "count",vjust = 1.5, color = "black")+
  labs(x="Overused/Not Overused",y="Frequency", title = "Pokémon by tier")+
  scale_x_discrete("Pokémon Tier", 
                      labels = c("Not Overused", "Overused"))
#We see quite the unbalance in our data here
#862 pokemon are not overused, while only 56 are. 

#First variable - Attack 
#Attack distribution by tier 
ggplot(data, 
       aes(x = Attack, 
           fill = tier_bin)) +
  geom_density(alpha = 0.4) +
  labs(title = "Pokémon Attack Statistic Distribution by Tier", x = "Attack",y = "Density")+
  scale_fill_discrete("Pokémon Tier", 
                      labels = c("Not Overused", "Overused"))
#Comments: The mean attack value for overused pokemon appears to be higher than 
#the mean attack value for non-overused pokemon
#The distribution for overused looks slightly right skewed, 
#with a greater density of values occuring at higher attack levels. 
#We see the blue lays to the right of the red, which indicates that 
#overused pokemon have a higher attack value than non overused ones 

#Special 
ggplot(data, 
       aes(x = Special_attack, 
           fill = tier_bin)) +
  geom_density(alpha = 0.4) +
  labs(title = "Pokémon Special Attack Statistic Distribution by Tier", x = "Special Attack", y = "Density")+
  scale_fill_discrete("Pokémon Tier", 
                      labels = c("Not Overused", "Overused"))
#Comments: the distribution of special attack for non overused pokemon
#is slightly right skewed, whereas the distribution of special attack for 
#overused pokemon is more normal. 
#A higher frequency of non overused pokemon have a lower special attack
#we see that blue lays to the right of the red, which indicates that overused
#pokemon have a higher special attack value than non overused pokemon. 

#Move Count
ggplot(data, 
       aes(x = MoveCount, 
           fill = tier_bin)) +
  geom_density(alpha = 0.4) +
  labs(title = "Pokémon Move Count Distribution by Tier", x = "Move Count", y = "Density")+
  scale_fill_discrete("Pokémon Tier", 
                      labels = c("Not Overused", "Overused"))
#Comments: 
#unlike the previous 2 plots, we do not see the blue laying much farther to the right of
#red. In some places this is true, but not througohut. 
#We see a higher density for non-overused pokemon around the mean, and a lower 
#density around more extreme values, when compared to overused poke which
#have a lower density centered around the mean and greater density 
#elsewhere, like in extreme values. 
#We observe a small bump in the far right of the plot for overused pokemon
#there are some overused with an extremely high move count, but no
#non-overused ones matching that count. 

ggplot(data, 
       aes(x = LowPowerCount, 
           fill = tier_bin)) +
  geom_density(alpha = 0.4) +
  labs(title = "Pokémon Low Power Move Count Distribution by Tier", x = "Low Power Move Count", y = "Density")+
  scale_fill_discrete("Pokémon Tier", 
                      labels = c("Not Overused", "Overused"))
#Comments: 
#We see a very similar relationship to move count. 
#We suspect this may be due to the fact that move count and low power count 
#are highly related. 
#We still feel that it is beneficial to include both in our model because
#low power count considers the power aspect of the moves list, 
#while move count only considers that length of that list and not the types of 
#moves in it. 

#Mtultivariate - Special_attack and attack
ggplot(data = data, mapping = aes(x = Special_attack, y = Attack)) +
  geom_point(aes(color = tier_bin))+
  labs(title = "Pokémon Special Attack vs Attack Statistic by Tier", x= "Special Attack",y = "Attack")+
  scale_color_discrete("Pokémon Tier",
                      labels = c("Not Overused", "Overused"))

#Comments:
#We see a positive relationship between attack and special attack 
#We also see random scatter of blue dots for special attack and attack values 
#both above 50. 
#We only observe one overused pokemon with a special attack and attack lower 
#than 50.
#though moderate to high special attack and attack values may not 
#indicate overuse, very low values for both might 

ggplot(data = data, mapping = aes(x = MoveCount, y = LowPowerCount)) +
  geom_point(aes(color = tier_bin))+
  labs(title = "Pokémon Move Count vs Low Power Move Count by Tier", x = "Move Count",y = "Low Power Move Count")+
  scale_color_discrete("Pokémon Tier",
                       labels = c("Not Overused", "Overused"))
#Comments: 
#We observe a strong, positive relationship between the two variables
#we also observe that pokemon with move count below 50 and low power move count below 50
#are all not overused
#extremely low values for both variable may indicate non-overuse


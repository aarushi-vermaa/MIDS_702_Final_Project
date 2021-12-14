rm(list = ls())
library(arm)
library(pROC)
library(e1071)
library(caret)
library(ggplot2)
library(dplyr)


#Reading the data
data <- read.csv('/Users/Aarushi/Duke/MIDS - Fall 2021/Fall 2021/702_IDS/Final Projects/in-vehicle-coupon-recommendation.csv')

#Looking at the data
dim(data)
str(data)
# The data has 12684 rows ans 26 columns


#Check for null values
sum(is.na(data))
#There are no null values

#Looking at unique values in columns
unique(data$car) #- #Empty values
unique(data$Bar) # - Null values
unique(data$CoffeeHouse) #Null values
unique(data$CarryAway) #EMpty values
unique(data$RestaurantLessThan20) #EMpty values
unique(data$Restaurant20To50) #EMpty values
unique(data$toCoupon_GEQ5min) #Only one unique value


#Converting blann spaces in columns to NA
data$car[data$car ==""] <- NA
data$CoffeeHouse[data$CoffeeHouse ==""] <- NA
data$CarryAway[data$CarryAway==""] <- NA
data$RestaurantLessThan20[data$RestaurantLessThan20 ==""] <- NA
data$Restaurant20To50[data$Restaurant20To50 ==""] <- NA
data$Bar[data$Bar ==""] <- NA

#Checking for null values again
namedCounts <- sapply(data, function(x) round((sum(is.na(x))/length(x))*100,2))
namedCounts <- namedCounts[namedCounts>0]
cat("Columns with missing value\n")
print(paste0(names(namedCounts)," :",unname(namedCounts),"%"))

#The column car has almost 100% missing values 
#whereas 4 other columns with less than 2% missing values
#Dropping car column and all rows with missing values

data_new<- subset(data, select= -c(car))
str(data_new)

coupon <- na.omit(data_new)

namedCounts <- sapply(coupon, function(x) round((sum(is.na(x))/length(x))*100,2))
namedCounts <- namedCounts[namedCounts>0]
cat("Columns with missing value\n")
print(paste0(names(namedCounts)," :",unname(namedCounts),"%"))

#No more null values in the dataset

###############################################Cleaning the data####################
coupon$coupon_accepted <- as.factor(coupon$Y)

#Renaming values in passenger column
coupon$passanger[which(coupon$passanger=="Friend(s)")] <- "Friends"
coupon$passanger[which(coupon$passanger=="Kid(s)")] <- "Kids"


#Created more subsets in the occupation column
unique(coupon$occupation)

coupon$occupation_class <- NA                                 

coupon$occupation_class[which(coupon$occupation %in% 
                              c("Healthcare Support", 
                                "Healthcare Practitioners & Technical"))] <- "Healthcare"

coupon$occupation_class[which(coupon$occupation %in% 
                              c("Life Physical Social Science", 
                                "Community & Social Services"))] <- "Social"

coupon$occupation_class[which(coupon$occupation %in% 
                              c("Construction & Extraction","Installation Maintenance & Repair", 
                                "Building & Grounds Cleaning & Maintenance", 
                                "Transportation & Material Moving","Food Preparation & Serving Related"))] <- "Trade Workers"

coupon$occupation_class[which(coupon$occupation %in% 
                              c("Sales & Related","Management","Office & Administrative Support",
                                "Business & Financial", "Legal","Education&Training&Library",
                                "Architecture & Engineering", "Arts Design Entertainment Sports & Media",
                                "Computer & Mathematical"))] <- "White Collar"

coupon$occupation_class[which(coupon$occupation %in% 
                              c("Retired"))] <- "Retired"

coupon$occupation_class[which(coupon$occupation %in% 
                              c("Student"))] <- "Student"

coupon$occupation_class[which(coupon$occupation %in% 
                              c("Production Occupations","Protective Service" ,
                                "Personal Care & Service" , "Farming Fishing & Forestry"))] <- "Others"

coupon$occupation_class[which(coupon$occupation %in% 
                              c("Unemployed"))] <- "Unemployed"
table(coupon$occupation)
table(coupon$occupation_class)

unique(coupon$education)
coupon$education[coupon$education == 'Graduate degree (Masters or Doctorate)'] <- 'Graduate degree'
coupon$education[coupon$education == 'Some college - no degree'] <- 'College - no degree'
coupon$education[coupon$education == 'Some High School'] <- 'High School - no degree'
unique(coupon$education)

########Age########
unique(coupon$age)
coupon$age[coupon$age == '21'] <- '21-30'
coupon$age[coupon$age == '26'] <- '21-30'
coupon$age[coupon$age == '31'] <- '31-40'
coupon$age[coupon$age == '36'] <- '31-40'
coupon$age[coupon$age == '41'] <- '41-50'
coupon$age[coupon$age == '46'] <- '41-50'
unique(coupon$age)

###########Restaurant20To50
unique(coupon$Restaurant20To50)
coupon$Restaurant20To50[coupon$Restaurant20To50 == '1~3'] <- '1-3'
coupon$Restaurant20To50[coupon$Restaurant20To50 == 'less1'] <- 'less than 1'
coupon$Restaurant20To50[coupon$Restaurant20To50 == 'gt8'] <- 'greater than 8'
coupon$Restaurant20To50[coupon$Restaurant20To50 == '4~8'] <- '4-8'
unique(coupon$Restaurant20To50)

###########RestaurantLessThan20To50
unique(coupon$RestaurantLessThan20)
coupon$RestaurantLessThan20[coupon$RestaurantLessThan20 == '1~3'] <- '1-3'
coupon$RestaurantLessThan20[coupon$RestaurantLessThan20 == 'less1'] <- 'less than 1'
coupon$RestaurantLessThan20[coupon$RestaurantLessThan20 == 'gt8'] <- 'greater than 8'
coupon$RestaurantLessThan20[coupon$RestaurantLessThan20 == '4~8'] <- '4-8'
unique(coupon$RestaurantLessThan20)

###########Bar
unique(coupon$Bar)
coupon$Bar[coupon$Bar == '1~3'] <- '1-3'
coupon$Bar[coupon$Bar == 'less1'] <- 'less than 1'
coupon$Bar[coupon$Bar == 'gt8'] <- 'greater than 8'
coupon$Bar[coupon$Bar == '4~8'] <- '4-8'
unique(coupon$Bar)

###########CoffeeHouse
unique(coupon$CoffeeHouse)
coupon$CoffeeHouse[coupon$CoffeeHouse == '1~3'] <- '1-3'
coupon$CoffeeHouse[coupon$CoffeeHouse == 'less1'] <- 'less than 1'
coupon$CoffeeHouse[coupon$CoffeeHouse == 'gt8'] <- 'greater than 8'
coupon$CoffeeHouse[coupon$CoffeeHouse == '4~8'] <- '4-8'
unique(coupon$CoffeeHouse)

###########CarryAway
unique(coupon$CarryAway)
coupon$CarryAway[coupon$CarryAway == '1~3'] <- '1-3'
coupon$CarryAway[coupon$CarryAway == 'less1'] <- 'less than 1'
coupon$CarryAway[coupon$CarryAway == 'gt8'] <- 'greater than 8'
coupon$CarryAway[coupon$CarryAway == '4~8'] <- '4-8'
unique(coupon$CarryAway)


#Converting character variables to factor variables
coupon$destination <- as.factor(coupon$destination) 
coupon$passanger <- as.factor(coupon$passanger) 
coupon$weather <- as.factor(coupon$weather)
coupon$time <- as.factor(coupon$time)
coupon$gender <- as.factor(coupon$gender)
coupon$coupon <- as.factor(coupon$coupon)
coupon$expiration <- as.factor(coupon$expiration)
coupon$age <- as.factor(coupon$age)
coupon$Bar <- as.factor(coupon$Bar)
coupon$maritalStatus <- as.factor(coupon$maritalStatus)
coupon$education <- as.factor(coupon$education)
coupon$occupation_class <- as.factor(coupon$occupation_class)
coupon$income <- as.factor(coupon$income)
coupon$CoffeeHouse <- as.factor(coupon$CoffeeHouse)
coupon$CarryAway <- as.factor(coupon$CarryAway)
coupon$Restaurant20To50 <- as.factor(coupon$Restaurant20To50)
coupon$RestaurantLessThan20 <- as.factor(coupon$RestaurantLessThan20)
coupon$temperature <- as.factor(coupon$temperature)
coupon$direction_opp <- as.factor(coupon$direction_opp)
coupon$toCoupon_GEQ15min <- as.factor(coupon$toCoupon_GEQ15min)
coupon$toCoupon_GEQ25min <- as.factor(coupon$toCoupon_GEQ25min)
coupon$has_children <- as.factor(coupon$has_children)

#Dropping GEQ5 min because only single value. Direction same and opp are correlated with -1
#Also dropping temp because weather and temp are very similar
coupon_sub<- subset(coupon, select= -c(direction_same, toCoupon_GEQ5min, temperature))


###########EDA################

table(coupon[,c("destination","coupon_accepted")])
table(coupon[,c("destination","Y")])/sum(table(coupon[,c("destination","Y")]))

table(coupon[,c("Y")])/sum(table(coupon[,c("Y")]))

table(coupon[,c("toCoupon_GEQ25min","Y")])
table(coupon[,c("toCoupon_GEQ25min","Y")])/sum(table(coupon[,c("toCoupon_GEQ25min","Y")]))

###########################################################################################
apply(table(coupon_sub[,c("Y","destination")])/sum(table(coupon_sub[,c("Y","destination")])),
      2,function(x) x/sum(x))
## Big change in Prob for No Urgent Place

apply(table(coupon_sub[,c("Y","passanger")])/sum(table(coupon_sub[,c("Y","passanger")])),
      2,function(x) x/sum(x))
# Higher probability fir Friends, lease for KIDS

apply(table(coupon_sub[,c("Y","weather")])/sum(table(coupon_sub[,c("Y","weather")])),
      2,function(x) x/sum(x))
# Highest in Sunny

apply(table(coupon_sub[,c("Y","time")])/sum(table(coupon_sub[,c("Y","time")])),
      2,function(x) x/sum(x))
#Highest around 10 AM and 2PM (Meal Times)

apply(table(coupon_sub[,c("Y","coupon_sub")])/sum(table(coupon_sub[,c("Y","coupon")])),
      2,function(x) x/sum(x))
#Highest for Carryout and TakeAway and Restaurant <20

apply(table(coupon_sub[,c("Y","expiration")])/sum(table(coupon_sub[,c("Y","expiration")])),
      2,function(x) x/sum(x))
#Highest for 24 hour validity

apply(table(coupon_sub[,c("Y","gender")])/sum(table(coupon_sub[,c("Y","gender")])),
      2,function(x) x/sum(x))
#Male driver higher prob

apply(table(coupon_sub[,c("Y","age")])/sum(table(coupon_sub[,c("Y","age")])),
      2,function(x) x/sum(x))
#Highest for below 21

apply(table(coupon_sub[,c("Y","maritalStatus")])/sum(table(coupon_sub[,c("Y","maritalStatus")])),
      2,function(x) x/sum(x))


apply(table(coupon_sub[,c("Y","has_children")])/sum(table(coupon_sub[,c("Y","has_children")])),
      2,function(x) x/sum(x))
#Highest for Single

apply(table(coupon_sub[,c("Y","education")])/sum(table(coupon_sub[,c("Y","education")])),
      2,function(x) x/sum(x))
#High School No degree highest

apply(table(coupon_sub[,c("Y","occupation_class")])/sum(table(coupon_sub[,c("Y","occupation_class")])),
      2,function(x) x/sum(x))
#Highest for Healthcare

apply(table(coupon_sub[,c("Y","income")])/sum(table(coupon_sub[,c("Y","income")])),
      2,function(x) x/sum(x))
#More or less than the same

apply(table(coupon_sub[,c("Y","toCoupon_GEQ15min")])/sum(table(coupon_sub[,c("Y","toCoupon_GEQ15min")])),
      2,function(x) x/sum(x))

apply(table(coupon_sub[,c("Y","direction_opp")])/sum(table(coupon_sub[,c("Y","direction_opp")])),
      2,function(x) x/sum(x))


table(coupon_sub[,c("education","Y")])
table(coupon_sub[,c("education","Y")])/sum(table(coupon_sub[,c("education","Y")]))

table(coupon_sub[,c("income","Y")])
table(coupon_sub[,c("income","Y")])/sum(table(coupon_sub[,c("income","Y")]))


table(coupon_sub[,c("destination","Y")])
table(coupon_sub[,c("destination","Y")])/sum(table(coupon_sub[,c("destination","Y")]))
chisq.test(table(coupon_sub[,c("Y","passanger")]))

table(coupon$has_children)



p1 <- ggplot(coupon, aes(x=destination, fill=Y)) +
  geom_bar(stat="count")
p1
g1 <- ggplot(coupon, aes(x=destination, fill=Y)) +
  geom_bar(position="fill")


ggplot(coupon, aes(x=temperature, fill=Y)) +
  geom_bar(stat ="count")


## Plotting data columns

library(gridExtra)
dim(data)



#Destination
p1 <- ggplot(coupon, aes(x=destination, fill=coupon_accepted)) +
  geom_bar(stat="count")
g1 <- ggplot(coupon, aes(x=destination, fill=Y)) +
  geom_bar(position="fill")

#passenger 
p2 <- ggplot(coupon, aes(x=passanger, fill=Y)) +
  geom_bar(stat="count")
g2 <- ggplot(coupon, aes(x=passanger, fill=Y)) +
  geom_bar(position="fill")

#weather
p3 <- ggplot(coupon, aes(x=weather, fill=Y)) +
  geom_bar(stat="count")
g3 <- ggplot(coupon, aes(x=weather, fill=Y)) +
  geom_bar(position="fill")

#time
#p4 <- ggplot(cleaned_data, aes(x=time, fill=Y)) +
#geom_bar(stat="count")

#gender
# p5 <- ggplot(cleaned_data, aes(x=gender, fill=Y)) +
#   geom_bar(stat="count")

#maritalStatus   
p6 <- ggplot(coupon, aes(x=maritalStatus, fill=Y)) +
  geom_bar(stat="count")+
  theme(axis.text.x = element_text(angle = 15, hjust = 1))
g6 <- ggplot(coupon, aes(x=maritalStatus, fill=Y)) +
  geom_bar(position="fill")+
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

grid.arrange(p1,g1, p2,g2, p3,g3, p6,g6, ncol=2)


p3 = ggplot(coupon, aes(coupon, group = coupon_accepted, fill = coupon_accepted)) +
  geom_bar(color = "black", position = "dodge") +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.3, position = position_dodge(0.9), size = 3) +
  facet_wrap(~expiration) +
  scale_fill_brewer(palette="Paired") +
  theme_classic(base_size = 11,) + labs(title = "Coupon Type vs. Coupon Accepted? by Gender")+ theme(axis.text.x = element_text(angle = 15, hjust = 1))
p3



#Chi square test between response and indicator variables
## For numerical variables
chisq.test(table(coupon[,c("Y","temperature")])) #p-value very small
chisq.test(table(coupon[,c("Y","destination")]))
chisq.test(table(coupon[,c("Y","passanger")]))
chisq.test(table(coupon[,c("Y","weather")]))
chisq.test(table(coupon[,c("Y","time")]))
chisq.test(table(coupon[,c("Y","coupon")]))
chisq.test(table(coupon[,c("Y","expiration")]))
chisq.test(table(coupon[,c("Y","gender")]))
chisq.test(table(coupon[,c("Y","age")]))
chisq.test(table(coupon[,c("Y","maritalStatus")]))
chisq.test(table(coupon[,c("Y","education")]))
chisq.test(table(coupon[,c("Y","occupation_class")]))
chisq.test(table(coupon[,c("Y","income")]))
chisq.test(table(coupon[,c("Y","Bar")]))
chisq.test(table(coupon[,c("Y","CoffeeHouse")]))
chisq.test(table(coupon[,c("Y","CarryAway")]))
chisq.test(table(coupon[,c("Y","RestaurantLessThan20")]))
chisq.test(table(coupon[,c("Y","Restaurant20To50")]))
chisq.test(table(coupon[,c("Y","toCoupon_GEQ15min")]))
chisq.test(table(coupon[,c("Y","toCoupon_GEQ25min")]))
chisq.test(table(coupon[,c("coupon_accepted","direction_opp")]))


####### Model Building#####
str(coupon_sub)

coupon_new<- subset(coupon_sub, select= -c(occupation, coupon_accepted)) #removing redundant columns
str(coupon_new)

coupreg_1 <- glm( Y ~ . ,  data = coupon_new, family = binomial,maxit = 100)
summary(coupreg1)

coupreg_2 <- glm( Y ~ destination + passanger + weather + coupon + expiration + gender + age
 +education + income + Bar + CoffeeHouse + direction_opp + occupation_class, data = coupon_new,
 family = binomial)

n <- nrow(coupon)


Model_stepwise <- step(coupreg_2, scope = formula(coupreg_1),direction="both",trace=0)
Model_stepwise$call

Model_backward <- step(coupreg_2, scope = formula(coupreg_1),direction="backward",trace=0,k = log(n))
Model_backward$call

Model_forward <- step(coupreg_2, scope = formula(coupreg_1),direction="forward",trace=0,k = log(n))
Model_forward$call


#BIC

#Model using backward selection with just main effects
Model_BIC <-glm(formula = Y ~ destination + passanger + weather  + 
                  coupon + expiration + gender + education + CoffeeHouse + 
                  direction_opp, family = binomial, data = coupon_new)
summary(Model_BIC)

#############################
coupreg_1 <- glm( Y ~ . + coupon*expiration + income*occupation_class + destination*passanger ,  data = coupon_new, family = binomial,maxit = 100)

coupreg_2 <- glm( Y ~ destination + passanger + weather + 
                    coupon + expiration + gender + age +
                    education + income + Bar + CoffeeHouse + 
                    direction_opp + occupation_class + coupon*expiration, data = coupon_new,
                  family = binomial)


Model_backward <- step(coupreg_2, scope = formula(coupreg_1),direction="backward",trace=0,k = log(n))
Model_backward$call

##### Including Interaction Effects
##Check using Anova

reg_1 <- glm( Y ~ destination + passanger + weather + 
                    coupon + expiration + gender + age +
                    education + income + Bar + CoffeeHouse + 
                    direction_opp + occupation_class ,  data = coupon_new, family = binomial,maxit = 100)

reg_2 <- glm( Y ~ destination + passanger + weather + 
                    coupon + expiration + gender + age +
                    education + income + Bar + CoffeeHouse + 
                    direction_opp + occupation_class + coupon*expiration, data = coupon_new,
                  family = binomial)

+ income*occupation_class + destination*passanger

#change in deviance tests to see if the full set of interactions are useful.

anova(reg_1, reg_2, test= "Chisq")


###########
reg_1 <- glm( Y ~ destination + passanger + weather + 
                coupon + expiration + gender + age +
                education + income + Bar + CoffeeHouse + 
                direction_opp + occupation_class +coupon *(expiration + gender)  ,  data = coupon_new, family = binomial,maxit = 100)

reg_2 <- glm( Y ~ destination + passanger + weather + 
                coupon + expiration + gender + age +
                education + income + Bar + CoffeeHouse + 
                direction_opp + occupation_class + coupon*(expiration + gender), data = coupon_new,
              family = binomial)
summary(reg_1)



#change in deviance tests to see if the full set of interactions are useful.

anova(reg_2, reg_1, test= "Chisq")


#############################################Binned plots
#Main Effects model
rawresid1 <- residuals(Model_BIC,"resp")
rawresid1

binnedplot(x=fitted(Model_BIC),y=rawresid1,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot_1",col.pts="navy")

######################################Interactions
rawresid2 <- residuals(reg_2,"resp")
rawresid2

binnedplot(x=fitted(Model_BIC),y=rawresid2,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot_1",col.pts="navy")

Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(reg_2) >= 0.542, "1","0")),
                            as.factor(coupon_new$Y),positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"];
Conf_mat$byClass[c("Sensitivity","Specificity")] #True positive rate and True negative rate

#let's repeat with the marginal percentage in the data
Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(reg_2) >= mean(coupon_new$Y), "1","0")),
                            as.factor(coupon_new$Y),positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"];
Conf_mat$byClass[c("Sensitivity","Specificity")]
#still not moving much.... the model can predict only so well


#ROC curve...
roc(coupon_new$Y,fitted(reg_2),plot=T,print.thres="best",legacy.axes=T,
    print.auc =T,col="red3")

#a little better still... but we really aren't gaining a whole lot.  this is about as
#good as we are going to get with only these variables, apparently.

###model interpretations

confint.default(arsreg5)   #on log odds scale
exp(confint.default(arsreg5))   #on odds scale










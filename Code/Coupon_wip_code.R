rm(list = ls())
library(arm)
library(pROC)
library(e1071)
library(caret)
library(ggplot2)
library(dplyr)
#require(gridExtra)


data <- read.csv('/Users/Aarushi/Duke/MIDS - Fall 2021/Fall 2021/702_IDS/Final Projects/in-vehicle-coupon-recommendation.csv')

#Looking at the data
dim(data)
str(data)
# The data has 12684 rows ans 26 columns


#Check is there are missing values
sum(is.na(data))
#There are no null values

#Looking at the unique values in each column

#EDA
unique(data$destination)
unique(data$passanger)
unique(data$weather)
unique(data$temperature)
unique(data$time)
unique(data$coupon)
unique(data$expiration)
unique(data$age)
unique(data$maritalStatus)
unique(data$car) #- #Empty values
unique(data$Bar) # - Null values
unique(data$CoffeeHouse) #Null values
unique(data$CarryAway) #EMpty values
unique(data$RestaurantLessThan20) #EMpty values
unique(data$Restaurant20To50) #EMpty values
unique(data$toCoupon_GEQ5min) #Only one unique value
unique(data$toCoupon_GEQ15min)
unique(data$toCoupon_GEQ25min)
unique(data$direction_same)
unique(data$direction_opp)
unique(data$Y)
unique(data$occupation)

# Need to change factor variables
# Look for any missing values

#Converting blank spaces to NA
data$car[data$car ==""] <- NA
data$CoffeeHouse[data$CoffeeHouse ==""] <- NA
data$CarryAway[data$CarryAway==""] <- NA
data$RestaurantLessThan20[data$RestaurantLessThan20 ==""] <- NA
data$Restaurant20To50[data$Restaurant20To50 ==""] <- NA
data$Bar[data$Bar ==""] <- NA


namedCounts <- sapply(data, function(x) round((sum(is.na(x))/length(x))*100,2))
namedCounts <- namedCounts[namedCounts>0]
cat("Columns with missing value\n")
print(paste0(names(namedCounts)," :",unname(namedCounts),"%"))

#The column car has almost 100% missing values 
#whereas 4 other columns with less than 2% missing values

#Cleaning the data
str(data)
data$coupon_accepted <- as.factor(data$Y)

#Renaming values in passenger column
data$passanger[which(data$passanger=="Friend(s)")] <- "Friends"
data$passanger[which(data$passanger=="Kid(s)")] <- "Kids"


#Created more subsets in the occupation column
unique(data$occupation)

# Unemployed     
# Student
# Retired   
# Healthcare -> "Healthcare Support", Healthcare Practitioners & Technical
# Social ->"Life Physical Social Science", "Community & Social Services",
# Trade Workers -> "Construction & Extraction","Installation Maintenance & Repair", 
                  #Building & Grounds Cleaning & Maintenance, Transportation & Material Moving",""Food Preparation & Serving Related" 
#White Collar -> "Sales & Related","Management","Office & Administrative Support","Business & Financial", "Legal"
                   #"Education&Training&Library","Architecture & Engineering", "Arts Design Entertainment Sports & Media"  "Computer & Mathematical"
     
#Others -> "Production Occupations","Protective Service" ,
             #"Personal Care & Service" , "Farming Fishing & Forestry" 
data$occupation_class <- NA                                 

data$occupation_class[which(data$occupation %in% 
                                     c("Healthcare Support", 
                                       "Healthcare Practitioners & Technical"))] <- "Healthcare"

data$occupation_class[which(data$occupation %in% 
                              c("Life Physical Social Science", 
                              "Community & Social Services"))] <- "Social"

data$occupation_class[which(data$occupation %in% 
                              c("Construction & Extraction","Installation Maintenance & Repair", 
                              "Building & Grounds Cleaning & Maintenance", 
                              "Transportation & Material Moving","Food Preparation & Serving Related"))] <- "Trade Workers"

data$occupation_class[which(data$occupation %in% 
                              c("Sales & Related","Management","Office & Administrative Support",
                                "Business & Financial", "Legal","Education&Training&Library",
                                "Architecture & Engineering", "Arts Design Entertainment Sports & Media",
                                "Computer & Mathematical"))] <- "White Collar"

data$occupation_class[which(data$occupation %in% 
                              c("Retired"))] <- "Retired"

data$occupation_class[which(data$occupation %in% 
                              c("Student"))] <- "Student"

data$occupation_class[which(data$occupation %in% 
                              c("Production Occupations","Protective Service" ,
             "Personal Care & Service" , "Farming Fishing & Forestry"))] <- "Others"

data$occupation_class[which(data$occupation %in% 
                              c("Unemployed"))] <- "Unemployed"


namedCounts <- sapply(data, function(x) round((sum(is.na(x))/length(x))*100,2))
namedCounts <- namedCounts[namedCounts>0]
cat("Columns with missing value\n")
print(paste0(names(namedCounts)," :",unname(namedCounts),"%"))

table(data$occupation)
table(data$occupation_class)


table(data$time)

#################Education#########

unique(data$education)
data$education[data$education == 'Graduate degree (Masters or Doctorate)'] <- 'Graduate degree'
data$education[data$education == 'Some college - no degree'] <- 'College - no degree'
data$education[data$education == 'Some High School'] <- 'High School - no degree'
unique(data$education)

########Age########
unique(data$age)
data$age[data$age == '21'] <- '21-30'
data$age[data$age == '26'] <- '21-30'
data$age[data$age == '31'] <- '31-40'
data$age[data$age == '36'] <- '31-40'
data$age[data$age == '41'] <- '41-50'
data$age[data$age == '46'] <- '41-50'
unique(data$age)

###########Restaurant20To50
unique(data$Restaurant20To50)
data$Restaurant20To50[data$Restaurant20To50 == '1~3'] <- '1-3'
data$Restaurant20To50[data$Restaurant20To50 == 'less1'] <- 'less than 1'
data$Restaurant20To50[data$Restaurant20To50 == 'gt8'] <- 'greater than 8'
data$Restaurant20To50[data$Restaurant20To50 == '4~8'] <- '4-8'
unique(data$Restaurant20To50)

###########RestaurantLessThan20To50
unique(data$RestaurantLessThan20)
data$RestaurantLessThan20[data$RestaurantLessThan20 == '1~3'] <- '1-3'
data$RestaurantLessThan20[data$RestaurantLessThan20 == 'less1'] <- 'less than 1'
data$RestaurantLessThan20[data$RestaurantLessThan20 == 'gt8'] <- 'greater than 8'
data$RestaurantLessThan20[data$RestaurantLessThan20 == '4~8'] <- '4-8'
unique(data$RestaurantLessThan20)

#Create a subset of data to check correlation
data_sub <- data[c(11,21:26)]


#install.packages("corrplot")
library(corrplot)
library(ggplot2)
library(GGally)
ggpairs(data_sub)
 # colorful number

# Correlation between column Opp direction and Same Direction is -1 therefore we can drop one of them
# Dropping toCoupon_GEQ5min because there is only 1 value

coupon<- subset(data, select= -c(car,direction_same, toCoupon_GEQ5min))
str(coupon)

table(data_sub$toCoupon_GEQ5min)

# Income - Creating a new column to give numerical weightage
# table(coupon$income)
# coupon$income_weightage <- NA
# coupon$income_weightage[which(coupon$income=="Less than $12500")] <- 1
# coupon$income_weightage[which(coupon$income=="$12500 - $24999")] <- 2
# coupon$income_weightage[which(coupon$income=="$25000 - $37499")] <- 3
# coupon$income_weightage[which(coupon$income=="$37500 - $49999")] <- 4
# coupon$income_weightage[which(coupon$income=="$50000 - $62499")] <- 5
# coupon$income_weightage[which(coupon$income=="$62500 - $74999")] <- 6
# coupon$income_weightage[which(coupon$income=="$75000 - $87499")] <- 7
# coupon$income_weightage[which(coupon$income=="$87500 - $99999")] <- 8
# coupon$income_weightage[which(coupon$income=="$100000 or More")] <- 9
# table(coupon$income_weightage)

str(coupon)

#Converting character variables to factor variables

coupon$destination <- as.factor(coupon$destination) 
coupon$passanger <- as.factor(coupon$passanger) 
coupon$weather <- as.factor(coupon$weather) 
coupon$gender <- as.factor(coupon$gender)
coupon$gender <- as.factor(coupon$gender)
coupon$coupon <- as.factor(coupon$coupon)
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

# To consider what to do for Expiration and Time

str(coupon)

unique(data$Restaurant20To50)

###########EDA

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

str(coupon)

table(coupon[,c("destination","coupon_accepted")])
table(coupon[,c("destination","Y")])/sum(table(coupon[,c("destination","Y")]))

table(coupon[,c("Y")])/sum(table(coupon[,c("Y")]))

table(coupon[,c("toCoupon_GEQ25min","Y")])
table(coupon[,c("toCoupon_GEQ25min","Y")])/sum(table(coupon[,c("toCoupon_GEQ25min","Y")]))

###########################################################################################
apply(table(coupon[,c("Y","destination")])/sum(table(coupon[,c("Y","destination")])),
      2,function(x) x/sum(x))
## Big change in Prob for No Urgent Place

apply(table(coupon[,c("Y","passanger")])/sum(table(coupon[,c("Y","passanger")])),
      2,function(x) x/sum(x))
# Higher probability fir Friends, lease for KIDS

apply(table(coupon[,c("Y","weather")])/sum(table(coupon[,c("Y","weather")])),
      2,function(x) x/sum(x))
# Highest in Sunny

apply(table(coupon[,c("Y","temperature")])/sum(table(coupon[,c("Y","temperature")])),
      2,function(x) x/sum(x))


apply(table(coupon[,c("Y","time")])/sum(table(coupon[,c("Y","time")])),
      2,function(x) x/sum(x))
#Highest around 10 AM and 2PM (Meal Times)

apply(table(coupon[,c("Y","coupon")])/sum(table(coupon[,c("Y","coupon")])),
      2,function(x) x/sum(x))
#Highest for Carryout and TakeAway and Restaurant <20

apply(table(coupon[,c("Y","expiration")])/sum(table(coupon[,c("Y","expiration")])),
      2,function(x) x/sum(x))
#Highest for 24 hour validity

apply(table(coupon[,c("Y","gender")])/sum(table(coupon[,c("Y","gender")])),
      2,function(x) x/sum(x))
#Male driver higher prob

apply(table(coupon[,c("Y","age")])/sum(table(coupon[,c("Y","age")])),
      2,function(x) x/sum(x))
#Highest for below 21

apply(table(coupon[,c("Y","maritalStatus")])/sum(table(coupon[,c("Y","maritalStatus")])),
      2,function(x) x/sum(x))


apply(table(coupon[,c("Y","has_children")])/sum(table(coupon[,c("Y","has_children")])),
      2,function(x) x/sum(x))
#Highest for Single

apply(table(coupon[,c("Y","education")])/sum(table(coupon[,c("Y","education")])),
      2,function(x) x/sum(x))
#High School No degree highest

apply(table(coupon[,c("Y","occupation_class")])/sum(table(coupon[,c("Y","occupation_class")])),
      2,function(x) x/sum(x))
#Highest for Healthcare

apply(table(coupon[,c("Y","income")])/sum(table(coupon[,c("Y","income")])),
      2,function(x) x/sum(x))
#More or less than the same

apply(table(coupon[,c("Y","toCoupon_GEQ15min")])/sum(table(coupon[,c("Y","toCoupon_GEQ15min")])),
      2,function(x) x/sum(x))

apply(table(coupon[,c("Y","direction_opp")])/sum(table(coupon[,c("Y","direction_opp")])),
      2,function(x) x/sum(x))


table(coupon[,c("education","Y")])
table(coupon[,c("education","Y")])/sum(table(coupon[,c("education","Y")]))

table(coupon[,c("income","Y")])
table(coupon[,c("income","Y")])/sum(table(coupon[,c("income","Y")]))


table(coupon[,c("destination","Y")])
table(coupon[,c("destination","Y")])/sum(table(coupon[,c("destination","Y")]))
chisq.test(table(coupon[,c("Y","passanger")]))



coupon$expiration[which(coupon$expiration =="1d")] <- "24"
coupon$expiration[which(coupon$expiration =="2h")] <- "2"

coupon$expiration <- as.numeric(coupon$expiration)

str(coupon)

table(coupon$has_children)



p1 <- ggplot(coupon, aes(x=destination, fill=Y)) +
  geom_bar(stat="count")
p1
g1 <- ggplot(coupon, aes(x=destination, fill=Y)) +
  geom_bar(position="fill")
g1

ggplot(coupon, aes(x=temperature, fill=Y)) +
  geom_bar(stat ="count")


## Plotting data columns

library(gridExtra)


#Destination
p1 <- ggplot(coupon, aes(x=destination, fill=coupon_accepted)) +
  geom_bar()
g1 <- ggplot(coupon, aes(x=destination, fill=coupon_accepted)) +
  geom_bar(position="fill")



ggplot(coupon, aes(x=destination, y=Y, fill=gender)) +
  geom_boxplot()


ggplot(coupon) +
  geom_bar(aes(x = education, fill = coupon_accepted), color = "black") +
  scale_fill_brewer(palette="Greens") +
  theme_classic() +
  labs(title = "Educ v Coupon by Gender") +
  facet_wrap("~gender")
table(coupon$education, coupon$gender, coupon$coupon_accepted)


p2 = ggplot(coupon, aes(education, group = coupon_accepted, fill = coupon_accepted)) +
  geom_bar(color = "black", position = "dodge") +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.3, position = position_dodge(0.9), size = 3) +
  facet_wrap(~gender) +
  scale_fill_brewer(palette="Blues") +
  theme_classic(base_size = 11) + labs(title = "Cap Surface v. Class by Gill Size")

p2


#passenger 
p2 <- ggplot(coupon, aes(x=passanger, fill=coupon_accepted)) +
  geom_bar(stat="count")
g2 <- ggplot(coupon, aes(x=passanger, fill=coupon_accepted)) +
  geom_bar(position="fill")

#weather
p3 <- ggplot(coupon, aes(x=weather, fill=coupon_accepted)) +
  geom_bar(stat="count")
g3 <- ggplot(coupon, aes(x=weather, fill=coupon_accepted)) +
  geom_bar(position="fill")

#time
#p4 <- ggplot(cleaned_data, aes(x=time, fill=Y)) +
#geom_bar(stat="count")

#gender
p5 <- ggplot(coupon, aes(x=gender, fill=coupon_accepted)) +
   geom_bar(stat="count")
g5 <- ggplot(coupon, aes(x=gender, fill=coupon_accepted)) +
  geom_bar(position="fill")


#maritalStatus   
p6 <- ggplot(coupon, aes(x=maritalStatus, fill=coupon_accepted)) +
  geom_bar(stat="count")+
  theme(axis.text.x = element_text(angle = 15, hjust = 1))
g6 <- ggplot(coupon, aes(x=maritalStatus, fill=coupon_accepted)) +
  geom_bar(position="fill")+
  theme(axis.text.x = element_text(angle = 15, hjust = 1))
p6
g6

grid.arrange(p1,g1, p2,g2, p3,g3, p5,g5,p6,g6, ncol=2)


####### Model Building#####


str(coupon)

coupon_new<- subset(coupon, select= -c(occupation,coupon_accepted))
str(coupon_new)
coupon_new$time <- as.factor(coupon_new$time) 

namedCounts <- sapply(coupon_new, function(x) round((sum(is.na(x))/length(x))*100,2))
namedCounts <- namedCounts[namedCounts>0]
cat("Columns with missing value\n")
print(paste0(names(namedCounts)," :",unname(namedCounts),"%"))

coupon_new <- na.omit(coupon_new)

namedCounts <- sapply(coupon_new, function(x) round((sum(is.na(x))/length(x))*100,2))
namedCounts <- namedCounts[namedCounts>0]
cat("Columns with missing value\n")
print(paste0(names(namedCounts)," :",unname(namedCounts),"%"))

coupreg2 <- glm( Y ~ . ,  data = coupon_new, family = binomial,maxit = 100)
summary(coupreg2)
# Factors significant - destination, passanger, weather, coupon, expiration, gender, age
#education, income, Bar, CH,Rest 20tto50, rest less than, direction opp, occupation,

str(coupon)

coupreg1 <- glm( Y ~ gender + age + education + maritalStatus + income + CarryAway ,  data = coupon_new, family = binomial,maxit = 100)

coupreg1 <- glm( Y ~ gender + age + education + maritalStatus ,  data = coupon_new, family = binomial)

n <- nrow(coupon)


Model_stepwise <- step(coupreg1, scope = formula(coupreg2),direction="both",trace=0)

Model_stepwise$call

Model_backward <- step(coupreg2,direction="backward",trace=0,k = log(n))
Model_backward$call

#AIC
Model_AIC <- glm(formula = Y ~ gender + education + maritalStatus + coupon + 
      expiration + CoffeeHouse + destination + weather + direction_opp + 
      passanger + occupation_class + income + time + Restaurant20To50 + 
      Bar + RestaurantLessThan20 + toCoupon_GEQ25min + toCoupon_GEQ15min, 
    family = binomial, data = coupon_new)

summary(Model_AIC)

#BIC
coupreg2 <- glm( Y ~ . + direction_opp*coupon +coupon*expiration,  data = coupon_new, family = binomial,maxit = 100)
Model_backward <- step(coupreg2,direction="both",trace=0,k = log(n))
Model_backward$call

Model_BIC <-glm(formula = Y ~ destination + passanger + weather + time + 
      coupon + expiration + gender + education + CoffeeHouse + 
      direction_opp, family = binomial, data = coupon_new)
summary(Model_BIC)


##### Including Interaction Effects




reg_int1 <-glm(Y ~ weather*time + gender*(destination + passanger) , family = binomial, data = coupon_new)
summary(reg_int1)






rawresid1 <- residuals(Model_BIC,"resp")
rawresid1

binnedplot(x=fitted(Model_BIC),y=rawresid1,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot_1",col.pts="navy")

Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(Model_BIC) >= 0.553, "1","0")),
                            as.factor(coupon_new$Y),positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"];
Conf_mat$byClass[c("Sensitivity","Specificity")] #True positive rate and True negative rate

#let's repeat with the marginal percentage in the data
Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(Model_BIC) >= mean(coupon_new$Y), "1","0")),
                            as.factor(coupon_new$Y),positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"];
Conf_mat$byClass[c("Sensitivity","Specificity")]
#still not moving much.... the model can predict only so well


#ROC curve...
roc(coupon_new$Y,fitted(Model_BIC),plot=T,print.thres="best",legacy.axes=T,
    print.auc =T,col="red3")

#a little better still... but we really aren't gaining a whole lot.  this is about as
#good as we are going to get with only these variables, apparently.

###model interpretations

confint.default(arsreg5)   #on log odds scale
exp(confint.default(arsreg5))   #on odds scale


#############################################NO#######
#Interpreting arsenic is a bit complicated
#let's make plots to display relationships.

#plot of predicted probabilities as arsenic increases for different groups.
#set distance = to average distance (centering means we don't need to worry about it when making predictions at the average distance)
#we can just set it to zero

#First set arsenic and dist values
new_arsenic <- seq(from = min(arsenic$arsenic), to = max(arsenic$arsenic), by = .1)
newdata <- data.frame(dist_c=rep(0,length(new_arsenic)))
newdata$logarsenic_c <- log(new_arsenic) - mean(arsenic$logarsenic)

#now for association = educnew = 0
newdata$assoc <- factor("Not active in community",levels=levels(arsenic$assoc))
newdata$educnew <- 0
predprobbaseline <- predict(arsreg5,newdata,type='response')

a <- exp(0.26727)
a


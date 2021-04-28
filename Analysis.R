# Project: ANES Analysis and Sexuality?
# Programmer: C. Deal
# Date Updated: 04/26/2021

#Read in Data
library(readr)
library(dplyr)
anes_data <- read.csv("C:\\Cameron Files\\Vanderbilt\\Research\\Independent Projects\\ANES Summer 2021\\anes_data.csv", na?strings = c("-9. Refused","-5. Interview breakoff (sufficient partial IW)","-8. Don't know","-2. Missing, other specify not coded for preliminary release", "-1. Inapplicable", "-6. No post-election interview", "-7. No post-election data, deleted due to inc?mplete interview"))
anes_data <- as_tibble(anes_data) 
anes_data <- select(anes_data, V201601, V201600, V201549x, V201617x, V201507x, V201508, V201511x, V201018, V202008, V202056, V201008, V202066, V202009, V202014, V202015, V202016, V202017, V202018, V202?19, V202021, V202022, V202023, V202024, V202025, V202026, V202029)

######Clean up Variables


#Sexuality
names(anes_data)[names(anes_data) == "V201601"] <- "orientation"
anes_data$orientation <- as.factor(anes_data$orientation)
#Guide: 1. Straight 2. Gay ?r Lesbian 3. Bisexual

#Sexuality Minority
anes_data$orientation1 <- anes_data$orientation
levels(anes_data$orientation1)[1] <- 0
levels(anes_data$orientation1)[c(2,3,4)] <- 1

#Sex
names(anes_data)[names(anes_data) == "V201600"] <- "sex"
anes_data$sex <- ?s.factor(anes_data$sex)
#Guide: 1. Male 2. Female

#Race
names(anes_data)[names(anes_data) == "V201549x"] <- "race"
anes_data$race <- as.factor(anes_data$race)
#Guide: 1. White, non-Hispanic 2. Black, non-Hispanic 3. Hispanic 4. Asian or Native Hawaiian/ot?er Pacific Islander, non-Hispanic alone 5. Native American/Alaska Native or other race, non-Hispanic alone 6. Multiple races, non-Hispanic

#Income
names(anes_data)[names(anes_data) == "V201617x"] <- "income"
anes_data$income <- as.factor(anes_data$income)?#Guide:
#1. Under $9,999 2. $10,000-14,999 #3. $15,000-19,999 4. $20,000-24,999 #5. $25,000-29,999 6. $30,000-34,999
#7. $35,000-39,999 8. $40,000-44,999 #9. $45,000-49,999 10. $50,000-59,999 #11. $60,000-64,999 12. $65,000-69,999
#13. $70,000-74,999 14. $?5,000-79,999 #15. $80,000-89,999 16. $90,000-99,999 #17. $100,000-109,999 18. $110,000-124,999
#19. $125,000-149,999 20. $150,000-174,999 #21. $175,000-249,999 22. $250,000 or more

#Age Continuous
names(anes_data)[names(anes_data) == "V201507x"] <- "age"
?#Age Categorical
anes_data$age[anes_data$age == "80. Age 80 or older"] <- 80
anes_data$age <- as.numeric(anes_data$age)
anes_data$agecat1<-cut(anes_data$age, c(18,29,39,49,59,69,79,109))

#Marriage Status
names(anes_data)[names(anes_data) == "V201508"] <- ?marriage"
anes_data$marriage <- as.factor(anes_data$marriage)
#Guide: 1. Married: spouse present 2. Married: spouse absent 3. Widowed 4. Divorced 5. Separated 6. Never married

#Education Level
names(anes_data)[names(anes_data) == "V201511x"] <- "education?
anes_data$education <- as.factor(anes_data$education)
#Guide:1. Less than high school credential 2. High school credential 3. Some post-high school, no bachelor's degree
#4. Bachelor's degree 5. Graduate degree

#Party of Registration
names(anes_data)[nam?s(anes_data) == "V201018"] <- "party_reg"


###Target of Political Participation
#Did someone talk to Respondent about Voting
names(anes_data)[names(anes_data) == "V202008"] <- "approached"
anes_data$approached <- as.factor(anes_data$approached)


###Agent?of Political Participation

##Electoral Participation- examine using logistic regression for binary

#When Respondent Registered To Vote
names(anes_data)[names(anes_data) == "V202056"] <- "time_reg"
anes_data$time_reg <- as.factor(anes_data$time_reg)

#Reg?stered to Vote
names(anes_data)[names(anes_data) == "V201008"] <- "registered"
anes_data$registered[anes_data$registered == "3. Not currently registered"] <- 0
anes_data$registered[anes_data$registered == "2. Registered at a different address"] <- 1
anes_d?ta$registered[anes_data$registered == "1. Registered at this address"] <- 1

#Did they vote?
names(anes_data)[names(anes_data) == "V202066"] <- "voted"
anes_data$voted[anes_data$voted == "4. I am sure I voted"] <- 1
anes_data$voted[anes_data$voted == "1. I?did not vote (in the election this November)"] <- 0
anes_data$voted[anes_data$voted == "3. I usually vote, but didn't this time"] <- 0
anes_data$voted[anes_data$voted == "2. I thought about voting this time, but didn't"] <- 0

##Non-Electoral Participation?
#Did Respondent talk to someone about Voting
anes_data[anes_data == "2. No"] <- "0"
anes_data[anes_data == "1. Yes"] <- "1"
anes_data[anes_data == "1. Have done this in past 12 months"] <- "1"
anes_data[anes_data == "2. Have not done this in the past 12 m?nths"] <- "0"
names(anes_data)[names(anes_data) == "V202009"] <- "advocate"

#Go to political meetings, rallies and speeches
names(anes_data)[names(anes_data) == "V202014"] <- "attendee"

#Wear Campaign sticker/button
names(anes_data)[names(anes_data) == "?202015"] <- "button"

#Work for Candidate
names(anes_data)[names(anes_data) == "V202016"] <- "campaign_worker"

#Contribute money to Candidate
names(anes_data)[names(anes_data) == "V202017"] <- "candidate_donation"

#Contribute money to Party
names(anes_da?a)[names(anes_data) == "V202019"] <- "party_donation"
View(anes_data$party_donation) 

#Contribute money to Political Group
names(anes_data)[names(anes_data) == "V202021"] <- "group_donation"

#Discuss politics with family or friends
names(anes_data)[names?anes_data) == "V202022"] <- "discuss_politics"

#How Many Days in Past Week Discussed Politics with Family or friends
anes_data$V202023[anes_data$V202023 == "0. Zero days"] <- 0
anes_data$V202023[anes_data$V202023 == "1. One day"] <- 1
anes_data$V202023[an?s_data$V202023 == "2. Two days"] <- 2
anes_data$V202023[anes_data$V202023 == "3. Three days"] <- 3
anes_data$V202023[anes_data$V202023 == "4. Four days"] <- 4
anes_data$V202023[anes_data$V202023 == "5. Five days"] <- 5
anes_data$V202023[anes_data$V202023 =? "6. Six days"] <- 6
anes_data$V202023[anes_data$V202023 == "7. Seven days"] <- 7
names(anes_data)[names(anes_data) == "V202023"] <- "frequency_discuss"

#Gotten in a Political Argument
names(anes_data)[names(anes_data) == "V202024"] <- "political_argument?

#Protest or Demonstration
names(anes_data)[names(anes_data) == "V202025"] <- "protest"

#Petition
names(anes_data)[names(anes_data) == "V202026"] <- "petition"

#Comment Posted Online
names(anes_data)[names(anes_data) == "V202029"] <- "comment_online"

#?dditive index
anes_data <- mutate(anes_data, additive_index = as.numeric(advocate) + as.numeric(attendee) + as.numeric(button) + as.numeric(campaign_worker) + as.numeric(candidate_donation) + as.numeric(party_donation) + as.numeric(group_donation) + as.num?ric(discuss_politics) + as.numeric(political_argument) + as.numeric(protest) + as.numeric(petition) + as.numeric(comment_online))

###Analysis

##Visualizations
#Box and Whisker Plot
plot(anes_data$additive_index ~ anes_data$orientation)

##Tests

#T test ?f Means
t.test(additive_index ~ orientation1, data=anes_data, var.equal=TRUE)

#Regression
fit <- lm(additive_index ~ orientation1 + sex + race + income + agecat1 + marriage + education, data=anes_data)
summary(fit)



#Difficulty to vote: V202118, V202119? v202120(a-k)

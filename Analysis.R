# Project: ANES Analysis and Sexuality
# Programmer: C. Deal
# Date Updated: 04/26/2021

#Read in Data
library(readr)
library(dplyr)
library(tidyr)
library(gmodels)
library(openxlsx)
library(catspec)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
libr?ry(survey)
anes_data <- read.csv("C:\\Cameron Files\\Vanderbilt\\Research\\Independent Projects\\ANES Summer 2021\\anes_data.csv", na.strings = c("-9. Refused","-5. Interview breakoff (sufficient partial IW)","-4. Technical error","-8. Don't know","-2. Mis?ing, other specify not coded for preliminary release", "-1. Inapplicable", "-6. No post-election interview", "-7. No post-election data, deleted due to incomplete interview"))
anes_data <- as_tibble(anes_data) 
anes_data <- select(anes_data, V200010b, V201?05, V200010c, V200010d, V201453, V202212, V201601, V201600, V201549x, V201617x, V201507x, V201508, V201511x, V201018, V202008, V202056, V201008, V202066, V202009, V202014, V202015, V202016, V202017, V202018, V202019, V202021, V202022, V202023, V202024, V20?025, V202026, V202029)




######Clean up Variables
#Sampling Weights
names(anes_data)[names(anes_data) == "V200010b"] <- "weights"

#PSU
names(anes_data)[names(anes_data) == "V200010c"] <- "psu"

#Strata
names(anes_data)[names(anes_data) == "V200010d"] <-?"strata"

#Sexuality
names(anes_data)[names(anes_data) == "V201601"] <- "orientation"
anes_data$orientation <- as.factor(anes_data$orientation)
#Guide: 1. Straight 2. Gay or Lesbian 3. Bisexual

#Sexuality Minority
anes_data$orientation1 <- anes_data$orien?ation
levels(anes_data$orientation1)[1] <- 0
levels(anes_data$orientation1)[c(2,3,4)] <- 1

#Sex
names(anes_data)[names(anes_data) == "V201600"] <- "sex"
anes_data$sex <- as.factor(anes_data$sex)
#Guide: 1. Male 2. Female

#Race
names(anes_data)[names(anes?data) == "V201549x"] <- "race"
anes_data$race <- as.factor(anes_data$race)
#Guide: 1. White, non-Hispanic 2. Black, non-Hispanic 3. Hispanic 4. Asian or Native Hawaiian/other Pacific Islander, non-Hispanic alone 5. Native American/Alaska Native or other ra?e, non-Hispanic alone 6. Multiple races, non-Hispanic

#Income
names(anes_data)[names(anes_data) == "V201617x"] <- "income"
anes_data$income <- as.factor(anes_data$income)
#Guide:
#1. Under $9,999 2. $10,000-14,999 #3. $15,000-19,999 4. $20,000-24,999 #5. ?25,000-29,999 6. $30,000-34,999
#7. $35,000-39,999 8. $40,000-44,999 #9. $45,000-49,999 10. $50,000-59,999 #11. $60,000-64,999 12. $65,000-69,999
#13. $70,000-74,999 14. $75,000-79,999 #15. $80,000-89,999 16. $90,000-99,999 #17. $100,000-109,999 18. $110,0?0-124,999
#19. $125,000-149,999 20. $150,000-174,999 #21. $175,000-249,999 22. $250,000 or more

#Age Continuous
names(anes_data)[names(anes_data) == "V201507x"] <- "age"

#Age Categorical
anes_data$age[anes_data$age == "80. Age 80 or older"] <- 80
anes_da?a$age <- as.numeric(anes_data$age)
anes_data$agecat1<-cut(anes_data$age, c(18,29,39,49,59,69,79,109))

#Marriage Status
names(anes_data)[names(anes_data) == "V201508"] <- "marriage"
anes_data$marriage <- as.factor(anes_data$marriage)
#Guide: 1. Married: sp?use present 2. Married: spouse absent 3. Widowed 4. Divorced 5. Separated 6. Never married

#Education Level
names(anes_data)[names(anes_data) == "V201511x"] <- "education"
anes_data$education <- as.factor(anes_data$education)
#Guide:1. Less than high scho?l credential 2. High school credential 3. Some post-high school, no bachelor's degree
#4. Bachelor's degree 5. Graduate degree

#Party of Registration
names(anes_data)[names(anes_data) == "V201018"] <- "party_reg"
anes_data$party_reg <- as.factor(anes_data?party_reg)

#Political Interest
names(anes_data)[names(anes_data) == "V201005"] <- "political_interest"
anes_data$political_interest <- as.factor(anes_data$political_interest)

#Religiosity
names(anes_data)[names(anes_data) == "V201453"] <- "religiosity"
a?es_data$religiosity <- as.factor(anes_data$religiosity)

#Sense of Efficacy
names(anes_data)[names(anes_data) == "V202212"] <- "efficacy"
anes_data$efficacy <- as.factor(anes_data$efficacy)


#Reexamine new additive index

#Mobilization
#Did someone talk t? Respondent about Voting
names(anes_data)[names(anes_data) == "V202008"] <- "approached"
anes_data$approached <- as.factor(anes_data$approached)


###Agent of Political Participation

##Electoral Participation- examine using logistic regression for binary
?#When Respondent Registered To Vote
names(anes_data)[names(anes_data) == "V202056"] <- "time_reg"
anes_data$time_reg <- as.factor(anes_data$time_reg)

#Registered to Vote
names(anes_data)[names(anes_data) == "V201008"] <- "registered"
anes_data$registered[?nes_data$registered == "3. Not currently registered"] <- 0
anes_data$registered[anes_data$registered == "2. Registered at a different address"] <- 1
anes_data$registered[anes_data$registered == "1. Registered at this address"] <- 1
anes_data$registered <- ?s.numeric(anes_data$registered)

#Did they vote?
names(anes_data)[names(anes_data) == "V202066"] <- "voted"
anes_data$voted[anes_data$voted == "4. I am sure I voted"] <- 1
anes_data$voted[anes_data$voted == "1. I did not vote (in the election this November?"] <- 0
anes_data$voted[anes_data$voted == "3. I usually vote, but didn't this time"] <- 0
anes_data$voted[anes_data$voted == "2. I thought about voting this time, but didn't"] <- 0
anes_data$voted <- as.numeric(anes_data$voted)

##Non-Electoral Participat?on

#Did Respondent talk to someone about Voting
anes_data[anes_data == "2. No"] <- "0"
anes_data[anes_data == "1. Yes"] <- "1"
anes_data[anes_data == "1. Have done this in past 12 months"] <- "1"
anes_data[anes_data == "2. Have not done this in the past 1? months"] <- "0"
names(anes_data)[names(anes_data) == "V202009"] <- "advocate"

#Go to political meetings, rallies and speeches
names(anes_data)[names(anes_data) == "V202014"] <- "attendee"

#Wear Campaign sticker/button
names(anes_data)[names(anes_data) =? "V202015"] <- "button"

#Work for Candidate
names(anes_data)[names(anes_data) == "V202016"] <- "campaign_worker"

#Contribute money to Candidate
names(anes_data)[names(anes_data) == "V202017"] <- "candidate_donation"

#Contribute money to Party
names(anes?data)[names(anes_data) == "V202019"] <- "party_donation"

#Contribute money to Political Group
names(anes_data)[names(anes_data) == "V202021"] <- "group_donation"

#Discuss politics with family or friends
names(anes_data)[names(anes_data) == "V202022"] <- ?discuss_politics"

#How Many Days in Past Week Discussed Politics with Family or friends
anes_data$V202023[anes_data$V202023 == "0. Zero days"] <- 0
anes_data$V202023[anes_data$V202023 == "1. One day"] <- 1
anes_data$V202023[anes_data$V202023 == "2. Two da?s"] <- 2
anes_data$V202023[anes_data$V202023 == "3. Three days"] <- 3
anes_data$V202023[anes_data$V202023 == "4. Four days"] <- 4
anes_data$V202023[anes_data$V202023 == "5. Five days"] <- 5
anes_data$V202023[anes_data$V202023 == "6. Six days"] <- 6
anes_da?a$V202023[anes_data$V202023 == "7. Seven days"] <- 7
names(anes_data)[names(anes_data) == "V202023"] <- "frequency_discuss"
anes_data$frequency_discuss <- as.numeric(anes_data$frequency_discuss)

#Binary Talking Politics
anes_data$frequency_discuss1 <- ane?_data$frequency_discuss
levels(anes_data$frequency_discuss1)[c(1,2,3)] <- 0
levels(anes_data$frequency_discuss1)[c(4,5,6,7)] <- 1

#Gotten in a Political Argument
names(anes_data)[names(anes_data) == "V202024"] <- "political_argument"

#Protest or Demonstr?tion
names(anes_data)[names(anes_data) == "V202025"] <- "protest"

#Petition
names(anes_data)[names(anes_data) == "V202026"] <- "petition"

#Comment Posted Online
names(anes_data)[names(anes_data) == "V202029"] <- "comment_online"

#Additive index
anes_dat? <- mutate(anes_data, additive_index = as.numeric(advocate) + as.numeric(attendee) + as.numeric(button) + as.numeric(campaign_worker) + as.numeric(candidate_donation) + as.numeric(party_donation) + as.numeric(group_donation) + as.numeric(discuss_politics) ? as.numeric(frequency_discuss1) + as.numeric(political_argument) + as.numeric(protest) + as.numeric(petition) + as.numeric(comment_online))


###Analysis############################################################################

##Visualizations
#Box and?Whisker Plot
plot(anes_data$additive_index ~ anes_data$orientation)

##Tests

#Means
svyby(~additive_index, ~orientation, design=anes_data, na.rm=TRUE, svymean)

#T test of Means
t.test(additive_index ~ orientation1, data=anes_data, var.equal=TRUE)

#Set S?rvey
anes_data <- subset(anes_data, !is.na(weights))
anes_data <- svydesign(id=~psu, weights=~weights, strata=~strata, nest=TRUE, survey.lonely.psu = "adjust", data=anes_data)

#Linear Regression with all controls
fit <- svyglm(additive_index ~ orientation? + sex + race + income + agecat1 + marriage + education + political_interest + religiosity + efficacy + party_reg + approached, design=anes_data)
summary(fit)
anova(fit) # anova table
tab_model(fit)

#Linear Regression with all controls- dropped political ?nterest (lowered p-value from .87 to .60), mobilization (lowered p-value from .60 to .54), party registration (dropped from .54 to .15), Religiosity (EXTREME effects-- dropped from .15 to .00001)
fit1 <- svyglm(additive_index ~ orientation1 + sex + race + ?ncome + agecat1 + marriage + education + political_interest + efficacy + party_reg + approached, design=anes_data)
summary(fit1)
anova(fit1) # anova table
tab_model(fit1)

#Logistic Regression
register_logit <- svyglm(registered ~ orientation1 + sex + race?+ income + agecat1 + marriage + education, design = anes_data, family = "binomial")
summary(register_logit)
tab_model(register_logit)

#Logistic Regression
voting_logit <- svyglm(voted ~ orientation1 + sex + race + income + agecat1 + marriage + education, ?esign = anes_data, family = "binomial")
summary(voting_logit)
tab_model(voting_logit)

##Cross Tabs
#Registering
register_orientation <- svytable(~registered + orientation1, design = anes_data)
register_orientation <- tibble(as.data.frame(register_orientat?on))
View(register_orientation)
register_orientation <- pivot_wider(register_orientation, 
                                    names_from = "orientation1", 
                                    values_from = "Freq")
#Voting
vote_orientation <- svytable(~vot?d + orientation1, design = anes_data)
vote_orientation <- tibble(as.data.frame(vote_orientation))
vote_orientation <- pivot_wider(vote_orientation, 
                                    names_from = "orientation1", 
                                    value?_from = "Freq")
View(vote_orientation)


#Trash Below
anes_data$additive_index1 <- factor(anes_data$additive_index)
index_orientation_table <- ctab(anes_data$orientation, anes_data$additive_index1, type="r")
write.csv(index_orientation_table, "C:\\Cameron ?iles\\Vanderbilt\\Research\\Independent Projects\\ANES Summer 2021\\anes_table.csv") 
print(index_orientation_table)
write.table(index_orientation_table, "clipboard", sep="\t", row.names=FALSE)
typeof(index_orientation_table)

write.xlsx(index_orientation_?able, "C:\\Cameron Files\\Vanderbilt\\Research\\Independent Projects\\ANES Summer 2021\\anes_table.xlsx")
#Difficulty to vote: V202118, V202119, v202120(a-k)

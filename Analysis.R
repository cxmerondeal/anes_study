# Project: ANES Analysis and Sexuality
# Programmer: C. Deal
# Date Updated: 07/11/2021

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
library(survey)
library(descr)
library(ltm)
anes_data <- read.csv("C:\\Cameron Files\\Vanderbilt\\Research\\Independent Projects\\ANES Summer 2021\\anes_data.csv", na.strings = c("-9. Refused", "998. Don't know", "-5. Interview breakoff (sufficient partial IW)","-4. Technical error","-8. Don't know","-2. Missing, other specify not coded for preliminary release", "-1. Inapplicable", "-6. No post-election interview", "-7. No post-election data, deleted due to incomplete interview"))
anes_data <- as_tibble(anes_data) 
anes_data <- dplyr::select(anes_data, V200010b, V201005, V201006, V200010c, V200010d, V201453, V202212, V202213, V202215, V201601, V201600, V201549x, V201617x, V201507x, V201508, V201511x, V201018, V202008, V202056, V201008, V202066, V202005, V202007, V202009, V202014, V202015, V202016, V202017, V202019, V202021, V202022, V202023, V202024, V202025, V202026, V202028, V202029, V202031, V202032, V202034, V202036, V202038, V202040, V202166)


######Clean up Variables

###Survey Design
#Sampling Weights
names(anes_data)[names(anes_data) == "V200010b"] <- "weights"

#PSU
names(anes_data)[names(anes_data) == "V200010c"] <- "psu"

#Strata
names(anes_data)[names(anes_data) == "V200010d"] <- "strata"

###Independent Variables:

##Demographics
#Sexuality
names(anes_data)[names(anes_data) == "V201601"] <- "orientation"
anes_data$orientation <- as.factor(anes_data$orientation)
#Guide: 1. Straight 2. Gay or Lesbian 3. Bisexual

#Sexuality Minority
anes_data$orientation1 <- anes_data$orientation
levels(anes_data$orientation1)[1] <- 0
levels(anes_data$orientation1)[c(2,3,4)] <- 1
anes_data$orientation1 <- as.factor(anes_data$orientation1)

#Sex
anes_data$V201600[anes_data$V201600 == "2. Female"] <- 0
anes_data$V201600[anes_data$V201600 == "1. Male"] <- 1
names(anes_data)[names(anes_data) == "V201600"] <- "male"
anes_data$male <- as.factor(anes_data$male)

#Race
names(anes_data)[names(anes_data) == "V201549x"] <- "race"
anes_data$race[anes_data$race == "4. Asian or Native Hawaiian/other Pacific Islander, non-Hispanic alone"] <- "All other races"
anes_data$race[anes_data$race == "5. Native American/Alaska Native or other race, non-Hispanic alone"] <- "All other races"
anes_data$race[anes_data$race == "6. Multiple races, non-Hispanic"] <- "All other races"
anes_data$race <- as.factor(anes_data$race)

#Age Continuous
names(anes_data)[names(anes_data) == "V201507x"] <- "age"
anes_data$age[anes_data$age == "80. Age 80 or older"] <- 80
anes_data$age <- as.numeric(anes_data$age)

#Age Square
anes_data <- mutate(anes_data, age_square = (anes_data$age)^2)

#Marriage Status
names(anes_data)[names(anes_data) == "V201508"] <- "marriage"
anes_data$marriage[anes_data$marriage == "2. Married: spouse absent {VOL}"] <- "1. Married: spouse present"
anes_data$marriage[anes_data$marriage == "3. Widowed"] <- "2. Previously married"
anes_data$marriage[anes_data$marriage == "4. Divorced"] <- "2. Previously married"
anes_data$marriage[anes_data$marriage == "5. Separated"] <- "2. Previously married"
anes_data$marriage <- as.factor(anes_data$marriage)
#Guide: 1. Married: spouse present 2. Married: spouse absent 3. Widowed 4. Divorced 5. Separated 6. Never married

##Resources
#Education Level
names(anes_data)[names(anes_data) == "V201511x"] <- "education"
anes_data$education <- as.factor(anes_data$education)
#Guide:1. Less than high school credential 2. High school credential 3. Some post-high school, no bachelor’s degree
#4. Bachelor’s degree 5. Graduate degree


#Income
names(anes_data)[names(anes_data) == "V201617x"] <- "income"
anes_data$income[anes_data$income == "1. Under $9,999"] <- "1. Less than $25,000"
anes_data$income[anes_data$income == "2. $10,000-14,999"] <- "1. Less than $25,000"
anes_data$income[anes_data$income == "3. $15,000-19,999"] <- "1. Less than $25,000"
anes_data$income[anes_data$income == "4. $20,000-24,999"] <- "1. Less than $25,000"
anes_data$income[anes_data$income == "5. $25,000-29,999"] <- "2. $25,000-50,000"
anes_data$income[anes_data$income == "6. $30,000-34,999"] <- "2. $25,000-50,000"
anes_data$income[anes_data$income == "7. $35,000-39,999"] <- "2. $25,000-50,000"
anes_data$income[anes_data$income == "8. $40,000-44,999"] <- "2. $25,000-50,000"
anes_data$income[anes_data$income == "9. $45,000-49,999"] <- "2. $25,000-50,000"
anes_data$income[anes_data$income == "10. $50,000-59,999"] <- "3. $50,000-80,000"
anes_data$income[anes_data$income == "11. $60,000-64,999"] <- "3. $50,000-80,000"
anes_data$income[anes_data$income == "12. $65,000-69,999"] <- "3. $50,000-80,000"
anes_data$income[anes_data$income == "13. $70,000-74,999"] <- "3. $50,000-80,000"
anes_data$income[anes_data$income == "14. $75,000-79,999"] <- "3. $50,000-80,000"
anes_data$income[anes_data$income == "15. $80,000-89,999"] <- "4. $80,000-125,000"
anes_data$income[anes_data$income == "16. $90,000-99,999"] <- "4. $80,000-125,000"
anes_data$income[anes_data$income == "17. $100,000-109,999"] <- "4. $80,000-125,000"
anes_data$income[anes_data$income == "18. $110,000-124,999"] <- "4. $80,000-125,000"
anes_data$income[anes_data$income == "19. $125,000-149,999"] <- "5. $125,000+"
anes_data$income[anes_data$income == "20. $150,000-174,999"] <- "5. $125,000+"
anes_data$income[anes_data$income == "21. $175,000-249,999"] <- "5. $125,000+"
anes_data$income[anes_data$income == "22. $250,000 or more"] <- "5. $125,000+"
anes_data$income[is.na(anes_data$income)] <- "6. Refused"
anes_data$income <- as.factor(anes_data$income)
freq(anes_data$income, cum = TRUE)




##Motivations
#Political Interest-Follows Politics
names(anes_data)[names(anes_data) == "V201005"] <- "political_interest"
anes_data$political_interest[anes_data$political_interest == "1. Always"] <- "1"
anes_data$political_interest[anes_data$political_interest == "2. Most of the time"] <- "0.75"
anes_data$political_interest[anes_data$political_interest == "3. About half the time"] <- "0.50"
anes_data$political_interest[anes_data$political_interest == "4. Some of the time"] <- "0.25"
anes_data$political_interest[anes_data$political_interest == "5. Never"] <- "0"
anes_data$political_interest <- as.numeric(anes_data$political_interest)

#Follows Campaigns
names(anes_data)[names(anes_data) == "V201006"] <- "campaign_interest"
anes_data$campaign_interest[anes_data$campaign_interest == "1. Very much interested"] <- "1"
anes_data$campaign_interest[anes_data$campaign_interest == "2. Somewhat interested"] <- "0.50"
anes_data$campaign_interest[anes_data$campaign_interest == "3. Not much interested"] <- "0"
anes_data$campaign_interest <- as.numeric(anes_data$campaign_interest)

#Political Interest Index
interest_frame <- dplyr::select(anes_data, campaign_interest, political_interest)
cronbach.alpha(interest_frame, CI = TRUE, B = 500, na.rm = TRUE)
anes_data <- mutate(anes_data, interest_index = (campaign_interest + political_interest)/2)

#Group Consciousness- Feeling Thermometer
names(anes_data)[names(anes_data) == "V202166"] <- "gay_feeling_therm"
anes_data$gay_feeling_therm <- as.numeric(anes_data$gay_feeling_therm)
anes_data <- mutate(anes_data, gay_feeling_scaled = (gay_feeling_therm)/100)


#Efficacy-No say in government
names(anes_data)[names(anes_data) == "V202213"] <- "representation_efficacy"
anes_data$representation_efficacy[anes_data$representation_efficacy == "1. Agree strongly"] <- "0"
anes_data$representation_efficacy[anes_data$representation_efficacy == "2. Agree somewhat"] <- "0.25"
anes_data$representation_efficacy[anes_data$representation_efficacy == "3. Neither agree nor disagree"] <- "0.50"
anes_data$representation_efficacy[anes_data$representation_efficacy == "4. Disagree somewhat"] <- "0.75"
anes_data$representation_efficacy[anes_data$representation_efficacy == "5. Disagree strongly"] <- "1"
anes_data$representation_efficacy <- as.numeric(anes_data$representation_efficacy)

#Efficacy-How well they understand political issues
names(anes_data)[names(anes_data) == "V202215"] <- "knowledge_efficacy"
anes_data$knowledge_efficacy[anes_data$knowledge_efficacy == "1. Extremely well"] <- "1"
anes_data$knowledge_efficacy[anes_data$knowledge_efficacy == "2. Very well"] <- "0.75"
anes_data$knowledge_efficacy[anes_data$knowledge_efficacy == "3. Moderately well"] <- "0.50"
anes_data$knowledge_efficacy[anes_data$knowledge_efficacy == "4. Slightly well"] <- "0.25"
anes_data$knowledge_efficacy[anes_data$knowledge_efficacy == "5. Not well at all"] <- "0"
anes_data$knowledge_efficacy <- as.numeric(anes_data$knowledge_efficacy)

#Efficacy- Public Officials don't care
names(anes_data)[names(anes_data) == "V202212"] <- "public_efficacy"
anes_data$public_efficacy[anes_data$public_efficacy == "1. Agree strongly"] <- "0"
anes_data$public_efficacy[anes_data$public_efficacy == "2. Agree somewhat"] <- "0.25"
anes_data$public_efficacy[anes_data$public_efficacy == "3. Neither agree nor disagree"] <- "0.50"
anes_data$public_efficacy[anes_data$public_efficacy == "4. Disagree somewhat"] <- "0.75"
anes_data$public_efficacy[anes_data$public_efficacy == "5. Disagree strongly"] <- "1"
anes_data$public_efficacy <- as.numeric(anes_data$public_efficacy)

#Efficacy Index
efficacy_frame <- dplyr::select(anes_data, representation_efficacy, public_efficacy)
cronbach.alpha(efficacy_frame, CI = TRUE, B = 500, na.rm = TRUE)
anes_data <- mutate(anes_data, external_efficacy_index = (representation_efficacy + public_efficacy)/2)



##Mobilization
#Did someone talk to Respondent about Voting
names(anes_data)[names(anes_data) == "V202008"] <- "approached"
anes_data$approached[anes_data$approached == "1. Yes, someone did"] <- "1"
anes_data$approached[anes_data$approached == "2. No, no one did"] <- "0"
anes_data$approached <- as.factor(anes_data$approached)


#Contacted by non-party
names(anes_data)[names(anes_data) == "V202007"] <- "non_party_mobil"



###Dependent Variables###########
anes_data[anes_data == "2. No"] <- "0"
anes_data[anes_data == "1. Yes"] <- "1"
anes_data[anes_data == "1. Have done this in past 12 months"] <- "1"
anes_data[anes_data == "2. Have not done this in the past 12 months"] <- "0"
anes_data[anes_data == "1. Yes, have done this in the past 12 months"] <- "1"
anes_data[anes_data == "2. No, have not done this"] <- "0"



##Donations
#Contribute money to Candidate
names(anes_data)[names(anes_data) == "V202017"] <- "candidate_donation"

#Contribute money to Party
names(anes_data)[names(anes_data) == "V202019"] <- "party_donation"

#Contribute money to Political Group
names(anes_data)[names(anes_data) == "V202021"] <- "pol_group_donation"

#Contribute money to Issue Group
names(anes_data)[names(anes_data) == "V202028"] <- "iss_group_donation"

#Additive index
donation_frame <- dplyr::select(anes_data, candidate_donation, party_donation, pol_group_donation, iss_group_donation)
cronbach.alpha(donation_frame, CI = TRUE, B = 500, na.rm = TRUE)
anes_data <- mutate(anes_data, donation_index = as.numeric(candidate_donation) + as.numeric(party_donation) + as.numeric(pol_group_donation) + as.numeric(iss_group_donation))


##Electoral/Campaigning Participation

#Registered to Vote
names(anes_data)[names(anes_data) == "V201008"] <- "registered"
anes_data$registered[anes_data$registered == "3. Not currently registered"] <- 0
anes_data$registered[anes_data$registered == "2. Registered at a different address"] <- 1
anes_data$registered[anes_data$registered == "1. Registered at this address"] <- 1
anes_data$registered <- as.numeric(anes_data$registered)

#Did they vote?
names(anes_data)[names(anes_data) == "V202066"] <- "voted"
anes_data$voted[anes_data$voted == "4. I am sure I voted"] <- 1
anes_data$voted[anes_data$voted == "1. I did not vote (in the election this November)"] <- 0
anes_data$voted[anes_data$voted == "3. I usually vote, but didn't this time"] <- 0
anes_data$voted[anes_data$voted == "2. I thought about voting this time, but didn't"] <- 0
anes_data$voted <- as.numeric(anes_data$voted)

#Did Respondent talk to someone about Voting
names(anes_data)[names(anes_data) == "V202009"] <- "advocate"
anes_data$advocate <- as.numeric(anes_data$advocate)

#Go to political meetings, rallies and speeches in support of political candidate
names(anes_data)[names(anes_data) == "V202014"] <- "attendee"
anes_data$attendee <- as.numeric(anes_data$attendee)

#Wear Campaign sticker/button
names(anes_data)[names(anes_data) == "V202015"] <- "button"
anes_data$button <- as.numeric(anes_data$button)

#Work for Candidate
names(anes_data)[names(anes_data) == "V202016"] <- "campaign_worker"
anes_data$campaign_worker <- as.numeric(anes_data$campaign_worker)

#Additive Index
electoral_frame <- dplyr::select(anes_data, registered, voted, advocate, attendee, button, campaign_worker)
cronbach.alpha(electoral_frame, CI = TRUE, B = 50, na.rm = TRUE)
anes_data <- mutate(anes_data, electoral_index = as.numeric(candidate_donation) + as.numeric(party_donation) + as.numeric(pol_group_donation) + as.numeric(iss_group_donation))


##Governmental/Non-Electoral Participation
#Protest or Demonstration
names(anes_data)[names(anes_data) == "V202025"] <- "protest"

#Petition
names(anes_data)[names(anes_data) == "V202026"] <- "petition"

#Comment Posted Online
names(anes_data)[names(anes_data) == "V202029"] <- "comment_online"

#Contacting Federal elected
names(anes_data)[names(anes_data) == "V202034"] <- "federal_elected"

#Contacting Federal non-elected
names(anes_data)[names(anes_data) == "V202036"] <- "federal_non_elected"

#Contacting State/local elected
names(anes_data)[names(anes_data) == "V202038"] <- "state_elected"

#Contacting State/local non-elected
names(anes_data)[names(anes_data) == "V202040"] <- "state_non_elected"

#Working with community members to solve a problem
names(anes_data)[names(anes_data) == "V202031"] <- "community_issue"

#Attending a local meeting
names(anes_data)[names(anes_data) == "V202032"] <- "community_meeting"

#Additive Index
non_electoral_frame <- dplyr::select(anes_data, protest, petition, comment_online, federal_elected, federal_non_elected, state_elected, state_non_elected, community_issue, community_meeting)
cronbach.alpha(non_electoral_frame, CI = TRUE, B = 500, na.rm = TRUE)
anes_data <- mutate(anes_data, non_electoral_index = as.numeric(federal_non_elected) + as.numeric(state_elected) + as.numeric(state_non_elected) + as.numeric(community_issue) + as.numeric(community_meeting) + as.numeric(federal_elected) + as.numeric(protest) + as.numeric(petition) + as.numeric(comment_online))


###Survey Data

#Set Survey
anes_data1 <- subset(anes_data, !is.na(weights))
anes_data_weighted <- svydesign(id=~psu, weights=~weights, strata=~strata, nest=TRUE, survey.lonely.psu = "adjust", data=anes_data1)

#Subpopulations
anes_data_sexmin <- subset(anes_data_weighted, orientation1 == 1)
anes_data_hetero <- subset(anes_data_weighted, orientation1 == 0) 
###Analysis############################################################################

##Visualizations
#Box and Whisker Plot
plot(anes_data$additive_index ~ anes_data$orientation1)


###Cross-tabulations that actually work!

#Descriptives:
crosstab(anes_data$male, anes_data$orientation1, prop.c=T, plot=F, digits=2, chisq = TRUE, weight=anes_data$weights)
crosstab(anes_data$race, anes_data$orientation1, prop.c=T, plot=F, digits=2, chisq = TRUE, weight=anes_data$weights)
svyby(~age, ~orientation1, design=anes_data_weighted, na.rm=TRUE, svymean)
crosstab(anes_data$marriage, anes_data$orientation1, prop.c=T, plot=F, digits=2, chisq = TRUE, weight=anes_data$weights)
crosstab(anes_data$education, anes_data$orientation1, prop.c=T, plot=F, digits=2, chisq = TRUE, weight=anes_data$weights)
crosstab(anes_data$income, anes_data$orientation1, prop.c=T, plot=F, digits=2, chisq = TRUE, weight=anes_data$weights)

#Motivations:
svyby(~interest_index, ~orientation1, design=anes_data_weighted, na.rm=TRUE, svymean)
desc1 <- svyglm(interest_index ~ orientation1, design=anes_data_weighted)
summary(desc1)
svyby(~gay_feeling_therm, ~orientation1, design=anes_data_weighted, na.rm=TRUE, svymean)
desc2 <- svyglm(gay_feeling_therm ~ orientation1, design=anes_data_weighted)
summary(desc2)
svyby(~external_efficacy_index, ~orientation1, design=anes_data_weighted, na.rm=TRUE, svymean)
desc3 <- svyglm(external_efficacy_index ~ orientation1, design=anes_data_weighted)
summary(desc3)
svyby(~knowledge_efficacy, ~orientation1, design=anes_data_weighted, na.rm=TRUE, svymean)
desc4 <- svyglm(knowledge_efficacy ~ orientation1, design=anes_data_weighted)
summary(desc4)

#Mobilization:
crosstab(anes_data$party_mobil, anes_data$orientation1, prop.c=T, plot=F, digits=2, chisq = TRUE, weight=anes_data$weights)
crosstab(anes_data$non_party_mobil, anes_data$orientation1, prop.c=T, plot=F, digits=2, chisq = TRUE, weight=anes_data$weights)
crosstab(anes_data$approached, anes_data$orientation1, prop.c=T, plot=F, digits=2, chisq = TRUE, weight=anes_data$weights)



#####Analysis#########

###No controls, just means (Table 2)
#Donation Index
svyby(~donation_index, ~orientation1, design=anes_data_weighted, na.rm=TRUE, svymean)
donation_raw <- svyglm(donation_index ~ orientation1, design=anes_data_weighted)
summary(donation_raw)

#Non-Electoral Participation Index
svyby(~non_electoral_index, ~orientation1, design=anes_data_weighted, na.rm=TRUE, svymean)
non_electoral_raw <- svyglm(non_electoral_index ~ orientation1, design=anes_data_weighted)
summary(non_electoral_raw)

#Electoral Participation Activities
#Registering to vote
crosstab(anes_data$registered, anes_data$orientation1, prop.c=T, plot=F, digits=2, chisq = TRUE, weight=anes_data$weights)
registered_raw <- svyglm(registered ~ orientation1, design=anes_data_weighted)
summary(registered_raw)

#Voting
crosstab(anes_data$voted, anes_data$orientation1, prop.c=T, plot=F, digits=2, chisq = TRUE, weight=anes_data$weights)
voted_raw <- svyglm(voted ~ orientation1, design=anes_data_weighted)
summary(voted_raw)

#Talking to someone about voting
crosstab(anes_data$advocate, anes_data$orientation1, prop.c=T, plot=F, digits=2, chisq = TRUE, weight=anes_data$weights)
advocate_raw <- svyglm(advocate ~ orientation1, design=anes_data_weighted)
summary(advocate_raw)

#Wearing a campaign button
crosstab(anes_data$button, anes_data$orientation1, prop.c=T, plot=F, digits=2, chisq = TRUE, weight=anes_data$weights)
button_raw <- svyglm(button ~ orientation1, design=anes_data_weighted)
summary(button_raw)

#Campaign worker
crosstab(anes_data$campaign_worker, anes_data$orientation1, prop.c=T, plot=F, digits=2, chisq = TRUE, weight=anes_data$weights)
campaign_worker_raw <- svyglm(campaign_worker ~ orientation1, design=anes_data_weighted)
summary(campaign_worker_raw)

#Attending a campaign Event
crosstab(anes_data$attendee, anes_data$orientation1, prop.c=T, plot=F, digits=2, chisq = TRUE, weight=anes_data$weights)
attendee_raw <- svyglm(attendee ~ orientation1, design=anes_data_weighted)
summary(attendee_raw)

###With Demographics (Table 3)
#Donation Index
donation_demos <- svyglm(donation_index ~ orientation1 + male + race + marriage + age + age_square, design=anes_data_weighted)
summary(donation_demos)
tab_model(donation_demos, show.se=TRUE, digits = 3)

#Non-Electoral Participation Index
non_electoral_demos <- svyglm(non_electoral_index ~ orientation1 + male + race + marriage + age + age_square, design=anes_data_weighted)
summary(non_electoral_demos)
tab_model(non_electoral_demos, show.se=TRUE, digits = 3)

#Voter Participation
voted_demos <- svyglm(voted ~ orientation1 + male + race + marriage + age + age_square, design=anes_data_weighted)
summary(voted_demos)
tab_model(voted_demos, show.se=TRUE, digits = 3)



#######TABLE 4##########

###Donation Index 

#Demos + Resources
donation_t4v1 <- svyglm(donation_index ~  orientation1 + male + race + marriage + age + age_square + education + income, design=anes_data_weighted)
summary(donation_t4v1)
tab_model(donation_t4v1, show.se=TRUE, digits = 3)

#Demos + Resources + Motivations
donation_t4v2 <- svyglm(donation_index ~  orientation1 + male + race + marriage + age + age_square + education + income + interest_index + gay_feeling_therm + external_efficacy_index + knowledge_efficacy, design=anes_data_weighted)
summary(donation_t4v2)
tab_model(donation_t4v2, show.se=TRUE, digits = 3)

#Demos + Resources + Motivations + Mobilization
donation_t4v3 <- svyglm(donation_index ~  orientation1 + male + race + marriage + age + age_square + education + income + interest_index + gay_feeling_therm + external_efficacy_index + knowledge_efficacy + non_party_mobil + approached, design=anes_data_weighted)
summary(donation_t4v3)
tab_model(donation_t4v3, show.se=TRUE, digits = 3)


###Non-Electoral Index 

#Demos + Resources
nonelectoral_t4v1 <- svyglm(non_electoral_index ~  orientation1 + male + race + marriage + age + age_square + education + income, design=anes_data_weighted)
summary(nonelectoral_t4v1)
tab_model(nonelectoral_t4v1, show.se=TRUE, digits = 3)

#Demos + Resources + Motivations
nonelectoral_t4v2 <- svyglm(non_electoral_index ~  orientation1 + male + race + marriage + age + age_square + education + income + interest_index + gay_feeling_therm + external_efficacy_index + knowledge_efficacy, design=anes_data_weighted)
summary(nonelectoral_t4v2)
tab_model(nonelectoral_t4v2, show.se=TRUE, digits = 3)

#Demos + Resources + Motivations + Mobilization
nonelectoral_t4v3 <- svyglm(non_electoral_index ~  orientation1 + male + race + marriage + age + age_square + education + income + interest_index + gay_feeling_therm + external_efficacy_index + knowledge_efficacy + non_party_mobil + approached, design=anes_data_weighted)
summary(nonelectoral_t4v3)
tab_model(nonelectoral_t4v3, show.se=TRUE, digits = 3)

###Voter Participation

#Demos + Resources
vote_t4v1 <- svyglm(voted ~  orientation1 + male + race + marriage + age + age_square + education + income, design=anes_data_weighted)
summary(vote_t4v1)
tab_model(vote_t4v1, show.se=TRUE, digits = 3)

#Demos + Resources + Motivations
vote_t4v2 <- svyglm(voted ~  orientation1 + male + race + marriage + age + age_square + education + income + interest_index + gay_feeling_therm + external_efficacy_index + knowledge_efficacy, design=anes_data_weighted)
summary(vote_t4v2)
tab_model(vote_t4v2, show.se=TRUE, digits = 3)

#Demos + Resources + Motivations + Mobilization
vote_t4v3 <- svyglm(voted ~  orientation1 + male + race + marriage + age + age_square + education + income + interest_index + gay_feeling_therm + external_efficacy_index + knowledge_efficacy + non_party_mobil + approached, design=anes_data_weighted)
summary(vote_t4v3)
tab_model(vote_t4v3, show.se=TRUE, digits = 3)


#####Table 5#####


#Donation Index for Sexual Minorities
donation_effects_sexmin <- svyglm(donation_index ~  male + race + marriage + age + age_square + education + income + interest_index + gay_feeling_therm + external_efficacy_index + knowledge_efficacy + non_party_mobil + approached, design=anes_data_sexmin)
summary(donation_effects_sexmin)
tab_model(donation_effects_sexmin, show.se=TRUE, digits = 3)

#Donation Index for Heterosexuals
donation_effects_hetero <- svyglm(donation_index ~  male + race + marriage + age + age_square + education + income + interest_index + gay_feeling_therm + external_efficacy_index + knowledge_efficacy + non_party_mobil + approached, design=anes_data_hetero)
summary(donation_effects_hetero)
tab_model(donation_effects_hetero, show.se=TRUE, digits = 3)

#Non Electoral Index for Sexual Minorities
nonelect_effects_sexmin <- svyglm(non_electoral_index ~  male + race + marriage + age + age_square + education + income + interest_index + gay_feeling_therm + external_efficacy_index + knowledge_efficacy + non_party_mobil + approached, design=anes_data_sexmin)
summary(nonelect_effects_sexmin)
tab_model(nonelect_effects_sexmin, show.se=TRUE, digits = 3)

#Non Electoral Index for Heterosexuals
nonelect_effects_hetero <- svyglm(non_electoral_index ~  male + race + marriage + age + age_square + education + income + interest_index + gay_feeling_therm + external_efficacy_index + knowledge_efficacy + non_party_mobil + approached, design=anes_data_hetero)
summary(nonelect_effects_hetero)
tab_model(nonelect_effects_hetero, show.se=TRUE, digits = 3)

#Voter Turnout for Sexual Minorities
vote_effects_sexmin <- svyglm(voted ~  male + race + marriage + age + age_square + education + income + interest_index + gay_feeling_therm + external_efficacy_index + knowledge_efficacy + non_party_mobil + approached, design=anes_data_sexmin)
summary(vote_effects_sexmin)
tab_model(vote_effects_sexmin, show.se=TRUE, digits = 3)

#Voter Turnout for Heterosexuals
vote_effects_hetero <- svyglm(voted ~  male + race + marriage + age + age_square + education + income + interest_index + gay_feeling_therm + external_efficacy_index + knowledge_efficacy + non_party_mobil + approached, design=anes_data_hetero)
summary(vote_effects_hetero)
tab_model(vote_effects_hetero, show.se=TRUE, digits = 3)
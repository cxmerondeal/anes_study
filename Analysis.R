# Project: ANES Analysis and Sexuality?
# Programmer: C. Deal
# Date Updated: 04/26/2021

#Read in Data
library(readr)
anes_data <- read.csv(choose.files(), na.strings = c("-9. Refused","-5. Interview breakoff (sufficient partial IW)","-8. Don't know","-2.?Missing, other specify not coded for preliminary release", "-1. Inapplicable", "-6. No post-election interview", "-7. No post-election data, deleted due to incomplete interview"))
anes_data <- as_tibble(anes_data)

######Clean up Variables
library(dplyr)

?Sexuality
names(anes_data)[names(anes_data) == "V201601"] <- "orientation"
View(anes_data$orientation)
#Guide: 1. Straight 2. Gay or Lesbian 3. Bisexual

#Sex
names(anes_data)[names(anes_data) == "V201600"] <- "sex"
View(anes_data$sex)
#Guide: 1. Male 2. F?male

#Race
names(anes_data)[names(anes_data) == "V201549x"] <- "race"
View(anes_data$race)
#Guide: 1. White, non-Hispanic 2. Black, non-Hispanic 3. Hispanic 4. Asian or Native Hawaiian/other Pacific Islander, non-Hispanic alone 5. Native American/Alaska N?tive or other race, non-Hispanic alone 6. Multiple races, non-Hispanic

#Income
names(anes_data)[names(anes_data) == "V201617x"] <- "income"
View(anes_data$income)
#Guide:
#1. Under $9,999 2. $10,000-14,999 #3. $15,000-19,999 4. $20,000-24,999 #5. $25,000-?9,999 6. $30,000-34,999
#7. $35,000-39,999 8. $40,000-44,999 #9. $45,000-49,999 10. $50,000-59,999 #11. $60,000-64,999 12. $65,000-69,999
#13. $70,000-74,999 14. $75,000-79,999 #15. $80,000-89,999 16. $90,000-99,999 #17. $100,000-109,999 18. $110,000-124,9?9
#19. $125,000-149,999 20. $150,000-174,999 #21. $175,000-249,999 22. $250,000 or more

#Age Continuous
names(anes_data)[names(anes_data) == "V201507x"] <- "age"
View(anes_data$age)

#Age Categorical
anes_data$age[anes_data$age == "80. Age 80 or older"] <? 80
anes_data$age <- as.numeric(anes_data$age)
anes_data$agecat1<-cut(anes_data$age, c(18,29,39,49,59,69,79,109))
View(anes_data$agecat1)

#Marriage Status
names(anes_data)[names(anes_data) == "V201508"] <- "marriage"
View(anes_data$marriage)
#Guide: 1. Ma?ried: spouse present 2. Married: spouse absent 3. Widowed 4. Divorced 5. Separated 6. Never married

#Education Level
names(anes_data)[names(anes_data) == "V201511x"] <- "education"
View(anes_data$education)
#Guide:1. Less than high school credential 2. Hi?h school credential 3. Some post-high school, no bachelor's degree
#4. Bachelor's degree 5. Graduate degree

#Party of Registration
names(anes_data)[names(anes_data) == "V201018"] <- "party_reg"
View(anes_data$party_reg)

###Target of Political Participati?n
#Did someone talk to Respondent about Voting
names(anes_data)[names(anes_data) == "V202008"] <- "approached"
View(anes_data$approached)


###Agent of Political Participation
#When Respondent Registered To Vote
names(anes_data)[names(anes_data) == "V20205?"] <- "time_reg"
View(anes_data$time_reg)

#Did Respondent talk to someone about Voting
names(anes_data)[names(anes_data) == "V202009"] <- "advocate"
View(anes_data$advocate)

#Go to political meetings, rallies and speeches
names(anes_data)[names(anes_data? == "V202014"] <- "attendee"
View(anes_data$attendee)

#Wear Campaign sticker/button
names(anes_data)[names(anes_data) == "V202015"] <- "button"
View(anes_data$button) 

#Work for Candidate
names(anes_data)[names(anes_data) == "V202016"] <- "campaign_worke?"
View(anes_data$campaign_worker) 

#Contribute money to Candidate
names(anes_data)[names(anes_data) == "V202017"] <- "candidate_donation"
View(anes_data$candidate_donation) 

#Contribute money to Party
names(anes_data)[names(anes_data) == "V202019"] <- "p?rty_donation"
View(anes_data$party_donation) 

#v202021, v202022, v202023, v202024, v202025, v202026, v202029, whether they voted (V202066), v202031, 

setwd("~/FINAL_LAB")
#
#  Data Processing
#
#
# 
#
#
# My query already restricted to:
# Property_Type = 1 : 1-4 family
# Occupancy = 1 : owner-occupied
# Action_Type <= 4: (1) loan originated (2) application approved but not accepted
#                   (3) application denied (4) application withdrawn by applicant

#
library('RODBC') # Selects the required library

# Establishes the required connection to hmdalab database on the VM

ch <- odbcConnect("hmdalab",uid="gpadmin",
      case="postgresql",pwd="FTnw10")
ch

rm(paRaw)

paRaw <- (sqlFetch(ch,"padan",as.is=T))

head(paRaw, n=50)

length(paRaw$rate_spread) 

summary(paRaw)

odbcClose(ch)


#
# Let's turn some of the codes into factors with names
#
# 
#
head(paRaw$loan_type, n=10)

paRaw$loan_type = factor(paRaw$loan_type)
paRaw$loan_purpose = factor(paRaw$loan_purpose)
paRaw$preapproval = factor(paRaw$preapproval)
paRaw$action_type = factor(paRaw$action_type)
paRaw$county_name=factor(paRaw$county_name)
paRaw$lien_status = factor(paRaw$lien_status)
paRaw$applicant_ethnicity = factor(paRaw$applicant_ethnicity)
paRaw$applicant_race_1 = factor(paRaw$applicant_race_1)
paRaw$applicant_sex = factor(paRaw$applicant_sex)
#
# an example of how to check quickly. the diagonal of the table will 
# show how the codes match to the factor names
#
with(paRaw, table(loan_type, applicant_ethnicity))


#
# convert income and rate spread to numeric. Check the NAs.
#
paRaw$applicant_income_ink = as.numeric(paRaw$applicant_income_ink)
paRaw$rate_spread = as.numeric(paRaw$rate_spread)
paRaw$tract_to_msamd_income_pct = as.numeric(paRaw$tract_to_msamd_income_pct)
paRaw$number_of_owner_occupied_units = as.numeric(paRaw$number_of_owner_occupied_units)
paRaw$minority_population_pct = as.numeric(paRaw$minority_population_pct)

with(paRaw, {
  print(paste("no income:", sum(is.na(applicant_income_ink))/length(applicant_income_ink)))
  print(paste("low rate spread:", sum(is.na(rate_spread))/length(rate_spread)))
  print(paste("tract to MSA income", sum(is.na(tract_to_msamd_income_pct))/length(tract_to_msamd_income_pct)))
  print(paste("owner occupied units", sum(is.na(number_of_owner_occupied_units)/length(number_of_owner_occupied_units))))
  print(paste("minority population", sum(is.na(minority_population_pct)/length(minority_population_pct))))
})

# most people are NA on the rate spread, small minorities everywhere else
# 1] "no income: 0.0392906709656305"
# [1] "low rate spread: 0.983923329757597"
# [1] "tract to MSA income 0.00585855035222009"
# [1] "owner occupied units 0.00587269168065648"
# [1] "minority population 0.00579794465892126"

with(paRaw, table(highrate = !is.na(rate_spread), action_type))

summary(paRaw$rate_spread)

head(paRaw , n=50)
#
#  of loans either originated/denied -- 3:1 ratio
#  2% of originated loans at reportable rate spread
#

# with(paRaw, table(highrate = !is.na(Rate_Spread), action_type)
# + )
#         actiontype
# highrate Originated Approved Not Accepted Denied Withdrawn
#    FALSE     303028                 24835 100775     58407
#    TRUE        7958                     0      0         0
# > 7958/393928

# save it
save(paRaw, file="paRaw.RData")

#---------------------------------------------
#
# Data analysis.
#

load("paRaw.RData")  # if they have come from another session..

# library(ggplot2)  # for my own plotting purposes. not part of the official lab

cnames = c("action_type", "loan_type", "loan_purpose", "loan_amount_ink", "preapproval", 
           "county_name", "applicant_income_ink", "lien_status", "applicant_race_1", "applicant_ethnicity", "applicant_sex",
           "tract_to_msamd_income_pct", "number_of_owner_occupied_units", "minority_population_pct",
           "hud_median_family_income", "hoepa_status", "rate_spread")
prob1data = paRaw[, cnames]

# let's just get rid of the people without income info
prob1data = subset(prob1data, !is.na(prob1data$applicant_income_ink))
length(prob1data$rate_spread)
write.csv(prob1data, "~/FINAL_LAB/ProjectDatacleansed.csv", sep="\t")
#
# relevel county, race and ethnicity -- this sets Allegheny County (where Pittsburgh is), 
# white and non hispanic latino to be the first category 
# value in their respective lists, hence they will be folded into the "reference situation" for the logistic model.
# Only necessary if they are doing a logistic model, and honestly, not strictly necessary even then.
# The model will work fine without the releveling, but this makes some of the explanation easier, perhaps.
#
prob1data$county_name = relevel(prob1data$county, "Adair")
prob1data$applicant_race_1 = relevel(prob1data$applicant_race_1, "White")
prob1data$applicant_ethnicity = relevel(prob1data$applicant_ethnicity, "Non Hispanic.Latino")

# make hud median family income (MSA level) numeric. some nulls
prob1data$hud_median_family_income = as.numeric(prob1data$hud_median_family_income)

write.csv(prob1data, "~/FINAL_LAB/ProjectDatacleansedHUD.csv", sep="\t")

tmp = sum(with(prob1data, is.na(tract_to_msamd_income_pct) | is.na(minority_population_pct) | is.na(hud_median_family_income)))
tmp/dim(prob1data)[1]  # 0.006. nuke them

# remove rows with nulls in msa income, minority pop, tract to msa pct
tmp = subset(prob1data, !(is.na(tract_to_msamd_income_pct) | is.na(minority_population_pct) | is.na(hud_median_family_income)))
prob1data=tmp

write.csv(prob1data, "~/FINAL_LAB/ProjectDatacleansedtmp.csv", sep="\t")

summary(prob1data)

write.csv(prob1data, "~/FINAL_LAB/ProjectDatacleansednoNulls.csv", sep="\t")

# make the estimate of tract median income -- this is a driver that I thought of; it turns out not to be significant.
# not strictly necessary by the students, but if we can breadcrumb them to it, it is a good exercise in
# being creative about variable creation/selection
tract_median_income = with(prob1data, hud_median_family_income*(tract_to_msamd_income_pct/100))
prob1data$tract_median_income_ink = round(tract_median_income/1000)  # useful to have it in the same units as loan amount and income

#ggplot(prob1data) + geom_density(aes(x=tract_median_income_inK)) +  scale_x_log10()  # reasonably normalish
# one of the following 2
den = density(prob1data$tract_median_income_ink)
plot(den, log="x")
axis(1, at = seq(10, 200, by = 10), las=2)
# or
den = density(log10(prob1data$tract_median_income_inK))
plot(den)
axis(1, at = seq(10, 200, by = 10), las=2)
abline(v=50)
abline(v=75)
abline(v=90)
#
# visualize the variables. They should visualize all (or many, at least) of them.
# I'm skipping the details here, and cutting to the chase.
# We observe that loan amount has a very odd, multi-modal distribution. this suggests
# that we have multiple borrower populations. This suggests to us that we might
# want to build separate models for the different loan purposes. Let's check.
#
# ggplot(prob1data, aes(x=Loan_Amount_inK)) + geom_density(adjust=0.5) + scale_x_log10()
plot(density(prob1data$loan_amount_ink))

plot(density(log10(prob1data$loan_amount_ink)))

# look how each loan amount is distributed.
# ggplot(prob1data, aes(x=Loan_Amount_inK, colour=loanpurpose)) + geom_density(adjust=0.5) + scale_x_log10()
with(prob1data,  {
 homepurchase = density(log10(subset(loan_amount_ink, loan_purpose=="Home purchase")))
 homeimprovement = density(log10(subset(loan_amount_ink, loan_purpose=="Home improvement")))
 refinance = density(log10(subset(loan_amount_ink, loan_purpose=="Refinancing")))
 
 plot(homepurchase, col="red", main="Classification of loans" , xlab = "Log Values of Loan Amount")
 lines(homeimprovement, col="blue")
 lines(refinance, col="green")
 
})


# so let's drop home improvements, just to make the experiment cleaner
prob1data = subset(prob1data, !(prob1data$loan_purpose %in% "Home improvement"))

write.csv(prob1data, "~/FINAL_LAB/ProjectDatacleansednoHomeImprovement.csv", sep="\t")

#prob1datafourk <- read.csv("~/FINAL_LAB/ProjectDatacleansednoNulls.csv", header = TRUE)


plot(density(prob1data$loan_amount_ink), main="Loan Amount Density" , xlab = "Loan amount in thousands")
plot(density(log10(prob1data$loan_amount_ink)))
abline(v=440)
abline(v=400)

# what is that spike at about 400K? Smooth up until then. That and past it is one or two other populations.
# Let's try a model for below that spike. In principle, we can develop a separate model beyond that spike
# (Note, they might also want to eliminate some of the very small loans that trail out on the right.
# I didn't do that here, but it would be a fair thing for them to do)
filter = prob1data$loan_amount_ink <= 400
# ggplot(prob1data[filter,], aes(x=Loan_Amount_inK, colour=loanpurpose)) + geom_density(adjust=0.5) + scale_x_log10()

plot(density(log10(prob1data$loan_amount_ink)), main="Loan Amount Density")


plot(density(log10(prob1data[filter,c("loan_amount_ink")])))

sum(filter)/length(filter) # we lose about 4% of the loan data.

prob1data = prob1data[filter,]

plot(density(prob1data$loan_amount_ink), main="Loan Amount Density <= 400 thousand", xlab="Income in thousands")
#
# till here
# subset to only originated and denied: reasoning -- borrowers will take most advantageous loan they can.
# This might be part of the starting scenario
#

filter = with(prob1data, action_type %in% c("Originated", "Denied"))
prob1data = prob1data[filter,]
prob1data$approved = prob1data$action_type=="Originated"
table(prob1data$approved)/dim(prob1data)[1]
#     FALSE      TRUE 
# 0.2217928 0.7782072 

write.csv(prob1data, "~/FINAL_LAB/ProjectDatacleansedno400k.csv", sep="\t")

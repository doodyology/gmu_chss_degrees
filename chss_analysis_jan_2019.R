# Reading the IPUMS ACS 2017 1-year estimates CSV
acs_raw = read.csv(file.choose(), header = TRUE)

# Make copy of ACS data for editing; ACS will remain raw & untouched.
data = acs_raw

# Grab the lookup table for categorical variables.
lookup = read.csv(file.choose())
chsscodes = read.csv(file.choose())

# check the data
head(acs_raw)
head(data)
head(lookup)
head(chsscodes)


# Get our survey data library imports.
library(devtools)
library(survey)
library(questionr)
library(plyr)
library(dplyr)
install.packages("matrixStats") # for weighting the median income calculations.
library(matrixStats) # we need the weightedMedian() function

#Check the vars.
data$DEGFIELDD
data$STATEICP
data$MET2013
data$EMPSTAT
mean(data$INCTOT)
median(data$INCTOT)

lookup$degree
lookup$state
lookup$metro
lookup$employment
lookup$occupation
lookup$industry
lookup$degtype

chsscodes$chsscode
chsscodes$chssdeg

# Match the files.
data$degree = lookup$degree[match(data$DEGFIELDD, lookup$DEGFIELDD)] # ACS coding for fields.
data$degtype = lookup$degtype[match(data$DEGFIELDD, lookup$degclass)] # No Degree, CHSS Degree, or Non-CHSS degree.
data$chssdeg = chsscodes$chssdeg[match(data$DEGFIELDD, chsscodes$chsscode)] # The actual CHSS degrees.

data$state = lookup$state[match(data$STATEICP, lookup$STATEICP)] 
data$metro = lookup$metro[match(data$MET2013, lookup$MET2013)] 

data$employment[data$EMPSTAT==3] <- NA
data$employment[data$EMPSTAT==2] <- "Unemployed"
data$employment[data$EMPSTAT==1] <- "Employed"
data$employment[data$EMPSTAT==0] <- NA

unique(data$employment)

data$occupation = lookup$occupation[match(data$OCC, lookup$OCC)] # occupation.
data$occupation[data$occupation=="N/A"] <- NA # Fixe unvailable.
data$industry = lookup$industry[match(data$IND, lookup$IND)] # industry.
data$industry[data$industry=="Unemployed"] <- NA

unique(data$employment)
unique(data$occupation)
unique(data$industry)
unique(data$degtype)
unique(data$chssdeg)

# Need to fix the INCTOT variable so 9999999 registers as missing, not income.
data$INCTOT[data$INCTOT==9999999] <- NA
mean(data$INCTOT)
range(data$INCTOT) # fixed.

# Limit the data set to ages 25 - 45
data <- subset(data, data$AGE >=25 & data$AGE <= 45)
min(data$AGE)
max(data$AGE)
range(data$AGE)

# Create the DC MSA data frame (MET2013 code = 47900)
dcmsa = subset(data, data$MET2013 == 47900)
unique(dcmsa$metro)
range(dcmsa$AGE)

# We now have our national and DC MSA data frames and begin analysis.

#=======================================================================#

# NATIONAL ANALYSIS

# Table 1: Occupation by Degree
table(data$occupation, data$chssdeg)
wtd.table(data$occupation, data$chssdeg, weights = data$PERWT)
tab1 <- wtd.table(data$occupation, data$chssdeg, weights = data$PERWT)
tab1
tab2 = apply(tab1, 2, prop.table) # will multiply by 100 in Excel.
tab2

write.table(tab2, file = "table1.csv", sep = ',')

# Table 2: Industry by Degree
table(data$industry, data$chssdeg)
wtd.table(data$industry, data$chssdeg, weights = data$PERWT)
tab3 <- wtd.table(data$industry, data$chssdeg, weights = data$PERWT)
tab3
tab4 = apply(tab3, 2, prop.table)
tab4

write.table(tab4, file = "table2.csv", sep = ",")


# Table 3: CHSS v. No Degree, Occupation
table(data$occupation, data$degtype)
wtd.table(data$occupation, data$degtype, weights = data$PERWT)
tab5 <- wtd.table(data$occupation, data$degtype, weights = data$PERWT)
tab5
tab6 = apply(tab5, 2, prop.table) # will multiply by 100 in Excel.
tab6

write.table(tab6, file = "table3.csv", sep=",")

# Table 4: CHSS v. No Degree, Industry
table(data$industry, data$degtype)
wtd.table(data$industry, data$degtype, weights = data$PERWT)
tab7 <- wtd.table(data$industry, data$degtype, weights = data$PERWT)
tab7
tab8 = apply(tab7, 2, prop.table)
tab8

write.table(tab8, file = "table4.csv", sep = ",")

?aggregate

# Tables 6,7,8,9: Income

# averge income
avginc <- 
  data %>%
  group_by(degtype) %>% 
  summarise(wtinc = weighted.mean(INCTOT, PERWT)) # function adds population weights.
avginc

write.table(avginc, file = "table5.csv", sep = ",")

chssincavg <- data %>%
  group_by(chssdeg) %>% 
  summarise(wtinc = weighted.mean(INCTOT, PERWT)) # function adds population weights.
chssincavg

write.table(chssincavg, file = "table6.csv", sep = ",")

# median income
chssmed <- data %>%
  group_by(chssdeg) %>%
  summarise(wtdmed = weightedMedian(INCTOT, PERWT)) # function adds population weights.
chssmed

write.table(chssmed, file = "table7.csv", sep = ",")

medinc <- data %>%
  group_by(degtype) %>%
  summarise(wtdmed = weightedMedian(INCTOT, PERWT)) # function adds population weights.
medinc

write.table(medinc, file = "table8.csv", sep = ",")

# Tables 10 & 11: Employment
table(data$chssdeg, data$employment)
wtd.table(data$chssdeg, data$employment, weights = data$PERWT)
tab9 <- wtd.table(data$chssdeg, data$employment, weights = data$PERWT)
tab9
tab10 = apply(tab9, 1, prop.table)
tab10

write.table(tab10, file = "table9.csv", sep = ",")

table(data$degtype, data$employment)
wtd.table(data$degtype, data$employment, weights = data$PERWT)
tab11 <- wtd.table(data$degtype, data$employment, weights = data$PERWT)
tab11
tab12 = apply(tab11, 1, prop.table)
tab12

write.table(tab12, file = "table10.csv", sep = ",")


#=======================================================================#

# DC MSA Analysis


# Table 1: Occupation by Degree
table(dcmsa$occupation, dcmsa$chssdeg)
wtd.table(dcmsa$occupation, dcmsa$chssdeg, weights = dcmsa$PERWT)
dc1 <- wtd.table(dcmsa$occupation, dcmsa$chssdeg, weights = dcmsa$PERWT)
dc1
dctab1 = apply(dc1, 2, prop.table) # will multiply by 100 in Excel.
dctab1

write.table(dctab1, file = "dc1.csv", sep = ',')

# Table 2: Industry by Degree
table(dcmsa$industry, dcmsa$chssdeg)
wtd.table(dcmsa$industry, dcmsa$chssdeg, weights = dcmsa$PERWT)
dc2 <- wtd.table(dcmsa$industry, dcmsa$chssdeg, weights = dcmsa$PERWT)
dc2
dctab2 = apply(dc2, 2, prop.table)
dctab2

write.table(dctab2, file = "dc2.csv", sep = ",")


# Table 3: CHSS v. No Degree, Occupation
table(dcmsa$occupation, dcmsa$degtype)
wtd.table(dcmsa$occupation, dcmsa$degtype, weights = dcmsa$PERWT)
dc3 <- wtd.table(dcmsa$occupation, dcmsa$degtype, weights = dcmsa$PERWT)
dc3
dctab3 = apply(dc3, 2, prop.table) # will multiply by 100 in Excel.
dctab3

write.table(dctab3, file = "dc3.csv", sep=",")

# Table 4: CHSS v. No Degree, Industry
table(dcmsa$industry, dcmsa$degtype)
wtd.table(dcmsa$industry, dcmsa$degtype, weights = dcmsa$PERWT)
dc4 <- wtd.table(dcmsa$industry, dcmsa$degtype, weights = dcmsa$PERWT)
dc4
dctab4 = apply(dc4, 2, prop.table)
dctab4

write.table(dctab4, file = "dc4.csv", sep = ",")

# Tables 6,7,8,9: Income

# averge income
dcavginc <- 
  dcmsa %>%
  group_by(degtype) %>% 
  summarise(dcwtinc = weighted.mean(INCTOT, PERWT)) # function adds population weights.
dcavginc

write.table(dcavginc, file = "dc5.csv", sep = ",")

dcchssincavg <- dcmsa %>%
  group_by(chssdeg) %>% 
  summarise(dcchasswtinc = weighted.mean(INCTOT, PERWT)) # function adds population weights.
dcchssincavg

write.table(dcchssincavg, file = "dc6.csv", sep = ",")

# median income
dcchssmed <- dcmsa %>%
  group_by(chssdeg) %>%
  summarise(dcwtdmed = weightedMedian(INCTOT, PERWT)) # function adds population weights.
dcchssmed

write.table(dcchssmed, file = "dc7.csv", sep = ",")

dcmedinc <- dcmsa %>%
  group_by(degtype) %>%
  summarise(dcwtdmed = weightedMedian(INCTOT, PERWT)) # function adds population weights.
dcmedinc

write.table(medinc, file = "dc8.csv", sep = ",")

# Tables 10 & 11: Employment
table(dcmsa$chssdeg, dcmsa$employment)
wtd.table(dcmsa$chssdeg, dcmsa$employment, weights = dcmsa$PERWT)
dc5 <- wtd.table(dcmsa$chssdeg, dcmsa$employment, weights = dcmsa$PERWT)
dc5
dctab9 = apply(dc5, 1, prop.table)
dctab9

write.table(dctab9, file = "dc9.csv", sep = ",")

table(dcmsa$degtype, dcmsa$employment)
wtd.table(dcmsa$degtype, dcmsa$employment, weights = dcmsa$PERWT)
dc6 <- wtd.table(dcmsa$degtype, dcmsa$employment, weights = dcmsa$PERWT)
dc6
dctab10 = apply(dc6, 1, prop.table)
dctab10

write.table(dctab10, file = "dc10.csv", sep = ",")
# TEXT MINING 

# IEMS 308 
# Renee Probetts

# install the necessary packages 
install.packages('e1071')
install.packages('SparseM')
install.packages('tm')
install.packages(c("NLP", "openNLP", "magrittr"))
install.packages("rJava", type = "source")
install.packages("stringr", dependencies = TRUE)
install.packages("quanteda")
library('e1071')
library('SparseM')
library('tm')
library('dplyr')
library("tidyr")
library("quanteda")
library(NLP)
library(openNLP)
library(stringr)


# read in the excel sheets (only for creating the dictionary for Initial Idea)
company <- read.csv("companies.csv", header = FALSE, stringsAsFactors = FALSE)
ceo <- read.csv("ceo.csv", header = FALSE, stringsAsFactors = FALSE)
df <- data.frame(ceo, stringsAsFactors = FALSE)
df$ceonames <- paste(df$V1,df$V2, sep = " ")
ceo <- data.frame(df$ceonames, stringsAsFactors = FALSE)


# read in the excel sheets
company <- read.csv("companies.csv", header = FALSE)
ceo <- read.csv("ceo.csv", header = TRUE)
df <- data.frame(ceo)
df$ceonames <- paste(df$NAMES,df$X, sep = " ")
ceo <- data.frame(df$ceonames)
percentage <- read.csv("percentage.csv", header = FALSE )


# create dictionary from the lists of ceos, companies, and percents with duplicates removed  
uniqueceo <- unique(ceo)
uniquecompany <- unique(company)
uniquepercent <- unique(percentage)
mydictionary <- dictionary(list(ceos=uniqueceo,companies = uniquecompany,percents = uniquepercent))

# download the text files and create the corpus 
pathname2013 <- file.path("D:", "Profiles", "rsp714", "Documents","TextMining", "2013")
dir(pathname2013) # make sure these are the correct text files
pathname2014 <- file.path("D:", "Profiles", "rsp714", "Documents","TextMining", "2014")
dir(pathname2014) # make sure these are the correct text files 
corpus <- VCorpus(DirSource(c(pathname2013,pathname2014)))
sentencecorpus2013 <- corpus_reshape(corpus2013, to = "sentences")
tokenizedsentence <- tokens(sentencecorpus2013)

## *** Initial Idea: use the dictionary to match words within the corpus 
potentialceo <- dfm(corpus, dictionary = mydictionary["ceos"])
potentialcompany <- dfm(corpus,dictionary = mydictionary["companies"])
potentialpercent <- dfm(corpus,dictionary = mydictionary["percents"])
## this method is inefficient and slow to run, but would be very effective with a smaller dictionary 


### CEO ANALYSIS 

### regular expressions and features for CEOs
ceoPatterns <- c("\\[A-Z][a-z]+\\","\\[A-Z][a-z]+\\[A-Z][a-z]+","[A-Z]", "CEO[ ,.:;][A-Z][a-z]+ [A-Z][a-z]+","[A-Z][a-z]+ [A-Z][a-z]+[ ,;:]CEO","[A-Z][a-z]+ [A-Z][a-z]+[ ,;:]cheif","cheif[ ,.:;][A-Z][a-z]+ [A-Z][a-z]+")
ceoPatternsCollapsed <- paste(ceoPatterns,collapse = "|") # if you want to search for all features at once 
ceoFeatures <- c("All Characters", "Two Words", "Less Than 20 characters", "UpperCase words", "CEO nearby", "'cheif' nearby" )

# create data table for the features
ceoRegularExpressions <- vector()
for (i in 1:nrow(ceo)){ceoRegularExpressions[i]<- grepl(ceoPatterns[1],as.String(ceo[i,1]))}
dfceo <- data.frame(ceoRegularExpressions)
for (i in 1:nrow(ceo)){ceoRegularExpressions[i]<- grepl(ceoPatterns[2],as.String(ceo[i,1]))}
dfceo <- data.frame(dfceo,ceoRegularExpressions)
ceoStringLength <-vector()
for (i in 1:nrow(ceo)){if (nchar(as.String(ceo[i,1])) > 8 & nchar(as.String(ceo[i,1])) < 25) {ceoStringLength[i] <- 1} else {ceoStringLength[i] <- 0}}
dfceo <- data.frame(dfceo,ceoStringLength)
for (i in 1:nrow(ceo)){ceoRegularExpressions[i]<- grepl(ceoPatterns[3],as.String(ceo[i,1]))}
dfceo <- data.frame(dfceo,ceoRegularExpressions)
# the following will all show up as 0 if using regular expressions, but to include it as a feature, we will randomize certain CEO names to have CEO/cheif next to the word 
CeoNearby <- rep(0, nrow(ceo))
rand <- sample.int(3000,750)
CeoNearby[rand] <- 1
dfceo <- data.frame(dfceo, CeoNearby)
CheifNearby <- rep(0,nrow(ceo))
rand <- sample.int(3000,750)
CheifNearby[rand] <- 1
dfceo <- data.frame(dfceo,CheifNearby)
dfceo <- lapply(dfceo,as.numeric)
dfceo <- data.frame(dfceo)
names(dfceo) <- ceoFeatures
ceoyesorno <- rep(1:0, c(2657,850))
dfceo <- data.frame(ceo, dfceo, ceoyesorno)

# train a model for the ceos based on this data 
ceomodel <- glm(ceoyesorno~ All.Characters + Two.Words + Less.Than.20.characters + UpperCase.words + CEO.nearby + X.cheif..nearby, data = dfceo, family = binomial)

# extract the potential CEO names from the corpus 
potentialCEOS1 <- str_extract_all(corpus, "\\s[A-Z][a-z]+\\s[A-Z][a-z]+") #NOTE many things that are not names 
df1 <- data.frame(as.vector(potentialCEOS1[[1]]),rep(0,length(as.vector(potentialCEOS1[[1]]))),rep(0,length(as.vector(potentialCEOS1[[1]]))))
potentialCEOS2 <- str_extract_all(corpus, "[A-Z][a-z]+\\s[A-Z][a-z]+\\sCEO")
potentialCEOS2[[1]] <- removeWords(potentialCEOS2[[1]], "CEO")
df2 <- data.frame(as.vector(potentialCEOS2[[1]]),rep(1,length(as.vector(potentialCEOS2[[1]]))),rep(0,length(as.vector(potentialCEOS2[[1]]))))
potentialCEOs3 <- str_extract_all(corpus, "CEO\\s[A-Z][a-z]+\\s[A-z][a-z]+")
potentialCEOs3[[1]] <- removeWords(potentialCEOs3[[1]], "CEO")
df3 <- data.frame(as.vector(potentialCEOs3[[1]]),rep(1,length(as.vector(potentialCEOs3[[1]]))),rep(0,length(as.vector(potentialCEOs3[[1]]))))
potentialCEOS4 <- str_extract_all(corpus, "[A-Z][a-z]+\\s[A-Z][a-z]+\\schief")
potentialCEOS4[[1]] <- removeWords(potentialCEOS4[[1]], "chief")
df4 <- data.frame(as.vector(potentialCEOS4[[1]]),rep(0,length(as.vector(potentialCEOS4[[1]]))),rep(1,length(as.vector(potentialCEOS4[[1]]))))
potentialCEOS5 <- str_extract_all(corpus, "chief\\sexecutive\\sofficer\\s[A-Z][a-z]+\\s[A-Z][a-z]+")
potentialCEOS5[[1]] <- removeWords(potentialCEOS2[[1]], c("chief","executive","officer"))
df5 <- data.frame(as.vector(potentialCEOS5[[1]]),rep(0,length(as.vector(potentialCEOS5[[1]]))),rep(1,length(as.vector(potentialCEOS5[[1]]))))

# create data frame of potential CEOs 
names(df1)<- c("Name", "CEOnearby", "cheifNearby")
names(df2) <-c("Name", "CEOnearby", "cheifNearby")
names(df3) <- c("Name", "CEOnearby", "cheifNearby")
names(df4) <- c("Name", "CEOnearby", "cheifNearby")
names(df5) <-c("Name", "CEOnearby", "cheifNearby")

potentialceo <- rbind.data.frame(df1,df2)
potentialceo <- rbind.data.frame(potentialceo,df3)
potentialceo <- rbind.data.frame(potentialceo,df4)
potentialceo <- rbind.data.frame(potentialceo,df5)
potentialceo <- unique(potentialceo[ ,1])

# because of the regex, these will all be true for features 1 and 2 
repetition <- rep(1,length(potentialceo))
dfpotentialceo <- data.frame(potentialceo,repetition,repetition, repetition)
ceopotentStringLength <-vector()
for (i in 1:nrow(potentialceo)){if (nchar(as.String(potentialceo[i,1])) > 8 & nchar(as.String(potentialceo[i,1])) < 25) {ceopotentStringLength[i] <- 1} else {ceopotentStringLength[i] <- 0}}
dfpotentialceo <- data.frame(dfpotentialceo,ceopotentStringLength)
names(potentialceo) <- c("Name", "CEOnearby", "cheifNearby", "All Characters", "UpperCase","Two Words", "Characters")

# Move the columns and change the names so they match the training data 
potential.ceo <- data.frame(dfpotentialceo[,1],dfpotentialceo[ ,4], dfpotentialceo[ ,6 ], dfpotentialceo[ ,7],dfpotentialceo[,5],dfpotentialceo[,2],dfpotentialceo[,3])
names(potential.ceo) <- colnames(dfceo[,1:7])

# use the logistic model to predict what they will be 
ceo.fit <- predict(ceomodel,potential.ceo,type = "response")
ceo.predict <- rep("Not",99831)
ceo.predict[ceo.fit > .40] <- "CEO" 

potential.ceo$prediction <- ceo.predict
CEOS <- potential.ceo[!potential.ceo$prediction == "Not",]

### COMPANY ANALYSIS

### regular expressions and features for companies 
companyPatterns <- c("\\s[A-Z][A-Z]+", "[A-Z][a-z]+(?=\\sInc|\\sCo|\\sLtd|\\sGroup|\\sCompany|\\sLab|\\sFinancial|\\sManagement|\\sVentures|\\sCapital|\\sLoans|\\sBank|\\sHoldings|\\sPartners|\\sMedia|\\sExchange|\\sEntertainment|\\sExchange|\\sCompany|\\sAdvisors)", "[A-Z][a-z]+\\sCo","[A-Z][a-z]+\\sLtd","[A-Z][a-z]\\sGroup", "[A-Z][a-z]+[A-Z][a-z]+", "[A-Z][a-z]+\\s[A-Z][a-z]+\\s[A-Z][a-z]+")
companyPatternsCollapsed <- paste(companyPatterns,collapse = "|")
companyFeatures <- c("All Caps","ends in Company Word", "Capital in the middle of a word", "Three Uppercase Words in a Row")

# create data table for features of Companies 
companyRegularExpressions<- vector() 
for (i in 1:nrow(company)) {companyRegularExpressions[i] <- grepl(companyPatterns[1],as.String(company[i,1]))}
dfcompany <- data.frame(companyRegularExpressions)
for (i in 1:nrow(company)) {companyRegularExpressions[i] <- grepl(companyPatterns[2],as.String(company[i,1]))}
dfcompany <- data.frame(dfcompany,companyRegularExpressions)
for (i in 1:nrow(company)) {companyRegularExpressions[i] <- grepl(companyPatterns[3],as.String(company[i,1]))}
dfcompany <- data.frame(dfcompany,companyRegularExpressions)
for (i in 1:nrow(company)) {companyRegularExpressions[i] <- grepl(companyPatterns[4],as.String(company[i,1]))}
dfcompany <- data.frame(dfcompany,companyRegularExpressions)
dfcompany <- lapply(dfcompany,as.numeric)
dfcompany <- data.frame(dfcompany)
names(dfcompany) <- companyFeatures
companyyesorno <- rep(1:0, c(4111,1292))
dfcompany <- data.frame(company,dfcompany, companyyesorno)

# train the model for companies using the data 
companymodel <- glm(companyyesorno~All.Caps + ends.in..Company.Word. + Uppercase.Word + Capital.in.the.middle.of.a.word + Three.Uppercase.Words.in.a.Row, data = dfcompany,family = binomial)

# extract the potential company names from the corpus 
potentialcompany <- str_extract_all(corpus,companyPatternsCollapsed)

potentialcompany <- as.vector(potentialcompany[[1]])
potentialcompany <- unique(potentialcompany)

# create data frame with potential companies and their features 
company1RegularExpressions<- vector() 
for (i in 1:nrow(potentialcompany)) {company1RegularExpressions[i] <- grepl(companyPatterns[1],as.String(potentialcompany[i,1]))}
df.potentialcompany <- data.frame(company1RegularExpressions)
for (i in 1:nrow(potentialcompany)) {company1RegularExpressions[i] <- grepl(companyPatterns[2],as.String(potentialcompany[i,1]))}
df.potentialcompany <- data.frame(df.potentialcompany,company1RegularExpressions)
for (i in 1:nrow(potentialcompany)) {compan1yRegularExpressions[i] <- grepl(companyPatterns[3],as.String(potentialcompany[i,1]))}
df.potentialcompany <- data.frame(df.potentialcompany,company1RegularExpressions)
for (i in 1:nrow(potentialcompany)) {company1RegularExpressions[i] <- grepl(companyPatterns[4],as.String(potentialcompany[i,1]))}
df.potentialcompany <- data.frame(df.potentialcompany,compan1yRegularExpressions)
df.potentialcompany <- lapply(df.potentialcompany,as.numeric)
df.potentialcompany <- data.frame(df,potentialcompany)
names(df.potentialcompany) <- companyFeatures


# predict whether it is a company or not using the logistic regression
company.fit <- predict(companymodel,df.potentialcompany,type = "response")
company.predict <- rep("Not",99831)
company.predict[company.fit > .40] <- "Company" 

df.potentialcompany$prediction <- company.predict
COMPANIES <- df.potentialcompany[!df.potentialcompany$prediction == "Not",]


### PERCENTAGE ANALYSIS 

### regular expressions and features for PERCENTAGES
percentPatterns <- c("[0-9]+\\.[0-9]+","[[:digit:]]+\\.[[:digit:]]+[ ]?\\%", "[[:digit:]]","[[:digit:]][ ]?percent","[a-z]+[ ]?percent")
percentPatternsCollapsed <- paste(percentPatterns, collapse = "|") # use if trying to get all Patterns at once 
percentFeatures <- c("Digit.Digit", "Digit.Digit % ", "Number", "Number 'percent'","'Number' 'percent'")
percentRegularExpressions <- vector()

# create a data table for the different patterns 
for (i in 1:nrow(percentage)) {percentRegularExpressions[i] <- grepl(percentPatterns[1],as.String(percentage[i,1]))}
dfpercent <- data.frame(percentRegularExpressions)
for (i in 1:nrow(percentage)) {percentRegularExpressions[i] <- grepl(percentPatterns[2],as.String(percentage[i,1]))}
dfpercent <- data.frame(dfpercent,percentRegularExpressions)
for (i in 1:nrow(percentage)) {percentRegularExpressions[i] <- grepl(percentPatterns[3],as.String(percentage[i,1]))}
dfpercent <- data.frame(dfpercent,percentRegularExpressions)
for (i in 1:nrow(percentage)) {percentRegularExpressions[i] <- grepl(percentPatterns[4],as.String(percentage[i,1]))}
dfpercent <- data.frame(dfpercent,percentRegularExpressions)
for (i in 1:nrow(percentage)) {percentRegularExpressions[i] <- grepl(percentPatterns[5],as.String(percentage[i,1]))}
dfpercent <- data.frame(dfpercent,percentRegularExpressions)
dfpercent <- lapply(dfpercent,as.numeric)
dfpercent <- data.frame(dfpercent)
names(dfpercent) <- percentFeatures
percentyesno <- rep(1:0, c(5376,860))
dfpercent <- data.frame(percentage,dfpercent,percentyesno) # create a column for whether or not it is an acutal percent or fake 

# train model based on this data
percentmodel <- glm(percentyesno~Digit.Digit + Digit.Digit... + Number + Number..percent. + X.Number...percent. , data = dfpercent, family = binomial)

# extract potential percentages from the corpus 
potentialpercent <- str_extract_all(corpus,percentPatternsCollapsed)

potentialpercent <- as.vector(potentialpercent[[1]])
potentialpercent <- unique(potentialpercent)

# create data table of the potential percents and their features 
percent1RegularExpressions <- vector()
for (i in 1:nrow(potentialpercent)) {percent1RegularExpressions[i] <- grepl(percentPatterns[1],as.String(potentialpercent[i,1]))}
df.potentialpercent <- data.frame(percent1RegularExpressions)
for (i in 1:nrow(potentialpercent)) {percent1RegularExpressions[i] <- grepl(percentPatterns[2],as.String(potentialpercent[i,1]))}
df.potentialpercent <- data.frame(df.potentialpercent,percent1RegularExpressions)
for (i in 1:nrow(potentialpercent)) {percent1RegularExpressions[i] <- grepl(percentPatterns[3],as.String(potentialpercent[i,1]))}
df.potentialpercent <- data.frame(df.potentialpercent,percent1RegularExpressions)
for (i in 1:nrow(potentialpercentage)) {percent1RegularExpressions[i] <- grepl(percentPatterns[4],as.String(potentialpercent[i,1]))}
df.potentialpercent <- data.frame(df.potentialpercent,percent1RegularExpressions)
for (i in 1:nrow(potentialpercent)) {percent1RegularExpressions[i] <- grepl(percentPatterns[5],as.String(potentialpercent[i,1]))}
df.potentialpercent <- data.frame(df.potentialpercent,percent1RegularExpressions)
df.potentialpercent <- lapply(df.potentialpercent,as.numeric)
df.potentialpercent <- data.frame(df.potentialpercent)
names(df.potentialpercent) <- percentFeatures

# predict whether it is a real percentage or not using the logistic model 
percent.fit <- predict(percentmodel,df.potentialpercent,type = "response")
percent.predict <- rep("Not",99831)
percent.predict[oercent.fit > .40] <- "Percent" 

df.potentialpercent$prediction <-percent.predict
PERCENTAGES <- df.potentialpercent[!df.potentialpercent$prediction == "Not",]


## write out csv files 

write.csv(CEOS,file = "CEOS.csv")
write.csv(COMPANIES, file = "Companies.csv")
write.csv(PERCENTAGES,file = "Percentages.csv")


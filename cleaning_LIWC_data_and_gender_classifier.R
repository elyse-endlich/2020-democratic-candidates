### Loading LIWC into R, grepping candidates, calculating months in office (MIO), building models to predict MIO and sex and comparing to Shipan's ratio. Also includes code for rebalancing data set to be larger and then remaking the models. No significant improvement through rebalancing. ###

# load LIWC data ----------------------------------------------------------

LIWC <- read.csv(file = "./LIWC/LIWC2015 Results.csv", header = T)

## making columns for candidate, month, and year ---------------------------

LIWC$CANDIDATE <- gsub("_.*", "", LIWC$Filename)

LIWC$YEAR <- gsub(".*([0-9]{4}).*", "\\1", LIWC$Filename)

LIWC$MONTH <- gsub(".*_(\\w+)_MERGE.TXT", "\\1", LIWC$Filename)



## year candidate entered politics -----------------------------------------

LIWC$FIRST_YEAR <- LIWC$CANDIDATE

LIWC$FIRST_YEAR <- gsub("Bennet", "2009", LIWC$FIRST_YEAR)
LIWC$FIRST_YEAR <- gsub("Biden", "1973", LIWC$FIRST_YEAR)
LIWC$FIRST_YEAR <- gsub("Booker", "2013", LIWC$FIRST_YEAR)
LIWC$FIRST_YEAR <- gsub("Gabbard", "2013", LIWC$FIRST_YEAR)
LIWC$FIRST_YEAR <- gsub("Harris", "2017", LIWC$FIRST_YEAR)
LIWC$FIRST_YEAR <- gsub("Klobuchar", "2007", LIWC$FIRST_YEAR)
LIWC$FIRST_YEAR <- gsub("Sanders", "1991", LIWC$FIRST_YEAR)
LIWC$FIRST_YEAR <- gsub("Trump", "2017", LIWC$FIRST_YEAR)
LIWC$FIRST_YEAR <- gsub("Warren", "2013", LIWC$FIRST_YEAR)

## calculating years in office -----------------------------------------

LIWC$YEARS_IN_OFFICE <- "NA"
LIWC$YEARS_IN_OFFICE <- as.numeric(LIWC$YEAR) - as.numeric(LIWC$FIRST_YEAR)

## calculating months in office by year and month of article -----------------------------------------

LIWC$LINEAR_MONTH <- LIWC$MONTH
LIWC$LINEAR_MONTH <- gsub("January", "0", LIWC$LINEAR_MONTH)
LIWC$LINEAR_MONTH <- gsub("February", "1", LIWC$LINEAR_MONTH)
LIWC$LINEAR_MONTH <- gsub("March", "2", LIWC$LINEAR_MONTH)
LIWC$LINEAR_MONTH <- gsub("April", "3", LIWC$LINEAR_MONTH)
LIWC$LINEAR_MONTH <- gsub("May", "4", LIWC$LINEAR_MONTH)
LIWC$LINEAR_MONTH <- gsub("June", "5", LIWC$LINEAR_MONTH)
LIWC$LINEAR_MONTH <- gsub("July", "6", LIWC$LINEAR_MONTH)
LIWC$LINEAR_MONTH <- gsub("August", "7", LIWC$LINEAR_MONTH)
LIWC$LINEAR_MONTH <- gsub("September", "8", LIWC$LINEAR_MONTH)
LIWC$LINEAR_MONTH <- gsub("October", "9", LIWC$LINEAR_MONTH)
LIWC$LINEAR_MONTH <- gsub("November", "10", LIWC$LINEAR_MONTH)
LIWC$LINEAR_MONTH <- gsub("December", "11", LIWC$LINEAR_MONTH)

LIWC$MONTHS_IN_OFFICE <- (as.numeric(LIWC$YEARS_IN_OFFICE) * 12) + (as.numeric(LIWC$LINEAR_MONTH))

## age -----------------------------------------

LIWC$BIRTH_YEAR <- LIWC$CANDIDATE
LIWC$BIRTH_YEAR <- gsub("Bennet", "1964", LIWC$BIRTH_YEAR)
LIWC$BIRTH_YEAR <- gsub("Biden", "1942", LIWC$BIRTH_YEAR)
LIWC$BIRTH_YEAR <- gsub("Booker", "1969", LIWC$BIRTH_YEAR)
LIWC$BIRTH_YEAR <- gsub("Gabbard", "1981", LIWC$BIRTH_YEAR)
LIWC$BIRTH_YEAR <- gsub("Harris", "1964", LIWC$BIRTH_YEAR)
LIWC$BIRTH_YEAR <- gsub("Klobuchar", "1960", LIWC$BIRTH_YEAR)
LIWC$BIRTH_YEAR <- gsub("Sanders", "1941", LIWC$BIRTH_YEAR)
LIWC$BIRTH_YEAR <- gsub("Trump", "1946", LIWC$BIRTH_YEAR)
LIWC$BIRTH_YEAR <- gsub("Warren", "1949", LIWC$BIRTH_YEAR)

LIWC$AGE <- as.numeric(LIWC$YEAR) - as.numeric(LIWC$BIRTH_YEAR)

## gender -----------------------------------------

LIWC$SEX <- LIWC$CANDIDATE
LIWC$SEX <- gsub("Bennet", "MALE", LIWC$SEX)
LIWC$SEX <- gsub("Biden", "MALE", LIWC$SEX)
LIWC$SEX <- gsub("Booker", "MALE", LIWC$SEX)
LIWC$SEX <- gsub("Gabbard", "FEMALE", LIWC$SEX)
LIWC$SEX <- gsub("Harris", "FEMALE", LIWC$SEX)
LIWC$SEX <- gsub("Klobuchar", "FEMALE", LIWC$SEX)
LIWC$SEX <- gsub("Sanders", "MALE", LIWC$SEX)
LIWC$SEX <- gsub("Trump", "MALE", LIWC$SEX)
LIWC$SEX <- gsub("Warren", "FEMALE", LIWC$SEX)

## race -----------------------------------------

LIWC$RACE <- LIWC$CANDIDATE

LIWC$RACE <- gsub("Bennet", "WHITE", LIWC$RACE)
LIWC$RACE <- gsub("Biden", "WHITE", LIWC$RACE)
LIWC$RACE <- gsub("Booker", "BLACK", LIWC$RACE)
LIWC$RACE <- gsub("Gabbard", "OTHER", LIWC$RACE)
LIWC$RACE <- gsub("Harris", "BLACK", LIWC$RACE)
LIWC$RACE <- gsub("Klobuchar", "WHITE", LIWC$RACE)
LIWC$RACE <- gsub("Sanders", "WHITE", LIWC$RACE)
LIWC$RACE <- gsub("Trump", "WHITE", LIWC$RACE)
LIWC$RACE <- gsub("Warren", "WHITE", LIWC$RACE)

# rebalance data set  -----------------------------------------

# make.sample <- function(df, n) {
#   df.new <- data.frame()
#   half.n <- round(n/2,0)
#   for(current.sex in c("MALE","FEMALE")) {
#     d.sub <- subset(df, SEX==current.sex)
#     d.sub.n <- nrow(d.sub)
#     if(half.n < d.sub.n) {
#       d.sub <- d.sub[sample(1:d.sub.n, half.n, replace=FALSE),]
#     } else if(half.n > d.sub.n) {
#       d.sub <- rbind(d.sub, d.sub[sample(1:d.sub.n, half.n - d.sub.n, replace=TRUE),])
#     }
#     df.new <- rbind(df.new, d.sub)
#   }
#   return(df.new)
# }
#
# LIWC.2 <- make.sample(LIWC, 2000)
# table(LIWC.2$CANDIDATE) / table(LIWC$CANDIDATE) # how many times each candidate is sampled in new set


# rebalance data set by candidate -----------------------------------------

make.sample.candidates <- function(df, n) {
  df.new <- data.frame()
  half.n <- round(n / 2, 0)
  for (current.sex in c("MALE", "FEMALE")) {
    d.sub <- subset(df, SEX == current.sex)
    d.sub.names <- names(table(d.sub$CANDIDATE))
    new.names <- sample(d.sub.names, half.n, replace = TRUE)
    for (name in new.names) {
      df.new <- rbind(df.new, subset(d.sub, CANDIDATE == name))
    }
  }
  return(df.new)
}

LIWC.2 <- make.sample.candidates(LIWC, 200)
table(LIWC.2$CANDIDATE) / table(LIWC$CANDIDATE) # how many times each candidate is sampled in new set


## from here on, if anything doesn't have ".2" at the end, that means it was modeled off a smaller data set. I included all the data in LIWC.2 just for ease of comparison, but if it doesn't have .2, that means it was modeled off LIWC, not LIWC.2 ##

# Shipan's metric -----------------------------------------

LIWC.2[, "FEMININE"] <- "NA"
LIWC.2[, "MASCULINE"] <- "NA"
feminine_function.2 <- function(x) {
  sum(LIWC.2$pronoun[x], LIWC.2$ipron[x], LIWC.2$auxverb[x], LIWC.2$verb[x], LIWC.2$posemo[x], LIWC.2$negemo[x], LIWC.2$social[x], LIWC.2$cogproc[x], LIWC.2$tentat[x])
}
masculine_function.2 <- function(x) {
  sum(LIWC.2$Sixltr[x], LIWC.2$we[x], LIWC.2$article[x], LIWC.2$prep[x], LIWC.2$anger[x], LIWC.2$swear[x])
}


LIWC.2$FEMININE.2 <- sapply(1:nrow(LIWC.2), feminine_function.2)
LIWC.2$MASCULINE.2 <- sapply(1:nrow(LIWC.2), masculine_function.2)

## ratio of male to female speech  -----------------------------------------

LIWC.2$RATIO.2 <- LIWC.2$FEMININE.2 / LIWC.2$MASCULINE.2


# machine learning model  -----------------------------------------

# dat.model <- lm(formula = as.formula(paste(colnames(LIWC)[102], "~", paste(colnames(LIWC)[c(4:83)], collapse = "+"), sep = "")), data = LIWC)
# step(dat.model, direction = "both")

## and with rebalanced set  -----------------------------------------

# dat.model.2 <- lm(formula = as.formula(paste(colnames(LIWC.2)[102], "~", paste(colnames(LIWC.2)[c(4:83)], collapse = "+"), sep = "")), data = LIWC.2)
# step(dat.model.2, direction = "both")

my.model.MIO <- lm(formula = MONTHS_IN_OFFICE ~ Analytic + Authentic + Tone +
  WPS + Dic + ppron + i + we + you + they + ipron + article +
  prep + auxverb + adverb + conj + negate + interrog + quant +
  affect + posemo + social + family + friend + female + male +
  insight + cause + discrep + differ + percept + see + feel +
  bio + sexual + affiliation + focusfuture + leisure + relig +
  death + informal + netspeak + assent + drives, data = LIWC.2) # so this model was trained on a smaller data set and is now being applied to the bigger data set

my.model2.MIO <- lm(formula = MONTHS_IN_OFFICE ~ Analytic + Authentic + Tone +
  WPS + Dic + ppron + i + we + you + they + ipron + article +
  prep + auxverb + adverb + conj + negate + interrog + quant +
  affect + posemo + social + family + friend + female + male +
  insight + cause + discrep + differ + percept + see + feel +
  bio + sexual + affiliation + focusfuture + leisure + relig +
  death + informal + netspeak + assent + drives + AGE + SEX + RACE, data = LIWC.2) # same model just including age, sex, and race

# shipan's model -----------------------------------------

shipans.model.MIO.2 <- lm(MONTHS_IN_OFFICE ~ RATIO.2, data = LIWC.2)

shipans.model2.MIO.2 <- lm(MONTHS_IN_OFFICE ~ RATIO.2 + AGE + SEX + RACE, data = LIWC.2)

my.model.MIO.2 <- lm(formula = MONTHS_IN_OFFICE ~ Analytic + Clout + Authentic +
  Tone + WPS + Sixltr + Dic + pronoun + ppron + i + we + you +
  they + ipron + article + prep + auxverb + adverb + conj +
  negate + verb + adj + compare + interrog + number + quant +
  affect + posemo + negemo + anx + anger + sad + social + family +
  friend + female + male + cogproc + insight + cause + discrep +
  tentat + certain + differ + percept + see + hear + feel +
  bio + health + sexual + ingest + drives + affiliation + achieve +
  power + reward + risk + focusfuture + relativ + space + time +
  leisure + home + relig + death + informal + netspeak + assent +
  nonflu, data = LIWC.2)


my.model2.MIO.2 <- lm(formula = MONTHS_IN_OFFICE ~ Analytic + Clout + Authentic +
  Tone + WPS + Sixltr + Dic + pronoun + ppron + i + we + you +
  they + ipron + article + prep + auxverb + adverb + conj +
  negate + verb + adj + compare + interrog + number + quant +
  affect + posemo + negemo + anx + anger + sad + social + family +
  friend + female + male + cogproc + insight + cause + discrep +
  tentat + certain + differ + percept + see + hear + feel +
  bio + health + sexual + ingest + drives + affiliation + achieve +
  power + reward + risk + focusfuture + relativ + space + time +
  leisure + home + relig + death + informal + netspeak + assent +
  nonflu + AGE + SEX + RACE, data = LIWC.2)




LIWC.2$predict.MIO <- predict(my.model.MIO, LIWC.2)
LIWC.2$predict2.MIO <- predict(my.model2.MIO, LIWC.2)

LIWC.2$predict.ship.MIO.2 <- predict(shipans.model.MIO.2, LIWC.2)
LIWC.2$predict.ship2.MIO.2 <- predict(shipans.model2.MIO.2, LIWC.2)

LIWC.2$predict.MIO.2 <- predict(my.model.MIO.2, LIWC.2)
LIWC.2$predict2.MIO.2 <- predict(my.model2.MIO.2, LIWC.2)

anova(my.model.MIO, my.model2.MIO) # adding age sex and race improves the model
anova(shipans.model.MIO.2, shipans.model2.MIO.2) # same for shipan
anova(my.model.MIO, shipans.model.MIO.2) # my model is a significant improvement
anova(my.model2.MIO, shipans.model2.MIO.2) # STILL an improvement after including age sex and race
anova(my.model.MIO, my.model.MIO.2) # larger training set is an improvement
anova(my.model2.MIO, my.model2.MIO.2) # still an improvement after age sex and race

# build a better gender classifier? -----------------------------------------

male.model.ship.2 <- lm(SEX == "MALE" ~ RATIO.2, data = LIWC.2)
female.model.ship.2 <- lm(SEX == "FEMALE" ~ RATIO.2, data = LIWC.2)

## male classifier using old data  -----------------------------------------

# dat.model2 <- lm(formula = as.formula(paste('SEX == "MALE" ~ ', paste(colnames(LIWC)[c(4:83)], collapse = "+"), sep = "")), data = LIWC)
# step(dat.model2, direction = "both")

## now with rebalanced data set -----------------------------------------

# dat.model2.2 <- lm(formula = as.formula(paste('SEX == "MALE" ~ ', paste(colnames(LIWC.2)[c(4:83)], collapse = "+"), sep = "")), data = LIWC.2)
# step(dat.model2.2, direction = "both")

##

my.male.model <- lm(formula = SEX == "MALE" ~ Clout + Authentic + Tone + Sixltr +
  function. + pronoun + ppron + you + shehe + they + prep +
  adverb + conj + number + quant + posemo + negemo + anger +
  family + friend + female + insight + tentat + certain + differ +
  percept + feel + bio + body + health + ingest + drives +
  affiliation + achieve + power + reward + risk + work + leisure +
  home + relig + informal + swear + netspeak + assent + nonflu +
  filler, data = LIWC.2)

my.male.model.2 <- lm(formula = SEX == "MALE" ~ Clout + Authentic + Tone + WPS +
  Sixltr + function. + pronoun + we + you + shehe + they +
  ipron + article + prep + adverb + conj + verb + adj + interrog +
  number + quant + affect + posemo + negemo + anx + anger +
  sad + social + family + friend + female + male + cogproc +
  insight + cause + discrep + tentat + certain + differ + percept +
  hear + feel + bio + body + health + sexual + ingest + drives +
  affiliation + achieve + power + reward + risk + focuspast +
  focuspresent + focusfuture + relativ + motion + space + time +
  work + leisure + home + money + relig + informal + swear +
  netspeak + assent + nonflu + filler, data = LIWC.2)


## now female using old data set  -----------------------------------------

# dat.model3 <- lm(formula = as.formula(paste('SEX == "FEMALE" ~ ', paste(colnames(LIWC)[c(4:83)], collapse = "+"), sep = "")), data = LIWC)
# step(dat.model3, direction = "both")

## and rebalanced  -----------------------------------------

# dat.model3.2 <- lm(formula = as.formula(paste('SEX == "FEMALE" ~ ', paste(colnames(LIWC.2)[c(4:83)], collapse = "+"), sep = "")), data = LIWC.2)
# step(dat.model3.2, direction = "both")

my.female.model <- lm(formula = SEX == "FEMALE" ~ Clout + Authentic + Tone + Sixltr +
  function. + pronoun + ppron + you + shehe + they + prep +
  adverb + conj + number + quant + posemo + negemo + anger +
  family + friend + female + insight + tentat + certain + differ +
  percept + feel + bio + body + health + ingest + drives +
  affiliation + achieve + power + reward + risk + work + leisure +
  home + relig + informal + swear + netspeak + assent + nonflu +
  filler, data = LIWC.2)

my.female.model.2 <- lm(formula = SEX == "FEMALE" ~ Clout + Authentic + Tone + WPS +
  Sixltr + function. + pronoun + we + you + shehe + they +
  ipron + article + prep + adverb + conj + verb + adj + interrog +
  number + quant + affect + posemo + negemo + anx + anger +
  sad + social + family + friend + female + male + cogproc +
  insight + cause + discrep + tentat + certain + differ + percept +
  hear + feel + bio + body + health + sexual + ingest + drives +
  affiliation + achieve + power + reward + risk + focuspast +
  focuspresent + focusfuture + relativ + motion + space + time +
  work + leisure + home + money + relig + informal + swear +
  netspeak + assent + nonflu + filler, data = LIWC.2)


LIWC.2$predict.MALE <- predict(my.male.model, LIWC.2)
LIWC.2$predict.FEMALE <- predict(my.female.model, LIWC.2)
LIWC.2$predict.RESULT <- "NA"

LIWC.2$predict.ship.MALE <- predict(male.model.ship.2, LIWC.2)
LIWC.2$predict.ship.FEMALE <- predict(female.model.ship.2, LIWC.2)
LIWC.2$predict.ship.RESULT <- "NA"

LIWC.2$predict.MALE.2 <- predict(my.male.model.2, LIWC.2)
LIWC.2$predict.FEMALE.2 <- predict(my.female.model.2, LIWC.2)
LIWC.2$predict.RESULT.2 <- "NA"

## predict sex by prediction score -----------------------------------------

for (row in 1:nrow(LIWC.2)) {
  if ((as.numeric(LIWC.2$predict.MALE[row]) > as.numeric(LIWC.2$predict.FEMALE[row]))) {
    LIWC.2$predict.RESULT[row] <- "MALE"
  }
  else {
    LIWC.2$predict.RESULT[row] <- "FEMALE"
  }
}

for (row in 1:nrow(LIWC.2)) {
  if ((as.numeric(LIWC.2$predict.MALE.2[row]) > as.numeric(LIWC.2$predict.FEMALE.2[row]))) {
    LIWC.2$predict.RESULT.2[row] <- "MALE"
  }
  else {
    LIWC.2$predict.RESULT.2[row] <- "FEMALE"
  }
}

for (row in 1:nrow(LIWC.2)) {
  if ((as.numeric(LIWC.2$predict.ship.MALE[row]) > as.numeric(LIWC.2$predict.ship.FEMALE[row]))) {
    LIWC.2$predict.ship.RESULT[row] <- "MALE"
  }
  else {
    LIWC.2$predict.ship.RESULT[row] <- "FEMALE"
  }
}

LIWC.2$predict.RESULT.SCORE <- "NA"

## give score based on if prediction is correct -----------------------------------------

for (row in 1:nrow(LIWC.2)) {
  if ((LIWC.2$predict.RESULT[row] == LIWC.2$SEX[row])) {
    LIWC.2$predict.RESULT.SCORE[row] <- "1"
  }
  else {
    LIWC.2$predict.RESULT.SCORE[row] <- "0"
  }
}

LIWC.2$predict.RESULT.SCORE.2 <- "NA"

for (row in 1:nrow(LIWC.2)) {
  if ((LIWC.2$predict.RESULT.2[row] == LIWC.2$SEX[row])) {
    LIWC.2$predict.RESULT.SCORE.2[row] <- "1"
  }
  else {
    LIWC.2$predict.RESULT.SCORE.2[row] <- "0"
  }
}

LIWC.2$predict.ship.RESULT.SCORE <- "NA"

for (row in 1:nrow(LIWC.2)) {
  if ((LIWC.2$predict.ship.RESULT[row] == LIWC.2$SEX[row])) {
    LIWC.2$predict.ship.RESULT.SCORE[row] <- "1"
  }
  else {
    LIWC.2$predict.ship.RESULT.SCORE[row] <- "0"
  }
}

## how often is each prediction right? -----------------------------------------

sum(as.numeric(LIWC.2$predict.RESULT.SCORE))
sum(as.numeric(LIWC.2$predict.RESULT.SCORE.2))
sum(as.numeric(LIWC.2$predict.ship.RESULT.SCORE))
# nrow(LIWC)
sum((as.numeric(LIWC.2$predict.RESULT.SCORE)) / (nrow(LIWC.2))) # old model is right 96.95% of the time
sum((as.numeric(LIWC.2$predict.ship.RESULT.SCORE)) / (nrow(LIWC.2))) # the shipan model is right less than 50% of the time
sum((as.numeric(LIWC.2$predict.RESULT.SCORE.2)) / (nrow(LIWC.2))) # new model trained on big data set improves accuracy by a fractional amount
#
#
anova(my.male.model, my.male.model.2) # significant improvement
anova(my.female.model, my.female.model.2) # same


## what is the percentage of male vs female segments per speaker  -----------------------------------------

LIWC.2$CANDIDATE[LIWC.2$SEX != LIWC.2$predict.RESULT.2] # where is it wrong
table(LIWC.2$CANDIDATE[LIWC.2$SEX != LIWC.2$predict.RESULT.2])
# mainly bennet, sanders, booker, and klobuchar - interesting
# test 2 - bennet 69, booker 40, klobuchar 48, sanders 48
# test 3:
# Bennet    Booker Klobuchar   Sanders
# 50        25        36        44
# test 4
# Bennet    Booker Klobuchar   Sanders
# 84        36        36        44

table(LIWC.2$Filename[LIWC.2$SEX != LIWC.2$predict.RESULT.2]) # which files are throwing it off and are they the same each time?
# test 1
# Bennet_Just_Facts_2009_November_MERGE.TXT
# 21
# Bennet_Just_Facts_2010_December_MERGE.TXT
# 21
# Bennet_Just_Facts_2019_April_MERGE.TXT
# 21
# Bennet_Just_Facts_2019_August_MERGE.TXT
# 21
# Booker_Just_Facts_2018_April_MERGE.TXT
# 18
# Booker_Just_Facts_2018_September_MERGE.TXT
# 18
# Klobuchar_Just_Facts_2009_February_MERGE.TXT
# 18
# Klobuchar_Just_Facts_2019_December_MERGE.TXT
# 18
# Sanders_Just_Facts_2014_October_MERGE.TXT
# 22
# Sanders_Just_Facts_2015_November_MERGE.TXT
# 22

# test 2
#
# Bennet_Just_Facts_2009_November_MERGE.TXT
# 17
# Bennet_Just_Facts_2010_December_MERGE.TXT
# 17
# Bennet_Just_Facts_2014_October_MERGE.TXT
# 17
# Bennet_Just_Facts_2019_April_MERGE.TXT
# 17
# Bennet_Just_Facts_2019_August_MERGE.TXT
# 17
# Booker_Just_Facts_2018_September_MERGE.TXT
# 22
# Klobuchar_Just_Facts_2009_February_MERGE.TXT
# 21
# Klobuchar_Just_Facts_2019_December_MERGE.TXT
# 21
# Sanders_Just_Facts_2014_October_MERGE.TXT
# 18
# Sanders_Just_Facts_2015_November_MERGE.TXT
# 18

## how well did it do vs a baseline guess  -----------------------------------------

LIWC.2$predict.baseline.RESULT <- "MALE"
LIWC.2$predict.baseline.RESULT.SCORE <- "NA"

for (row in 1:nrow(LIWC.2)) {
  if ((LIWC.2$predict.baseline.RESULT[row] == LIWC.2$SEX[row])) {
    LIWC.2$predict.baseline.RESULT.SCORE[row] <- "1"
  }
  else {
    LIWC.2$predict.baseline.RESULT.SCORE[row] <- "0"
  }
}

sum(as.numeric(LIWC.2$predict.baseline.RESULT.SCORE))
sum((as.numeric(LIWC.2$predict.baseline.RESULT.SCORE)) / (nrow(LIWC.2))) # right about 63% of the time

## visualize, visualize, visualize! ##
## always plot before you make a regression ##

# boxplot(LIWC$predict.ship.MIO.2-LIWC$MONTHS_IN_OFFICE)
#
# with(LIWC, boxplot(predict.old.MIO-MONTHS_IN_OFFICE, predict.MIO-MONTHS_IN_OFFICE))
#
# with(LIWC, boxplot(predict.old2.MIO-MONTHS_IN_OFFICE, predict2.MIO-MONTHS_IN_OFFICE))


plot(density(LIWC.2$predict.MIO - LIWC.2$MONTHS_IN_OFFICE))
plot(density(LIWC.2$predict2.MIO - LIWC.2$MONTHS_IN_OFFICE))

plot(density(LIWC.2$predict.ship.MIO.2 - LIWC.2$MONTHS_IN_OFFICE))
plot(density(LIWC.2$predict.ship2.MIO.2 - LIWC.2$MONTHS_IN_OFFICE))

plot(density(LIWC.2$predict.MIO.2 - LIWC.2$MONTHS_IN_OFFICE))
plot(density(LIWC.2$predict2.MIO.2 - LIWC.2$MONTHS_IN_OFFICE))
#
# with(LIWC.2, mean(abs(predict.MIO-MONTHS_IN_OFFICE)))
#
# with(LIWC, mean(abs(predict.MIO-MONTHS_IN_OFFICE)))
#
with(LIWC.2, t.test(abs(predict.MIO.2 - MONTHS_IN_OFFICE), abs(predict.MIO - MONTHS_IN_OFFICE)))

# summary(dat.model3)
#
with(LIWC.2, plot(predict.MIO, MONTHS_IN_OFFICE))
abline(0, 1)
with(LIWC.2, plot(predict2.MIO, MONTHS_IN_OFFICE))
abline(0, 1)

with(LIWC.2, plot(predict.ship.MIO.2, MONTHS_IN_OFFICE))
abline(0, 1)
with(LIWC.2, plot(predict.ship2.MIO.2, MONTHS_IN_OFFICE))
abline(0, 1)

with(LIWC.2, plot(predict.MIO.2, MONTHS_IN_OFFICE))
abline(0, 1)
with(LIWC.2, plot(predict2.MIO.2, MONTHS_IN_OFFICE))
abline(0, 1)

with(subset(LIWC.2, CANDIDATE == "Bennet"), table(predict.RESULT.2, SEX))

summary(LIWC.2$predict.RESULT.2)

# Klobuchar = wrong 34 out of 1105
# Sanders = 38 out of 2090
# Bennet = wrong 85 out of 272
# Booker = wrong 26 out of 598

# try 10 times

# if there's consistency, why? months? factors?

# residualize data


## take formula built on HPC and apply it to JustFacts ##


# take time now to write down what I've done
# outline, bulleted, capture project knowledge





# ## making a new ratio?? ## ignore this, it's under construction
#
# LIWC.2$MY.RATIO <- LIWC.2$predict.FEMALE/LIWC.2$predict.MALE
# LIWC.2$MY.RATIO.2 <- LIWC.2$predict.FEMALE.2/LIWC.2$predict.MALE.2
#
# my.model.MIO.SEX <- lm(MONTHS_IN_OFFICE ~ MY.RATIO, data = LIWC.2)
# my.model2.MIO.SEX <- lm(MONTHS_IN_OFFICE ~ MY.RATIO + AGE + SEX + RACE, data = LIWC.2)
#
# my.model.MIO.SEX.2 <- lm(MONTHS_IN_OFFICE ~ MY.RATIO.2, data = LIWC.2)
# my.model2.MIO.SEX.2 <- lm(MONTHS_IN_OFFICE ~ MY.RATIO.2 + AGE + SEX + RACE, data = LIWC.2)
#
#
# shipans.model.MIO <- lm(MONTHS_IN_OFFICE ~ RATIO.2, data = LIWC.2)
#
# shipans.model2.MIO <- lm(MONTHS_IN_OFFICE ~ RATIO.2 + AGE + SEX + RACE, data = LIWC.2)
#
#
# anova(my.model.MIO.SEX, my.model.MIO.SEX.2) # doesn't seem to be much improvement

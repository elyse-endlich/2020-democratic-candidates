# First shot at making an R script that can take the CSV files created by using the Web Scraper extension on the Just Facts website and parse them to remove all speakers except the speaker of interest and organize the data by date. #

setwd("~/Summer Research")

# read in CSVs # -----------------------------------------

warren.justfacts.interviews <- read.csv(file = "warren_interviews_justfacts_earlier.csv", header = T)
biden.justfacts.interviews <- read.csv(file = "biden_interviews_justfacts_earlier (6).csv", header = T)
sanders.justfacts.interviews <- read.csv(file = "sanders_interviews_justfacts_even_earlier (6).csv", header = T)
klobuchar.justfacts.interviews <- read.csv(file = "klobuchar_interviews_justfacts_earlier (1).csv", header = T)
gabbard.justfacts.interviews <- read.csv(file = "gabbard_interviews_justfacts_earlier.csv", header = T)
booker.justfacts.interviews <- read.csv(file = "booker_justfacts_interviews_earlier.csv", header = T)
bennet.justfacts.interviews <- read.csv(file = "bennet_interviews_justfacts_earlier.csv", header = T)
harris.justfacts.interviews <- read.csv(file = "harris_interviews_justfacts_earlier (1).csv", header = T)
trump.justfacts.interviews <- read.csv(file = "trump_interviews_justfacts_earlier (1).csv", header = T)
trump.remarks.gov <- read.csv(file = "trump_remarks_gov.csv", header = T)

## get rid of useless columns # -----------------------------------------

byebyecolumns <- function(x) {
  x[, c("link", "text", "date")]
}
warren.justfacts.interviews <- byebyecolumns(warren.justfacts.interviews)
sanders.justfacts.interviews <- byebyecolumns(sanders.justfacts.interviews)
biden.justfacts.interviews <- byebyecolumns(biden.justfacts.interviews)
gabbard.justfacts.interviews <- byebyecolumns(gabbard.justfacts.interviews)
klobuchar.justfacts.interviews <- byebyecolumns(klobuchar.justfacts.interviews)
booker.justfacts.interviews <- byebyecolumns(booker.justfacts.interviews)
bennet.justfacts.interviews <- byebyecolumns(bennet.justfacts.interviews)
harris.justfacts.interviews <- byebyecolumns(harris.justfacts.interviews)
trump.justfacts.interviews <- byebyecolumns(trump.justfacts.interviews)

# turn articles into separate txt files # -----------------------------------------

for (i in 1:nrow(warren.justfacts.interviews)) {
  write.table(warren.justfacts.interviews$text[i], file = (paste0("Warren, Just Facts, ", warren.justfacts.interviews$date[i], ".txt")))
}

for (i in 1:nrow(biden.justfacts.interviews)) {
  write.table(biden.justfacts.interviews$text[i], file = (paste0("Biden, Just Facts, ", biden.justfacts.interviews$date[i], ".txt")))
}

for (i in 1:nrow(sanders.justfacts.interviews)) {
  write.table(sanders.justfacts.interviews$text[i], file = (paste0("Sanders, Just Facts, ", sanders.justfacts.interviews$date[i], ".txt")))
}

for (i in 1:nrow(klobuchar.justfacts.interviews)) {
  write.table(klobuchar.justfacts.interviews$text[i], file = (paste0("Klobuchar, Just Facts, ", klobuchar.justfacts.interviews$date[i], ".txt")))
}

for (i in 1:nrow(gabbard.justfacts.interviews)) {
  write.table(gabbard.justfacts.interviews$text[i], file = (paste0("Gabbard, Just Facts, ", gabbard.justfacts.interviews$date[i], ".txt")))
}

for (i in 1:nrow(booker.justfacts.interviews)) {
  write.table(booker.justfacts.interviews$text[i], file = (paste0("Booker, Just Facts, ", booker.justfacts.interviews$date[i], ".txt")))
}

for (i in 1:nrow(bennet.justfacts.interviews)) {
  write.table(bennet.justfacts.interviews$text[i], file = (paste0("Bennet, Just Facts, ", bennet.justfacts.interviews$date[i], ".txt")))
}

for (i in 1:nrow(harris.justfacts.interviews)) {
  write.table(harris.justfacts.interviews$text[i], file = (paste0("Harris, Just Facts, ", harris.justfacts.interviews$date[i], ".txt")))
}

for (i in 1:nrow(trump.justfacts.interviews)) {
  write.table(trump.justfacts.interviews$text[i], file = (paste0("Trump, Just Facts, ", trump.justfacts.interviews$date[i], ".txt")))
}

# move files into subfolders # -----------------------------------------

WarrenFiles <- list.files(pattern = "Warren, Just Facts, .*\\.txt$")
BidenFiles <- list.files(pattern = "Biden, Just Facts, .*\\.txt$")
SandersFiles <- list.files(pattern = "Sanders, Just Facts, .*\\.txt$")
KlobucharFiles <- list.files(pattern = "Klobuchar, Just Facts, .*\\.txt$")
GabbardFiles <- list.files(pattern = "Gabbard, Just Facts, .*\\.txt$")
BookerFiles <- list.files(pattern = "Booker, Just Facts, .*\\.txt$")
BennetFiles <- list.files(pattern = "Bennet, Just Facts, .*\\.txt$")
HarrisFiles <- list.files(pattern = "Harris, Just Facts, .*\\.txt$")
TrumpFiles <- list.files(pattern = "Trump, Just Facts, .*\\.txt$")

library(filesstrings)
dir.create("Warren")
file.move(WarrenFiles, "Warren", overwrite = T)
dir.create("Biden")
file.move(BidenFiles, "Biden", overwrite = T)
dir.create("Sanders")
file.move(SandersFiles, "Sanders", overwrite = T)
dir.create("Klobuchar")
file.move(KlobucharFiles, "Klobuchar", overwrite = T)
dir.create("Gabbard")
file.move(GabbardFiles, "Gabbard", overwrite = T)
dir.create("Booker")
file.move(BookerFiles, "Booker", overwrite = T)
dir.create("Bennet")
file.move(BennetFiles, "Bennet", overwrite = T)
dir.create("Harris")
file.move(HarrisFiles, "Harris", overwrite = T)
dir.create("Trump")
file.move(TrumpFiles, "Trump", overwrite = T)


# END OF SEPARATING FILES- now onto parsing the files! -----------------------------------------

# read in CSVs # -----------------------------------------

warren.transcripts.msnbc <- read.csv(file = "warren_transcripts_msnbc.csv", header = T)
biden.transcripts.msnbc <- read.csv(file = "biden_transcripts_msnbc.csv", header = T)
sanders.transcripts.msnbc <- read.csv(file = "sanders_transcripts_msnbc.csv", header = T)
klobuchar.transcripts.msnbc <- read.csv(file = "klobuchar_transcripts_msnbc.csv", header = T)
gabbard.transcripts.msnbc <- read.csv(file = "gabbard_transcripts_msnbc.csv", header = T)
booker.transcripts.msnbc <- read.csv(file = "booker_transcripts_msnbc.csv", header = T)
bennet.transcripts.msnbc <- read.csv(file = "bennet_transcripts_msnbc.csv", header = T)
harris.transcripts.msnbc <- read.csv(file = "harris_transcripts_msnbc.csv", header = T)

# get rid of useless columns # -----------------------------------------

byebyecolumns <- function(x) {
  x[, c("link", "transcript", "date", "guests")]
}
warren.transcripts.msnbc <- byebyecolumns(warren.transcripts.msnbc)
sanders.transcripts.msnbc <- byebyecolumns(sanders.transcripts.msnbc)
biden.transcripts.msnbc <- byebyecolumns(biden.transcripts.msnbc)
gabbard.transcripts.msnbc <- byebyecolumns(gabbard.transcripts.msnbc)
klobuchar.transcripts.msnbc <- byebyecolumns(klobuchar.transcripts.msnbc)
booker.transcripts.msnbc <- byebyecolumns(booker.transcripts.msnbc)
bennet.transcripts.msnbc <- byebyecolumns(bennet.transcripts.msnbc)
harris.transcripts.msnbc <- byebyecolumns(harris.transcripts.msnbc)

byebyecolumns <- function(x) {
  x[, c("title", "date", "transcript")]
}
trump.remarks.gov <- byebyecolumns(trump.remarks.gov)

# Now we need to remove data that doesn't have the candidates as guests -----------------------------------------

warren.transcripts.msnbc.subset <- warren.transcripts.msnbc[grep("Elizabeth Warren", warren.transcripts.msnbc$guests), ]
bennet.transcripts.msnbc.subset <- bennet.transcripts.msnbc[grep("Michael Bennet", bennet.transcripts.msnbc$guests), ]
biden.transcripts.msnbc.subset <- biden.transcripts.msnbc[grep("Joe Biden", biden.transcripts.msnbc$guests), ]
booker.transcripts.msnbc.subset <- booker.transcripts.msnbc[grep("Cory Booker", booker.transcripts.msnbc$guests), ]
gabbard.transcripts.msnbc.subset <- gabbard.transcripts.msnbc[grep("Tulsi Gabbard", gabbard.transcripts.msnbc$guests), ]
harris.transcripts.msnbc.subset <- harris.transcripts.msnbc[grep("Kamala Harris", harris.transcripts.msnbc$guests), ]
klobuchar.transcripts.msnbc.subset <- klobuchar.transcripts.msnbc[grep("Amy Klobuchar", klobuchar.transcripts.msnbc$guests), ]
sanders.transcripts.msnbc.subset <- sanders.transcripts.msnbc[grep("Bernie Sanders", sanders.transcripts.msnbc$guests), ]

trump.remarks.gov.subset <- trump.remarks.gov[grep("Trump", trump.remarks.gov$title), ]

warren.transcripts.msnbc.subset$date <- gsub("[[:punct:]]", "-", warren.transcripts.msnbc.subset$date)
bennet.transcripts.msnbc.subset$date <- gsub("[[:punct:]]", "-", bennet.transcripts.msnbc.subset$date)
biden.transcripts.msnbc.subset$date <- gsub("[[:punct:]]", "-", biden.transcripts.msnbc.subset$date)
booker.transcripts.msnbc.subset$date <- gsub("[[:punct:]]", "-", booker.transcripts.msnbc.subset$date)
gabbard.transcripts.msnbc.subset$date <- gsub("[[:punct:]]", "-", gabbard.transcripts.msnbc.subset$date)
harris.transcripts.msnbc.subset$date <- gsub("[[:punct:]]", "-", harris.transcripts.msnbc.subset$date)
klobuchar.transcripts.msnbc.subset$date <- gsub("[[:punct:]]", "-", klobuchar.transcripts.msnbc.subset$date)
sanders.transcripts.msnbc.subset$date <- gsub("[[:punct:]]", "-", sanders.transcripts.msnbc.subset$date)

# turn articles into separate txt files # -----------------------------------------

for (i in 1:nrow(warren.transcripts.msnbc.subset)) {
  write.table(warren.transcripts.msnbc.subset$transcript[i], file = (paste0("Warren, MSNBC, ", warren.transcripts.msnbc.subset$date[i], ".txt")))
}

for (i in 1:nrow(biden.transcripts.msnbc.subset)) {
  write.table(biden.transcripts.msnbc.subset$transcript[i], file = (paste0("Biden, MSNBC, ", biden.transcripts.msnbc.subset$date[i], ".txt")))
}

for (i in 1:nrow(sanders.transcripts.msnbc.subset)) {
  write.table(sanders.transcripts.msnbc.subset$transcript[i], file = (paste0("Sanders, MSNBC, ", sanders.transcripts.msnbc.subset$date[i], ".txt")))
}

for (i in 1:nrow(klobuchar.transcripts.msnbc.subset)) {
  write.table(klobuchar.transcripts.msnbc.subset$transcript[i], file = (paste0("Klobuchar, MSNBC, ", klobuchar.transcripts.msnbc.subset$date[i], ".txt")))
}

for (i in 1:nrow(gabbard.transcripts.msnbc.subset)) {
  write.table(gabbard.transcripts.msnbc.subset$transcript[i], file = (paste0("Gabbard, MSNBC, ", gabbard.transcripts.msnbc.subset$date[i], ".txt")))
}

for (i in 1:nrow(booker.transcripts.msnbc.subset)) {
  write.table(booker.transcripts.msnbc.subset$transcript[i], file = (paste0("Booker, MSNBC, ", booker.transcripts.msnbc.subset$date[i], ".txt")))
}

for (i in 1:nrow(bennet.transcripts.msnbc.subset)) {
  write.table(bennet.transcripts.msnbc.subset$transcript[i], file = (paste0("Bennet, MSNBC, ", bennet.transcripts.msnbc.subset$date[i], ".txt")))
}

for (i in 1:nrow(harris.transcripts.msnbc.subset)) {
  write.table(harris.transcripts.msnbc.subset$transcript[i], file = (paste0("Harris, MSNBC, ", harris.transcripts.msnbc.subset$date[i], ".txt")))
}

for (i in 1:nrow(trump.remarks.gov.subset)) {
  write.table(trump.remarks.gov.subset$transcript[i], file = (paste0("Trump, White House, ", trump.remarks.gov.subset$date[i], ".txt")))
}

# move files into subfolders # -----------------------------------------

WarrenFiles <- list.files(pattern = "Warren, MSNBC, .*\\.txt$")
BidenFiles <- list.files(pattern = "Biden, MSNBC, .*\\.txt$")
SandersFiles <- list.files(pattern = "Sanders, MSNBC, .*\\.txt$")
KlobucharFiles <- list.files(pattern = "Klobuchar, MSNBC, .*\\.txt$")
GabbardFiles <- list.files(pattern = "Gabbard, MSNBC, .*\\.txt$")
BookerFiles <- list.files(pattern = "Booker, MSNBC, .*\\.txt$")
BennetFiles <- list.files(pattern = "Bennet, MSNBC, .*\\.txt$")
HarrisFiles <- list.files(pattern = "Harris, MSNBC, .*\\.txt$")
TrumpFiles <- list.files(pattern = "Trump, White House, .*\\.txt$")

library(filesstrings)
file.move(WarrenFiles, "Warren", overwrite = T)
file.move(BidenFiles, "Biden", overwrite = T)
file.move(SandersFiles, "Sanders", overwrite = T)
file.move(KlobucharFiles, "Klobuchar", overwrite = T)
file.move(GabbardFiles, "Gabbard", overwrite = T)
file.move(BookerFiles, "Booker", overwrite = T)
file.move(BennetFiles, "Bennet", overwrite = T)
file.move(HarrisFiles, "Harris", overwrite = T)
file.move(TrumpFiles, "Trump", overwrite = T)

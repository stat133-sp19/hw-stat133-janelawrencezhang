##title: /make-shots-data-script
##date: 3/9/2019
##input: 
##output:

setwd("/Users/piper/workout01/code")
kevin <- read.csv(file ="../data/kevin-durant.csv", stringsAsFactors = FALSE)
klay <- read.csv(file ="../data/klay-thompson.csv",stringsAsFactors = FALSE)
andre <- read.csv(file ="../data/andre-iguodala.csv",stringsAsFactors = FALSE)
stephen <- read.csv(file ="../data/stephen-curry.csv",stringsAsFactors = FALSE)
graymond <- read.csv(file ="../data/draymond-green.csv",stringsAsFactors = FALSE)

kevin$name <- c("Kevin Durant")
klay$name <- c("Klay Thompson")
andre$name <- c("Andre Iguodala")
stephen$name <- c("Stephen Curry")
graymond$name <- c("Draymond Green")

kevin$shot_made_flag[kevin$shot_made_flag=="n"] <- c("shot_no")
kevin$shot_made_flag[kevin$shot_made_flag=="y"] <- c("shot_yes")

klay$shot_made_flag[klay$shot_made_flag=="n"] <- c("shot_no")
klay$shot_made_flag[klay$shot_made_flag=="y"] <- c("shot_yes")

andre$shot_made_flag[andre$shot_made_flag=="n"] <- c("shot_no")
andre$shot_made_flag[andre$shot_made_flag=="y"] <- c("shot_yes")

stephen$shot_made_flag[stephen$shot_made_flag=="n"] <- c("shot_no")
stephen$shot_made_flag[stephen$shot_made_flag=="y"] <- c("shot_yes")

graymond$shot_made_flag[graymond$shot_made_flag=="n"] <- c("shot_no")
graymond$shot_made_flag[graymond$shot_made_flag=="y"] <- c("shot_yes")

kevin$minute <- (12*kevin$period) - kevin$minutes_remaining
andre$minute <- (12*andre$period) - andre$minutes_remaining
klay$minute <- (12*klay$period) - klay$minutes_remaining
graymond$minute <- (12*graymond$period) - graymond$minutes_remaining
stephen$minute <- (12*stephen$period) - stephen$minutes_remaining

sink(file = "../output/andre-iguodala-summary.txt")
summary(andre)
sink()

sink(file = "../output/draymond-green-summary.txt")
summary(graymond)
sink()

sink(file = "../output/kevin-durant-summary.txt")
summary(kevin)
sink()

sink(file = "../output/klay-thompson-summary.txt")
summary(klay)
sink()


sink(file = "../output/stephen-curry-summary.txt")
summary(stephen)
sink()

combine <- rbind(andre,graymond,kevin,klay,stephen)

write.csv(x=combine_tgt,file="../data/shots-data.csv")

sink(file = "../output/shots-data-summary.txt")
summary(combine)
sink()


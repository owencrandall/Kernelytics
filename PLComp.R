# Title: How does the Prospect League make hitters better?
# Author: Owen Crandall

# Loading the necessary packages.
library(rvest)
library(stringr)
library(dplyr)
library(tidyr)

setwd("~/Downloads/CornBelters/PL Comp Project")

# Here we are loading in the data sets containing the names and PL statistics of
# the top 100 players by plate appearances from the 2013 to 2023 PL seasons.
# This data can be found on Baseball Reference at this address:
# https://www.baseball-reference.com/register/leader.cgi?type=bat&id=5cdc244e&sort_by=PA.
PLB13 <- read.csv("PLB13.csv"); PLB14 <- read.csv("PLB14.csv")
PLB15 <- read.csv("PLB15.csv"); PLB16 <- read.csv("PLB16.csv")
PLB17 <- read.csv("PLB17.csv"); PLB18 <- read.csv("PLB18.csv")
PLB19 <- read.csv("PLB19.csv"); PLB21 <- read.csv("PLB21.csv")
PLB22 <- read.csv("PLB22.csv"); PLB23 <- read.csv("PLB23.csv")

# Here we are creating a "year" column for each data set. This will allow for
# finding the correct players when BR is accessed in the loop later.
PLB13["Year"] <- 2013
PLB14["Year"] <- 2014
PLB15["Year"] <- 2015
PLB16["Year"] <- 2016
PLB17["Year"] <- 2017
PLB18["Year"] <- 2018
PLB19["Year"] <- 2019
PLB21["Year"] <- 2021
PLB22["Year"] <- 2022
PLB23["Year"] <- 2023

# Here we are correcting a small naming error with the 2019 data and combining
# the data into one data set.
colnames(PLB19)[8] <- "PA"
players <- rbind(PLB13, PLB14, PLB15, PLB16, PLB17, PLB18, PLB19, PLB21, PLB22, 
                 PLB23)

# This creates the list of players containing their names and the season they
# played in the PL. A specific data point is also removed (the manager issue
# discussed in the accompanying paper).
players_list <- players[c("Name", "Year")]
players_list <- players_list[-c(112), ]
rownames(players_list) <- NULL

# This loop goes through the player list and ensures they are all properly
# formatted (i.e. removing special characters).
for (i in 1:nrow(players_list)){
  players_list[i,1] <- str_replace_all(players_list[i,1], "([.])", "")
  players_list[i,1] <- str_replace_all(players_list[i,1], "(['])", "")
  if (substring(players_list[i,1], nchar(players_list[i,1])) == "*") {
    players_list[i,1] <- substring(players_list[i,1], 1, 
                                   (nchar(players_list[i,1]) - 1))
  }
  else if (substring(players_list[i,1], nchar(players_list[i,1])) == "?") {
    players_list[i,1] <- substring(players_list[i,1], 1, 
                                   (nchar(players_list[i,1]) - 1))
  }
}

# Setting up the before and after data frames.
beforedf <- data.frame()
afterdf <- data.frame()

# This loop goes through the player list, generates their URL code, and attempts
# to access their BR page. It then finds the correct data and stores it, or 
# moves on if the data is unavailable.
for (i in 1:nrow(players_list)) {
  # This section generates the player-specific section of the URL being accessed.
  player <- players_list[i,1]
  name <- strsplit(player, " ")
  first <- name[[1]][1]
  first_code <- substring(first, 1, 3)
  j <- 0
  while (j < 1) {
    if (nchar(first_code) < 3) {
      first_code <- paste0(first_code, "-")
    }
    else {
      j <- 1
    }
  }
  last <- paste0(name[[1]][2:length(name[[1]])], collapse = "")
  last_code <- substring(last, 1, 6)
  k <- 0
  while (k < 1) {
    if (nchar(last_code) < 6) {
      last_code <- paste0(last_code, "-")
    }
    else {
      k <- 1
    }
  }
  # Now that we have the code info for our player, we can begin accessing the data.
  num <- 0
  l <- 0
  while (l < 1) {
    before <- data.frame()
    after <- data.frame()
    prospect <- data.frame()
    # This creates the URL code for each player.
    code <- paste0(last_code, "00", num, first_code)
    # This specific print was used to pinpoint potential errors in the code.
    print(code)
    # This creates the URL being accessed.
    site <- paste0("https://www.baseball-reference.com/register/player.fcgi?id=", 
                   code)
    # Before the site is accessed each time, the system is delayed slightly over
    # six seconds, in order to avoid the scraping limitations enforced by BR.
    Sys.sleep(6.001)
    site_info <- read_html(site)
    table_node <- html_nodes(site_info, "table") 
    table_content <- try(html_table(table_node)[[1]])
    # This if statement/try-catch is in place to catch errors with accessing the 
    # data in the site.
    if (inherits(table_content, "try-error")) {
      # If the maximum amount of codes has been tried, the loop will move on to
      # the next player.
      if (num == 9) {
        l <- 1
      }
      # If not, it will keep trying with different codes. This is due to certain
      # URLs not having the data available, meaning they aren't for the players
      # we're looking for.
      else (num <- num + 1)
    }
    # This determines if the player is primarily a pitcher.
    else if (colnames(table_content[,8]) == "W") {
      l <- 1
    }
    # Now that we have the correct player, we check if their data is sufficient
    # for inclusion.
    else if (l == 0) {
      # If they have before and after data, they should be good.
      before <- filter(table_content, table_content["Year"] == players_list[i,2] 
                       & table_content["Lev"] == "NCAA")
      before["Name"] <- player
      after <- filter(table_content, table_content["Year"] == (players_list[i,2] + 1) 
                      & table_content["Lev"] == "NCAA")
      after["Name"] <- player
      # This checks that the player being looked at played in the PL in the
      # season being examined. If they didn't, we probably have the wrong player.
      prospect <- filter(table_content, table_content["Year"] == (players_list[i,2]) 
                         & table_content["Lg"] == "PROS")
      # This checks that the before, after, and PL check data frames all have
      # data. If they don't, the number code will be increased to check the next
      # URL. If they do, the data will be added to the larger data sets.
      if (dim(before)[1] == 0 | dim(after)[1] == 0 | dim(prospect)[1] == 0) {
        num <- num + 1
        if (dim(prospect)[1] != 0) {
          l <- 1
        }
      }
      else if (dim(prospect)[1] != 0) {
        beforedf <- rbind(beforedf, before)
        afterdf <- rbind(afterdf, after)
        l <- 1
      }
    }
    # This checks if the player at the cutoff point for the numeric URL codes is
    # the correct one, collecting his data if so.
    if (num == 9) {
      if (dim(before)[1] != 0 & dim(after)[1] != 0) {
        beforedf <- rbind(beforedf, before)
        afterdf <- rbind(afterdf, after)
      }
      l <- 1
    }
  }
}

# Saving the data so I can't accidentally wipe it again.
write.csv(beforedf, '~/Downloads/CornBelters/PL Comp Project/before.csv')
write.csv(afterdf, '~/Downloads/CornBelters/PL Comp Project/after.csv')

# Re-reading in the data for analysis.
beforedf <- read.csv('~/Downloads/CornBelters/PL Comp Project/before.csv')
afterdf <- read.csv('~/Downloads/CornBelters/PL Comp Project/after.csv')

# Mutating the data to fix data types and remove NAs.
beforedf <- beforedf %>% mutate(across(G:IBB, as.numeric))
afterdf <- afterdf %>% mutate(across(G:IBB, as.numeric))
beforedf <- beforedf %>% mutate(across(G:IBB, ~ replace_na(.x, 0)))
afterdf <- afterdf %>% mutate(across(G:IBB, ~ replace_na(.x, 0)))

# Filtering the data to only include qualified hitters.
before1 <- filter(beforedf, PA >= 50)
after1 <- filter(afterdf, PA >= 50)

# Finding the mean values for the statistics being examined.
mean(before1$BA)
mean(after1$BA)
mean(before1$OBP)
mean(after1$OBP)
mean(before1$SLG)
mean(after1$SLG)
mean(before1$OPS)
mean(after1$OPS)

# Running two-sample t-tests to determine the significance of the data.
t.test(before1$BA, after1$BA, alternative = 'two.sided')
t.test(before1$OBP, after1$OBP, alternative = 'two.sided')
t.test(before1$SLG, after1$SLG, alternative = 'two.sided')

# Creating a data frame containing the mean statistics and a bar plot to show
# the differences.
times <- c("Before", "After")
trip_stats <- c("AVG", "OBP", "SLG")
colors <- c("#2C5234", "#FFCD00")
meansdf <- matrix(c(mean(before1$BA), mean(before1$OBP), mean(before1$SLG), 
                  mean(after1$BA), mean(after1$OBP),  mean(after1$SLG)), 
                  nrow=2, ncol=3, byrow=T)
barplot(meansdf, names.arg=trip_stats, beside=T, col=colors, ylim=c(0,0.45), 
        main = "Triple Slash in Before/After Seasons")
legend("topleft", times, cex = 0.7, fill = colors)

# This creates a "total" data set with differences between the triple slash 
# numbers to find the players who had the biggest jumps in performances.
total_data <- cbind(beforedf, afterdf)
total_data$BAdiff <- (total_data[,54] - total_data[,22])
total_data$OBPdiff <- (total_data[,55] - total_data[,23])
total_data$SLGdiff <- (total_data[,56] - total_data[,24])
write.csv(total_data, '~/Downloads/CornBelters/PL Comp Project/total_data.csv')

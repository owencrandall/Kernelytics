# Title: Creating wOBA for the KCL
# Author: Owen Crandall
# Adapted from SQL script created by Tom Tango at 
# https://www.insidethebook.com/ee/index.php/site/article/woba_year_by_year_calculations/

library(dplyr)

# Here we are reading in and reformatting the data for the 4 KCL teams. This
# section is specific to this data, but the run value and wOBA scale creation 
# should work with any league data.
bluecaps <- read.csv('Blue Caps 2024 Summer 2024 Stats.csv', header=T)
names(bluecaps) <- bluecaps[1,]
bluecaps <- bluecaps[-1,]
num_del <- nrow(bluecaps) - 2
bluecaps <- bluecaps[-c(num_del:nrow(bluecaps)),]
bluecaps$Team <- 'Blue Caps'

bobcats <- read.csv('Bobcats 2024 Summer 2024 Stats.csv', header=T)
names(bobcats) <- bobcats[1,]
bobcats <- bobcats[-1,]
num_del <- nrow(bobcats) - 2
bobcats <- bobcats[-c(num_del:nrow(bobcats)),]
bobcats$Team <- 'Bobcats'

merchants <- read.csv('Merchants 2024 Summer 2024 Stats.csv', header=T)
names(merchants) <- merchants[1,]
merchants <- merchants[-1,]
num_del <- nrow(merchants) - 2
merchants <- merchants[-c(num_del:nrow(merchants)),]
merchants$Team <- 'Merchants'

groundsloths <- read.csv('Ground Sloths 2024 Summer 2024 Stats.csv', header=T)
names(groundsloths) <- groundsloths[1,]
groundsloths <- groundsloths[-1,]
num_del <- nrow(groundsloths) - 2
groundsloths <- groundsloths[-c(num_del:nrow(groundsloths)),]
groundsloths$Team <- 'Ground Sloths'

# Here we combine the four teams into one data set, reduce it to include only
# the necessary hitting statistics (as well as innings pitched from the pitching 
# data), and convert the statistics to numeric values.
league <- rbind(bluecaps, bobcats, merchants, groundsloths)
league <- league[,c(0:28,55,158)]
league <- league %>% mutate(across(GP:IP, as.numeric))

# This for loop converts the innings pitch numbers into a format that can be
# summed to calculate total outs (i.e. converting 0.1 and 0.2 to 0.333 and 0.667).
for (i in seq_along(league$IP)) {
  ip <- league$IP[i]
  if (((ip + 0.9) %% 1) == 0) {
    league$IP[i] <- ip + 0.233
  } else if (((ip + 0.8) %% 1) == 0) {
    league$IP[i] <- ip + 0.467
  }
}

# Now we calculate the total outs recorded in the league games.
league_outs <- sum(league$IP) * 3

# Here we calculate runs per out to determine the league run environment and 
# calculate the run values of each offensive event. Almost everything starting
# with this section has been adapted for R from the SQL script published by Tom
# Tango, the original creator of wOBA. It can be found at this link: 
# https://www.insidethebook.com/ee/index.php/site/article/woba_year_by_year_calculations/
RperOut <- sum(league$R) / league_outs
RperOut
runBB <- RperOut + 0.14
runHB <- runBB + 0.025
run1B <- runBB + 0.155
run2B <- run1B + 0.3
run3B <- run2B + 0.27
runHR <- 1.4
runSB <- 0.2
runCS <- 2 * RperOut + 0.075

attach(league)

# Now we create what will be the numerator for calculating both the Run Minus and
# Run Plus for each player.
league$pRunMult <- runBB*BB + runHB*HBP + run1B*league$'1B' + run2B*league$'2B' 
                  + run3B*league$'3B'+ runHR*HR

# Here the denominators for Run Minus and Run Plus are calculated. The definitions
# of these terms can be found in the writing accompanied by this script.
league$pRunMinDen <- AB - H  + SF
league$pRunPlusDen <- BB + HBP + H

# Now the league Run Minus and Run Plus are created using the prior pieces.
runMinus <- sum(league$pRunMult) / sum(league$pRunMinDen)
runPlus <- sum(league$pRunMult) / sum(league$pRunPlusDen)

# Now using the Run Plus and Run Minus, the wOBA scale is created and used to 
# scale the run values for each offensive event into our desired format.
wOBAscale <- 1 / (runPlus + runMinus)
wobaBB <- (runBB + runMinus) * wOBAscale
wobaHB <- (runHB + runMinus) * wOBAscale 
woba1B <- (run1B + runMinus) * wOBAscale 
woba2B <- (run2B + runMinus) * wOBAscale 
woba3B <- (run3B + runMinus) * wOBAscale 
wobaHR <- (runHR + runMinus) * wOBAscale 
wobaSB <- runSB * wOBAscale
wobaCS <- runCS * wOBAscale

# Using the wOBA values for each event, the wOBA for each player is calculated
# using the following formula.
league$wOBA <- round((wobaBB*BB + wobaHB*HBP + woba1B*league$'1B' + woba2B*league$'2B' 
              + woba3B*league$'3B' + wobaHR*HR) / PA, 3)

# These lines are for viewing the data and do not include any more calculations.
# Here, the players are filtered to include only players with a qualifying number
# of plate appearances. Their names, plate appearances, triple slash, OPS, and wOBA
# are then selected.
wOBAview <- filter(league[c(3, 2, 30, 5, 7, 8, 10, 9, 34)], PA >= 28)
wOBAview <- arrange(wOBAview, desc(wOBA))
rownames(wOBAview) <- NULL

# Here the league average wOBA is calculated. At the time of this script creation
# the average KCL wOBA is much higher than the MLB, which makes sense given that 
# a higher than average number of runs are scored in the KCL.
avg_wOBA <- mean(wOBAview$wOBA)

# Here we create the wOBA difference to show how much above or below the league
# average each player is.
wOBAview$wOBAdiff <- round(wOBAview$wOBA - avg_wOBA, 3)

# Here the weights given to each event in the wOBA formula are displayed along with
# the league average wOBA. This could be adapted to include the weights for each
# season.
weights <- data.frame(avg_wOBA, wobaBB,wobaHB, woba1B, woba2B, woba3B, wobaHR, 
                        wobaSB, wobaCS)
weights

# Finally, the "wOBA view" created earlier is written to a csv for distribution.
write.csv(wOBAview, '~/Downloads/CornBelters/kcl_wOBA.csv')

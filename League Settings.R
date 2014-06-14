###########################
# File: League Settings.R
# Description: User sets league settings
# i.e., the maximum bid up to which the player is still on the best team (the team that maximizes your possible points)
# Date: 6/1/2013
# Author: Isaac Petersen (isaactpetersen@gmail.com)
# Notes: Set for Livin the Dream
###########################

#Roster
numQBstarters <- 1
numRBstarters <- 2
numWRstarters <- 2
numTEstarters <- 1
numTotalStarters <- 9
numTotalPlayers <- 16

#League settings
#defaultCap <- 200 #what the typical cap is for your service (ESPN, Yahoo, etc.) -- used for placing "avg cost" in context
#leagueCap <- 225 #your league's cap
#maxCost <- leagueCap - (numTotalPlayers - numTotalStarters)

#Scoring
passYdsMultiplier <- (1/25) #1 pt per 25 pass yds
passTdsMultiplier <- 4      #4 pts per pass td
passIntMultiplier <- -2     #-2 pts per INT
rushYdsMultiplier <- (1/10) #1 pt per 10 rush yds
rushTdsMultiplier <- 6      #6 pts per rush td
#recMultiplier <- (1/1)    #1 pt per reception (not included in NLF projections)
recYdsMultiplier <- (1/10)   #1 pt per 10 rec yds
recTdsMultiplier <- 6       #6 pts per rec td
twoPtsMultiplier <- 2       #2 pts per 2-point conversion (not included in ESPN or CBS projections)
fumlMultiplier <- -2        #-2 pts per fumble lost (not included in ESPN projections)
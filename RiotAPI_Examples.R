# Blaine Harper
# RiotAPI.R extension
# RiotAPI_Examples.R
# Truman State University

# This is normally done via include.packages() with a formal package
# but this is only a alpha so, it's not quite ready to be made into a package
source("./RiotAPI.R")

# This will setup the environment with the proper variables to ease 
# use for the end user.
lol.setup("danealue","RGAPI-ab05bbc7-482b-4fbe-a733-dcb8c6bf9906")

# This will plot the mastery data for whichever account is in the setup document.
plot.masteries(sum.name, api.key)

# This will plot champion preference for the last 100 games (this includes ARAM, etc.)
plot.champ.freq(sum.name, api.key)

# I've only got these two plot functions, but I'm working on creating additional functionality.
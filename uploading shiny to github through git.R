# set directory to shiny app folder
setwd("D:/PhD related/Shiny")

# check git working
system("git --version")

system("git init")# Initialized empty Git repository in D:/PhD related/Shiny/.git/

# from the terminal
# add files
git add . # do it from terminal

# once added, then commit
git commit -m "Initial commit" 

# next connect github repository
git remote add origin https://github.com/anwar79melb/Grasshopper-SDM-Shiny.git

git push -u origin master

# after edited
git status

git add app.R # modified file

git commit -m "Short description of changes"
git push

# to publish in shiny/connect.posit.cloud
library(rsconnect)
rsconnect::writeManifest()




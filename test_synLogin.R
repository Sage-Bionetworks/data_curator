# detach("package:reticulate", unload=TRUE)
# unloadNamespace("reticulate")
library(synapser)

### login and get team members in allowed team 
synLogin()
htanTab <- synTableQuery("SELECT * from syn20446927")
htanTab <- as.data.frame(htanTab)
unique(htanTab$allowedTeam)

htanTeamMembers <- synGetTeamMembers(unique(htanTab$allowedTeam))
htanTeamMembers <- htanTeamMembers$asList()

### get logged in user profile
userProfile <- synGetUserProfile()
userProfile$userName %in% htanTeamMembers
htanTeamMembers[[1]]$member$userName ## will have to loop through usernames

userAllowedTeams <- list() ### to save the allowed teams in a list
### for each allowedTeamMember, check if the userProfile is part of that and save to userAllowedTeams
for (i in seq_along(htanTeamMembers)){
  print(i)
  teamMemberUserName <- htanTeamMembers[[i]]$member$userName
  
  if (userProfile$userName == teamMemberUserName) {
    print(teamMemberUserName)
    userAllowedTeams[i] <- teamMemberUserName
  }
}

### synQuery the table of by userAllowedTeams

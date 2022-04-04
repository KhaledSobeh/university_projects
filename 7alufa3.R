###################################
##### Project - Alternative 3 #####
###################################


##---------- 0.  all libraries ----------##

# library(rlang)
# library(MASS)
# library(fitdistrplus)
# library(magrittr)
# library(dplyr)
# library(lazyeval)
# library(parallel)
# library(e1071)
# library(plotly)
# library(ggplot2)
# library(triangle)
# library(sqldf)
# library(readxl)
# library(knitr)
# library(rmarkdown)
# library(simmer)
# library(simmer.plot)
# library(stringr)


##---------- 1.  all functions ----------##
# This function is a short-cut to the seize>timeout>release process
addService<- function  (trajectory,sname,timeDist){
  updatedPath <- seize(trajectory, resource = sname, amount = 1)%>%
    timeout(timeDist) %>%
    release(resource = sname, amount = 1)
  
  return(updatedPath)
}

#trimmed norm
trimmedNorm<-function(mu,sd){
  while(TRUE){
    sample<-rnorm(1,mu,sd)
    if (sample>0)
      return (sample)
  }
}


initialOpinion_road_probFunc <- function(){#Step 3
  ########################################## get initial option
  initial_opinion <- 0
  x <- runif(1,0,1)
  if(x<0.5){#right
    initial_opinion <- 1
  }
  else if(x>=0.5 & x<0.9){#left
    initial_opinion <- 3
  }
  else{#center
    initial_opinion <- 2
  }
  
  ########################################## probability vector
  z <- 0
  x<-runif(1)
  x2<-runif(1)
  if(x<=0.5){
    z <- (x2/8)^(1/3)
  }else{
    z <- (0.5*x2)
  }
  
  if(initial_opinion==1){
    return(c(as.integer(initial_opinion),0.25-z/2,0.5-z/2,0.25+z))
  }
  else if(initial_opinion==2){
    return(c(as.integer(initial_opinion),0.25+z,0.5-z/2,0.25-z/2))
  }
  else{
    return(c(as.integer(initial_opinion),0.25-z/2,0.5+z,0.25-z/2))
  }
}
#get place of the voter
get_places <- function(opinion){
  if(opinion==1){
    return(c(3,2,1))
  }
  if(opinion==2){
    return(c(3,1,2))
  }
  return(c(1,2,3))
}
#this func to get the center stands array
get_stands_center <- function(opinion,place){
  if(opinion==1){
    return(c("center_stand_left","center_stand_center","center_stand_right")[place])
  }
  if(opinion==2){
    return(c("center_stand_left","center_stand_right","center_stand_center")[place])
  }
  return(c("center_stand_right","center_stand_center","center_stand_left")[place])
}
#this func to get the north stands array
get_stands_north <- function(opinion,place){
  if(opinion==1){
    return(c("north_stand_left","north_stand_center","north_stand_right")[place])
  }
  if(opinion==2){
    return(c("north_stand_left","north_stand_right","north_stand_center")[place])
  }
  return(c("north_stand_right","north_stand_center","north_stand_left")[place])
}
#this func to get the south stands array
get_stands_south <- function(opinion,place){
  if(opinion==1){
    return(c("south_stand_left","south_stand_center","south_stand_right")[place])
  }
  if(opinion==2){
    return(c("south_stand_left","south_stand_right","south_stand_center")[place])
  }
  return(c("south_stand_right","south_stand_center","south_stand_left")[place])
}

#these three functions to know if the voter in left stands(do5n)
get_stands_center_api <- function(opinion,place){
  if(opinion==1){
    if(str_detect(c("center_stand_left","center_stand_center","center_stand_right")[place],"left")){
      return(1)
    }
    return(0)
  }
  if(opinion==2){
    if(str_detect(c("center_stand_left","center_stand_right","center_stand_center")[place],"left")){
      return(1)
    }
    return(0)
  }
  if(str_detect(c("center_stand_right","center_stand_center","center_stand_left")[place],"left")){
    return(1)
  }
  return(0)
}
get_stands_north_api <- function(opinion,place){
  if(opinion==1){
    if(str_detect(c("north_stand_left","north_stand_center","north_stand_right")[place],"left")){
      return(1)
    }
    return(0)
  }
  if(opinion==2){
    if(str_detect(c("north_stand_left","north_stand_right","north_stand_center")[place],"left")){
      return(1)
    }
    return(0)
  }
  if(str_detect(c("north_stand_right","north_stand_center","north_stand_left")[place],"left")){
    return(1)
  }
  return(0)
}
get_stands_south_api <- function(opinion,place){
  if(opinion==1){
    if(str_detect(c("south_stand_left","south_stand_center","south_stand_right")[place],"left")){
      return(1)
    }
    return(0)
  }
  if(opinion==2){
    if(str_detect(c("south_stand_left","south_stand_right","south_stand_center")[place],"left")){
      return(1)
    }
    return(0)
  }
  if(str_detect(c("south_stand_right","south_stand_center","south_stand_left")[place],"left")){
    return(1)
  }
  return(0)
}


isRenege <- function(right_prob,center_prob,left_prob,place){# place: 1:right, 2:center, 3:left
  vec <- c(right_prob,center_prob,left_prob,place)
  if(length(unique(vec))!=3){
    return(100)
  }
  if(vec[place]==max(vec)){
    return(100)
  }
  return(runif(1,3.5,6.5))
}

isRenege_7alufa1 <- function(right_prob,center_prob,left_prob,place,resource){# place: 1:right, 2:center, 3:left
  resource <- as.character(resource)
  vec <- c(right_prob,center_prob,left_prob,place)
  if(length(unique(vec))!=3){
    return(100)
  }
  if(vec[place]==max(vec)){
    return(100)
  }
  if(str_detect(resource,"left")){
    return(runif(1,5.5,8.5))
  }
  return(runif(1,3.5,6.5))
}

#this function is to change the prop vec after reneging from ado5n
change_probs_func_renege <- function(right_prob,center_prob,left_prob,place){
  x <- runif(1,0,0.25)
  vec <- c(right_prob,center_prob,left_prob)
  for(i in 1:3){
    if(i==place){
      vec[i] <- vec[i]-x
    }
    else{
      vec[i] <- vec[i]+(x/2)
    }
  }
  if (vec[1] < 0) {
    vec[1] <- 0
    vecSum <- vec[2] + vec[3]
    vec[2] <- vec[2] / vecSum
    vec[3] <- vec[3] / vecSum
  }
  if (vec[1] > 1) {
    vec[1] <- 1
    vec[2] <- 0
    vec[3] <- 0
  }
  
  
  if (vec[2] < 0) {
    vec[2] <- 0
    vecSum <- vec[1] + vec[3]
    vec[1] <- vec[1] / vecSum
    vec[3] <- vec[3] / vecSum
  }
  if (vec[2] > 1) {
    vec[2] <- 1
    vec[1] <- 0
    vec[3] <- 0
  }
  
  
  if (vec[3] < 0) {
    vec[3] <- 0
    vecSum <- vec[2] + vec[1]
    vec[2] <- vec[2] / vecSum
    vec[1] <- vec[1] / vecSum
  }
  if (vec[3] > 1) {
    vec[3] <- 1
    vec[2] <- 0
    vec[1] <- 0
  }
  return(vec)
}
#this function is to change the prop vec after not reneging from ado5n
change_probs_func_not_renege <- function(right_prob,center_prob,left_prob,place){
  x <- runif(1,0,0.5)
  vec <- c(right_prob,center_prob,left_prob)
  for(i in 1:3){
    if(i==place){
      vec[i] <- vec[i]+x
    }
    else{
      vec[i] <- vec[i]-(x/2)
    }
  }
  if (vec[1] < 0) {
    vec[1] <- 0
    vecSum <- vec[2] + vec[3]
    vec[2] <- vec[2] / vecSum
    vec[3] <- vec[3] / vecSum
  }
  if (vec[1] > 1) {
    vec[1] <- 1
    vec[2] <- 0
    vec[3] <- 0
  }
  
  
  if (vec[2] < 0) {
    vec[2] <- 0
    vecSum <- vec[1] + vec[3]
    vec[1] <- vec[1] / vecSum
    vec[3] <- vec[3] / vecSum
  }
  if (vec[2] > 1) {
    vec[2] <- 1
    vec[1] <- 0
    vec[3] <- 0
  }
  
  
  if (vec[3] < 0) {
    vec[3] <- 0
    vecSum <- vec[2] + vec[1]
    vec[2] <- vec[2] / vecSum
    vec[1] <- vec[1] / vecSum
  }
  if (vec[3] > 1) {
    vec[3] <- 1
    vec[2] <- 0
    vec[1] <- 0
  }
  return(vec)
}

#this func is for cheating after the sim
cheating <- function(){
  x <- runif(1,0,1)
  if(x<0.4){
    return(rdiscrete (1, c(1/3,1/3,1/3),c(1,2,3)))
  }
  return(0)
}

change_probs_func_not_renege_7alufa1 <- function(right_prob,center_prob,left_prob,place,resource){
  resource <- as.character(resource)
  if(str_detect(resource,"left")){
    x <- runif(1,0,0.5) + runif(1,0.1,0.4)
  }
  else{
    x <- runif(1,0,0.5)
  }
  vec <- c(right_prob,center_prob,left_prob)
  for(i in 1:3){
    if(i==place){
      vec[i] <- vec[i]+x
    }
    else{
      vec[i] <- vec[i]-(x/2)
    }
  }
  if (vec[1] < 0) {
    vec[1] <- 0
    vecSum <- vec[2] + vec[3]
    vec[2] <- vec[2] / vecSum
    vec[3] <- vec[3] / vecSum
  }
  if (vec[1] > 1) {
    vec[1] <- 1
    vec[2] <- 0
    vec[3] <- 0
  }
  
  
  if (vec[2] < 0) {
    vec[2] <- 0
    vecSum <- vec[1] + vec[3]
    vec[1] <- vec[1] / vecSum
    vec[3] <- vec[3] / vecSum
  }
  if (vec[2] > 1) {
    vec[2] <- 1
    vec[1] <- 0
    vec[3] <- 0
  }
  
  
  if (vec[3] < 0) {
    vec[3] <- 0
    vecSum <- vec[2] + vec[1]
    vec[2] <- vec[2] / vecSum
    vec[1] <- vec[1] / vecSum
  }
  if (vec[3] > 1) {
    vec[3] <- 1
    vec[2] <- 0
    vec[1] <- 0
  }
  return(vec)
}



kpi_1 <- function(left,center,right){
  return(left/sum(c(left,right,center)))
}
kpi_2 <- function(renege_from_left,all_left){
  return(renege_from_left/all_left) 
}



##---------- 2.  all simulation parameters ----------##
#sim time
simulationTime <- 15*60 + 21

##---------- 3.  Init Simulation and add all resources  ----------##

election <- simmer("election")%>%
  add_resource("resource_ballotbox_south_big1",capacity=1, queue_size=Inf) %>%
  add_resource("resource_ballotbox_south_big2",capacity=1, queue_size=Inf) %>%
  add_resource("resource_ballotbox_south_big3",capacity=1, queue_size=Inf) %>%
  add_resource("resource_ballotbox_south_big4",capacity=1, queue_size=Inf) %>%
  add_resource("resource_ballotbox_south_small1",capacity=1, queue_size=Inf) %>%
  add_resource("resource_ballotbox_south_small2",capacity=1, queue_size=Inf) %>%
  add_resource("south_stand_right",capacity=5, queue_size=0) %>%
  add_resource("south_stand_center",capacity=5, queue_size=0) %>%
  add_resource("south_stand_left",capacity=5, queue_size=0) %>%
  
  add_resource("resource_ballotbox_north_big1",capacity=1, queue_size=Inf) %>%
  add_resource("resource_ballotbox_north_big2",capacity=1, queue_size=Inf) %>%
  add_resource("resource_ballotbox_north_small1",capacity=1, queue_size=Inf) %>%
  add_resource("north_stand_right",capacity=4, queue_size=0) %>%
  add_resource("north_stand_center",capacity=5, queue_size=0) %>%
  add_resource("north_stand_left",capacity=7, queue_size=0) %>%
  
  add_resource("resource_ballotbox_center_big1",capacity=1, queue_size=Inf) %>%
  add_resource("resource_ballotbox_center_big2",capacity=1, queue_size=Inf) %>%
  add_resource("resource_ballotbox_center_big3",capacity=1, queue_size=Inf) %>%
  add_resource("resource_ballotbox_center_big4",capacity=1, queue_size=Inf) %>%
  add_resource("resource_ballotbox_center_big5",capacity=1, queue_size=Inf) %>%
  add_resource("resource_ballotbox_center_big6",capacity=1, queue_size=Inf) %>%
  add_resource("resource_ballotbox_center_small1",capacity=1, queue_size=Inf) %>%
  add_resource("resource_ballotbox_center_small2",capacity=1, queue_size=Inf) %>%
  add_resource("resource_ballotbox_center_small3",capacity=1, queue_size=Inf) %>%
  add_resource("center_stand_right",capacity=10, queue_size=0) %>%
  add_resource("center_stand_center",capacity=10, queue_size=0) %>%
  add_resource("center_stand_left",capacity=12, queue_size=0)


##---------- 4.  All trajectories, start from main trajectory and add sub-trajectories ABOVE IT it . ----------##
#these trajectories for voting foe each party 
vote_to_left <- trajectory("vote_to_left") %>%
  set_global(keys="voting_to_left",value= +1, mod= "+", init = 0)
vote_to_center <- trajectory("vote_to_center") %>%
  set_global(keys="voting_to_center",value= +1, mod= "+", init = 0)
vote_to_right <- trajectory("vote_to_right") %>%
  set_global(keys="voting_to_right",value= +1, mod= "+", init = 0)
voting <- trajectory("voting") %>%
  branch(option = function() rdiscrete(1,c(get_attribute(election,"right_prob"),get_attribute(election,"center_prob"),
                                           get_attribute(election,"left_prob")),c(1,2,3)),
         c(F,F,F), vote_to_right,vote_to_center,vote_to_left)                       

#######################################################
############south_compound#############################
#######################################################
#these 4 traj for south big ballotboxes
ballotbox_south_big_1 <- trajectory("ballotbox_south_big_1") %>% 
  addService(sname = "resource_ballotbox_south_big1", timeDist =function() rexp(1,3/2)+trimmedNorm(1,6/60)) %>%
  join(voting)
ballotbox_south_big_2 <- trajectory("ballotbox_south_big_2") %>% 
  addService(sname = "resource_ballotbox_south_big2", timeDist =function() rexp(1,3/2)+trimmedNorm(1,6/60)) %>%
  join(voting)
ballotbox_south_big_3 <- trajectory("ballotbox_south_big_3") %>% 
  addService(sname = "resource_ballotbox_south_big3", timeDist =function() rexp(1,3/2)+trimmedNorm(1,6/60)) %>%
  join(voting)
ballotbox_south_big_4 <- trajectory("ballotbox_south_big_4") %>% 
  addService(sname = "resource_ballotbox_south_big4", timeDist =function() rexp(1,3/2)+trimmedNorm(1,6/60)) %>%
  join(voting)
#this traj for dividing the voters to the 4 big ballotboxes 
ballotbox_south_big <- trajectory("ballotbox_south_big") %>%
  branch(option=function() { rdiscrete (1, c(0,1/4,1/4,1/4,1/4),c(0,1,2,3,4)) },
         c(F,F,F,F),ballotbox_south_big_1,ballotbox_south_big_2,ballotbox_south_big_3,
         ballotbox_south_big_4)
#these 3 traj for south small ballotboxes
ballotbox_south_small_1 <- trajectory("ballotbox_south_small_1") %>%
  addService(sname = "resource_ballotbox_south_small1", timeDist =function() rexp(1,3/2)+trimmedNorm(1,6/60)) %>%
  join(voting)
ballotbox_south_small_2 <- trajectory("ballotbox_south_small_2") %>%
  addService(sname = "resource_ballotbox_south_small2", timeDist =function() rexp(1,3/2)+trimmedNorm(1,6/60)) %>%
  join(voting)
#this traj for dividing the voters to the 3 small ballotboxes
ballotbox_south_small <- trajectory("ballotbox_south_small") %>%
  branch(option=function() { rdiscrete (1, c(0,1/2,1/2),c(0,1,2)) },
         c(F,F),ballotbox_south_small_1,ballotbox_south_small_2)


#this traj for the voters who renged from the third do5n and here happen the prop changing
south_compound_do5n3_renege <- trajectory("south_compound_do5n3_renege") %>%
  set_global("renege",function() get_stands_south_api(get_attribute(election,"initial_opinion"),3), mod= "+") %>%
  set_attribute(key =  c("right_prob","center_prob","left_prob"),
                value = function() change_probs_func_renege(get_attribute(election,"right_prob"),get_attribute(election,"center_prob"),
                                                            get_attribute(election,"left_prob"),get_attribute(election,"place3"))) %>%
  branch(option = function() get_attribute(election,"voter_type"),
         c(F,F),ballotbox_south_small,ballotbox_south_big)
#this traj for the third stand in this compound that sending them to  the do5n3_renege  traj or to the balotbox
south_compound_do5n3 <- trajectory("south_compound_do5n3") %>%
  set_global("come_to_left",function() get_stands_south_api(get_attribute(election,"initial_opinion"),3), mod= "+") %>%
  simmer::select( resources = function() get_stands_south(get_attribute(election,"initial_opinion"),3), id = 9) %>%
  seize_selected(amount = 1, continue = FALSE, id = 9, reject = trajectory() %>% branch(option = function() get_attribute(election,"voter_type"),
                                                                                        c(F,F),ballotbox_south_small,ballotbox_south_big)) %>%
  renege_in(t = function()isRenege(get_attribute(election,"right_prob_origin"),get_attribute(election,"center_prob_origin"),
                                   get_attribute(election,"left_prob_origin"), get_attribute(election,"place2")), out = south_compound_do5n3_renege) %>%
  timeout(function () runif(1, 2, 6)) %>%
  renege_abort() %>%
  release_selected(amount = 1, id = 9) %>%
  set_attribute(key =  c("right_prob","center_prob","left_prob"),
                value = function() change_probs_func_not_renege(get_attribute(election,"right_prob"),get_attribute(election,"center_prob"),
                                                                get_attribute(election,"left_prob"),get_attribute(election,"place3"))) %>%
  branch(option = function() get_attribute(election,"voter_type"),
         c(F,F),ballotbox_south_small,ballotbox_south_big)


#this traj for the voters who renged from the second do5n and here happen the prop changing
south_compound_do5n2_renege <- trajectory("south_compound_do5n2_renege") %>%
  set_global("renege",function() get_stands_south_api(get_attribute(election,"initial_opinion"),2), mod= "+") %>%
  set_attribute(key =  c("right_prob","center_prob","left_prob"),
                value = function() change_probs_func_renege(get_attribute(election,"right_prob"),get_attribute(election,"center_prob"),
                                                             get_attribute(election,"left_prob"),get_attribute(election,"place2"))) %>%
  join(south_compound_do5n3)
#this traj for the secnd stand in this compound that sending them to the do5n3 taj or the do5n2_renege  traj
south_compound_do5n2 <- trajectory("south_compound_do5n2") %>%
  set_global("come_to_left",function() get_stands_south_api(get_attribute(election,"initial_opinion"),2), mod= "+") %>%
  simmer::select( resources = function() get_stands_south(get_attribute(election,"initial_opinion"),2), id = 8) %>%
  seize_selected(amount = 1, continue = FALSE, id = 8, reject = south_compound_do5n3) %>%
  renege_in(
    t = function()isRenege(get_attribute(election,"right_prob_origin"),get_attribute(election,"center_prob_origin"),
                           get_attribute(election,"left_prob_origin"), get_attribute(election,"place2")), out = south_compound_do5n2_renege) %>%
  timeout(function () runif(1, 2, 6)) %>%
  renege_abort() %>%
  release_selected(amount = 1, id = 8) %>%
  set_attribute(key =  c("right_prob","center_prob","left_prob"),
                value = function() change_probs_func_not_renege(get_attribute(election,"right_prob"),get_attribute(election,"center_prob"),
                                                                get_attribute(election,"left_prob"),get_attribute(election,"place2"))) %>%
  join(south_compound_do5n3)

#this traj for the voters who renged from the first do5n and here happen the prop changing
south_compound_do5n1_renege <- trajectory("south_compound_do5n1_renege") %>%
  set_global("renege",function() get_stands_south_api(get_attribute(election,"initial_opinion"),1), mod= "+") %>%
  set_attribute(key =  c("right_prob","center_prob","left_prob"),
                value = function() change_probs_func_renege(get_attribute(election,"right_prob"),get_attribute(election,"center_prob"),
                                                            get_attribute(election,"left_prob"),get_attribute(election,"place1"))) %>%
  join(south_compound_do5n2)
#this traj for the first stand in this compound that sending them to the do5n2 taj or the do5n1_renege  traj
south_compound_do5n1 <- trajectory("south_compound_do5n1") %>%
  set_global("come_to_left",function() get_stands_south_api(get_attribute(election,"initial_opinion"),1), mod= "+") %>%
  simmer::select( resources = function() get_stands_south(get_attribute(election,"initial_opinion"),1), id = 7) %>%
  seize_selected(amount = 1, continue = FALSE, id = 7, reject = south_compound_do5n2) %>%
  renege_in(
    t = function()isRenege(get_attribute(election,"right_prob_origin"),get_attribute(election,"center_prob_origin"),
                           get_attribute(election,"left_prob_origin"), get_attribute(election,"place1")), out = south_compound_do5n1_renege) %>%
  timeout(function () runif(1, 2, 6)) %>%
  renege_abort() %>%
  release_selected(amount = 1, id = 7) %>%
  set_attribute(key =  c("right_prob","center_prob","left_prob"),
                value = function() change_probs_func_not_renege(get_attribute(election,"right_prob"),get_attribute(election,"center_prob"),
                                                                get_attribute(election,"left_prob"),get_attribute(election,"place1"))) %>%
  join(south_compound_do5n2)


################################################################################
##########################north_compound########################################
################################################################################


############################ #compounds trajectories.
#these 2 traj for north big ballotboxes
ballotbox_north_big_1 <- trajectory("ballotbox_north_big_1") %>% 
  addService(sname = "resource_ballotbox_north_big1", timeDist =function() rexp(1,3/2)+trimmedNorm(1,6/60)) %>%
  join(voting)
ballotbox_north_big_2 <- trajectory("ballotbox_north_big_2") %>% 
  addService(sname = "resource_ballotbox_north_big2", timeDist =function() rexp(1,3/2)+trimmedNorm(1,6/60)) %>%
  join(voting)
#this traj for dividing the voters to the 2 big ballotboxes
ballotbox_north_big <- trajectory("ballotbox_north_big") %>%
  branch(option=function() { rdiscrete (1, c(0,1/2,1/2),c(0,1,2)) },
         c(F,F),ballotbox_north_big_1,ballotbox_north_big_2)

ballotbox_north_small_1 <- trajectory("ballotbox_north_small_1") %>%
  addService(sname = "resource_ballotbox_north_small1", timeDist =function() rexp(1,3/2)+trimmedNorm(1,6/60)) %>%
  join(voting)

#this traj for the voters who renged from the third do5n and here happen the prop changing
north_compound_do5n3_renege <- trajectory("north_compound_do5n3_renege") %>%
  set_global("renege",function() get_stands_north_api(get_attribute(election,"initial_opinion"),3), mod= "+") %>%
  set_attribute(key =  c("right_prob","center_prob","left_prob"),
                value = function() change_probs_func_renege(get_attribute(election,"right_prob"),get_attribute(election,"center_prob"),
                                                            get_attribute(election,"left_prob"),get_attribute(election,"place3"))) %>%
  branch(option = function() get_attribute(election,"voter_type"),
         c(F,F),ballotbox_north_small_1,ballotbox_north_big)
#this traj for the third stand in this compound that sending them to  the do5n3_renege  traj or to the balotbox
north_compound_do5n3 <- trajectory("north_compound_do5n3") %>%
  set_global("come_to_left",function() get_stands_north_api(get_attribute(election,"initial_opinion"),3), mod= "+") %>%
  simmer::select( resources = function() get_stands_north(get_attribute(election,"initial_opinion"),3), id = 6) %>%
  seize_selected(amount = 1, continue = FALSE, id = 6, reject = trajectory() %>% branch(option = function() get_attribute(election,"voter_type"),
                                                                                        c(F,F),ballotbox_north_small_1,ballotbox_north_big)) %>%
  renege_in(t = function()isRenege(get_attribute(election,"right_prob_origin"),get_attribute(election,"center_prob_origin"),
                                   get_attribute(election,"left_prob_origin"), get_attribute(election,"place2")), out = north_compound_do5n3_renege) %>%
  timeout(function () runif(1, 2, 6)) %>%
  renege_abort() %>%
  release_selected(amount = 1, id = 6) %>%
  set_attribute(key =  c("right_prob","center_prob","left_prob"),
                value = function() change_probs_func_not_renege(get_attribute(election,"right_prob"),get_attribute(election,"center_prob"),
                                                                get_attribute(election,"left_prob"),get_attribute(election,"place3"))) %>%
  branch(option = function() get_attribute(election,"voter_type"),
         c(F,F),ballotbox_north_small_1,ballotbox_north_big)


#this traj for the voters who renged from the second do5n and here happen the prop changing
north_compound_do5n2_renege <- trajectory("north_compound_do5n2_renege") %>%
  set_global("renege",function() get_stands_north_api(get_attribute(election,"initial_opinion"),2), mod= "+") %>%
  set_attribute(key =  c("right_prob","center_prob","left_prob"),
                value = function() change_probs_func_renege(get_attribute(election,"right_prob"),get_attribute(election,"center_prob"),
                                                            get_attribute(election,"left_prob"),get_attribute(election,"place2"))) %>%
  join(north_compound_do5n3)
#this traj for the secnd stand in this compound that sending them to the do5n3 taj or the do5n2_renege  traj
north_compound_do5n2 <- trajectory("north_compound_do5n2") %>%
  set_global("come_to_left",function() get_stands_north_api(get_attribute(election,"initial_opinion"),2), mod= "+") %>%
  simmer::select( resources = function() get_stands_north(get_attribute(election,"initial_opinion"),2), id = 5) %>%
  seize_selected(amount = 1, continue = FALSE, id = 5, reject = north_compound_do5n3) %>%
  renege_in(
    t = function()isRenege(get_attribute(election,"right_prob_origin"),get_attribute(election,"center_prob_origin"),
                           get_attribute(election,"left_prob_origin"), get_attribute(election,"place2")), out = north_compound_do5n2_renege) %>%
  timeout(function () runif(1, 2, 6)) %>%
  renege_abort() %>%
  release_selected(amount = 1, id = 5) %>%
  set_attribute(key =  c("right_prob","center_prob","left_prob"),
                value = function() change_probs_func_not_renege(get_attribute(election,"right_prob"),get_attribute(election,"center_prob"),
                                                                get_attribute(election,"left_prob"),get_attribute(election,"place2"))) %>%
  join(north_compound_do5n3)

#this traj for the voters who renged from the first do5n and here happen the prop changing
north_compound_do5n1_renege <- trajectory("north_compound_do5n1_renege") %>%
  set_global("renege",function() get_stands_north_api(get_attribute(election,"initial_opinion"),1), mod= "+") %>%
  set_attribute(key =  c("right_prob","center_prob","left_prob"),
                value = function() change_probs_func_renege(get_attribute(election,"right_prob"),get_attribute(election,"center_prob"),
                                                            get_attribute(election,"left_prob"),get_attribute(election,"place1"))) %>%
  join(north_compound_do5n2)
#this traj for the first stand in this compound that sending them to the do5n2 taj or the do5n1_renege  traj
north_compound_do5n1 <- trajectory("north_compound_do5n1") %>%
  set_global("come_to_left",function() get_stands_north_api(get_attribute(election,"initial_opinion"),1), mod= "+") %>%
  simmer::select( resources = function() get_stands_north(get_attribute(election,"initial_opinion"),1), id = 4) %>%
  seize_selected(amount = 1, continue = FALSE, id = 4, reject = north_compound_do5n2) %>%
  renege_in(
    t = function()isRenege(get_attribute(election,"right_prob_origin"),get_attribute(election,"center_prob_origin"),
                           get_attribute(election,"left_prob_origin"), get_attribute(election,"place1")), out = north_compound_do5n1_renege) %>%
  timeout(function () runif(1, 2, 6)) %>%
  renege_abort() %>%
  release_selected(amount = 1, id = 4) %>%
  set_attribute(key =  c("right_prob","center_prob","left_prob"),
                value = function() change_probs_func_not_renege(get_attribute(election,"right_prob"),get_attribute(election,"center_prob"),
                                                                get_attribute(election,"left_prob"),get_attribute(election,"place1"))) %>%
  join(north_compound_do5n2)


################################################################################
################################################################################
################################################################################

ballotbox_center_big_1 <- trajectory("ballotbox_center_big_1") %>% 
  addService(sname = "resource_ballotbox_center_big1", timeDist =function() rexp(1,3/2)+trimmedNorm(1,6/60)) %>%
  join(voting)
ballotbox_center_big_2 <- trajectory("ballotbox_center_big_2") %>%
  addService(sname = "resource_ballotbox_center_big2", timeDist =function() rexp(1,3/2)+trimmedNorm(1,6/60)) %>%
  join(voting)
ballotbox_center_big_3 <- trajectory("ballotbox_center_big_3") %>%
  addService(sname = "resource_ballotbox_center_big3", timeDist =function() rexp(1,3/2)+trimmedNorm(1,6/60)) %>%
  join(voting)
ballotbox_center_big_4 <- trajectory("ballotbox_center_big_4") %>%
  addService(sname = "resource_ballotbox_center_big4", timeDist =function() rexp(1,3/2)+trimmedNorm(1,6/60)) %>%
  join(voting)
ballotbox_center_big_5 <- trajectory("ballotbox_center_big_5") %>%
  addService(sname = "resource_ballotbox_center_big5", timeDist =function() rexp(1,3/2)+trimmedNorm(1,6/60)) %>%
  join(voting)
ballotbox_center_big_6 <- trajectory("ballotbox_center_big_6") %>%
  addService(sname = "resource_ballotbox_center_big6", timeDist =function() rexp(1,3/2)+trimmedNorm(1,6/60)) %>%
  join(voting)
ballotbox_center_big <- trajectory("ballotbox_center_big") %>%
  branch(option=function() { rdiscrete (1, c(0,1/6,1/6,1/6,1/6,1/6,1/6),c(0,1,2,3,4,5,6)) },
         c(F,F,F,F,F,F),ballotbox_center_big_1,ballotbox_center_big_2,ballotbox_center_big_3,
         ballotbox_center_big_4,ballotbox_center_big_5,ballotbox_center_big_6)

ballotbox_center_small_1 <- trajectory("ballotbox_center_small_1") %>%
  addService(sname = "resource_ballotbox_center_small1", timeDist =function() rexp(1,3/2)+trimmedNorm(1,6/60)) %>%
  join(voting)
ballotbox_center_small_2 <- trajectory("ballotbox_center_small_2") %>%
  addService(sname = "resource_ballotbox_center_small2", timeDist =function() rexp(1,3/2)+trimmedNorm(1,6/60)) %>%
  join(voting)
ballotbox_center_small_3 <- trajectory("ballotbox_center_small_3") %>%
  addService(sname = "resource_ballotbox_center_small3", timeDist =function() rexp(1,3/2)+trimmedNorm(1,6/60)) %>%
  join(voting)
ballotbox_center_small <- trajectory("ballotbox_center_small") %>%
  branch(option=function() { rdiscrete (1, c(0,1/3,1/3,1/3),c(0,1,2,3)) },
         c(F,F,F),ballotbox_center_small_1,ballotbox_center_small_2,ballotbox_center_small_3)

###


############################ #compounds trajectories.
#this traj for the voters who renged from the third do5n and here happen the prop changing
center_compound_do5n3_renege <- trajectory("center_compound_do5n3_renege") %>%
  set_global("renege",function() get_stands_center_api(get_attribute(election,"initial_opinion"),3), mod= "+") %>%
  set_attribute(key =  c("right_prob","center_prob","left_prob"),
                value = function() change_probs_func_renege(get_attribute(election,"right_prob"),get_attribute(election,"center_prob"),
                                                            get_attribute(election,"left_prob"),get_attribute(election,"place3"))) %>%
  branch(option = function() get_attribute(election,"voter_type"),
         c(F,F),ballotbox_center_small,ballotbox_center_big)
#this traj for the third stand in this compound that sending them to  the do5n3_renege  traj or to the balotbox
center_compound_do5n3 <- trajectory("center_compound_do5n3") %>%
  set_global("come_to_left",function() get_stands_center_api(get_attribute(election,"initial_opinion"),3), mod= "+") %>%
  simmer::select( resources = function() get_stands_center(get_attribute(election,"initial_opinion"),3), id = 3) %>%
  seize_selected(amount = 1, continue = FALSE, id = 3, reject = trajectory() %>% branch(option = function() get_attribute(election,"voter_type"),
                                                                                        c(F,F),ballotbox_center_small,ballotbox_center_big)) %>%
  renege_in(
    t = function()isRenege_7alufa1(get_attribute(election,"right_prob_origin"),get_attribute(election,"center_prob_origin"),
                                   get_attribute(election,"left_prob_origin"), get_attribute(election,"place2"),get_seized_selected(election,id=3)), out = center_compound_do5n3_renege) %>%
  timeout(function () runif(1, 2, 6)) %>%
  renege_abort() %>%
  release_selected(amount = 1, id = 3) %>%
  set_attribute(key =  c("right_prob","center_prob","left_prob"),
                value = function() change_probs_func_not_renege_7alufa1(get_attribute(election,"right_prob"),get_attribute(election,"center_prob"),
                                                                        get_attribute(election,"left_prob"),get_attribute(election,"place3"),get_seized_selected(election,id=3))) %>%
  branch(option = function() get_attribute(election,"voter_type"),
         c(F,F),ballotbox_center_small,ballotbox_center_big)


#this traj for the voters who renged from the second do5n and here happen the prop changing
center_compound_do5n2_renege <- trajectory("center_compound_do5n2_renege") %>%
  set_global("renege",function() get_stands_center_api(get_attribute(election,"initial_opinion"),2), mod= "+") %>%
  set_attribute(key =  c("right_prob","center_prob","left_prob"),
                value = function() change_probs_func_renege(get_attribute(election,"right_prob"),get_attribute(election,"center_prob"),
                                                            get_attribute(election,"left_prob"),get_attribute(election,"place2"))) %>%
  join(center_compound_do5n3)
#this traj for the secnd stand in this compound that sending them to the do5n3 taj or the do5n2_renege  traj
center_compound_do5n2 <- trajectory("center_compound_do5n2") %>%
  set_global("come_to_left",function() get_stands_center_api(get_attribute(election,"initial_opinion"),2), mod= "+") %>%
  simmer::select( resources = function() get_stands_center(get_attribute(election,"initial_opinion"),2), id = 2) %>%
  seize_selected(amount = 1, continue = FALSE, id = 2, reject = center_compound_do5n3) %>%
  renege_in(
    t = function()isRenege_7alufa1(get_attribute(election,"right_prob_origin"),get_attribute(election,"center_prob_origin"),
                                   get_attribute(election,"left_prob_origin"), get_attribute(election,"place2"),get_seized_selected(election,id=2)), out = center_compound_do5n2_renege) %>%
  timeout(function () runif(1, 2, 6)) %>%
  renege_abort() %>%
  release_selected(amount = 1, id = 2) %>%
  set_attribute(key =  c("right_prob","center_prob","left_prob"),
                value = function() change_probs_func_not_renege_7alufa1(get_attribute(election,"right_prob"),get_attribute(election,"center_prob"),
                                                                        get_attribute(election,"left_prob"),get_attribute(election,"place2"),get_seized_selected(election,id=2))) %>%
  join(center_compound_do5n3)



####################
####################
####################
#this traj for the voters who renged from the first do5n and here happen the prop changing
center_compound_do5n1_renege <- trajectory("center_compound_do5n1_renege") %>%
  set_global("renege",function() get_stands_center_api(get_attribute(election,"initial_opinion"),1), mod= "+") %>%
  set_attribute(key =  c("right_prob","center_prob","left_prob"),
                value = function() change_probs_func_renege(get_attribute(election,"right_prob"),get_attribute(election,"center_prob"),
                                                            get_attribute(election,"left_prob"),get_attribute(election,"place1"))) %>%
  join(center_compound_do5n2)
#this traj for the first stand in this compound that sending them to the do5n2 taj or the do5n1_renege  traj
center_compound_do5n1 <- trajectory("center_compound_do5n1") %>%
  set_global("come_to_left",function() get_stands_center_api(get_attribute(election,"initial_opinion"),1), mod= "+") %>%
  simmer::select( resources = function() get_stands_center(get_attribute(election,"initial_opinion"),1), id = 1) %>%
  seize_selected(amount = 1, continue = FALSE, id = 1, reject = center_compound_do5n2) %>%
  renege_in(
    t = function()isRenege_7alufa1(get_attribute(election,"right_prob_origin"),get_attribute(election,"center_prob_origin"),
                                   get_attribute(election,"left_prob_origin"), get_attribute(election,"place1"),get_seized_selected(election,id=1)), out = center_compound_do5n1_renege) %>%
  timeout(function () runif(1, 2, 6)) %>%
  renege_abort() %>%
  release_selected(amount = 1, id = 1) %>%
  set_global(keys="not_renege",value= +1, mod= "+", init = 0) %>%
  set_attribute(key =  c("right_prob","center_prob","left_prob"),
                value = function() change_probs_func_not_renege_7alufa1(get_attribute(election,"right_prob"),get_attribute(election,"center_prob"),
                                                                        get_attribute(election,"left_prob"),get_attribute(election,"place1"),get_seized_selected(election,id=1))) %>%
  join(center_compound_do5n2)




############################################# # setups
#these setup trajectoyes foe dividing the voter to the 3 compounds
setup_big <- trajectory("setup_big") %>%
  set_attribute("voter_type",2) %>%
  set_attribute(key=c("initial_opinion","right_prob","center_prob","left_prob"),
                value=function() initialOpinion_road_probFunc())%>%
  set_attribute(key=c("right_prob_origin","center_prob_origin","left_prob_origin"),
                value=function() c(get_attribute(election,"right_prob"),get_attribute(election,"center_prob"),get_attribute(election,"left_prob")))%>%
  set_attribute(key = c("place1","place2","place3"), 
                value = function() get_places(get_attribute(election,"initial_opinion"))) %>%
  branch(option=function() { rdiscrete (1, c(0,0.3,0.4,0.3),c(0,1,2,3)) },
         c(F,F,F),north_compound_do5n1,center_compound_do5n1,south_compound_do5n1)

setup_small <- trajectory("setup_small") %>%
  set_attribute("voter_type",1) %>%
  set_attribute(key=c("initial_opinion","right_prob","center_prob","left_prob"),
                value=function() initialOpinion_road_probFunc())%>%
  set_attribute(key=c("right_prob_origin","center_prob_origin","left_prob_origin"),
                value=function() c(get_attribute(election,"right_prob"),get_attribute(election,"center_prob"),get_attribute(election,"left_prob")))%>%
  set_attribute(key = c("place1","place2","place3"), 
                value = function() get_places(get_attribute(election,"initial_opinion"))) %>%
  branch(option=function() { rdiscrete (1, c(0,0.3,0.4,0.3),c(0,1,2,3)) },
         c(F,F,F),north_compound_do5n1,center_compound_do5n1,south_compound_do5n1)

setup_disability <- trajectory("setup_disability") %>%
  set_attribute("voter_type",2) %>%
  set_attribute(key=c("initial_opinion","right_prob","center_prob","left_prob"),
                value=function() initialOpinion_road_probFunc())%>%
  set_attribute(key=c("right_prob_origin","center_prob_origin","left_prob_origin"),
                value=function() c(get_attribute(election,"right_prob"),get_attribute(election,"center_prob"),get_attribute(election,"left_prob")))%>%
  set_attribute(key = c("place1","place2","place3"), 
                value = function() get_places(get_attribute(election,"initial_opinion"))) %>%
  branch(option=function() { rdiscrete (1, c(0,0.3,0.4,0.3),c(0,1,2,3)) },
         c(F,F,F),north_compound_do5n1,center_compound_do5n1,south_compound_do5n1)


################################
################################
################################
#the checker trjectories who comes every 3 hours for each compound 
small_ballotbox_center_check <- trajectory("small_ballotbox_center_check") %>%
  set_capacity(resource = "resource_ballotbox_center_small1",value = 0)%>%
  set_queue_size(resource = "resource_ballotbox_center_small1",value = 0)%>%
  timeout(function() rtriangle(n=1,a=3,b=15,c=9)) %>%
  set_capacity(resource = "resource_ballotbox_center_small1",value = 1)%>%
  set_queue_size(resource = "resource_ballotbox_center_small1",value = Inf) %>%
  set_capacity(resource = "resource_ballotbox_center_small2",value = 0)%>%
  set_queue_size(resource = "resource_ballotbox_center_small2",value = 0)%>%
  timeout(function() rtriangle(n=1,a=3,b=15,c=9)) %>%
  set_capacity(resource = "resource_ballotbox_center_small2",value = 1)%>%
  set_queue_size(resource = "resource_ballotbox_center_small2",value = Inf) %>%
  set_capacity(resource = "resource_ballotbox_center_small3",value = 0)%>%
  set_queue_size(resource = "resource_ballotbox_center_small3",value = 0)%>%
  timeout(function() rtriangle(n=1,a=3,b=15,c=9)) %>%
  set_capacity(resource = "resource_ballotbox_center_small3",value = 1)%>%
  set_queue_size(resource = "resource_ballotbox_center_small3",value = Inf)

big_ballotbox_center_check <- trajectory("big_ballotbox_center_check") %>%
  set_capacity(resource = "resource_ballotbox_center_big1",value = 0)%>%
  set_queue_size(resource = "resource_ballotbox_center_big1",value = 0)%>%
  timeout(function() rtriangle(n=1,a=3,b=15,c=9)) %>%
  set_capacity(resource = "resource_ballotbox_center_big1",value = 1)%>%
  set_queue_size(resource = "resource_ballotbox_center_big1",value = Inf) %>%
  set_capacity(resource = "resource_ballotbox_center_big2",value = 0)%>%
  set_queue_size(resource = "resource_ballotbox_center_big2",value = 0)%>%
  timeout(function() rtriangle(n=1,a=3,b=15,c=9)) %>%
  set_capacity(resource = "resource_ballotbox_center_big2",value = 1)%>%
  set_queue_size(resource = "resource_ballotbox_center_big2",value = Inf) %>%
  set_capacity(resource = "resource_ballotbox_center_big3",value = 0)%>%
  set_queue_size(resource = "resource_ballotbox_center_big3",value = 0)%>%
  timeout(function() rtriangle(n=1,a=3,b=15,c=9)) %>%
  set_capacity(resource = "resource_ballotbox_center_big3",value = 1)%>%
  set_queue_size(resource = "resource_ballotbox_center_big3",value = Inf) %>%
  set_capacity(resource = "resource_ballotbox_center_big4",value = 0)%>%
  set_queue_size(resource = "resource_ballotbox_center_big4",value = 0)%>%
  timeout(function() rtriangle(n=1,a=3,b=15,c=9)) %>%
  set_capacity(resource = "resource_ballotbox_center_big4",value = 1)%>%
  set_queue_size(resource = "resource_ballotbox_center_big4",value = Inf) %>%
  set_capacity(resource = "resource_ballotbox_center_big5",value = 0)%>%
  set_queue_size(resource = "resource_ballotbox_center_big5",value = 0)%>%
  timeout(function() rtriangle(n=1,a=3,b=15,c=9)) %>%
  set_capacity(resource = "resource_ballotbox_center_big5",value = 1)%>%
  set_queue_size(resource = "resource_ballotbox_center_big5",value = Inf) %>%
  set_capacity(resource = "resource_ballotbox_center_big6",value = 0)%>%
  set_queue_size(resource = "resource_ballotbox_center_big6",value = 0)%>%
  timeout(function() rtriangle(n=1,a=3,b=15,c=9)) %>%
  set_capacity(resource = "resource_ballotbox_center_big6",value = 1)%>%
  set_queue_size(resource = "resource_ballotbox_center_big6",value = Inf)
##################
small_ballotbox_north_check <- trajectory("small_ballotbox_north_check") %>%
  set_capacity(resource = "resource_ballotbox_north_small1",value = 0)%>%
  set_queue_size(resource = "resource_ballotbox_north_small1",value = 0)%>%
  timeout(function() rtriangle(n=1,a=3,b=15,c=9)) %>%
  set_capacity(resource = "resource_ballotbox_north_small1",value = 1)%>%
  set_queue_size(resource = "resource_ballotbox_north_small1",value = Inf)


big_ballotbox_north_check <- trajectory("big_ballotbox_north_check") %>%
  set_capacity(resource = "resource_ballotbox_north_big1",value = 0)%>%
  set_queue_size(resource = "resource_ballotbox_north_big1",value = 0)%>%
  timeout(function() rtriangle(n=1,a=3,b=15,c=9)) %>%
  set_capacity(resource = "resource_ballotbox_north_big1",value = 1)%>%
  set_queue_size(resource = "resource_ballotbox_north_big1",value = Inf) %>%
  set_capacity(resource = "resource_ballotbox_north_big2",value = 0)%>%
  set_queue_size(resource = "resource_ballotbox_north_big2",value = 0)%>%
  timeout(function() rtriangle(n=1,a=3,b=15,c=9)) %>%
  set_capacity(resource = "resource_ballotbox_north_big2",value = 1)%>%
  set_queue_size(resource = "resource_ballotbox_north_big2",value = Inf)

##############
small_ballotbox_south_check <- trajectory("small_ballotbox_south_check") %>%
  set_capacity(resource = "resource_ballotbox_south_small1",value = 0)%>%
  set_queue_size(resource = "resource_ballotbox_south_small1",value = 0)%>%
  timeout(function() rtriangle(n=1,a=3,b=15,c=9)) %>%
  set_capacity(resource = "resource_ballotbox_south_small1",value = 1)%>%
  set_queue_size(resource = "resource_ballotbox_south_small1",value = Inf) %>%
  set_capacity(resource = "resource_ballotbox_south_small2",value = 0)%>%
  set_queue_size(resource = "resource_ballotbox_south_small2",value = 0)%>%
  timeout(function() rtriangle(n=1,a=3,b=15,c=9)) %>%
  set_capacity(resource = "resource_ballotbox_south_small2",value = 1)%>%
  set_queue_size(resource = "resource_ballotbox_south_small2",value = Inf)

big_ballotbox_south_check <- trajectory("big_ballotbox_south_check") %>%
  set_capacity(resource = "resource_ballotbox_south_big1",value = 0)%>%
  set_queue_size(resource = "resource_ballotbox_south_big1",value = 0)%>%
  timeout(function() rtriangle(n=1,a=3,b=15,c=9)) %>%
  set_capacity(resource = "resource_ballotbox_south_big1",value = 1)%>%
  set_queue_size(resource = "resource_ballotbox_south_big1",value = Inf) %>%
  set_capacity(resource = "resource_ballotbox_south_big2",value = 0)%>%
  set_queue_size(resource = "resource_ballotbox_south_big2",value = 0)%>%
  timeout(function() rtriangle(n=1,a=3,b=15,c=9)) %>%
  set_capacity(resource = "resource_ballotbox_south_big2",value = 1)%>%
  set_queue_size(resource = "resource_ballotbox_south_big2",value = Inf) %>%
  set_capacity(resource = "resource_ballotbox_south_big3",value = 0)%>%
  set_queue_size(resource = "resource_ballotbox_south_big3",value = 0)%>%
  timeout(function() rtriangle(n=1,a=3,b=15,c=9)) %>%
  set_capacity(resource = "resource_ballotbox_south_big3",value = 1)%>%
  set_queue_size(resource = "resource_ballotbox_south_big3",value = Inf) %>%
  set_capacity(resource = "resource_ballotbox_south_big4",value = 0)%>%
  set_queue_size(resource = "resource_ballotbox_south_big4",value = 0)%>%
  timeout(function() rtriangle(n=1,a=3,b=15,c=9)) %>%
  set_capacity(resource = "resource_ballotbox_south_big4",value = 1)%>%
  set_queue_size(resource = "resource_ballotbox_south_big4",value = Inf)  

checker_small_kalpi_traj <- trajectory("checker_small_kalpi_traj") %>%
  clone(n=3,small_ballotbox_south_check,small_ballotbox_center_check,small_ballotbox_north_check)

checker_big_kalpi_traj <- trajectory("checker_big_kalpi_traj") %>%
  clone(n=3,big_ballotbox_south_check,big_ballotbox_center_check,big_ballotbox_north_check)


#################
#these trjectoryes for cheating after the sim
cheat_traj_center <- trajectory("cheat_traj_center") %>%
  branch(function() rdiscrete(1,c(0.4,0.6),c(0,1)),c(F),trajectory() %>% log_("no chating in center")) %>%
  set_global(keys= function() rdiscrete(1,c(1/3,1/3,1/3),c("voting_to_right","voting_to_center","voting_to_left")),value= +50, mod= "+", init = 0)

cheat_traj_south <- trajectory("cheat_traj_south") %>%
  branch(function() rdiscrete(1,c(0.4,0.6),c(0,1)),c(F),trajectory() %>% log_("no chating in south")) %>%
  set_global(keys= function() rdiscrete(1,c(1/3,1/3,1/3),c("voting_to_right","voting_to_center","voting_to_left")),value= +50, mod= "+", init = 0)

cheat_traj_north <- trajectory("cheat_traj_north") %>%
  branch(function() rdiscrete(1,c(0.4,0.6),c(0,1)),c(F),trajectory() %>% log_("no chating in north")) %>%
  set_global(keys= function() rdiscrete(1,c(1/3,1/3,1/3),c("voting_to_right","voting_to_center","voting_to_left")),value= +50, mod= "+", init = 0)

cheat_traj <- trajectory("cheat_traj") %>%
  clone(n=3, cheat_traj_center, cheat_traj_south, cheat_traj_north)
################
voting_results_traj <- trajectory("voting_results_traj") %>%
  log_(function() paste("right voters: ", get_global(election,"voting_to_right")) ) %>%
  log_(function() paste("center voters: ", get_global(election,"voting_to_center")) ) %>%
  log_(function() paste("left voters: ", get_global(election,"voting_to_left")) ) 

close_big_ballotbox_traj <- trajectory("close_big_ballotbox_traj") %>%
  set_capacity(resource = "resource_ballotbox_center_big1",value = 0)%>%
  set_queue_size(resource = "resource_ballotbox_center_big1",value = 0)%>%
  set_capacity(resource = "resource_ballotbox_center_big2",value = 0)%>%
  set_queue_size(resource = "resource_ballotbox_center_big2",value = 0)%>%
  set_capacity(resource = "resource_ballotbox_center_big3",value = 0)%>%
  set_queue_size(resource = "resource_ballotbox_center_big3",value = 0)%>%
  set_capacity(resource = "resource_ballotbox_center_big4",value = 0)%>%
  set_queue_size(resource = "resource_ballotbox_center_big4",value = 0)%>%
  set_capacity(resource = "resource_ballotbox_center_big5",value = 0)%>%
  set_queue_size(resource = "resource_ballotbox_center_big5",value = 0)%>%
  set_capacity(resource = "resource_ballotbox_center_big6",value = 0)%>%
  set_queue_size(resource = "resource_ballotbox_center_big6",value = 0)%>%
  
  set_capacity(resource = "resource_ballotbox_north_big1",value = 0)%>%
  set_queue_size(resource = "resource_ballotbox_north_big1",value = 0)%>%
  set_capacity(resource = "resource_ballotbox_north_big2",value = 0)%>%
  set_queue_size(resource = "resource_ballotbox_north_big2",value = 0)%>%
  
  set_capacity(resource = "resource_ballotbox_south_big1",value = 0)%>%
  set_queue_size(resource = "resource_ballotbox_south_big1",value = 0)%>%
  set_capacity(resource = "resource_ballotbox_south_big2",value = 0)%>%
  set_queue_size(resource = "resource_ballotbox_south_big2",value = 0)%>%
  set_capacity(resource = "resource_ballotbox_south_big3",value = 0)%>%
  set_queue_size(resource = "resource_ballotbox_south_big3",value = 0)%>%
  set_capacity(resource = "resource_ballotbox_south_big4",value = 0)%>%
  set_queue_size(resource = "resource_ballotbox_south_big4",value = 0)


close_small_ballotbox_traj <- trajectory("close_small_ballotbox_traj") %>%
  set_capacity(resource = "resource_ballotbox_center_small1",value = 0)%>%
  set_queue_size(resource = "resource_ballotbox_center_small1",value = 0)%>%
  set_capacity(resource = "resource_ballotbox_center_small2",value = 0)%>%
  set_queue_size(resource = "resource_ballotbox_center_small2",value = 0)%>%
  set_capacity(resource = "resource_ballotbox_center_small3",value = 0)%>%
  set_queue_size(resource = "resource_ballotbox_center_small3",value = 0)%>%
  set_capacity(resource = "resource_ballotbox_south_small1",value = 0)%>%
  set_queue_size(resource = "resource_ballotbox_south_small1",value = 0)%>%
  set_capacity(resource = "resource_ballotbox_south_small2",value = 0)%>%
  set_queue_size(resource = "resource_ballotbox_south_small2",value = 0)%>%
  set_capacity(resource = "resource_ballotbox_north_small1",value = 0)%>%
  set_queue_size(resource = "resource_ballotbox_north_small1",value = 0)

#this traj for showing the measurements result in a table for each replication
results_traj <- trajectory("results_traj") %>%
  set_attribute("KPI1",function() kpi_1(get_global(election,"voting_to_left"),get_global(election,"voting_to_center"),
                                        get_global(election,"voting_to_right"))) %>%
  set_attribute("KPI2",function() kpi_2(get_global(election,"renege"),get_global(election,"come_to_left")))


##---------- 5.  All Generators, ALWAYS LAST. ----------##

election %>%
  add_generator("voter_big", setup_big, distribution =  to(stop_time=15*60, dist=function() rexp(1,rate=1.77514)),mon = 2, priority = 1) %>%
  add_generator("voter_small", setup_small, distribution = from_to(start_time=60, stop_time=780, dist=function() rexp(1,rate=1/1.11)),mon = 2, priority = 2) %>%
  add_generator("voter_disability", setup_disability, distribution = function() rexp(1, 0.4457),mon = 2, priority = 2) %>%
  add_generator("checkers_big_kalpi", checker_big_kalpi_traj, distribution  = at(seq(0,900,by=180)),mon = 2,priority = 3) %>%
  add_generator("checkers_small_kalpi", checker_small_kalpi_traj, distribution = at(seq(60,900,by=180)),mon = 2,priority = 3) %>%
  add_generator("voting_results", voting_results_traj, distribution = at(15*60+20) ,mon = 2) %>%
  add_generator("cheaters", cheat_traj, distribution = at(15*60+5) ,mon = 2) %>%
  add_generator("close_big_ballotbox", close_big_ballotbox_traj, distribution = at(15*60) ,mon = 2) %>%
  add_generator("close_small_ballotbox", close_small_ballotbox_traj, distribution = at(13*60) ,mon = 2) %>%
  add_generator("results", results_traj, distribution = at(15*60+20) ,mon = 2)


##---------- 6.  reset, run, plots, outputs ----------##



mm3envs <- mclapply(1:25, function(i) {
  set.seed(9*i+100)
  reset(election)%>%run(until=simulationTime) %>%
    wrap()
})


#########################################
######################################### KPI's
#########################################

el3_3 <- get_mon_attributes(mm3envs)

vote_to_left_prob3 <-sqldf("select value as left_voters_prob_global
                    from el3_3
                   where key=='KPI1'
                    group by replication")[,1]

reneged_from_left_prob3 <-sqldf("select value as reneged_from_left_do5n
                    from el3_3
                   where key=='KPI2'
                    group by replication")[,1]



# Export the data so we cab import it back into the Rms file for further analysis
data3 <- data.frame(vote_to_left_prob3,reneged_from_left_prob3)

write.csv(data3, "data3.csv")


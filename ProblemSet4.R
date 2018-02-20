############################
###### Problem Set 4 #######
############################
#### Benjamin Schneider ####
############################

###GETTING STARTED###
#Debug this code:
myFunction<-function(doorthing, doorthing2, x){
  doorthing1<-doorthing2<-sample(1:3, 1)
  if (doorthing1==doorthing2){ x<-TRUE } else { x==FALSE }
  x
}
myFunction(sample(1:3, 1), sample(1:3, 1))
debug(myFunction)

myFunction<-function(doorthing1, doorthing2){
  if (doorthing1==doorthing2){ x<-TRUE } else { x==FALSE }
  return(x)
}
door1<-sample(1:3, 1)
door2<-sample(1:3, 1)
myFunction(door1, door2)

# Should return a TRUE if these samples are equal and
# a false if they are not

###MOVING ON###

##Problem 1

chosenDoor<-sample(1:3,1)
carDoor<-sample(1:3,1)
switch<-c(TRUE,FALSE)

setClass(Class="Door", #Here we set up the class 4 object
         representation=representation(
           chosenDoor = "integer",
           carDoor="integer",
           switch="logical"),
         prototype=prototype(
           chosenDoor=integer(),
           carDoor=integer(),
           switch=logical()))

#now we need to do a validity test to make sure that we are only putting in values that make sense
setValidity("Door", function(object){
  chosenDoortest<-(is.integer(chosenDoor))
  carDoortest<-is.integer(carDoor)
  switchtest<-is.logical(switch)
  if(!chosenDoortest || !carDoortest || !switchtest){return("Not a valid input")}
} )

door1<-new("Door", chosenDoor=3L, carDoor=3L, switch=TRUE) #This one works!
door2<-new("Door", chosenDoor=3, carDoor=3, switch=4) #This causes all of the red alerts to go off.

##Problem 2

###SIMULATION###

##Problem 1

##Problem 2

##Problem 3
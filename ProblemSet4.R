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

myFunction(sample(1:3, 1), sample(1:3, 1)) #so as of right now, we can see that this function always returns true. This is not correct.
debug(myFunction) #using the debug fucntion to go line by line and figure out where things went wrong
myFunction(sample(1:3, 1), sample(1:3, 1)) #we have to run the code again to actuall do the debugging

#This is my corrected function. I used the debug function a couple times to make sure everything was ok.
myFunction<-function(doorthing1, doorthing2){
  if (doorthing1==doorthing2){ x<-TRUE } else { x<-FALSE } #the == was a big culprit and was making things wacky
  return(x)
}
door1<-sample(1:3, 1) #I made external objects to put in that can easily be tracked so we know if the function is returning what we want it to
door2<-sample(1:3, 1) #That way I can check that the function will return both TRUE and FALSE values 
myFunction(door1, door2)#depending on if we can track in the global environment what door1 and door2 are, this should return True and False appropriately

debug(myFunction) #going to run the debug again to just make sure everything is ok and be extra thorough
myFunction(door1, door2) #success!

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
  chosenDoortest<-(is.integer(chosenDoor)) #these are integers
  carDoortest<-is.integer(carDoor) #these are integers
  switchtest<-is.logical(switch) #this is Boolean
  if(!chosenDoortest || !carDoortest || !switchtest){return("Not a valid input")}
} )

door1<-new("Door", chosenDoor=3L, carDoor=3L, switch=TRUE) #This one works!
door2<-new("Door", chosenDoor=3, carDoor=3, switch=4) #This causes all of the red alerts to go off.

##Problem 2
#So for this we first need to set a method for our class This is just extrapolationg code from past examples
setMethod("initialize", "Door", function(.Object, ...) {
  value = callNextMethod()
  validObject(value)
  return(value)
})

#here we are setting a generic function which we will expand into a S4 class function below. 
setGeneric("PlayGame", #setting our name of the generic
           function(object="Door"){ #setting the class of the generic
             standardGeneric("PlayGame")} #listing that this is a standard generic and matching the names correctly (did that wrong at first)
           )

#ok, now time to set up the method accordingly to make sure that

###SIMULATION###

##Problem 1

##Problem 2

##Problem 3
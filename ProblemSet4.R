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
} #something's not right! Something is missing! Something here just doesn't belong...

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

chosenDoor<-sample(1:3,1) #setting these up to call below
carDoor<-sample(1:3,1) #setting these up to call below
switch<-c(TRUE,FALSE) #setting these up to call below

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
}) #we have to do this in order to be able to do the generic properly

#here we are setting a generic function which we will expand into a S4 class function below. 
setGeneric("PlayGame", #setting our name of the generic
           function(object="Door"){ #setting the class of the generic
             standardGeneric("PlayGame")} #listing that this is a standard generic and matching the names correctly (did that wrong at first)
           )

#ok, now time to set up the method accordingly to make sure that we can do this function!

setMethod(f="PlayGame",signature="Door",
          def=function(object){
            chosenDoor <- object@chosenDoor #this is taking in the value of the object and breaking it down to use in function
            carDoor <- object@carDoor #this is taking in the value of the object and breaking it down to use in function
            switch <- object@switch #this is taking in the value of the object and breaking it down to use in function
            firstdraw<-as.integer(sample(1:3,1)) #this simulates our first picked door
            carDoor<-as.integer(sample(1:3,1)) #this simulates the door that the car is behind
            drawdoor<-c(firstdraw,carDoor) #This concatanates the drawn door and the door with the car as a vector 
            dooroptions<-c(1:3) #this creates a different vector that can contain the values of all three doors
            drawdoor #creating an if else for the drawdoor vector we just created we just created
            if (isTRUE(switch)==F) {finalDoor<-firstdraw} #this creates a variable of final door, because switch is false the chosen door is kept
            else { #now we are setting the else condition for when the switch is not F aka TRUE
            deletedoor<-sample(subset(dooroptions, !(dooroptions %in% drawdoor)),1) #here we are picking the door that does not have the car in it, and was not chosen by the player
            cannotchoose<-c(firstdraw,deletedoor) #this creates a vector with the door that was originally chosen and the removed door
            switchdoor<-dooroptions[!dooroptions %in% cannotchoose] #this picks out the door that is left over from above
            finalDoor<-sample(switchdoor,1)}#this draws out just a single value for the purpose of getting the door that was switched to
            if (finalDoor==carDoor){winner<-TRUE} #create an if else in order to show the win condition
            else {winner<-FALSE} #in this case the else shows that the do not win
            print(winner)
          })

game1<-new("Door", chosenDoor=1L, carDoor=2L, switch=FALSE) #Creation of a variable to plug into the game to test it
PlayGame(game1) #Yay! it worked, this will return a True if won and a false if lost

###SIMULATION###

##Problem 1

#so we are now going to use the sapply variant of the apply function to run the simulation 1000 times because we cannot use for loops!
no.switch.simulation<-sapply(1:1000, function(i){PlayGame(new("Door", chosenDoor=1L, carDoor=1L, switch=FALSE))}) #looking at the help file we know that the sapply returns us a vector of out boolean measures that we should be able to consequently manipulate
table(no.switch.simulation) #just to look at the output to check that the percentage is done correctly
no.switch.simulation<-as.data.frame(table(no.switch.simulation)) #as a data frame so the results can be taken directly from the simulation and do the math
no.switch.simulation[2,2]/10 #we can think of this as the proportion/1000*100 which simplifies as dividing by 10 for a pure percentage


##Problem 2

#this is exactly the same as above, but with the switch condition in effect. The figuring out of the percentage of the win and such is the same in spirit so refer to the documentation above
switch.simulation<-sapply(1:1000, function(i){PlayGame(new("Door", chosenDoor=1L, carDoor=1L, switch=TRUE))}) #same as above
table(switch.simulation) #same as above
switch.simulation<-as.data.frame(table(switch.simulation)) #same as above
switch.simulation[2,2]/10 #this percent is higher than the one above

##Problem 3

#In this case, we can see that the strategy with switch is definitely the superior one. Employing theories of conditional probability and of course Bayes' Rule, this definitely makes sense!
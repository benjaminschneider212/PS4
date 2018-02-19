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



##Problem 2

###SIMULATION###

##Problem 1

##Problem 2

##Problem 3
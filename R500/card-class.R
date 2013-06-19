# the card class, for R500
# alyssa frazee 6/19/2013

setClass("card", 
         representation(
           suit = "character",  		# the card's suit
           number = "character",			# the card's value (2-10, J, Q, K, A)
           trump = "logical"	# whether it falls into the called trump suit
         )
)

# card constructor:
card = function(suit, number, trump=FALSE) {
  return(new("card", suit=suit, number=number, trump=trump))
}  

# define a slot setter for trump (need to change based on bid results):
setGeneric("trump<-", function(x, value) standardGeneric("trump<-"))
setReplaceMethod("trump", "card", function(x, value) {x@trump <- value; x})

# define the slot getters:
setGeneric("suit", function(x) standardGeneric("suit"))
setMethod("suit", "card", function(x) x@suit)
setGeneric("number", function(x) standardGeneric("number"))
setMethod("number", "card", function(x) x@number)
setGeneric("trump", function(x) standardGeneric("trump"))
setMethod("trump", "card", function(x) x@trump)

# define the show method:
setMethod("show", "card", 
          function(object){
            if(number(object)=="joker") {cat("joker [trump]\n")}
            else if(trump(object)) {cat(paste(number(object), suit(object), "[trump]", "\n"))}
            else {cat(paste(number(object), suit(object), "\n"))}
          }
)


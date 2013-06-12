# coding up the game of 500 in R, just for fun!
# AF june 3, 2013
# updated again june 11, 2013

# set up an S4 class for the cards
setClass("card", 
         representation(
           suit = "character",			# the card's suit
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


# function to create a deck (of cards):
makeDeck = function(){
  deck = list() #list to hold the cards
  deck.df = expand.grid(c(4:10, "J", "Q", "K", "A"), c("spades", "clubs", "diamonds", "hearts"), stringsAsFactors = FALSE)
  names(deck.df) = c("number", "suit")
  deck.df = rbind(deck.df, c("joker", "none"))
  for(i in 1:nrow(deck.df)){
    deck[[i]] = card(suit = deck.df$suit[i], number=deck.df$number[i])
  }
  return(deck)
}

# function to sort a list of cards:
# [HELPER] function to compare two cards and see if they are the same:
compare = function(c1, c2){
  # this function recurses, WOAH
  if(class(c1)=="list"){
    return(lapply(c1, function(x) compare(x, c2)))
  }
  return(suit(c1)==suit(c2) & number(c1)==number(c2))
}

# now the sorting:
sortHand = function(hand){
  deck = makeDeck()
  inds = sapply(hand, function(x) which(compare(deck, x)==TRUE))
  return(deck[sort(inds)])
}

# function to show a hand:
showHand = function(hand){
  for(i in 1:length(hand)){
    print(hand[[i]])
  }
}

# function to divide cards into hands (or, as they say, "deal"):
deal = function(deck){
  # first: shuffle
  deck.shuffled = sample(deck)
  
  # deal the cards (threes, fours (2 to the kitty), threes) and sort the hands
  hand1 = sortHand(deck.shuffled[c(1:3, 16:19, 34:36)])
  hand2 = sortHand(deck.shuffled[c(4:6, 20:23, 37:39)])
  hand3 = sortHand(deck.shuffled[c(7:9, 24:27, 40:42)])
  dealerhand = sortHand(deck.shuffled[c(10:12, 28:31, 43:45)])
  kitty = sortHand(deck.shuffled[c(13:15, 32:33)])
  
  # return the dealt hand
  return(list(hand1=hand1, hand2=hand2, hand3=hand3, dealerhand=dealerhand, kitty=kitty))
}

deck = makeDeck()
deal(deck)



# function to compare bids:
compareBids = function(bid1, bid2){
  # bid1/bid2 are length-2 vectors of strings (num + suit), except for that you can put in NULL for bid1 and "pass" for bid2
  # want to know if bid2 is higher than bid1
  if(is.null(bid1)) return(TRUE)
  if(length(bid2)==1){
    if(bid2=="pass"){
      return(TRUE)
    }
  }
  if(bid2[1] > bid1[1]) return(TRUE)
  if(bid2[1] < bid1[1]) return(FALSE)
  if(bid2[2] == bid1[2]) return(FALSE)
  if(bid2[2] < bid1[2]) return(FALSE)
  return(TRUE)
}

# function to check the bid for rule-breaking:
checkBid = function(bid, highBid){
  
  # if you bid something weird:
  if(length(bid)!=2){
    if(bid!="pass"){
    theBid = strsplit(readline("invalid bid - try another bid: "), split=" ")[[1]]
    if(theBid[1]!="pass"){
      if(theBid[2]=="spades") theBid[2] = "aspades"
    }
    return(checkBid(theBid, highBid))
    }
  }
  
  # if you bid less than 6:
  if(bid[1] < 6){
    theBid = strsplit(readline("please bid at least 6: "), split=" ")[[1]]
    if(theBid[1]!="pass"){
      if(theBid[2]=="spades") theBid[2] = "aspades"
    }
    return(checkBid(theBid, highBid))
  }
  
  # if you bid lower than the current highest bid:
  # (NOTE THAT THIS SHOULD ONLY BE USED IN THE play500() FUNCTION)
  if(!compareBids(highBid, bid)){
    message(paste0("You must bid higher than the current high bid (",paste(highBid, collapse=" "),")"))
    theBid = strsplit(readline("new bid: "), split=" ")[[1]]
    if(theBid[1]!="pass"){
      if(theBid[2]=="spades") theBid[2] = "aspades"
    }
    return(checkBid(theBid, highBid))
  }
  
  return(bid) 
}

#############################
######## PLAY BALL!! ########
#############################


play500 = function(){
  
  deck = makeDeck()
  
  ###################################
  # deal the cards:
  hands = deal(deck)
  
  ###################################
  # have players bid:
  highBid = NULL
  firstTwo = list()
  leadPlayer = 0
  for(i in 1:4){
    if(i==4) message("[dealer]")
    message(paste0("player ", i,": here is your hand." ))
    showHand(hands[[i]])
    if(i==4 | i==3) message(paste0("your partner has bid ", paste(firstTwo[[i-2]], collapse=" ")))
    theBid = strsplit(readline("Please make your bid: "), split=" ")[[1]]
    if(theBid[1]!="pass"){
      if(theBid[2]=="spades") theBid[2] = "aspades"
    }
    theBid = checkBid(theBid, highBid)  # this will ALWAYS result in either a "pass" or a new high bid.
    
    if(length(theBid)>1){
        highBid = theBid 
        leadPlayer = i
      }
    
    # for printing only:
    if(i==2 | i==1){
      if(theBid[1]!="pass"){
        if(theBid[2]=="aspades") theBid[2] <- "spades"
      }
      firstTwo[[i]] <- theBid
    }
      
  } #end loop: finished bidding.

  
  ###################################
  # determine the winning bid:
  if(is.null(highBid)) print("Everyone has passed: re-dealing.")
  if(highBid[1]=="6") print("House rules: play only conitnues if the highest bid is at least 7.  Re-dealing.")
  
  if(highBid[2]=="aspades") highBid[2] = "spades"
  message(paste0("Player ",leadPlayer," wins the bid with ",paste(highBid, collapse=" ")))
  
  ###################################
  # winning player gets the kitty
  message(paste0("Player ", leadPlayer,": the kitty is here:"))
  showHand(hands$kitty)
  message("and again, here is your hand:")
  showHand(hands[[leadPlayer]])
  message("of these 15 cards, enter the 10 you would like to keep.")
  
  # (pick 10 cards)
  for(j in 1:10){
    cardname = strsplit(readline(paste0("card ",j,": ")),split=" ")[[1]]
    
    # if the card is the joker:
    if(length(cardname)==1){
      if(cardname != "joker") stop("what card are you trying to enter?")
      ### FIX LATER TO ALLOW PLAYER TO RE-ENTER
      hands[[leadPlayer]][[j]] = card(suit = "none", number="joker", trump=TRUE )
    }
    
    # if the card is not the joker:
    if(length(cardname)>1){
      # assign trump (dealing with low bower)
      if(cardname[2] == highBid[2]){
        isTrump = TRUE
      }else if(cardname[1]=="J" & highBid[2]=="spades" & cardname[2]=="clubs"){
        isTrump = TRUE
      }else if(cardname[1]=="J" & highBid[2]=="clubs" & cardname[2]=="spades"){
        isTrump = TRUE
      }else if(cardname[1]=="J" & highBid[2]=="hearts" & cardname[2]=="diamonds"){
        isTrump = TRUE
      }else if(cardname[1]=="J" & highBid[2]=="diamonds" & cardname[2]=="hearts"){
        isTrump = TRUE
      }else{isTrump = FALSE}
      
      hands[[leadPlayer]][[j]] = card(suit = cardname[2], number=cardname[1], trump=isTrump ) 
    }
  } # finish choosing hand
  
  showHand(hands[[leadPlayer]])
  
}











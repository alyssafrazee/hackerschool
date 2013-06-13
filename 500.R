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
makeDeck = function(trump = NULL){
  deck = list() #list to hold the cards
  deck.df = expand.grid(c(4:10, "J", "Q", "K", "A"), c("spades", "clubs", "diamonds", "hearts"), stringsAsFactors = FALSE)
  names(deck.df) = c("number", "suit")
  deck.df = rbind(deck.df, c("joker", "none"))
  for(i in 1:nrow(deck.df)){
    deck[[i]] = card(suit = deck.df$suit[i], number=deck.df$number[i])
  }
  
  if(!is.null(trump)){
    if(trump=="hearts") trumpCardInds = c(34:40, 42:44, 30, 41, 45)
    if(trump=="diamonds") trumpCardInds = c(23:29, 31:33, 41, 30, 45)
    if(trump=="spades") trumpCardInds = c(1:7, 9:11, 19, 8, 45)
    if(trump=="clubs") trumpCardInds = c(12:18, 20:22, 8, 19, 45)
    nonTrumpInds = c(1:45)[-trumpCardInds]
    for(i in trumpCardInds) trump(deck[[i]]) = TRUE
    deck = deck[c(nonTrumpInds, trumpCardInds)]
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
sortHand = function(hand, trump=NULL){
  deck = makeDeck(trump=trump)
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

#deck = makeDeck()
#deal(deck)



# function to compare bids:
compareBids = function(bid1, bid2){
  # bid1/bid2 are length-2 vectors of strings (num + suit), except for that you can put in NULL for bid1 and "pass" for bid2
  # want to know if bid2 is higher than bid1
  # relies on alphabetical order of "aspades", "clubs", "diamonds", "hearts", "notrump"
  if(is.null(bid1)) return(TRUE)
  if(length(bid2)==1){
    if(bid2=="pass"){
      return(TRUE)
    }
  }
  if(as.numeric(bid2[1]) > as.numeric(bid1[1])) return(TRUE)
  if(as.numeric(bid2[1]) < as.numeric(bid1[1])) return(FALSE)
  if(bid2[2] == bid1[2]) return(FALSE)
  if(bid2[2] < bid1[2]) return(FALSE)
  return(TRUE)
}

# function to check the bid for rule-breaking:
checkBid = function(bid, highBid){
  
  # if you bid something weird:
  ##[too long or too short]
  if(length(bid)!=2){
    if(bid[1] == "pass") return(bid)
    if(length(bid)>2 | (length(bid)==1 & bid[1]!="pass")){
      theBid = strsplit(readline("invalid bid - try again: "), split=" ")[[1]]
      if(length(theBid)>1){
        if(theBid[2]=="spades") theBid[2] = "aspades"
      }
      return(checkBid(theBid, highBid))      
    }    
  }
  
  ##[not bidding a number]
  if(suppressWarnings(is.na(as.numeric(bid[1])))){
    theBid = strsplit(readline("invalid bid (bid <NUMBER><space><SUIT>) - try again: "), split=" ")[[1]]
    if(length(theBid)>1){
      if(theBid[2]=="spades") theBid[2] = "aspades"
    }
    return(checkBid(theBid, highBid))    
  }
  
  ##[bidding an invalid suit]
  if(bid[2]!="hearts" & bid[2]!="spades" & bid[2]!="diamonds" & bid[2]!="clubs" & bid[2]!="notrump" & bid[2]!="aspades"){
    theBid = strsplit(readline("invalid bid (suits are hearts, spades, diamonds, clubs, and notrump) - try again: "), split=" ")[[1]]
    if(length(theBid)>1){
      if(theBid[2]=="spades") theBid[2] = "aspades"
    }
    return(checkBid(theBid, highBid))
  }
  
  # if you bid less than 6:
  if(bid[1] < 6){
    theBid = strsplit(readline("please bid at least 6: "), split=" ")[[1]]
    if(length(theBid)>1){
      if(theBid[2]=="spades") theBid[2] = "aspades"
    }
    return(checkBid(theBid, highBid))
  }
  
  # if you bid lower than the current highest bid:
  if(!compareBids(highBid, bid)){
    if(highBid[2]=="aspades") highBid[2] = "spades"
    message(paste0("You must bid higher than the current high bid (",paste(highBid, collapse=" "),")"))
    theBid = strsplit(readline("new bid: "), split=" ")[[1]]
    if(length(theBid)>1){
      if(theBid[2]=="spades") theBid[2] = "aspades"
    }
    return(checkBid(theBid, highBid))
  }
  
  return(bid) 
}



# function to find a given card in a hand
findCard = function(hand, cardname){
  # cardname = string, such as "joker", "A hearts", etc.
  # hand is a list of cards.
  splitCard = strsplit(cardname, split=" ")[[1]]
  if(length(splitCard) == 1){
    if(splitCard != "joker") stop("invalid card name")
    myCard = card(suit="none", number="joker")
  }else{
    myCard = card(suit=splitCard[2], number=splitCard[1])
  }
  tf = compare(hand, myCard)
  cardInd = which(tf==TRUE)
  return(cardInd)
}




#############################
######## PLAY BALL!! ########
#############################


play500 = function(){
  
  score13 = 0
  score24 = 0
  
  dealer = 4 # player 4 starts as the dealer.
  
  # create the score table (just once, before the loop)
  scoreTable = matrix(seq(140, 520, by=20), ncol=5, byrow=TRUE)
  colnames(scoreTable) = c("spades", "clubs", "diamonds", "hearts", "notrump")
  rownames(scoreTable) = c("7", "8", "9", "10")  
  
  while(score13<500 & score13>(-500) & score24<500 & score24>(-500)){
    deck = makeDeck()
  
    ###################################
    # deal the cards:
    dealer = ifelse(dealer <= 4, dealer, dealer %% 4)
    message(paste("Player",dealer,"is dealing."))
    hands = deal(deck)
  
    ###################################
    # have players bid:
    highBid = NULL
    firstTwo = list() # so that second-partner-bidders can be reminded
    firstBidders = c(1:3)+dealer
    firstBidders = sapply(firstBidders, function(x){
      if(x > 4){return(x %% 4)}; return(x)
    })
    biddingOrder = c(firstBidders, dealer)
    for(i in 1:4){
      if(i==4) message("[dealer]")
      message(paste0("player ", biddingOrder[i],": here is your hand." ))
      showHand(hands[[i]])
      if(i==4 | i==3) message(paste0("your partner has bid ", paste(firstTwo[[i-2]], collapse=" ")))
      theBid = strsplit(readline("Please make your bid: "), split=" ")[[1]]
      if(length(theBid)==2){
        if(theBid[2]=="spades") theBid[2] = "aspades"
      }
      theBid = checkBid(theBid, highBid)  # this will ALWAYS result in either a "pass" or a new high bid.
    
      if(length(theBid)>1){
        highBid = theBid 
        leadPlayer = biddingOrder[i]
      }
    
      # for printing only:
      if(i==2 | i==1){
        if(theBid[1]!="pass"){
          if(theBid[2]=="aspades") theBid[2] <- "spades"
        }
        firstTwo[[i]] <- theBid
      }
      
    } #end loop: finished bidding.
    bidWinner = leadPlayer #(leadPlayer will change later)
    print(bidWinner)

  
    ###################################
    # determine the winning bid:
    if(is.null(highBid)) print("Everyone has passed: re-dealing.")
    if(highBid[1]=="6") print("House rules: play only continues if the highest bid is at least 7.  Re-dealing.")
  
    if(highBid[2]=="aspades") highBid[2] = "spades"
    message(paste0("Player ",leadPlayer," wins the bid with ",paste(highBid, collapse=" ")))
  
    ###################################
    # winning player gets the kitty
    message(paste0("Player ", leadPlayer,": the kitty is here:"))
    showHand(hands$kitty)
    message("and again, here is your hand:")
    showHand(hands[[biddingOrder[leadPlayer]]])
    message("of these 15 cards, enter the 10 you would like to keep.")
  
    # (pick 10 cards)
    j = 1
    newHand = list() #create empty list, for his new hand
    while(j <= 10){
      cardname = strsplit(readline(paste0("card ",j,": ")),split=" ")[[1]]
    
      # if the card is the joker:
      if(length(cardname)==1){
        if(cardname != "joker"){
          message("Invalid card - try again.") 
          next
        }
        tryJoker = findCard(hands[[biddingOrder[leadPlayer]]], "joker")
        if(length(tryJoker)==0){
          tryJokerKitty = findCard(hands$kitty, "joker")
            if(length(tryJokerKitty)==0){
              message("the joker is not in your hand or the kitty - try again.")
              next
            }
        }
        alreadyJokered = findCard(newHand, "joker")
        if(length(alreadyJokered)!=0){
          message("you already have the joker in your hand - try again.")
          next
        }
        newHand[[j]] = card(suit = "none", number="joker", trump=TRUE )
      }
    
      # if the card is not the joker:
      if(length(cardname)>1){
        
        # make sure it's in this player's hand:
        tryCard = findCard(hands[[biddingOrder[leadPlayer]]], paste(cardname, collapse=" "))
        if(length(tryCard)==0){
          tryCardKitty = findCard(hands$kitty, paste(cardname, collapse=" "))
          if(length(tryCardKitty)==0){
            message("this card is not in your hand or the kitty - try again.")
            next
          }
        }
        
        # make sure he didn't already pick it:
        cardAlready = findCard(newHand, paste(cardname, collapse=" "))
        if(length(cardAlready)!=0){
          message("you already have this card in your hand - try again.")
          next
        }
        
        # assuming they've picked a valid card, assign trump (deal w/ low bower)
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
      
        newHand[[j]] = card(suit = cardname[2], number=cardname[1], trump=isTrump ) 
      }
      
      j = j+1
      
    } # finish choosing hand
    hands[[biddingOrder[leadPlayer]]] = newHand
  
    ###################################
    # sort and assign trump to each player's hand
    for(phand in 1:4) hands[[phand]] = sortHand(hands[[phand]], trump=highBid[2])
  
    ###################################
    # keep track of who takes which tricks
    numTricks13 = 0
    numTricks24 = 0
  
    ###################################
    # play the game :) 
  
    # we already have a lead player (leadPlayer = 1, 2, 3, or 4 depending on who won the bid)
    for(trick in 1:10){
      cardsPlayed = list() 
      message(paste0("Player ", leadPlayer,": here is your hand. It's your lead!"))
      showHand(sortHand(hands[[biddingOrder[leadPlayer]]], trump = highBid[2]))
      ledCard = readline("what card would you like to play? ")
      ledCardInd = findCard(hands[[biddingOrder[leadPlayer]]], ledCard)
      while(length(ledCardInd)==0){
        ledCard = readline("this card is not in your hand - choose another: ")
        ledCardInd = findCard(hands[[biddingOrder[leadPlayer]]], ledCard)
        }
      
      # add card to the middle:
      cardsPlayed = append(cardsPlayed, hands[[biddingOrder[leadPlayer]]][[ledCardInd]])
    
      # remove card from your hand:
      hands[[biddingOrder[leadPlayer]]] = hands[[biddingOrder[leadPlayer]]][-ledCardInd]
    
      # figure out which suit was led:
      ledSuit = ifelse(trump(cardsPlayed[[1]]), highBid[2], suit(cardsPlayed[[1]]))
    
      # have the other players play, following suit.
      nextPlayers = c(1:3)+leadPlayer
      nextPlayers = sapply(nextPlayers, function(x){
        if(x > 4){return(x %% 4)}; return(x)
      })
    
      for(player in nextPlayers){
        message(paste0("Player ", player,": here is your hand. It's your turn!"))
        showHand(sortHand(hands[[biddingOrder[player]]], trump = highBid[2]))
        message(paste("led:", ledCard))
        
        if(length(cardsPlayed)>1){
          for(cnum in 2:length(cardsPlayed)){
            if(number(cardsPlayed[[cnum]])=="joker"){
              message("joker")
            }else{
            message(paste(number(cardsPlayed[[cnum]]), suit(cardsPlayed[[cnum]])))
          }# end if/else
         }# end for loop
        }# end if(length(cardsPlayed)>1)
        
        chosenCard = readline("what card would you like to play? ")
        chosenCardInd = findCard(hands[[biddingOrder[player]]], chosenCard)
        while(length(chosenCardInd)==0){
          chosenCard = readline("this card is not in your hand - choose another: ")
          chosenCardInd = findCard(hands[[biddingOrder[player]]], chosenCard)
        }
        chosenCard.obj = hands[[biddingOrder[player]]][[chosenCardInd]]
      
        # did the player have any of the suit that was led?
        if(trump(cardsPlayed[[1]])){
          # if trump was led:
          howManyTrump = sum(sapply(hands[[biddingOrder[player]]], function(x) trump(x)))
          while(!trump(chosenCard.obj) & howManyTrump!=0){
            message(paste0("you must follow suit (suit led: ", ledSuit,")"))
            chosenCard = readline(paste0("please play a ", substr(ledSuit,1,nchar(ledSuit)-1),": "))
            chosenCardInd = findCard(hands[[biddingOrder[player]]], chosenCard)
            chosenCard.obj = hands[[biddingOrder[player]]][[chosenCardInd]]
          }
        }
        if(!trump(cardsPlayed[[1]])){
          # if off-suit was led:
          howManyOfSuit = sum(sapply(hands[[biddingOrder[player]]], function(x) suit(x)==ledSuit))
          while(suit(chosenCard.obj)!=ledSuit & howManyOfSuit!=0){
            message(paste0("you must follow suit (suit led: ", ledSuit,")"))
            chosenCard = readline(paste0("please play a ", substr(ledSuit,1,nchar(ledSuit)-1),": "))
            chosenCardInd = findCard(hands[[biddingOrder[player]]], chosenCard)
            chosenCard.obj = hands[[biddingOrder[player]]][[chosenCardInd]]          
          }
        }
      
        # add card to the middle:
        cardsPlayed = append(cardsPlayed, chosenCard.obj)
      
        # remove card from your hand:
        hands[[biddingOrder[player]]] = hands[[biddingOrder[player]]][-chosenCardInd]
      } # all players are done playing
    
      showHand(cardsPlayed)
    
      # figure out which card wins:
      playerList = c(leadPlayer, nextPlayers)
      trumpDeck = makeDeck(trump = highBid[2])
      trumpBool = sapply(cardsPlayed, function(x) trump(x))
      anyTrump = sum(trumpBool)
    
      if(anyTrump > 0){
        trumpInds = which(trumpBool)
        deckRank = sapply(trumpInds, function(x) findCard(trumpDeck, paste(number(cardsPlayed[[x]]), suit(cardsPlayed[[x]]))))
        winInd = trumpInds[which.max(deckRank)]
      }
    
      if(anyTrump == 0){
        followSuitInds = which(sapply(cardsPlayed, function(x) suit(x)==ledSuit))
        deckRank = sapply(followSuitInds, function(x) findCard(trumpDeck, paste(number(cardsPlayed[[x]]), suit(cardsPlayed[[x]]))))
        winInd = followSuitInds[which.max(deckRank)]
      }
    
      winningPlayer = playerList[winInd]
      winningCard = paste(number(cardsPlayed[[winInd]]), suit(cardsPlayed[[winInd]]))
      if(number(cardsPlayed[[winInd]]) == "joker") winningCard = "joker"
      message(paste0("Player ",winningPlayer," wins, with ", winningCard))
    
      if(winningPlayer==1 | winningPlayer == 3){
        numTricks13 = numTricks13 + 1
      }
      if(winningPlayer==2 | winningPlayer == 4){
        numTricks24 = numTricks24 + 1
      }
      
      leadPlayer = winningPlayer
      message("Players 1/3 have this many tricks:")
      print(numTricks13)
      message("Players 2/4 have this many tricks:")
      print(numTricks24)
    }  # finish playing tricks.
  
    ###################################
    # determine winner and return the score
    rowInd = which(rownames(scoreTable)==highBid[1])
    colInd = which(colnames(scoreTable)==highBid[2])
  
    if((bidWinner==1 | bidWinner==3) & numTricks13>=as.numeric(highBid[1])){
      message("players 1 and 3 have made their bid!")
      score13 = score13 + scoreTable[rowInd, colInd]
      score24 = score24 + 10*numTricks24
    }
    if((bidWinner==2 | bidWinner==4) & numTricks24>=as.numeric(highBid[1])){
      message("players 2 and 4 have made their bid!")
      score24 = score24 + scoreTable[rowInd, colInd]
      score13 = score13 + 10*numTricks13
    }
    if((bidWinner==1 | bidWinner==3) & numTricks13<as.numeric(highBid[1])){
      message("bummer - players 1 and 3 have been set.")
      score13 = score13 - scoreTable[rowInd, colInd]
      score24 = score24 + 10*numTricks24
    }
    if((bidWinner==2 | bidWinner==4) & numTricks24<as.numeric(highBid[1])){
      message("bummer - players 2 and 4 have been set.")
      score24 = score24 - scoreTable[rowInd, colInd]
      score13 = score13 + 10*numTricks13
    }
  
    message("score update:")
    message(paste0("players 1 and 3 have ", score13," points."))
    message(paste0("players 2 and 4 have ", score24," points."))
    
    dealer = dealer + 1
    dealer = ifelse(dealer<=4, dealer, dealer %% 4)
      
  } ### END GIANT WHILE LOOP (nobody is above 500 or below -500)
  
  
  if(score13 >= 500){
    message("Players 1 and 3 WIN!")
  }
  if(score13 <= (-500)){
    message("Players 2 and 4 win, by virtue of players 1 and 3 LOSING!")
  }
  if(score24 >= 500){
    message("Players 2 and 4 WIN!")
  }
  if(score24 <= (-500)){
    message("Players 1 and 3 win, by virtue of players 2 and 4 LOSING!")
  }
  
  message("thank you for playing!")
  
}













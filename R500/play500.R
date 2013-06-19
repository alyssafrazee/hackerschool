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
    bidObject = makeBids(dealer, hands)
    leadPlayer = bidObject$leadPlayer
    
    # check that a valid bid was actually made
    while(is.null(leadPlayer)){
      newDealer = bidObject$dealer+1
      newDealer = ifelse(newDealer == 5, 1, newDealer)
      if(!is.null(bidObject$highBid)){
        message(paste("House rules: play only continues if the highest bid is at least 7. Deal moves to player", newDealer))
      }
      if(is.null(bidObject$highBid)){
        message(paste("Everyone has passed.  Deal moves to player",newDealer))
      }
      message(paste("Player",newDealer,"is dealing."))
      hands = deal(deck)
      bidObject = makeBids(newDealer, hands)
      dealer = newDealer # to use at end of the hand
      leadPlayer = bidObject$leadPlayer
    }
    
    ###################################
    # determine the winning bid, and in which order everyone bid:
    bidWinner = leadPlayer #(leadPlayer will change in later tricks, but bidWinner needs to be stored)
    highBid = bidObject$highBid
    biddingOrder = bidObject$biddingOrder
    
    if(highBid[2]=="aspades") highBid[2] = "spades"
    message(paste0("Player ",leadPlayer," wins the bid with ",paste(highBid, collapse=" ")))
    
    ###################################
    # winning player gets the kitty
    message(paste0("Player ", leadPlayer,": the kitty is here:"))
    showHand(hands$kitty)
    message("and again, here is your hand:")
    leadHandIndex = which(biddingOrder==leadPlayer)
    showHand(hands[[leadHandIndex]])
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
        tryJoker = findCard(hands[[leadHandIndex]], "joker")
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
        tryCard = findCard(hands[[leadHandIndex]], paste(cardname, collapse=" "))
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
    hands[[leadHandIndex]] = newHand
    
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
    # OR lead player was set at the end of the previous trick.
    for(trick in 1:10){
      leadHandIndex = which(biddingOrder==leadPlayer)
      cardsPlayed = list() 
      message(paste0("Player ", leadPlayer,": here is your hand. It's your lead!"))
      showHand(sortHand(hands[[leadHandIndex]], trump = highBid[2]))
      ledCard = readline("what card would you like to play? ")
      ledCardInd = findCard(hands[[leadHandIndex]], ledCard)
      while(length(ledCardInd)==0){
        ledCard = readline("this card is not in your hand - choose another: ")
        ledCardInd = findCard(hands[[leadHandIndex]], ledCard)
      }
      
      # add card to the middle:
      cardsPlayed = append(cardsPlayed, hands[[leadHandIndex]][[ledCardInd]])
      
      # remove card from your hand:
      hands[[leadHandIndex]] = hands[[leadHandIndex]][-ledCardInd]
      
      # figure out which suit was led:
      ledSuit = ifelse(trump(cardsPlayed[[1]]), highBid[2], suit(cardsPlayed[[1]]))
      
      # have the other players play, following suit.
      nextPlayers = c(1:3)+leadPlayer
      nextPlayers = sapply(nextPlayers, function(x){
        if(x > 4){return(x %% 4)}; return(x)
      })
      
      for(player in nextPlayers){
        handIndex = which(biddingOrder == player)
        message(paste0("Player ", player,": here is your hand. It's your turn!"))
        showHand(sortHand(hands[[handIndex]], trump = highBid[2]))
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
        chosenCardInd = findCard(hands[[handIndex]], chosenCard)
        while(length(chosenCardInd)==0){
          chosenCard = readline("this card is not in your hand - choose another: ")
          chosenCardInd = findCard(hands[[handIndex]], chosenCard)
        }
        chosenCard.obj = hands[[handIndex]][[chosenCardInd]]
        
        # did the player have any of the suit that was led?
        if(trump(cardsPlayed[[1]])){
          # if trump was led:
          howManyTrump = sum(sapply(hands[[handIndex]], function(x) trump(x)))
          while(!trump(chosenCard.obj) & howManyTrump!=0){
            message(paste0("you must follow suit (suit led: ", ledSuit,")"))
            chosenCard = readline(paste0("please play a ", substr(ledSuit,1,nchar(ledSuit)-1),": "))
            chosenCardInd = findCard(hands[[handIndex]], chosenCard)
            chosenCard.obj = hands[[handIndex]][[chosenCardInd]]
          }
        }
        if(!trump(cardsPlayed[[1]])){
          # if off-suit was led:
          howManyOfSuit = sum(sapply(hands[[handIndex]], function(x) (suit(x)==ledSuit & !trump(x))))
          while(suit(chosenCard.obj)!=ledSuit & howManyOfSuit!=0){
            message(paste0("you must follow suit (suit led: ", ledSuit,")"))
            chosenCard = readline(paste0("please play a ", substr(ledSuit,1,nchar(ledSuit)-1),": "))
            chosenCardInd = findCard(hands[[handIndex]], chosenCard)
            chosenCard.obj = hands[[handIndex]][[chosenCardInd]]          
          }
        }
        
        # add card to the middle:
        cardsPlayed = append(cardsPlayed, chosenCard.obj)
        
        # remove card from your hand:
        hands[[handIndex]] = hands[[handIndex]][-chosenCardInd]
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

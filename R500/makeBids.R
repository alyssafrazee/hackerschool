### bidding action:


makeBids = function(dealer, hands){
  ## "dealer" = id of player who is now the dealer
  ## "hands" = dealt deck object
  highBid = NULL #(nobody has bid yet)
  firstTwo = list() # so that second-partner-bidders can be reminded
  firstBidders = c(1:3)+dealer
  firstBidders = sapply(firstBidders, function(x){
    if(x > 4){return(x %% 4)}; return(x)
  })
  biddingOrder = c(firstBidders, dealer)
  
  leadPlayer = NULL #(nobody is winning yet)
  for(i in 1:4){
    if(i==4) message("[dealer]")
    message(paste0("player ", biddingOrder[i],": here is your hand." ))
    showHand(hands[[i]])
    if(i==4 | i==3) message(paste0("your partner has bid ", paste(firstTwo[[i-2]], collapse=" ")))
    theBid = strsplit(readline("Please make your bid: "), split=" ")[[1]]
    if(length(theBid)>1){
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
  if(!is.null(highBid)){
    if(highBid[1]=="6") leadPlayer = NULL #we don't play 6 bids    
  }
  
  return(list(leadPlayer=leadPlayer, dealer=dealer, highBid=highBid, biddingOrder=biddingOrder))
  
} # end bidding function


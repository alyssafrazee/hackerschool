# function to check bids for rule-breaking:

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

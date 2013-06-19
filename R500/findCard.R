# function to find a given card in a hand

findCard = function(hand, cardname){
  # cardname = string, such as "joker", "A hearts", etc.
  # hand is a list of cards.
  splitCard = strsplit(cardname, split=" ")[[1]]
  if(length(splitCard) == 0) return(NULL)
  if(length(splitCard) == 1){
    if(splitCard != "joker") return(NULL)
    myCard = card(suit="none", number="joker")
  }else{
    myCard = card(suit=splitCard[2], number=splitCard[1])
  }
  tf = compare(hand, myCard)
  cardInd = which(tf==TRUE)
  return(cardInd)
}

# function to sort a hand of cards

sortHand = function(hand, trump=NULL){
  deck = makeDeck(trump=trump)
  inds = sapply(hand, function(x) which(compare(deck, x)==TRUE))
  return(deck[sort(inds)])
}

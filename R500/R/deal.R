# function to randomly assign cards to different hands

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

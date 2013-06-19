# function to create deck of cards, properly sorted


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

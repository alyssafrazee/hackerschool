# helper function to see whether two cards are equal.

compare = function(c1, c2){
  # this function recurses, WOAH
  if(class(c1)=="list"){
    return(lapply(c1, function(x) compare(x, c2)))
  }
  return(suit(c1)==suit(c2) & number(c1)==number(c2))
}

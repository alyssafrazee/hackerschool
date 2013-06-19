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
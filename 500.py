## trying to program 500 in python also
## AF June 17 2013


#### the card class:
class card:
    def __init__(self, suit, number, trump=False, lowBower=False):
        self.suit = suit
        self.number = number
        self.trump = trump
        self.lowBower = lowBower
        
    def __cmp__(self, other):
        if self.suit == other.suit and self.number == other.number:
            return 0
        elif self.number=="joker":
            return 1
        elif other.number=="joker":
            return -1
        elif self.trump and not other.trump:
            return 1
        elif not self.trump and other.trump:
            return -1
        elif self.trump and other.trump:
            if self.number == "J" and other.number == "J":
                if self.lowBower:
                    return -1 #other is the high bower
                else:
                    return 1 #other is the low bower
            else:
                numOrder = range(4,11)+["Q","K","A","J"]
                if numOrder.index(self.number) > numOrder.index(other.number):
                    return 1
                else:
                    return -1
        elif self.suit == other.suit:
            numOrder = range(4,11)+["J","Q","K","A"]
            if numOrder.index(self.number) > numOrder.index(other.number):
                return 1
            else:
                return -1
        else: 
            suitOrder = ['spades','clubs','diamonds','hearts','none']
            if suitOrder.index(self.suit) > suitOrder.index(other.suit):
                return 1
            else:
                return -1
                
    def __repr__(self):
        if self.number == 'joker':
            return 'joker [trump]'
        else:
            shown = str(self.number)+" "+self.suit
            if self.trump:
                shown = shown+" [trump]"
            return shown
        
class bid:
    def __init__(self, number, suit):
        self.number = number
        self.suit = suit
    
    def __cmp__(self, other):
        suitOrder = ['spades','clubs','diamonds','hearts','notrump']
        if self.suit == other.suit and self.number == other.number:
            return 0
        elif self.number > other.number:
            return 1
        elif other.number > self.number:
            return -1
        elif suitOrder.index(self.suit) > suitOrder.index(other.suit):
            return 1
        else:
            return -1
    
    def __repr__(self):
        if self.number == 0:
            return 'pass'
        else:
            return str(self.number)+' '+self.suit


hearts7 = card("hearts",7)
hearts8 = card("hearts",8)
hearts9 = card("hearts",9)
spadesK = card("spades", 'K')
diamondsQ = card('diamonds','Q',trump=True)
diamondsJ = card('diamonds','J',trump=True)
heartsJ = card('hearts','J',trump=True,lowBower=True)
joker = card(suit='none', number='joker', trump=True)

cardList = [hearts9, hearts7, heartsJ, diamondsJ, joker, spadesK, hearts8, diamondsQ]
cardList
cardList.sort()
cardList

for elem in cardList:
    print elem
    
# helper function 1: shuffle and deal the deck
def shuffleDeal(deck, handSize, kittySize):
    import random
    inds = range(0, len(deck))
    random.shuffle(inds)
    hands = {}
    hands['1'] = [deck[i] for i in inds[0:(handSize)]]
    hands['2'] = [deck[i] for i in inds[handSize:(2*handSize)]]
    hands['3'] = [deck[i] for i in inds[(2*handSize):(3*handSize)]]
    hands['4'] = [deck[i] for i in inds[(3*handSize):(4*handSize)]]
    hands['kitty'] = [deck[i] for i in inds[-kittySize:]]
    return hands

def getPlayer(x):
    return x - (((x-1)/4)*4)

# helper function 2: bids
def getBids(dealer, hands):
    firstBidder = dealer+1
    players = [getPlayer(x) for x in range(firstBidder, firstBidder+4)]
    winningPlayer = 0
    
    for p in players:
        if p == players[0]:
            currentBid = bid(0,'spades') 
        print "Player "+str(p)+": here is your hand.  It's your bid." 
        hands[str(p)].sort()
        for c in hands[str(p)]:
            print c
        theBid = validateBid(raw_input("what do you bid? "), currentBid)
        if theBid.number != 0:
            currentBid = theBid
            winningPlayer = p
    
    return [currentBid, winningPlayer]

# helper function for bidding: checking whether a bid is valid
def validateBid(theBid, currentBid):

    if theBid == 'pass':
        return bid(number=0, suit='spades')
    
    else:
        theBid = theBid.split(' ')
        
        if len(theBid) != 2 or theBid[0] not in ['6','7','8','9','10'] or theBid[1] not in ['spades','hearts','diamonds','clubs','notrump']:
            theBid = raw_input('invalid bid, please try again: ')
            return validateBid(theBid, currentBid)
        
        else:
            theBid = bid(number=int(theBid[0]), suit=theBid[1])
            if theBid <= currentBid:
                theBid = raw_input("you must bid higher than the current bid ("+repr(currentBid)+"): ")
                return validateBid(theBid, currentBid)
            else:
                return theBid
    
# low bower helper function
def getLowBower(trump):
    if trump == "hearts":
        lb = card(suit = "diamonds", number = "J")
    elif trump == "diamonds":
        lb = card(suit = "hearts", number = "J")
    elif trump == "spades":
        lb = card(suit = "clubs", number = "J")
    elif trump == "clubs":
        lb = card(suit = "spades", number = "J")
    elif trump == "notrump":
        lb = card(suit = "none", number = 0)
    else:
        print "invalid suit"
        lb = card(suit = "none", number = 0)
    return lb

# helper function 3: choosing cards from kitty
def pickUpKitty(highBid, hands):
    print "player "+str(highBid[0])+" wins the bid with "+repr(highBid[0])
    print "player "+str(highBid[0])+": here is the kitty:"
    
    # sort hands and kitty with trump information:
    for k in hands.keys():
        for c in hands[k]:
            if c.suit == highBid[1].suit:
                c.trump = True
            else:  
                if c == getLowBower(trump = highBid[1].suit):
                    
        if hands[k]
        hands[k].sort()
    
    print c for c in hands['kitty']
    print "and again, here is your hand:"
    print c for c in hands[str(highBid[0])]
    newHand = []
    for newcard in range(10):
        newHand[newcard] = validateCard(raw_input("card "+str(newcard+1)+": "), hands[str(bidWinner)]+hands['kitty'])
    hands[str(highBid[0])] = newHand
    return hands

# helper function for choosing and playing cards:
def validateCard(cardString, hand):
    # hand is list of possible cards the card could come from
    
    if cardString == "joker":
        theCard = card(suit="none", number="joker", trump=True)
    
    else:
        cardList = cardString.split(' ')
        if len(cardList) != 2 or cardList[0] not in ['4','5','6','7','8','9','10','J','Q','K','A'] or cardList[1] not in ['spades','hearts','diamonds','clubs']:
            theCard = validateCard(raw_input("invalid card - try again: "), hand)
        else:
            if cardList[0] in [str(i) for i in range(4,11)]:
                cardList[0] = int(cardList[0])
            theCard = card(suit=cardList[1], number=cardList[0])
        
        if theCard not in hand:
            theCard = validateCard(raw_input("you don't have the joker, please enter another card: "))
        
    return theCard


#### PLAY GAME

def play500():    
    print "welcome to python 500!"
    
    # keep track of each team's score:
    score13 = 0
    score24 = 0
    
    # assign an initial dealer:
    dealer = 4
    
    # create lookup table for scoring:
    scoreDict = {}
    points = 140
    for num in range(7,11):
        for suit in 'spades','clubs','diamonds','hearts','notrump':
            k = str(num)+' '+suit
            scoreDict[k] = points
            points += 20
    
    # create a deck:
    deck = []
    for num in range(4,11)+["J","Q","K","A"]:
        for suit in 'spades','clubs','diamonds','hearts':
            deck.append(card(suit=suit, number=num))
    deck.append(card(suit='none', number='joker'))
    
    # begin game play:
    while score13<500 and score13>-500 and score24<500 and score24>-500:
        print "player "+str(getPlayer(dealer))+" is dealing."
        
        # shuffle and deal:
        hands = shuffleDeal(deck, handSize = 10, kittySize = 5)
        
        # bid:
        highBid = getBids(dealer, hands)
        
        # make sure the bid was high enough:
        if highBid[0].number == 0 or highBid[0].number == 6:
            if highBid[0].number == 0:
                print "everyone has passed. deal passes to player "+str(getPlayer(dealer+1))
            else:
                print "house rules: we don't play 6 bids.  deal passes to player "+str(getPlayer(dealer+1))
            dealer += 1
            continue
        
        # bid winner picks up the kitty
        hands = pickUpKitty(bidWinner = highBid[1], hands)
        
        
        
        
        
        # pass the deal to the next player:
        dealer += 1


    
    
    
    
    
    
    
    
    
    
    
    
    



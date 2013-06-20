## trying to program 500 in python also
## AF June 17 2013


#### the card class:
class card(object):
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
                
    def __str__(self):
        if self.number == 'joker':
            return 'joker [trump]'
        else:
            shown = str(self.number)+" "+self.suit
            if self.trump:
                shown = shown+" [trump]"
            return shown
        
class bid(object):
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
    
    def __str__(self):
        if self.number == 0:
            return 'pass'
        else:
            return str(self.number)+' '+self.suit

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
        print "Player ", p, ": here is your hand.  It's your bid." 
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
    
    #if theBid == "score":
    #    getScore()
    #    return validateBid(raw_input("enter bid: "), currentBid)

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
                theBid = raw_input("you must bid higher than the current bid ("+str(currentBid)+"): ")
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
    print "player ", highBid[1], " wins the bid with ", highBid[0]
    print "player ", highBid[1], ": here is the kitty:"
    
    # sort hands and kitty with trump information:
    for k in hands.keys():
        if highBid[0].suit != "notrump":
            for c in hands[k]:
                if c.suit == highBid[0].suit:
                    c.trump = True
                else:  
                    if c == getLowBower(trump = highBid[0].suit):
                        c.trump = True
                        c.lowBower = True
                    elif c.number == 'joker':
                        c.suit = highBid[0].suit
        hands[k].sort()
    
    for c in hands['kitty']:
        print c
    print "and again, here is your hand:"
    for c in hands[str(highBid[1])]:
        print c
    newHand = []
    newcard = 0
    print "of these 15 cards, choose the 10 you would like to keep."
    for newcard in range(10):
        newHand.append(validateCard(raw_input("card "+str(newcard+1)+": "), hands[str(highBid[1])]+hands['kitty'], highBid[0].suit, newHand))
        
    newHand.sort()
    hands[str(highBid[1])] = newHand
    return hands

# helper function for choosing and playing cards:
def validateCard(cardString, hand, trump, newHand):
    # hand is list of possible cards the card could come from
    #if cardString == "score":
    #    getScore()
    #    theCard = validateCard(raw_input("enter card: "), hand, trump, newHand)
    
    if cardString == "joker":
        theCard = card(suit=trump, number="joker", trump=True)
    
    else:
        cardList = cardString.split(' ')
        
        if len(cardList) != 2 or cardList[0] not in ['4','5','6','7','8','9','10','J','Q','K','A'] or cardList[1] not in ['spades','hearts','diamonds','clubs']:
            theCard = validateCard(raw_input("invalid card - try again: "), hand, trump, newHand)
        else:
            if cardList[0] in [str(i) for i in range(4,11)]:
                cardList[0] = int(cardList[0])
            theCard = card(suit=cardList[1], number=cardList[0], trump = cardList[1]==trump)
            if theCard == getLowBower(trump):
                theCard.trump = True
                theCard.lowBower = True
        
        if theCard not in hand:
            theCard = validateCard(raw_input("you don't have this card, please enter another card: "), hand, trump, newHand)
        
        if newHand:
            if theCard in newHand:
                theCard = validateCard(raw_input("you have already chosen this card, please choose a different one: "), hand, trump, newHand)
        
    return theCard

# helper function 4: playing a card
def playCard(selectedCard, trump, p, hands, cardsPlayed):
    # selectedCard must already be validated for hands[str(p)]
    
    def goAhead():
        cardsPlayed.append(selectedCard)
        hands[str(p)].remove(selectedCard)
    
    if cardsPlayed == []:
        goAhead()

    else:
        if cardsPlayed[0] == getLowBower(trump):
            ledSuit = trump
        else:
            ledSuit = cardsPlayed[0].suit

        if selectedCard.suit != ledSuit:
            if selectedCard == getLowBower(trump) and ledSuit == trump:
                goAhead()
            else:
                suitsInHand = {c.suit for c in hands[str(p)]}
                if getLowBower(trump) in hands[str(p)]:
                    suitsInHand.add(trump)
                if ledSuit in suitsInHand:
                    selectedCard = validateCard(raw_input("you must follow suit, please play a " + ledSuit[:-1] + ": "), hands[str(p)], trump, None)
                    playCard(selectedCard, trump, p, hands, cardsPlayed)
                else:
                    goAhead() 
        else:
            goAhead()

# seeing the scores:
#def getScore():
#    print "players 1 and 3 have "+score13+" points, and they have taken #"+tricks13+" tricks so far."
#    print "players 2 and 4 have "+score24+" points, and they have taken #"+tricks24+" tricks so far."

def assign_points(score_dict, highbid, tricks13, tricks24, score13, score24):
    if highBid[1] == 1 or highBid[1] == 3:
        if tricks13 >= highBid[0].number:
            print "players 1 and 3 have made their bid!"
            score13 += scoreDict[repr(highBid[0])]
        else:
            print "players 1 and 3 have been set."
            score13 -= scoreDict[repr(highBid[0])]
    else:
        if tricks24 >= highBid[0].number:
            print "players 2 and 4 have made their bid!"
            score24 += scoreDict[repr(highBid[0])]
        else:
            print "players 2 and 4 have been set."
            score24 -= scoreDict[repr(highBid[0])]

    return score13, score24


#### PLAY GAME

def play500():    
    print "welcome to python 500!"
    
    # keep track of each team's score:
    score13 = 0
    score24 = 0
    tricks13 = 0
    tricks24 = 0
        
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
    while -500 < score13 < 500 and -500 < score24 < 500:
        print "player ", getPlayer(dealer), " is dealing."
        
        # shuffle and deal:
        hands = shuffleDeal(deck, handSize = 10, kittySize = 5)
        
        # bid:
        highBid = getBids(dealer, hands)
        
        # make sure the bid was high enough:
        if highBid[0].number == 0 or highBid[0].number == 6:
            if highBid[0].number == 0:
                print "everyone has passed. deal passes to player ", getPlayer(dealer+1)
            else:
                print "house rules: we don't play 6 bids.  deal passes to player ", getPlayer(dealer+1)
            dealer += 1
            continue
        
        # bid winner picks up the kitty
        hands = pickUpKitty(highBid, hands)
        
        # play the tricks:
        leadPlayer = highBid[1] 
        for trick in range(10):
            playOrder = [getPlayer(x) for x in range(leadPlayer, leadPlayer+4)]
            cardsPlayed = []
            for p in playOrder:
                print "player ", p, ": it's your turn. Here is your hand: "
                for c in hands[str(p)]:
                    print c
                selectedCardString = raw_input("Which card would you like to play? ")
                selectedCard = validateCard(selectedCardString, hands[str(p)], highBid[0].suit, None)
                playCard(selectedCard, highBid[0].suit, p, hands, cardsPlayed)
            contenders = [x for x in cardsPlayed if x.suit==cardsPlayed[0].suit or x.trump]
            winningCard = max(contenders)
            winningPlayer = playOrder[cardsPlayed.index(winningCard)]
            print "player", winningPlayer, " wins with ", winningCard
            
            # increment hand scores:
            if winningPlayer==1 or winningPlayer==3:
                tricks13 += 1
            else:
                tricks24 += 1
            
            # pass lead to winning player:
            leadPlayer = winningPlayer
        
        score13, score24 = assign_points(score_dict, highbid, tricks13, tricks24, score13, score24)
        
        # reset trump:
        for c in deck:
            if c.number != 'joker':
                c.trump = False
            else:
                c.suit = 'none'
        
        # pass the deal to the next player:
        dealer += 1

    # someone has won!
    if score13>=500:
        print "players 1 and 3 win!"
    elif score13 <= -500:
        print "players 2 and 4 win, because players 1 and 3 lose!"
    elif score24 >= 500:
        print "players 2 and 4 win!"
    else:
        print "players 1 and 3 win, because players 2 and 4 lose!"
    
    # goodbye.
    print "thank you for playing!"    

play500()
    

    
    
    
    
    
    



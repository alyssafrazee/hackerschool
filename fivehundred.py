## trying to program 500 in python also
## AF June 17 2013


#### the card class:
class Card(object):
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
        
class Bid(object):
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
    
    def __hash__(self):
        return hash(str(self))


# helper function 1: shuffle and deal the deck
def shuffleDeal(deck, handsize, kittySize):
    import random
    random.shuffle(deck)
    hands = {}
    hand_names = [str(i) for i in range(1,5)] + ['kitty']
    for i, hand in enumerate(hand_names):
        hands[hand] = deck[i*handsize:(i+1)*handsize]
    return hands

def getPlayer(x):
    while x > 4:
        x -= 4
    return x

# helper function 2: bids
def get_high_bid(dealer, hands):
    firstBidder = dealer+1
    players = [str(getPlayer(x)) for x in range(firstBidder, firstBidder+4)]
    winningPlayer = 0
    
    currentBid = Bid(0,'spades')
    for p in players:
        print "Player", p, "- here is your hand.  It's your bid." 
        hands[p].sort()
        for c in hands[p]:
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
        return Bid(number=0, suit='spades')
    
    else:
        theBid = theBid.split(' ')
        
        if len(theBid) != 2 or theBid[0] not in ['6','7','8','9','10'] or theBid[1] not in ['spades','hearts','diamonds','clubs','notrump']:
            theBid = raw_input('invalid bid, please try again: ')
            return validateBid(theBid, currentBid)
        
        else:
            theBid = Bid(number=int(theBid[0]), suit=theBid[1])
            if theBid <= currentBid:
                theBid = raw_input("you must bid higher than the current bid ("+str(currentBid)+"): ")
                return validateBid(theBid, currentBid)
            else:
                return theBid
    
# low bower helper function
def getLowBower(trump):
    lowBower = {
                "hearts" : Card(suit = "diamonds", number = "J"),
                "diamonds" : Card(suit = "hearts", number = "J"),
                "spades" : Card(suit = "clubs", number = "J"),
                "clubs" : Card(suit = "spades", number = "J"),
                "notrump" : None,
                }

    try:
        return lowBower[trump]
    except KeyError:
        print "invalid suit in getLowBower"
        sys.exit()


# helper function 3: choosing cards from kitty
def pickUpKitty(high_bid, leadPlayer, hands):
    print "player ", leadPlayer, " wins the bid with ", high_bid
    print "player ", leadPlayer, ": here is the kitty:"
    
    # sort hands and kitty with trump information:
    for k in hands.keys():
        if high_bid.suit != "notrump":
            for c in hands[k]:
                if c.suit == high_bid.suit:
                    c.trump = True
                else:  
                    if c == getLowBower(trump = high_bid.suit):
                        c.trump = True
                        c.lowBower = True
                    elif c.number == 'joker':
                        c.suit = high_bid.suit
        hands[k].sort()
    
    for c in hands['kitty']:
        print c
    print "and again, here is your hand:"
    for c in hands[str(leadPlayer)]:
        print c
    newHand = []
    newcard = 0
    print "of these 15 cards, choose the 10 you would like to keep."
    for newcard in range(10):
        newHand.append(validateCard("card "+str(newcard+1)+": ", hands[str(leadPlayer)]+hands['kitty'], high_bid.suit, newHand))
        
    newHand.sort()
    hands[str(leadPlayer)] = newHand
    return hands

# helper function for choosing and playing cards:
def validateCard(message, hand, trump, newHand):
    card = raw_input(message)
    # hand is list of possible cards the card could come from
    
    card_values = ['4','5','6','7','8','9','10','J','Q','K','A']
    card_suits = ['spades','hearts','diamonds','clubs']
    
    if card == "joker":
        theCard = Card(suit=trump, number="joker", trump=True)
    
    else:
        card_val_and_suit = card.split(' ')
        
        if len(card_val_and_suit) != 2 or card_val_and_suit[0] not in card_values or card_val_and_suit[1] not in card_suits:
            message = "invalid card - try again: "
            theCard = validateCard(message, hand, trump, newHand)
        else:
            card_value, card_suit = card_val_and_suit
            try:
                card_value = int(card_value)
            except ValueError: # face card
                pass
            theCard = Card(suit=card_suit, number=card_value, trump = (card_suit==trump) )
            if theCard == getLowBower(trump):
                theCard.trump = True
                theCard.lowBower = True
        
        if theCard not in hand:
            message = "you don't have this card, please enter another card: "
            theCard = validateCard(message, hand, trump, newHand)
        
        if newHand:
            if theCard in newHand:
                message = "you have already chosen this card, please choose a different one: "
                theCard = validateCard(message, hand, trump, newHand)
        
    return theCard


def validate_move(selectedCard, trump, p, hands, cardsPlayed):
    # selectedCard must already be validated for hands[p]

    if cardsPlayed == []:
       return True

    else:
        if cardsPlayed[0] == getLowBower(trump):
            ledSuit = trump
        else:
            ledSuit = cardsPlayed[0].suit

        if selectedCard.suit != ledSuit:
            if selectedCard == getLowBower(trump) and ledSuit == trump:
               return True
            else:
                suitsInHand = {c.suit for c in hands[str(p)]}
                if getLowBower(trump) in hands[str(p)]:
                    suitsInHand.add(trump)
                if ledSuit in suitsInHand:
                    print "Follow suit!"
                    return False
                else:
                    return True
        else:
           return True


# seeing the scores:
#def getScore():
#    print "players 1 and 3 have "+score13+" points, and they have taken #"+tricks13+" tricks so far."
#    print "players 2 and 4 have "+score24+" points, and they have taken #"+tricks24+" tricks so far."

def assign_points(score_dict, highBid, tricks13, tricks24, score13, score24):
    if highBid[1] == 1 or highBid[1] == 3:
        if tricks13 >= highBid[0].number:
            print "players 1 and 3 have made their bid!"
            score13 += score_dict[highBid[0]]
        else:
            print "players 1 and 3 have been set."
            score13 -= score_dict[highBid[0]]
    else:
        if tricks24 >= highBid[0].number:
            print "players 2 and 4 have made their bid!"
            score24 += score_dict[highBid[0]]
        else:
            print "players 2 and 4 have been set."
            score24 -= score_dict[highBid[0]]

    return score13, score24

def end_game_message(score13, score24):
    if score13>=500:
        print "players 1 and 3 win!"
    elif score13 <= -500:
        print "players 2 and 4 win, because players 1 and 3 lose!"
    elif score24 >= 500:
        print "players 2 and 4 win!"
    else:
        print "players 1 and 3 win, because players 2 and 4 lose!"
    
    print "thank you for playing!"    

def build_deck():
    deck = []
    for num in range(4,11)+["J","Q","K","A"]:
        for suit in 'spades','clubs','diamonds','hearts':
            deck.append(Card(suit=suit, number=num))
    deck.append(Card(suit='none', number='joker'))
    return deck

def score_table():
    score_dict = {}

    for num in range(7, 11):
        for i, suit in enumerate(['spades','clubs','diamonds','hearts','notrump']):
            score_dict[Bid(num, suit)] = 140 + (20 * i) + 100 * (num - 7)
    return score_dict

play500(True)

#### PLAY GAME

def play500(testmodule):    
    print "welcome to python 500!"
    
    # keep track of each team's score:
    score13 = 0
    score24 = 0
    tricks13 = 0
    tricks24 = 0
        
    # assign an initial dealer:
    dealer = 4
    
    score_dict = score_table()
    
    deck = build_deck()
    
    # begin game play:
    while -500 < score13 < 500 and -500 < score24 < 500:
        print "player", getPlayer(dealer), "is dealing."
        
        # shuffle and deal:
        hands = shuffleDeal(deck, handsize = 10, kittySize = 5)
        
        # bid:
        high_bid, leadPlayer = get_high_bid(dealer, hands)
        
        # make sure the bid was high enough:
        if high_bid.number == 0 or high_bid.number == 6:
            if high_bid.number == 0:
                print "everyone has passed. deal passes to player", getPlayer(dealer+1)
            else:
                print "house rules: we don't play 6 bids.  deal passes to player", getPlayer(dealer+1)
            dealer += 1
            continue
        
        # bid winner picks up the kitty
        hands = pickUpKitty(high_bid, leadPlayer, hands)
        
        # play the tricks:
        for trick in range(10):
            playOrder = [str(getPlayer(x)) for x in range(leadPlayer, leadPlayer+4)]
            cardsPlayed = []
            for p in playOrder:
                print "player", p, ": it's your turn. Here is your hand: "
                for c in hands[p]:
                    print c
                valid_move = False
                while not valid_move:
                    selectedCard = validateCard("Which card would you like to play? ", hands[p], high_bid.suit, None)
                    valid_move = validate_move(selectedCard, high_bid.suit, p, hands, cardsPlayed)
                cardsPlayed.append(selectedCard)
                hands[p].remove(selectedCard)

            contenders = [x for x in cardsPlayed if x.suit==cardsPlayed[0].suit or x.trump]
            winningCard = max(contenders)
            winningPlayer = playOrder[cardsPlayed.index(winningCard)]
            print "player", winningPlayer, "wins with", winningCard
            
            # increment hand scores:
            if winningPlayer==1 or winningPlayer==3:
                tricks13 += 1
            else:
                tricks24 += 1
            
            # pass lead to winning player:
            leadPlayer = winningPlayer
        
        score13, score24 = assign_points(score_dict, highBid, tricks13, tricks24, score13, score24)
        
        # reset trump:
        for c in deck:
            if c.number != 'joker':
                c.trump = False
            else:
                c.suit = 'none'
        
        # pass the deal to the next player:
        dealer += 1

    end_game_message(score13, score24)


if __name__ == '__main__':
    play500()
    

    
    
    
    
    



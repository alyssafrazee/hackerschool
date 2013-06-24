import fivehundred as f

def test_deck_creation():
    deck = f.build_deck()
    assert len(deck) == 45

def test_score_table():
    score_table = f.score_table()
    assert score_table[f.Bid(10, 'diamonds')] == 480
    assert score_table[f.Bid(8, 'notrump')] == 320

if __name__ == '__main__':
    test_deck_creation()
    test_score_table()
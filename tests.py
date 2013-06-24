import fivehundred as f

def test_deck_creation():
    deck = f.build_deck()
    assert len(deck) == 45

if __name__ == '__main__':
    test_deck_creation()
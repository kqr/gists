from random import choice

# Change this to alter how many times to play each hand.
# Currently it's set to play each hand a million times.
hands_played = 1000000

for value in range(10, 21):
    for soft_hand in (True, False):
        busts = 0
        blackjacks = 0
        betters = 0

        for i in range(0, hands_played):
            # Pick another card assuming a brand new deck.
            next_card = choice([2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 10, 10, 11])

            # Magic to work out the status of the hand based on whether it is soft or not.
            if any([value + next_card == 21, \
                    value + next_card - (10 if soft_hand or next_card == 11 else 0) == 21]):
                blackjacks += 1
            elif any([value + next_card < 21, \
                      value + next_card - (10 if soft_hand or next_card == 11 else 0) < 21]):
                betters += 1
            else:
                busts += 1


        print("Statistics for drawing cards with {} hand valued {}:".format("soft" if soft_hand else "hard", value))
        print("    Bust: {:.2f}%".format(busts/hands_played * 100))
        print("    Better hand: {:.2f}%".format(betters/hands_played * 100))
        print("    Blackjack: {:.2f}%".format(blackjacks/hands_played * 100))

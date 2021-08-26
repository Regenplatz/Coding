#!/usr/local/bin/python

import random


def computersChoice(possibles):
    """ randomly select rock, stone or paper as computer's choice
        :param possibles: list,  list of possibilities (rock, stone, paper)
        :return: String, a randomly chosen element of "possibles"
    """
    choiceIndex = random.randint(0, 2)
    return possibles[choiceIndex]


def playersTurn(choice):
    """ convert player's input to lower case and eliminate spaces
        :param choice: String, player's input
        :return: String, cleaned player's input
    """
    playersChoice = choice.lower().strip()
    return playersChoice


def boldOutput(word):
    """ convert word to bold writing
        :param word: String, word to be converted to bold writing
        :return: String, word in bold writing
    """
    boldWord = "\033[1m" + word + "\033[0m"
    return boldWord


def convertAbbrev(abbreviation):
    """ convert abbreviation to the corresponding word
        :param abbreviation: String, player's abbreviated input
    """
    if abbreviation in ["r", "R"]:
        abbreviation = "rock"
    elif abbreviation in ["p", "P"]:
        abbreviation = "paper"
    elif abbreviation in ["s", "S"]:
        abbreviation = "scissors"
    return abbreviation


def evaluation(playerInput, computersTurn):
    """ evaluate the winner of the game and print it on the console
        :param playerInput: String, input of player
        :param computersTurn: String, computer's random choice
    """
    print("Your turn: ", playerInput)
    print("computers turn: ", computersTurn)

    if playerInput == computersTurn:
        result = "tie"

    elif playerInput == "rock":
        if computersTurn == "paper":
            result = "You lost!"
        else:
            result = "You won!"

    elif playerInput == "paper":
        if computersTurn == "rock":
            result = "You won!"
        else:
            result = "You lost!"

    else:
        if computersTurn == "rock":
            result = "You lost"
        else:
            result = "You won!"

    print(boldOutput(result))
    return result


def adaptResults(scores, res):
    """ adapt scores
        :param scores: dictionary, contains scores for won, lost, tie
        :param res: result of a single game
        :return: dictionary, containing adapted value
    """
    if "won" in res:
        scores["won"] += 1
    elif "lost" in res:
        scores["lost"] += 1
    else:
        scores["tie"] += 1
    return scores


def game():
    """ a single game play
        :return: game's outcome
    """

    possibility = ["rock", "paper", "scissors"]

    # computer's turn (in the background)
    computersTurn = computersChoice(possibility)

    # player's turn (input via console)
    subject = input("Please select from " + str(possibility) + ": ")
    if subject in ["r", "R", "p", "P", "s", "S"]:
        subject = convertAbbrev(subject)
    while subject not in possibility:
        subject = input("Wrong input. Please select from " + str(possibility) + ": ")
    playersChoice = playersTurn(subject)

    # evaluate and return single game's outcome
    result = evaluation(playersChoice, computersTurn)
    return result


def main():
    """ run game and repeat if wished, show overall results when finished """

    score = {"won": 0, "lost": 0, "tie": 0}

    # play several games, adapt score after every game
    counter = 0
    while counter < 5:
        gameOutcome = game()
        adaptResults(score, gameOutcome)
        counter += 1
    print("\nOverall results: ", score)


if __name__ == "__main__":
    main()
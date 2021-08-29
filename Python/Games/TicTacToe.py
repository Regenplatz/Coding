#!/usr/local/bin/python

import random
import itertools

# define tokens for player and computer
playersToken = " X "
computersToken = " O "

# define winning triples
winningTriples = [(0,1,2), (3,4,5), (6,7,8), (0,3,6), (1,4,7), (2,5,8), (0,4,8), (2,4,6)]


def printGray(delim=True):
    """
    convert to gray color
    :param delim: boolean, if True: vertical delimiter for gameBoard cells
    """
    if delim == True:
        delimiter = "|"
        fields = f"\033[0;37m{delimiter}\033[0;0m"
        return fields
    else:
        fields = {}
        for i in range(9):
            fields[i] = f"\033[0;37m {i} \033[0;0m"
        return fields


def gameBoard(cells):
    """
    print plain game board
    :param cells: dictionary, information about which cell contains which content
    """
    delimiter = printGray(True)
    board = delimiter + cells[0] + delimiter + cells[1] + delimiter + cells[2] + delimiter + "\n" + \
            delimiter + cells[3] + delimiter + cells[4] + delimiter + cells[5] + delimiter + "\n" + \
            delimiter + cells[6] + delimiter + cells[7] + delimiter + cells[8] + delimiter
    print(board)


def newToken(cells, index, token):
    """
    player's or computer's turn,
    set token to a given cell,
    color of displayed token(s) is set to black (automatically without further formatting)
    :param cells: dictionary, information about which cell contains which content
    :param index: integer, index of cell on game board
    :param token: String, player or computer specific token
    :return: dictionary, adapted to including the token just set
    """
    cells[index] = token
    return cells


def limitToFreeFields(cells):
    """
    generate a list of indices for emtpy cells
    :param cells: dictionary, information about which cell contains which content
    :return: list of indices for emtpy cells
    """
    stringNumbers = []
    for i in range(9):
        stringNumbers.append(f" {i} ")

    freeCells = []
    for key, value in cells.items():
        # extract empty spaces and append to list
        # if value in list(range(9)):
        if value[7:10] in stringNumbers:
            freeCells.append(key)
    return freeCells


def showOccupation(cells):
    """
    create a dictionary that lists all cell indices per kind of cell occupancy
    :param cells: dictionary, updated occupation of all cells of the game board
    :return: dictionary, for each kind of cell occupancy list corresponding cell indices
    """
    cellOccupation = {playersToken: [], computersToken: [], "free": []}
    for key, value in cells.items():
        if value == playersToken:
            cellOccupation[playersToken].append(key)
        elif value == computersToken:
            cellOccupation[computersToken].append(key)
        else:
            cellOccupation["free"].append(key)
    return cellOccupation


def updateGame(fields):
    """
    according to field occupation update game board
    :param fields: dictionary, information about which cell contains which content
    :return: dictionary, updated information about which cell contains which content and list of free cells
    """
    freeCells = limitToFreeFields(fields)
    gameBoard(fields)
    return fields, freeCells


def evaluateDuos(onesPartyTokens, triplesToCheck, freeCells):
    """
    create a list with all duos that can be created by one's party's cell occupation
    :param onesPartyTokens: list, indices of all tokens (of the party of interest)
    :return: list, 2-tuples (duos) which are part of potential winning triples
    """
    duos = []
    toBeOccupied = []
    if len(onesPartyTokens) > 1:
        # create a list with all duos that can be created by one's party's cell occupation
        for subset in itertools.combinations(onesPartyTokens, 2):
            duos.append(subset)
        # check if any duos are part of potential winning triples and write those missing indices to list
        for triple in range(len(triplesToCheck)):
            for duo in range(len(duos)):
                diffToTriples = list(set(triplesToCheck[triple]) - set(duos[duo]))
                if len(diffToTriples) == 1 and diffToTriples[0] in freeCells:
                    toBeOccupied.extend(diffToTriples)
    return toBeOccupied


def evaluateWinningCells(occupiedCells, freeCells):
    """
    create a list with potential winning cells
    :param occupiedCells:
    :param freeCells:
    :return:
    """
    someList = []
    winningCells = evaluateDuos(occupiedCells, freeCells)
    # potential winning move
    for wc in winningCells:
        if wc in freeCells:
            someList.append(wc)
    # other moves
    for fc in freeCells:
        someList.append(fc)
    return someList


def checkCloseToTriple(occupiedCells, freeCells):
    """
    check if 2 out of 3 tokens are already set and which index is finally needed to complete a winning token,
    (either use it to find the computer's winning token or to find the position to prevent the player from winning)
    :param occupiedCells: dictionary, kind of token (keys), list of indices (values)
    :param freeCells: list, indices of free cells
    :return:
    """
    remainingCells = []
    allDuos = []
    remainingTriples = []
    remainingDuos = []
    toBeOccupied = []

    # computer's 1st token position
    if len(occupiedCells[playersToken]) == 1:
        toBeOccupied = random.choice(freeCells)

    else:
        # complete computer's winning triple to win
        computersWinnings = evaluateDuos(occupiedCells[computersToken], winningTriples, freeCells)
        if len(computersWinnings) > 0:
            toBeOccupied = random.choice(computersWinnings)
        else:
            # impede player from winning
            playersWinnings = evaluateDuos(occupiedCells[playersToken], winningTriples, freeCells)
            if len(playersWinnings) > 0:
                toBeOccupied = random.choice(playersWinnings)
            # set computer's token randomly
            else:
                toBeOccupied = random.choice(freeCells)
    return toBeOccupied


def computersTurn(fields, freeCells):
    """
    set computer's input,
    update dictionary with cell occupation info,
    draw game board based on the updated occupation dictionary
    :param fields: dictionary, info about cell occupation
    :param freeCells: list, cell indices which are not yet occupied by any token (neither player's nor computer's)
    :return: 1) updated dictionary with info about cell occupation, 2) list of unoccupied cells
    """
    if len(freeCells) > 0:
        print("Computer's turn:")
        # update dictionary with latest player's action
        cellOccupation = showOccupation(fields)
        toSetNextToken = checkCloseToTriple(cellOccupation, freeCells)
        fields = newToken(fields, toSetNextToken, computersToken)
        fields, freeCells = updateGame(fields)
        # return fields, freeCells
    else:
        print("No more possible moves!")
        showResult(fields)
    return fields, freeCells


def playersTurn(fields, freeCells):
    """
    use player's input to set token on the corresponding cell index,
    update dictionary with cell occupation info,
    draw game board based on the updated occupation dictionary
    :param fields: dictionary, info about the occupation (value) of each cell index (key)
    :param freeCells: list, cell indices which are not yet occupied by any token (neither player's nor computer's)
    :return: updated dictionary with info about cell occupation and updated list of free cells
    """
    # set last player's token automatically to the only remaining cell
    if len(freeCells) == 1:
        print("Set last token automatically.")
        playersChoice = freeCells[0]
    # set player's tokens 1-4
    else:
        while True:
            try:
                playersChoice = int(input("Your Turn. Please select a free cell index for your token: "))
                if playersChoice > 8 or playersChoice < 0:
                    print("This index value is not valid. Please select a free cell index for your token! ")
                elif playersChoice not in freeCells:
                    print("This cell is already occupied. Please select a free cell index for your token! ")
                else:
                    break
            except ValueError:
                print("Wrong input! Please enter an index of a free cell!")
    fields = newToken(fields, playersChoice, playersToken)
    fields, freeCells = updateGame(fields)
    return fields, freeCells


def showResult(updatedBoard):
    """
    show if player won or lost and quit game
    :param updatedBoard: dictionary, updated information about the cells occupation
    """
    # create dictionary with their corresponding occupation indices
    cellOccupation = showOccupation(updatedBoard)
    pT = cellOccupation[playersToken]
    cT = cellOccupation[computersToken]
    # check if any winningTriple is filled with either player's or computer's tokens and print result
    for triple in winningTriples:
        # if triple[0] in x and triple[1] in x and triple[2] in cellOccupation[playersToken]:
        if triple[0] in pT and triple[1] in pT and triple[2] in pT:
            print("You won! Winning triple:", triple)
            quit()
        elif triple[0] in cT and triple[1] in cT and triple[2] in cT:
            print("You lost! Computer's winning triple:", triple)
            quit()
        else:
            None


def main():

    # draw initial game board with empty fields, print with corresponding indices in the cells
    fields = printGray(False)
    gameBoard(fields)

    # run game as long as there is still free cells
    freeCells = limitToFreeFields(fields)
    # while any(freeCells) in list(range(5)):
    while len(freeCells) > 0:
        # player's turn
        fields, freeCells = playersTurn(fields, freeCells)
        showResult(fields)
        # computer's turn
        fields, freeCells = computersTurn(fields, freeCells)
        showResult(fields)


if __name__ == "__main__":
    main()
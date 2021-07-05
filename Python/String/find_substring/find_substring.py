#!/usr/bin/env python3

import itertools


def find_index(string1, string2):
    """
    Function find_index:
        Find all indices of each letter of string2 within string1.
        Write these indices into a dictionary.
    :param string1: str:
        String in which substring string2 is to be searched for
    :param string2: str:
        Substring which is to be searched for in the longer string string1
    :return letter_dict: dictionary
        Dictionary with the letters of string2 as key and the indices as
        their corresponding values.
    """
    letter_dict = {}
    for i,char in enumerate(string1):
        if char in string2:
            if char in letter_dict:
                letter_dict[char].append(i)
            else:
                letter_dict[char] = [i]
    return letter_dict


def cross_prod(letter_dict):
    """
    Function cross_prod:
        First, transfer dictionary value lists into a new list.
        Then, iterate through this list and create the corresponding cross products.
        Find minimum distance between min and max values of the cross product.
    :param letter_dict: dictionary
        Derived from function find_index(string1, string2),
        dictionary with all indices of each letter of string2 within string1
    :return best_min, best max: int, int
        Minimum and maximum index values derived from the cross product with the
        minimum distance between min and max values
    """
    all_variations = []
    for key in letter_dict.keys():
        all_variations.append(letter_dict[key])
    for i in itertools.product(*all_variations):
        best_min = 0
        best_max = 10000
        if max(i) - min(i) < best_max - best_min:
            best_min = min(i)
            best_max = max(i)
    return best_min, best_max


def main():
    """
    :function: main()
        Read string and substring,
        call functions find_index(string, substring) and cross_prod(dict)
    :param: None
    :return: None
        Print statements only.
    """
    long_string = "XASDOFIJASDK DFUSXEROHALL"
    short_string = "FUSX"
    char_dict = find_index(long_string, short_string)
    best_min, best_max = cross_prod(char_dict)
    print(long_string[best_min:best_max+1])
    print(find_index.__doc__)
    print(cross_prod.__doc__)


if __name__ == "__main__":
    main()

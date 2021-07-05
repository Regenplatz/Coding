#!/usr/bin/env python3


def prod_odd(start_num, end_num):
    """
    :Function prod_odd:
        Recursively creates the product of a list of natural odd numbers.
    :param start_num: int
        Start number of the chosen range of integers
    :param end_num: int
        End number of the chosen range of integers
    :return result: list
        List of the recursively calculated products
    """
    if end_num % 2 == 0:
        end_num -= 1
    if end_num == start_num:
        result = end_num
    elif end_num > start_num:
        result = end_num * prod_odd(start_num, end_num-2)
    else:
        result = end_num * prod_odd(start_num, end_num+2)
    return result


def main():
    """
    :Function main:
        Takes first and last number of the analyzed range as input variables.
    :return: None
        Result list is printed instead
    """
    try:
        first_number = int(input("Please enter the first number as integer between 1 and 20: "))
        if first_number > 20:
            raise Exception("Only values below 20 are allowed!")
        last_number = int(input("Please enter the last number as integer between 1 and 20: "))
        if last_number > 20:
            raise Exception("Only values below 20 are allowed!")
        result_list = []
        if first_number <= last_number:
            for i in range(first_number, last_number + 1, 2):
                result_list.append(prod_odd(first_number, i))
        else:
            for i in range(last_number, first_number + 1, 2):
                result_list.append(prod_odd(last_number, i))
        print("List of products of odd numbers: ", result_list)
        print(prod_odd.__doc__)
        print(main.__doc__)
    except ValueError:
        print("No integer value given. Please try again using an integer!")


if __name__ == "__main__":
    main()

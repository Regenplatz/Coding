#!/usr/bin/env python3


def find_target(num_list, num, start_index=0, end_index=-2):
    """
    :Function find_target:
        Uses a recursive binary search algorithm to find the index
        of the target number within a given sorted list.
    :param num_list: list
        Sorted list of integers
    :param num: int
        The target number to be searched for index within num_list
    :param start_index: int
        Start index of the respective half window
    :param end_index: int
        End index of the respective half window
    :return result: int
        Index of the target value within num_list
    """
    if end_index == -2:
        end_index = len(num_list)-1
    elif end_index < start_index:
        return -1
    mid = start_index + (end_index - start_index)//2
    mid_value = num_list[mid]
    if mid_value == num:
        result = mid
    elif mid_value > num:
        result = find_target(num_list, num, start_index, mid-1)
    else:
        result = find_target(num_list, num, mid+1, end_index)
    return result


def main():
    """
    :function main:
        Sets a list of integers and sorts it.
        Calls function find_target.
        Prints list, sorted list, the target index position and the documentation.
    :return: None
        Print statements only
    """
    some_list = [8, 14, 1, 6, 3, 4, 7, 10, 13]
    print("list: ", some_list)
    print("sorted list: ", sorted(some_list))
    index_in_list = find_target(sorted(some_list), 10)
    print("index position is: ", index_in_list)
    print(find_target.__doc__)


if __name__ == "__main__":
    main()



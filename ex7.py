"""
Author: Stanislava Poizlova
Matr.Nr.: k12023677
Exercise 7
"""

import re


def get_file_metadata(data_as_string: str):
    separated_elements = data_as_string.split('\n')

    start = '% SeqHeadStart'
    end = '% SeqHeadEnd'
    data_end = '% End of data'

    start_ind = -1
    end_ind = -1
    data_end_ind = -1

    for i, value in enumerate(separated_elements):
        if value == start:
            start_ind = i
        elif value == end:
            end_ind = i
        elif value == data_end:
            data_end_ind = i

    if start_ind == -1 or end_ind == -1 or data_end_ind == -1:
        raise AttributeError("Start of header, End of header or end of data is missing")
    elif end_ind > data_end_ind:
        raise AttributeError("End data is before the end of the header !")

    before_header = separated_elements[0:start_ind]
    for value in before_header:
        if value != '':
            raise AttributeError("<AttributeError> should be raised")

    header_value = separated_elements[start_ind:end_ind]

    id = '% ID:'
    date = "% Date:"
    column_names = "% Columns:"

    some_id = ""
    some_date = ""
    some_column_names = ""

    for value in header_value:
        id_line = re.match(id, value)
        if id_line:
            some_id = value[len(id):len(value)]
            some_id = some_id.strip()
        date_line = re.match(date, value)
        if date_line:
            some_date = value[len(date):len(value)]
            some_date = some_date.strip()
        column_names_line = re.match(column_names, value)
        if column_names_line:
            some_column_names = value[len(column_names):len(value)]
            some_column_names = some_column_names.strip()
        if value == "":
            raise AttributeError("Invalid Line - One of the header lines is empty!")
        line = re.match("%", value)
        if not line:
            raise AttributeError("Invalid Line - One of the header lines is does not begging with % !")

    if some_id == "":
        raise AttributeError("ID is missing!")
    elif some_date == "":
        raise AttributeError("Date is missing!")
    elif some_column_names == "":
        raise AttributeError("Column names are missing!")

    try:
        some_date = int(some_date)
    except ValueError:
        raise TypeError("<TypeError> should be raised")

    column_list = some_column_names.split(';')

    return some_id, some_date, column_list

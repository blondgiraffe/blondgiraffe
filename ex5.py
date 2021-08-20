"""
Author: Stanislava Poizlova
Matr.Nr.: k12023677
Exercise 5

"""


def count_bases_and_subsequence(data_as_string: str, subsequence: str):
    upper_list = data_as_string.upper()
    subsequence = subsequence.upper()
    new_list = []
    good_list = upper_list.split('\n')
    # print(good_list)

    for current_element in good_list:
        if current_element == '% END OF DATA':
            break
        if not (current_element == "") and not (current_element.startswith("%")):
            new_list.append(current_element)
        else:
            new_list.append("W;X;0.0")
        print(current_element)

    a_count = 0
    c_count = 0
    g_count = 0
    t_count = 0
    other = 0


    base_list = []
    for current_element in new_list:
        line_a = current_element.split(';')
        print(line_a)
        base_list.append(line_a[1])

        if line_a[1] == "A" and float(line_a[2]) >= 0.08:
            a_count += 1
        elif line_a[1] == "C" and float(line_a[2]) >= 0.08:
            c_count += 1
        elif line_a[1] == "G" and float(line_a[2]) >= 0.08:
            g_count += 1
        elif line_a[1] == "T" and float(line_a[2]) >= 0.08:
            t_count += 1
        else:
            other += 1
    # print(a_count, c_count, g_count, t_count)
    base_counts = dict(a=a_count, c=c_count, g=g_count, t=t_count)
    # print(base_counts)
    print(base_list)

    base_final_list = []
    for current_element in new_list:
        line_a = current_element.split(';')
        if current_element == '-' or current_element == '_' or float(line_a[2]) < 0.08 :
            base_final_list.append('X')
        else:
            base_final_list.append(line_a[1])


    print(base_final_list)


    i=0
    print(len(base_final_list))
    subsequence_counter = 0
    print(len(subsequence))
    for i in range(len(base_final_list)):
        #print(i)
        j = 0
        string = ''
        while j < len(subsequence):
            z = base_final_list[i]
            string += z
            j += 1
            i += 1
            if string == subsequence:
                subsequence_counter += 1
        print(string)
        if i == (len(base_final_list)):
           break




    print(subsequence_counter)
    #print(base_counts)

    return (subsequence_counter, base_counts)


filename = "00/data_00-000.raw.seq"  # This is the name or path of the file to read
with open(filename) as fh:
   file_content = fh.read()

count_bases_and_subsequence(file_content, "aT")

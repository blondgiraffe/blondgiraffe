from typing import List, Any, Union
import os

os.chdir('C:/Users/stank/Stanuska/Linz/work')

filename = "resources/Siemens/2-20B.json"  # This is the name or path of the file to read
with open(filename, encoding='cp437') as fh:
   file_content = fh.read()

file_content1 = file_content.replace('{','')
file_content2 = file_content1.replace('[','')
file_content3 = file_content2.replace('}','')
file_content4 = file_content3.replace(']','')
file_content5 = file_content4.replace(':','')
file_content6 = file_content5.replace(',','')
file_content7 = file_content6.replace('"','')
upper_list = file_content7.upper()
new_list = []
good_list = upper_list.split(' ')
#print(good_list)
#print(good_list[0])

for current_element1 in good_list:
    if (current_element1 != 'NAME') and (current_element1 != '') and (current_element1 != 'HONEYWELL') and (current_element1 != 'STRUCTURENAME') :
        new_list.append(current_element1)
    #print(current_element1)
#print(new_list)

filename = "resources/Siemens/LED2-10manuf.txt"  # This is the name or path of the file to read
with open(filename, encoding='cp437') as fh:
   file__content = fh.read()

upper_list2 = file__content.upper()
good_list3 = upper_list2.split('\n')
#print(good_list3[0])
new_list2 = []


for current_element2 in good_list3:
    new_current = current_element2.split('_')
    #print(new_current)
    new_list2.append(new_current[0])
#print(new_list2)

res=set(new_list).intersection(new_list2)
print(sorted(res))
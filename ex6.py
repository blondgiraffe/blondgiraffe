"""
Author: Stanislava Poizlova
Matr.Nr.: k12023677
Exercise 6

"""




import os
import glob


def get_hamsters(folderpath: str):
    found_files = glob.glob(os.path.join(folderpath,'**', '*.raw.seq'), recursive=True)
    filename = list(sorted(found_files))
    for i in filename:
        with open(i, 'r') as f:
            file_content = f.read()
        yield os.path.basename(i), file_content
        f.close()




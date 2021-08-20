"""
Author: Stanislava Poizlova
Matr.Nr.: k12023677
Exercise 6
"""

import copy
import numpy as np


def __get_next_state__(state: np.ndarray):
    state_new = copy.deepcopy(state)
    state_new = np.array(state_new, dtype=np.int)

    i = 0
    while i < len(state):
        j = 0
        while j < len(state[0]):
            n_counter = 0
            if j == 0 and i == 0:
                n_counter += np.count_nonzero(state[i, j + 1] == 1)
                n_counter += np.count_nonzero(state[i + 1, j:j + 2] == 1)

            elif j == len(state[1]) - 1 and i == 0:
                n_counter += np.count_nonzero(state[i, j - 1] == 1)
                n_counter += np.count_nonzero(state[i + 1, j - 1:j + 1] == 1)

            elif j == 0 and i == len(state) - 1:
                n_counter += np.count_nonzero(state[i - 1, j:j + 2] == 1)
                n_counter += np.count_nonzero(state[i, j + 1] == 1)

            elif j == len(state[1]) - 1 and i == len(state) - 1:
                n_counter += np.count_nonzero(state[i - 1, j - 1:j + 1] == 1)
                n_counter += np.count_nonzero(state[i, j - 1] == 1)

            elif j == 0:
                n_counter += np.count_nonzero(state[i - 1, j:j + 2] == 1)
                n_counter += np.count_nonzero(state[i, j + 1] == 1)
                n_counter += np.count_nonzero(state[i + 1, j:j + 2] == 1)

            elif i == len(state) - 1:
                n_counter += np.count_nonzero(state[i - 1, j - 1:j + 2] == 1)
                n_counter += np.count_nonzero(state[i, j - 1] == 1)
                n_counter += np.count_nonzero(state[i, j + 1] == 1)

            elif j == len(state[1]) - 1:
                n_counter += np.count_nonzero(state[i - 1, j - 1:j + 1] == 1)
                n_counter += np.count_nonzero(state[i + 1, j - 1:j + 1] == 1)
                n_counter += np.count_nonzero(state[i, j - 1] == 1)

            elif i == 0:
                n_counter += np.count_nonzero(state[i + 1, j - 1:j + 2] == 1)
                n_counter += np.count_nonzero(state[i, j - 1] == 1)
                n_counter += np.count_nonzero(state[i, j + 1] == 1)

            else:
                n_counter += np.count_nonzero(
                    state[i - 1, j - 1:j + 2] == 1)
                n_counter += np.count_nonzero(
                    state[i + 1, j - 1:j + 2] == 1)
                n_counter += np.count_nonzero(state[i, j - 1] == 1)
                n_counter += np.count_nonzero(state[i, j + 1] == 1)

            value = 0
            if n_counter == 2 and state[i, j] == 1:
                value = 1
            elif n_counter == 3 and state[i, j] == 1:
                value = 1
            elif n_counter < 2 and state[i, j] == 1:
                value = 0
            elif n_counter > 3 and state[i, j] == 1:
                value = 0
            elif n_counter == 3 and state[i, j] == 0:
                value = 1

            state_new[i, j] = value
            j += 1
        i += 1

    return state_new

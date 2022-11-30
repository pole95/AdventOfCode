import itertools
new_throws = list(itertools.product([1,2,3], repeat=3))
sum_throws = [sum(_) for _ in new_throws]
freq_throws = {_:sum_throws.count(_) for _ in set(sum_throws)}
print(freq_throws)

import functools

@functools.cache
def calculate_number_of_wins(start_score_1, start_pos_1, start_score_2, start_pos_2, turn):
    res = 0

    for el in freq_throws:
        pos_1 = start_pos_1
        pos_2 = start_pos_2
        score_1 = start_score_1
        score_2 = start_score_2
        # roll += el
        if turn == 0: # Player 1
            pos_1 += el
            r = pos_1 % 10
            if r == 0:
                score_1 += 10
            else:
                score_1 += r

            if score_1 >= 21:
                res += freq_throws[el]
            else:
                res += freq_throws[el] * calculate_number_of_wins(score_1, pos_1, score_2, pos_2, 1)
        else:
            pos_2 += el
            r = pos_2 % 10
            if r == 0:
                score_2 += 10
            else:
                score_2 += r

            if score_2 >= 21:
                continue # Do not count
            else:
                res += freq_throws[el] * calculate_number_of_wins(score_1, pos_1, score_2, pos_2, 0)

    return res

pos_1 = 4 
pos_2 = 5
score_1 = 0
score_2 = 0
wins = calculate_number_of_wins(score_1, pos_1, score_2, pos_2, 0)
print(wins)

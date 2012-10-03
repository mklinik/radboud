S    #  ->  q1   L
S    0  ->  S    R
S    1  ->  S    R
q1   1  ->  b1   #
q1   0  ->  a1   #
q1   #  ->  q2   R
b1   #  ->  qb   L
a1   #  ->  qa   L
qb   1  ->  qb   L
qb   0  ->  aa   1
qb   #  ->  q2   1
aa   1  ->  qa   L
qa   0  ->  qa   L
qa   1  ->  bb   0
qa   #  ->  q2   0
bb   0  ->  qb   L
q2   0  ->  q2   R
q2   1  ->  q2   R
q2   #  ->  halt #

Tape:
#100110110101011000010#
S    #  ->  S1   L
S    a  ->  S1   L
S    b  ->  S1   L
S1   #  ->  q1   R
S1   a  ->  S1   L
S1   b  ->  S1   L
q1   #  ->  S    R
q1   a  ->  qa   #
q1   b  ->  qb   #
qa   #  ->  aa   R
qb   #  ->  bb   R
aa   #  ->  q2   a
aa   a  ->  aa   R
aa   b  ->  qb   a
qb   a  ->  bb   R
bb   #  ->  q2   b
bb   b  ->  bb   R
bb   a  ->  qa   b
qa   b  ->  aa   R
q2   a  ->  S    R
q2   b  ->  S    R

Tape:
#a##baab###########abbaaaabb#
## Authors

hadriwer :)

## Abstract

The goal of this repo is to implement different game's solver in Ocaml.

## Usage 

Respect the structure of the file is very important.  
**do not put comment on the file as shown below with '#'**

### Sudoku

```
Sudoku_solver.solver path; (* in the main.ml *)
```
**In files/sudoku.txt :**  
Spaces between element with ' ' (space) and '.' to describe an empty square.
```
4           # size of the sudoku
. . . 1     # first line 
. . 3 .     # second line
. . 1 .     # .. line
. . 2 .     # last line
```

Don't worry if the initial grid of the sudoku is incorrect the programm will raise an error.

### Wordle

The wordle is made in an interactive way. Two langage are accepted french "mots.txt" and english "words.txt".

```
Wordle_solver.solve ~beg:(None) Wordle_solver.ENTROPY path;
(* with entropy *)
```
or
```
Wordle_solver.solve ~beg:(None) Wordle_solver.RANDOM path;
(* with random *)
```

**Entropy method is better than Random method.**

On the terminal :

```
guess : roate                       
Usage for each char separated by spaces : n -> not in word ; w -> wrong spot ; c -> correct spot
your turn : 

```

then write following the rules and \<enter\>:

```
guess : roate                       
Usage for each char separated by spaces : n -> not in word ; w -> wrong spot ; c -> correct spot
your turn : 
n n n n n               # example of what you might write (in this case all the letter are not in the final words)
```

Repeat steps :

```
guess : roate                       
Usage for each char separated by spaces : n -> not in word ; w -> wrong spot ; c -> correct spot
your turn : 
n n n n n
[Notin(r) Notin(o) Notin(a) Notin(t) Notin(e) ]
guess : husky
Usage for each char separated by spaces : n -> not in word ; w -> wrong spot ; c -> correct spot
your turn : 

n c w n c
[Notin(h) Correct (u, 1) Wrong (s,2) Notin(k) Correct (y, 4) ]
guess : sully
Usage for each char separated by spaces : n -> not in word ; w -> wrong spot ; c -> correct spot
your turn : 

c c c c c
[Correct (s, 0) Correct (u, 1) Correct (l, 2) Correct (l, 3) Correct (y, 4) ]
The word is : sully
```

### Fubuki

```
Fubuki_solver.solve path; (* in main.ml *)
```

**In files/fubuki.txt** :  
Spaces between element with ' ' (space) and '.' to describe an empty square.
```
3                   # size of the grid
1 3 5 7 8 9         # possible value to add on empty square
17 18 10            # result column (on the left)
20 12 13            # result line   (on the bottom)
4 . .               
. 6 .
. . 2
```

Is equivalent to solve :
```
4 + ? + ? = 17
+   +   +
? + 6 + ? = 18
+   +   +
? + ? + 2 = 10
⏸   ⏸   ⏸
20  12  13
```
with '?' in the domain [1, 3, 5, 7, 8, 9]. **It can be use once**.
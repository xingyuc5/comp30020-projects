%% COMP30020 Declarative Programming Project 2
%% File: proj2.pl
%% Title: Maths Puzzles
%% Author: Xingyu Chen
%% Unimelb ID: 930428
%% Account: xingyuc5
%% Email: xingyuc5@student.unimelb.edu.au

%%%%%%%%%%%%%%%%%%%%%%%%%% About the Project %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Purpose of this project: Come out with a predicate: puzzle_solution/1.
%% puzzle_solution/1. takes in a puzzle, which is a list of lists representing
%% a maths puzzle. This predicate would hold if the puzzle is valid and find
%% the solution to the puzzle.


%% Puzzle is the representation of a maths puzzle
%% List of lists: Outer list represents the whole puzzle, each element is a row.
%%                Inner list represents a row, each element is a number.


%% Representation of 3x3 puzzle in nested list:
%% [[0, colHeader1, colHeader2, colHeader3],
%%  [rowHeader1, number, number, number],
%%  [rowHeader2, number, number, number],
%%  [rowHeader3, number, number, number]]


%% A solution of puzzle is a unique combination of numbers in the squares of
%% puzzles, which satisfies the following constraints:
%%     1. Numbers in squares range from [1..9]
%%     2. Numbers in each row/col are all distinct from each other
%%     3. Sum/Product of numbers in a row/col equals to the row/col header
%%     4. Numbers on the top-left to bottom-right diagonal are the same

%% Our strategy: Use constraint programming's constrain and search strategy,
%%               apply constraints mentioned above to reduce search space of 
%%               possible numbers for each square, until each square has a 
%%               domain of size one, which represent a solution to puzzle.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Library Used %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This program uses clpfd library:
%%     #=/2.
%%     label/1.
%%     transpose/2.
%%     ins/2.
%%     sum/3.

:-ensure_loaded(library(clpfd)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%% Solution Predicate %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% puzzle_solution(+Puzzle):
%% Puzzle: a list of lists representing a maths puzzle. Each element in the
%%         list represents a row in the maths puzzle. 
%% This predicate fills the squares with numbers which form the unique solution 
%% to the puzzle if Puzzle is valid.
puzzle_solution(Puzzle):-
    
    %% Apply diagonal constraint.
    diagonal_constraint(Puzzle),
    
    %% Transpose puzzle, Rows in TransposedPuzzle are Columns in Puzzle.
    transpose(Puzzle, TransposedPuzzle),
    
    %% Try to solve the puzzle by rows.
    solve(Puzzle),
    
    %% Try to solve the puzzle by columns.
    solve(TransposedPuzzle),
    
    %% Label the puzzle to ensure all squares are grounded.
    maplist(label, (Puzzle)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Helper Predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% diagonal_constraint(+[_|Rows]):
%% [_|Rows]: a nested list representing a maths puzzle. 
%% This predicate unifies all the squares on the top-left to bottum-right 
%% diagonal, excluding the top-left corner. 
diagonal_constraint([_|Rows]):-
    
    %% remove the first row of puzzle which is the row contains column headers.
    diagonal1(Rows).


%% diagonal1(+Rows):
%% Rows: nested list representing all rows except the first row in a maths 
%% puzzle.
diagonal1(Rows):-
    
    %% Get the first row in the body of puzzle.
    Rows = [Row1|Rest],
    
    %% Get the first square on the diagonal which is (row1,index1).
    nth0(1, Row1, DiagValue),
    
    %% Unifies all other squares on diagonal with first square.
    diagonal2(DiagValue, 2, Rest).


%% diagonal2(+DiagValue, +Index, +Rows):
%% DiagValue: Value of square on diagonal
%% Index: The index of diagonal square in current row
%% Rows: The remaining rows in puzzle.
%% This predicate recursively call itself until all diagonal squares are unified
%% with the first diagonal square. (i.e. they all have the same value)
diagonal2(_,_,[]).
diagonal2(DiagValue, Index, Rows):-
    
    %% Get first row of remaining rows.
    Rows = [Row1|Rest],
    
    %% Get the diagonal square in first row
    nth0(Index, Row1, DiagCurr),
    
    %% Let current diagonal square equals to last diagonal square.
    DiagCurr #= DiagValue,
    
    %% Increment index in order to get diagonal square from next row
    Index1 is Index+1,
    
    %% Tail recursion, call this predicate with updated inputs.
    diagonal2(DiagCurr, Index1, Rest).


%% num_range(+[_|Row]):
%% [_|Row]: a list representing a row in the maths puzzle.
%% This predicate let the squares in Row (except header) be elements of domain
%% 1..9. 
num_range([_|Row]):-
    Row ins 1..9. 


%% distinct_row(+[_|Row]):
%% [_|Row]: a list representing a row in the maths puzzle.
%% This predicates ensure the squares in Row (except header) are pair-wise 
%% distinct from each other.
distinct_row([_|Row]):-
    all_distinct(Row).


%% solve([_|Rows]):
%% [_|Rows]: a nested list representing a maths puzzle. 
%% This predicate apply constraints on each row of puzzle.
solve([_|Rows]):-
    
    %% Apply range constaint to ensure squares in a row are in range 1..9.
    maplist(num_range, Rows),
    
    %% Apply dinsinct constraint to ensure squares in a row are pairwise 
    %% distinct.
    maplist(distinct_row, Rows),
    
    %% Apply sum/product constraint.
    maplist(solve_row, Rows).


%% solve_row(+[Header|Row]):
%% [Header|Row]: A list representing a row in puzzle
%% This predicate ensure either the sum of squares in a row equlas to header,
%% or the product of them equlas to header.
solve_row([Header|Row]):-
    sum(Row, #=, Header);
    product_constraint(Header, Row).


%% product_constraint(+Product, +List):
%% Product: An interger
%% List: A list of integers.
%% This predicate let the products of elements from List equals to Product.
product_constraint(Product, List):-
    
    %% Number 1 is used as identity element.
    product_constraint(Product, 1, List).

%% product_constraint(+Product, +Product0, List):
%% Prodcut: The target product we want.
%% Product0: Accumulator, contains the current product of previous elements 
%%           from List.
%% List: A list of integers.
%% This predicate let the sum of numbers from List equal to Product.

%% Base case of predicate is when Accumulator equals to Product, and List is
%% empty list.
product_constraint(Product, Product, []).
product_constraint(Product, Product0, [L|Ls]):-
    
    %% Let Product1 equals to Product0 multiplies head of List.
    Product1 #= Product0 * L,
    
    %% Tail recursion, call this predicate with updated inputs until reach base
    %% case.
    product_constraint(Product, Product1, Ls).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  End  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




















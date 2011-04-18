-module(sequential).
-export([data_types/0,
         pattern_matching/0,
         all_fibs/1,
         all_squared/1,
         quicksort/1,
         reverse/1,
         reverse2/1,
         conditionals/1]).

%% All basic stuff is explained here
%%  http://www.erlang.org/doc/reference_manual/users_guide.html

%% record definition. This is explained in data_types/0 function
-record(person, {name, age, sex = yes_please}).


%%% Data types
%% any piece of data (of any type) is a *term*. Remember that word - *TERM*
data_types() ->
    %% variables
    %%
    %% In erlang variables start with capital letter. This is a bit
    %% strange to call them variables as you can never change their
    %% value - you have only one chance to set the value of Variable.
    %%
    %% This is not as limiting as it may seem at first. If you want to
    %% 'modify' the variable you create new one and forget previous
    %% one (garbage collector will take care of the memory)
    Number1 = 1,
    %% Number1 = Number1 + 1, % THIS IS INVALID!
    Number2 = Number1 + 1,
    Number3 = Number2 + 1,
    Number4 = Number3 + 1,
    io:format("~p~n", [Number4]),
    %% In Erlang most data structures are persistent. That means every
    %% operation on the data structure that would modify it in C, will
    %% now return new data structure. That way you can always access
    %% old and new version of data.
    %%
    %% Persistent data structure are extremely powerful and useful,
    %% but we won't cover this topic now.

    %% integers
    Integer  = 1000 + 300 + 30 + 7, % this is standard, small integer
    %% fixnums - 28bit signed integers on 32bit machines, 60bit on 64bit architecture
    %% bignums - above 28bit/60bit machines switches to bignum implementation
    %% fixnums are obviously more efficient than bignums
    BigInteger = 3 + 5 + 17 + 257 + 65537 + 4294967297 + 18446744073709551617, % no overflow. at last!
    AnswerToLifeUniversAndEverything = 2#101010, % You can use different bases easily
    AnswerToLifeUniversAndEverything = 32#1A, % You can use any base in 2..36 (here we use 32). 32#1A = 2#101010 = 42 = 10#42
    AsciiA = $A, % same as 'A' in C. $A == 32
    AsciiNewline = $\n, % '\n'
    io:format("~p ~p ~p ~p ~p~n", [Integer, BigInteger, AnswerToLifeUniversAndEverything, AsciiA, AsciiNewline]),

    %% numbers
    E = 2.7182818283490, % IEEE 754 64-bit format
    G = 6.674286767e-11, % scientific notation
    io:format("~p ~p~n", [E, G]),

    %% atoms
    %%
    %%  + Variables start with capital letter while atoms with
    %%    lowercase letter. atoms are like enums in other languages.
    %%  + Comparing atoms for equality is really fast (like comparing
    %%    integers).
    %%  + You create atoms on the fly - by using the name.
    %%  + They are usefull for tagging things.
    %%
    %% You can convert any string to atom with list_to_atom/1 function
    %%
    %% *WARNING*: Atoms are not garbage collected, so creating too many
    %% canl be a little dangerous
    Variable1 = atom1,
    Variable2 = atom2,
    Variable3 = atom3,
    %% atom1 = atom2, % exception error: no match of right hand side value atom2 (see pattern matching)
    atom1 = Variable1, % this is ok
    ExitAtom = 'EXIT', % strings in '' are atoms too.
    EmailAtom = 'me@mail.com', % NOT only letters are allowed

    %% lists - singly linked list
    %%
    %% Basic Erlang data structure (as in many functional programming languages)
    %% Use lists module to process lists
    %%
    %% You use lists to store variable number of things (you don't
    %% know in code how many things you will process). Otherwise use
    %% tuples
    %%
    %% Accessing nth element is O(n) operation
    %%
    %% Usually it is faster to create reversed list while processing
    %% the data and reverse it at the end with lists:reverse
    List1 = [1,"hello",myatom,'another@atom',[]],
    List2 = lists:seq(1,4),
    Head = hd(List2), % == 1
    Tail = tl(List2), % == [2,3,4]
    List3 = [0|List2], % == [0,1,2,3,4]
    List4 = List2 ++ [5,6,7,8], % == [1,2,3,4,5,6,7,8]
    io:format("~p~n~p~n~p~n~p~n", [List1, List2, List3, List4]),

    %% list comprehensions
    Numbers = lists:seq(1,10), % [1,2,3,4,5,6,7,8,9,10]
    NumbersSquared = [ N * N || N <- Numbers ], % create lists of N^2 for each N in Numbers list
    NumbersSquaredEven = [ N || N <- NumbersSquared, N rem 2 == 0 ], % take all even N from NumbersSquared list
    SmallPythagorean = [ {A,B,C} || A <- Numbers, B <- Numbers, C <-Numbers,
                                    A*A + B*B == C*C, A < B, B < C ], % guess what is computed here
    io:format("~p~n~p~n", [NumbersSquaredEven, SmallPythagorean]),

    %% strings
    %%
    %% Strings are lists of integers. If the all integers in the list
    %% are printable then it is assumed that the list is a string. All
    %% following right hand side expressions mean the same:
    StringHello = "Hello",
    StringHello = [$H, $e, $l, $l, $o],
    StringHello = [72,101,108,108,111],
    io:format("hello string = ~p~n", [StringHello]),   % print: Hello
    io:format("hello string = ~w~n", [StringHello]),   % print: [72,101,108,108,111]
    io:format("hello string = ~p~n", [[0|StringHello]]), % print: [0,72,101,108,108,111] (very useful trick in the shell.)

    %% tuples
    %%
    %% Tuples are as popular as lists. n-tuple is a sequence of n elements.
    %% Use tuples if you have fixed number of things to store
    %%
    %% You can do random access to elements in tuple. Elements are
    %% stored contiguously in memory. Access to elements is in
    %% constant time
    Tuple1 = {first_element, 2, "Third element", 'fourth!@#$element'},
    T1 = element(1, Tuple1), % retrieve 1st element
    T2 = element(2, Tuple1), % retrieve 2nd element
    T3 = element(3, Tuple1), % retrieve 3rd element
    NewValue = 22,
    Tuple2 = setelement(2, Tuple1, NewValue), % remember persistent data structures?
                                              % This sets second element of Tuple1 to NewValue
    io:format("~p ~p~n", [Tuple1, Tuple2]),

    %% records
    %%
    %% Records are like C structs. Records are also like tuples with named elements.
    %%
    %% see person record definition at the top. You define record like this:
    %% -record(RECORD_NAME, {FIELD_NAME1, FIELD_NAME2,...})
    %% you can give a default value for each field (see person record definition at the top)
    %%
    %% Records are implemented as a hack. Every record is a tuple with
    %% first element set as the atom describing type of the record. So
    %% #person{name = "john", age=25, sex = male}
    %% is the same as
    %% {person, "john", 25, male}
    %%
    %% You are strongly discouraged to use this knowledge.
    %%
    %% See http://www.erlang.org/doc/man/shell.html and read commands
    %% rd, rf, rl,rp, rr that are used to handle records in the shell
    %% (as record syntax is not available in the shell)
    PersonTuple = {"john", 25, male}, % this is tuple describing person
    PersonRecord = #person{name = "john", age = 25, sex = male},  % create record variable
    PersonRecordDefaultSex = #person{name = "mike", age = 20}, % using default values
    PersonName = PersonRecord#person.name, % accessing field is very convenient
    PersonRecordOlder = PersonRecord#person{age = PersonRecord#person.age + 20}, % modifying records
    io:format("~p ~p~n", [PersonRecord, PersonRecordOlder]),

    ok.



%%% pattern matching
pattern_matching() ->
    %% = is pattern matching operator (NOT assignment operator like in
    %% C). Both sides of = must be erlang terms. For example
    1 = 1,
    {tuple, "string", [list, with, atoms]} = {tuple, "string", [list, with, atoms]},
    %% as long as left side is the same as right side (after replacing
    %% variables with its values), everything is ok. If sides are not
    %% identical, you get matching error.
    %%
    %% You can use variables. You can use any variable on left side,
    %% but only variable with value on right side (bound variables).
    %%
    %% If you use unbound variable (variable without value) on left
    %% side it will be bound to part of right side that matches
    X = 1, % this is ok, now X is 1
    %% X = 2, % this will fail now as left hand side is not the same as right hand side
    Y = {tuple, "string", [list, with, atoms]}, % this is also ok
    %% 1 = UnboundVariable % this will fail with "variable 'UnboundVariable' is unbound" error
    %%
    %% The real power of this feature comes with more complex expressions on left hand side
    {tuple, String, [ListFirst, with, ListThird]} = Y,
    %% String, ListFirst, ListThird are unbound variables. In this expressions they will match
    %% String = "string"
    %% ListFirst = list
    %% ListThird = atoms
    %%
    %% you can use bound variables on left hand side. If they match to
    %% right hand side counterpart then everything is ok
    {tuple, String, [ListFirst, ListSecond, ListThird]} = Y,
    %% now ListSecond = with

    %% this works with every Erlang data type (i.e. with records).
    %% Pattern matching is so usefull that you will see it all the
    %% time in Erlang code.
    ok.


%%% functions, funs, guards, BIFs

%% we defined two functions (data_types/0 and pattern_matching/0). To
%% define a function you write its name (atom), list of arguments in ()
%% separated with comman, arrow (->) and instructions that should be
%% exected.
%%
%% You can use pattern matching in function arguments part
fib(0) -> % if the argument is 0 then executed this version of function (clause)
    1; % value of last expressions is returned value (here it is 1). ';' separates clauses
fib(1) -> % if the argument is 1 then try this one
    1;
fib(N) -> % here we use unbound variable N. If previous clauses failed to match. This one is used. Argument is bound to N
    fib(N-1) + fib(N-2). % call fib functions twice (with different arguments) and return sum of their result
                         % . ends function definition (use it only in the last clause)

%% as you can see fib(0) and fib(1) are exactly the same. You can
%% write this code using guards.
fib2(N) when N < 2 -> % this will be used only when N < 2. This is a guard.
    1;
fib2(N) ->
    fib(N-1) + fib(N-2).
%% you can use conjunction of conditions in guard separating them with comma
%% you can use disjunction of contiguously in that separating them with semicolon

%% fib and fib2 are in fact fib/1, fib2/1 (the number after slash is
%% the number of arguments). fib/1 and fib/2 would be two totally
%% different functions (this is *not* overloading as in C)

%% In guards you can't use your own functions. You can use only Build
%% In Functions (BIFs for short). For list of BIFs consult
%% http://www.erlang.org/doc/man/erlang.html

%% erlang is functional programming language, so functions are first
%% class data.  You can store functions in variables, pass functions
%% as arguments to other functions and return functions as a result of
%% function call
all_fibs(N) ->
    lists:map(fun fib/1, lists:seq(1,N)). % create new list with all elements E in [1..N] replaced with fib(E)
                                          % all(5) returns [1,2,3,5,8]

%% you can create functions ad hoc (lambda functions)
all_squared(N) ->
    Fun = fun (A) ->   % create function that squares its argument and store the function in Fun variable
                  A*A
          end,
    lists:map(Fun, lists:seq(1,N)). % apply Fun to every element in [1..N]


%%% recursion, tail recursion
%%
%% In Erlang there you can't use loops. You have much more powerful
%% mechanism - recursion. Recursive functions is a function that calls
%% itself (could be indirectly - function A calls function B which
%% calls A again)
%%
%% our fib and fib2 were recursive. Resursion allows you to write
%% short, concise code. For example we could implement quicksort like
%% this
quicksort([]) ->
    [];
quicksort([E]) ->
    [E];
quicksort([H|T]) ->
    quicksort([E || E <- T, E =< H]) ++ [H] ++ quicksort([E || E <- T, E > H]).

%% look at reverse function that reverses a list. What's wrong with this function?
reverse([]) ->
    [];
reverse([H|T]) ->
    reverse(T) ++ [H].
%% if you don't know - try calling it like this
%% sequential:reverse(lists:seq(1,10000))
%% sequential:reverse(lists:seq(1,100000))
%% and see how much time it takes. Unfortunatly our algorithm uses O(n^2) time and O(n) memory (why?)
%%
%% The problem with this code is that ++ is last operation executed in
%% reverse. This means that every call to reverse must leave it's
%% state on the stack and compute reverse(T) (to be able to backtrack
%% to that place later and return result of applying ++ function)
%%
%% If last call was to reverse (and this would be the only call to
%% reverse within reverse) then Erlang would not need stack (there is
%% no computation left that you have to come back later to).
%%
%% If you write tail-recursive function (function that calls itself as
%% last operation) then Erlang will optimize it and will not use stack!
%%
%% How to change our reverse function? You could add another
%% parameter, which would be the reversed part of the list.
reverse2([], Reversed) ->
    Reversed;
reverse2([H|T], Reversed) ->
    reverse2(T, [H|Reversed]). % here reverse2 is last call (tail recursive call)

%% it is a bit unfortunate that reverse2/2 has two arguments (users of
%% our library don't want to know internals of reverse2 function)
%%
%% let's write reverse2/1 which will be exported that uses reverse2/2
%% to do the hard job
reverse2(L) ->
    reverse2(L, []).

%% now try
%% sequential:reverse2(lists:seq(1,10000))
%% sequential:reverse2(lists:seq(1,100000))
%% or even
%% sequential:reverse2(lists:seq(1,1000000))
%% sequential:reverse2(lists:seq(1,10000000))

%% This is very common pattern in functional programming. Try to
%% convert fib to tail recursive version (you will need at least two
%% additional arugments)


%%% conditionals

%% You could live without 'if' statement - you could use pattern
%% matching in function clauses for everything. That would be a bit
%% awkward as for every branch you would have to code a function.
%%
%% In Erlang you have 'if' and 'case' statements to control flow
conditionals(N) ->
    %% with 'case' you use pattern matching + guards and with 'if' you
    %% use guards

    %% case example
    case fib(N) of
        1 -> % this is pattern matching
            io:format("small fib~n");
        F when F > 100000 -> % with guards
            io:format("huge fib~n");
        _Else -> % match everything
            io:format("normal fib~n")
    end,

    %% if example
    Fib = fib(N),
    if
        Fib == 1 -> % predicate (guard) before arrow (->)
            io:format("small fib~n");
        Fib > 10000 ->
            io:format("huge fib~n");
        true ->
            io:format("normal fib~n")
    end.


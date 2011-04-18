-module(sequential).
-export([data_types/0]).

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

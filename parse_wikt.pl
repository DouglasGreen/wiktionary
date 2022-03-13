main :-
    setup_call_cleanup(
        open('enwiktionary-latest-pages-articles.xml', read, In),
        read_file(In, '', -1),
        close(In)
    ).

main :- !.

% Read and process the current line of the file.
read_file(In, Title, NameSpace) :-
    stream_line(In, Line),
    string_codes(Line, Codes),

    % Tokenize the current line of codes.
    phrase(lexer(Tokens), Codes),
    (
        % Parse a new title.
        (phrase(parse_title(Title1), Tokens), !);

        % Parse a new namespace identifier and write the title for this namespace.
        (phrase(parse_namespace(NameSpace1), Tokens), write_title(Title, NameSpace1), !);

        % Continue using the old title and namespace identifier.
        (parse(Tokens, Title, NameSpace), Title1 = Title, NameSpace1 = NameSpace, !)
    ),
    read_file(In, Title1, NameSpace1).

% Read a line from the stream.
stream_line(In, Line) :-
    repeat,
    (   read_line_to_string(In, Line0),
        Line0 \== end_of_file
    ->  Line0 = Line
    ;   !,
        fail
    ).

% Recognize and label a list of tokens.
lexer([Token|Tokens]) -->
    lexem(Token),
    lexer(Tokens).

lexer(Tokens) -->
    [S],
    {char_type(S, space)},
    lexer(Tokens).

lexer([]) -->
    [].

% Recognize the current token (lexem).
lexem(word(W)) -->
    char(C),
    chars(Cs),
    !,
    {atom_chars(W, [C|Cs])}.

lexem(punct(P)) -->
    punct(P).

% Recognize a character. Letters and numbers are combined in the same token.
char(C) -->
    [C],
    {
        char_type(C, alpha);
        char_type(C, digit)
    }.

% Recognize a list of characters.
chars([C|Cs]) -->
    char(C),
    chars(Cs).

chars([]) -->
    [].
    
% Recognize a punctuation mark.
punct(P) -->
    [C],
    {
        char_type(C, punct),
        atom_chars(P, [C])
    }.

parse(Tokens, _, dict) :-
    phrase(parse_page, Tokens),
    writeln('---'),
    !.

% We have to read all titles regardless of namespace because namespace tag comes after title tag.
parse(Tokens, Title, _) :-
    phrase(parse_title(Title), Tokens),
    !.

% Parse a list of tokens.
parse(Tokens, _, dict) :-
    phrase(parse_heading(Heading, N), Tokens),
    write_hashes(N),
    write(' '),
    writeln(Heading).

% Ignore other XML.
parse(Tokens, _, dict) :-
    phrase(parse_other_xml, Tokens, _),
    !.

parse(Tokens, _, dict) :-
    write('Unrecognized: '),
    writeln(Tokens),
    halt.

parse(_, _, NameSpace) :-
    NameSpace \= dict.

parse_page -->
    [punct(<), word(page), punct(>)].

% Write a number of hashes. Heading level 1 is reserved for page titles so write heading at level 2 or more.
write_hashes(N) :-
    N > 2,
    write('#'),
    N1 is N - 1,
    write_hashes(N1).

write_hashes(2) :-
    write('##').

write_hashes(1) :-
    write('##').

% Write the current title if we're in the dictionary namespace.
write_title(Title, dict) :-
    write('# '),
    writeln(Title).

write_title(_, N) :-
    N \= dict.

% Parse a title tag.
parse_title(Title) -->
    [punct(<), word(title), punct(>)],
    parse_words(Title),
    [punct(<), punct(/), word(title), punct(>)].

% Parse a namespace tag.
parse_namespace(NameSpace) -->
    [punct(<), word(ns), punct(>)],
    parse_words([W]),
    {
        atom_number(W, N),
        namespace(N, NameSpace)
    },
    [punct(<), punct(/), word(ns), punct(>)].

% Parse a MediaWiki heading surrounded by equal signs.
parse_heading(Ws, N) -->
    equals(N),
    parse_words(Ws),
    equals(N).

parse_heading(Ws, N) -->
    equals(N),
    parse_words(Ws),
    equals(N).

% Parse any other line beginning with an XML tag.
parse_other_xml -->
    [punct(<), word(W)],
    { W \== page, W \== title }.

parse_other_xml -->
    [punct(<), punct(/)].

% Parse the given number of equal signs.
equals(N1) -->
    [punct(=)],
    equals(N),
    {N1 is N + 1}.

equals(1) -->
    [punct(=)].

% Parse a list of words.
parse_words([W|Ws]) -->
    parse_word(W),
    parse_words(Ws).

parse_words([W]) -->
    parse_word(W).

parse_word(W) -->
    [word(W)].

parse_word(W) -->
    [punct(W)],
    { W \== '<', W \== '>' }.

% Convert an arbitrary namespace number into a meaningful atom.
namespace(0, dict).

namespace(N, other) :-
    N \= 0.

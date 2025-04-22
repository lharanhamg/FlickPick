:- use_module(library(csv)).

% -------------------- Carregamento do CSV --------------------
carregar_filmes(Fs) :-
    csv_read_file('movies.csv',
                  [ movie(id,title,year,duration,genre,rating_imdb,oscar)
                  | Fs
                  ],
                  [ functor(movie), convert(true), strip(true) ]).

% -------------------- Funções de Leitura de Filtros --------------------

% Ano mínimo ou sem restrição (0)
obter_valor_minimo_ano(MinAno) :-
    repeat,
    writeln('Escolha um ano mínimo entre 1980 e 2024 (0 = sem restrição):'),
    write('> '), flush_output,
    read_line_to_string(user_input, Input),
    ( atom_number(Input, MinAno),
      ( MinAno =:= 0 ; (MinAno >= 1980, MinAno =< 2024) )
    -> !
    ; writeln('Ano inválido. Tente novamente.'), fail ).

% Duração máxima ou sem restrição (0)
obter_valor_maximo_duracao(MaxDur) :-
    repeat,
    writeln('Escolha a duração máxima em minutos (0 = sem restrição):'),
    write('> '), flush_output,
    read_line_to_string(user_input, Input),
    ( atom_number(Input, MaxDur), MaxDur >= 0
    -> !
    ; writeln('Duração inválida. Tente novamente.'), fail ).

% Gêneros (lista separada por vírgula) ou sem restrição (0)
obter_valor_genero(Genres) :-
    repeat,
    writeln('Escolha um ou mais gêneros separados por vírgula (ex: Action,Comedy) ou 0 (sem restrição):'),
    write('> '), flush_output,
    read_line_to_string(user_input, Input),
    ( Input == "0"
    -> Genres = []
    ; split_string(Input, ",", " ", List),
      List \= [],
      maplist(validar_genero, List),
      Genres = List
    -> !
    ; writeln('Entrada inválida. Use apenas letras, dígitos e vírgulas.'), fail
    ).

% Rating mínimo ou sem restrição (0)
obter_valor_rating_imdb(MinRat) :-
    repeat,
    writeln('Escolha o rating mínimo do IMDb (0 = sem restrição, até 10):'),
    write('> '), flush_output,
    read_line_to_string(user_input, Input),
    ( atom_number(Input, MinRat), MinRat >= 0, MinRat =< 10
    -> !
    ; writeln('Rating inválido. Tente novamente.'), fail ).

% Oscars mínimos ou sem restrição (0)
obter_valor_minimo_oscars(MinOsc) :-
    repeat,
    writeln('Escolha o número mínimo de Oscars ganhos (0 = sem restrição):'),
    write('> '), flush_output,
    read_line_to_string(user_input, Input),
    ( atom_number(Input, MinOsc), MinOsc >= 0
    -> !
    ; writeln('Número inválido. Tente novamente.'), fail ).

% Validação simples de string de gênero
validar_genero(Gen) :-
    Gen \= "",
    string_chars(Gen, Chs),
    maplist({}/[C]>>(char_type(C, alnum); char_type(C, space)), Chs).

% -------------------- Montagem de Critérios --------------------

montar_criterios(MinAno, MaxDur, Genres, MinRat, MinOsc, Crits) :-
    ( MinAno > 0 -> C1 = [filtrar_ano(MinAno)] ; C1 = [] ),
    ( MaxDur > 0 -> C2 = [filtrar_duracao(MaxDur)] ; C2 = [] ),
    ( Genres \= [] -> C3 = [filtrar_multiplos_genero(Genres)] ; C3 = [] ),
    ( MinRat > 0 -> C4 = [filtrar_rating_imdb(MinRat)] ; C4 = [] ),
    ( MinOsc > 0 -> C5 = [filtrar_oscar(MinOsc)] ; C5 = [] ),
    append([C1,C2,C3,C4,C5], Crits).

% -------------------- Predicados de Filtro --------------------

filtrar_ano(Min, movie(_,_,Ano,_,_,_,_))         :- Ano >= Min.
filtrar_duracao(Max, movie(_,_,_,Dur,_,_,_))     :- Dur =< Max.
filtrar_rating_imdb(Min, movie(_,_,_,_,_,Rat,_)) :- Rat >= Min.
filtrar_oscar(Min, movie(_,_,_,_,_,_,Osc))      :- Osc >= Min.

% Filme deve conter todos os gêneros escolhidos
filtrar_multiplos_genero(Genres, movie(_,_,_,_,GenStr,_,_)) :-
    split_string(GenStr, ",", " ", MovieGenres),
    forall(member(G, Genres), member(G, MovieGenres)).

% Aplica todos os filtros em sequência
filtrar_filmes([], Fs, Fs).
filtrar_filmes([F|Fs], In, Out) :-
    include(F, In, Mid),
    filtrar_filmes(Fs, Mid, Out).

% -------------------- Paginação e Impressão --------------------

% Remove os N primeiros elementos de uma lista
drop(0, L, L) :- !.
drop(_, [], []) :- !.
drop(N, [_|T], R) :- N > 0, N1 is N-1, drop(N1, T, R).

% Imprime um filme numerado
print_indexed(movie(ID,T,Y,D,G,R,O), I) :-
    format('~w - ID: ~w, Title: ~w, Year: ~w, Duration: ~w mins, Genre: ~w, IMDB: ~w, Oscar: ~w~n',
           [I,ID,T,Y,D,G,R,O]).

% Exibe até N filmes, devolvendo o restante
exibir_index([], _, _, []).
exibir_index(Rest, _, 0, Rest) :- !.
exibir_index([M|Ms], I, N, Rem) :-
    N > 0,
    print_indexed(M, I),
    I1 is I+1, N1 is N-1,
    exibir_index(Ms, I1, N1, Rem).

% Menu pós-página
menu(Fs, Page, Rem) :-
    writeln('[1] - Mais filmes'),
    writeln('[2] - Nova busca'),
    writeln('[3] - Sair'),
    write('> '), flush_output,
    read_line_to_string(user_input, Op),
    ( Op = "1" ->
        ( Rem = [] ->
            writeln('Não há mais páginas.'),
            menu(Fs, Page, Rem)
        ; P1 is Page + 1, loop_filmes(Fs, P1)
        )
    ; Op = "2" -> main
    ; Op = "3" -> writeln('Saindo...')
    ; writeln('Opção inválida.'), menu(Fs, Page, Rem)
    ).

loop_filmes(Fs) :- loop_filmes(Fs, 0).
loop_filmes(Fs, Page) :-
    Size = 10,
    Offset is Page * Size,
    drop(Offset, Fs, ToShow),
    ( ToShow = [] ->
        writeln('Não há mais filmes para este filtro.'), Rem = []
    ; Start is Offset + 1,
      exibir_index(ToShow, Start, Size, Rem)
    ),
    menu(Fs, Page, Rem).

% -------------------- Ponto de Entrada --------------------

main :-
    carregar_filmes(Fs),
    obter_valor_minimo_ano(A),
    obter_valor_maximo_duracao(D),
    obter_valor_genero(Gs),
    obter_valor_rating_imdb(R),
    obter_valor_minimo_oscars(O),
    montar_criterios(A, D, Gs, R, O, Crits),
    filtrar_filmes(Crits, Fs, Filtered),
    loop_filmes(Filtered).

% -------------------- Auxiliar de Gêneros --------------------

listar_generos :-
    carregar_filmes(Fs),
    findall(G,
        ( member(movie(_,_,_,_,GenStr,_,_), Fs),
          split_string(GenStr, ",", " ", L),
          member(G, L)
        ),
        All),
    sort(All, Unique),
    writeln('Gêneros disponíveis no CSV:'),
    forall(member(G, Unique), writeln(G)).

:- use_module(library(csv)).

% -------------------- Comentários sobre o funcionamento do código --------------------
% Este código implementa um sistema de recomendação de filmes em Prolog, utilizando um arquivo CSV como base de dados.
% Abaixo está uma explicação detalhada de cada seção do código:

% -------------------- Carregamento do CSV --------------------
% O predicado carregar_filmes/1 lê os dados do arquivo 'data/movies.csv' e os transforma em uma lista de termos Prolog.
% Cada linha do CSV é convertida em um termo movie(id, title, year, duration, genre, rating_imdb, oscar).
% A biblioteca csv é usada para facilitar a leitura e conversão dos dados.

carregar_filmes(Fs) :-
    csv_read_file('data/movies.csv',
                  [ movie(id,title,year,duration,genre,rating_imdb,oscar)
                  | Fs
                  ],
                  [ functor(movie), convert(true), strip(true) ]).

% -------------------- Funções de Leitura de Filtros --------------------
% Esta seção contém predicados que interagem com o usuário para coletar critérios de busca:
% - obter_valor_minimo_ano/1: Solicita ao usuário um ano mínimo para os filmes (ou 0 para sem restrição).
% - obter_valor_maximo_duracao/1: Solicita uma duração máxima em minutos (ou 0 para sem restrição).
% - obter_valor_genero/1: Permite ao usuário escolher gêneros específicos ou sem restrição (0).
% - obter_valor_rating_imdb/1: Solicita um rating mínimo do IMDb (ou 0 para sem restrição).
% - obter_valor_minimo_oscars/1: Solicita um número mínimo de Oscars ganhos (ou 0 para sem restrição).
% - validar_genero/1: Verifica se um gênero fornecido pelo usuário é válido com base nos dados do CSV.

% Ano mínimo ou sem restrição (0)
obter_valor_minimo_ano(MinAno) :-
    repeat,
    writeln('Escolha um ano mínimo entre 1980 e 2024 (0 = sem restrição):'),
    write('> '), flush_output,
    read_line_to_string(user_input, Input),
    ( atom_number(Input, MinAno),
      ( MinAno =:= 0 ; (MinAno >= 1980, MinAno =< 2024) )
    -> !
    ; writeln('[!]Ano inválido. Tente novamente.'), fail ).

% Duração máxima ou sem restrição (0)
obter_valor_maximo_duracao(MaxDur) :-
    repeat,
    writeln('Escolha a duração máxima em minutos (0 = sem restrição):'),
    write('> '), flush_output,
    read_line_to_string(user_input, Input),
    ( atom_number(Input, MaxDur), MaxDur >= 0
    -> !
    ; writeln('[!]Duração inválida. Tente novamente.'), fail ).

% Predicado auxiliar para verificar se a string contém apenas zeros
all_zeros(S) :-
    string_chars(S, Chars),
    Chars \= [],
    forall(member(C, Chars), C = '0').

% Gêneros (lista separada por vírgula) ou sem restrição (apenas 0 ou múltiplos zeros)
obter_valor_genero(Genres) :-
    repeat,
    writeln('Escolha um ou mais gêneros separados por vírgula (ex: Action,Comedy) ou 0 (sem restrição):'),
    write('> '), flush_output,
    read_line_to_string(user_input, Input),
    ( all_zeros(Input)
    -> Genres = []
    ; split_string(Input, ",", " ", List),
      ( List == []
      -> writeln('[!]Entrada inválida. Use apenas letras, dígitos e vírgulas.'), fail
      ; maplist(validar_genero, List),
        carregar_filmes(Fs),
        findall(G,
            ( member(movie(_,_,_,_,GenStr,_,_), Fs),
              split_string(GenStr, ",", " ", L),
              member(G, L)
            ),
            All),
        sort(All, Unique),
        ( forall(member(Gen, List), member(Gen, Unique))
        -> Genres = List
        ; writeln('[!]Gênero inexistente. Confira os gêneros disponíveis.'), fail
        )
      )
    ).

% Rating mínimo ou sem restrição (0)
obter_valor_rating_imdb(MinRat) :-
    repeat,
    writeln('Escolha o rating mínimo do IMDb (0 = sem restrição, até 10):'),
    write('> '), flush_output,
    read_line_to_string(user_input, Input),
    ( atom_number(Input, MinRat), MinRat >= 0, MinRat =< 10
    -> !
    ; writeln('[!]Rating inválido. Tente novamente.'), fail ).

% Oscars mínimos ou sem restrição (0)
obter_valor_minimo_oscars(MinOsc) :-
    repeat,
    writeln('Escolha o número mínimo de Oscars ganhos (0 = sem restrição):'),
    write('> '), flush_output,
    read_line_to_string(user_input, Input),
    ( atom_number(Input, MinOsc), MinOsc >= 0
    -> !
    ; writeln('[!]Número inválido. Tente novamente.'), fail ).

% Validação simples de string de gênero
validar_genero(Gen) :-
    Gen \= "",
    string_chars(Gen, Chs),
    maplist({}/[C]>>(char_type(C, alnum); char_type(C, space)), Chs).

% -------------------- Montagem de Critérios --------------------
% O predicado montar_criterios/6 cria uma lista de filtros com base nos critérios fornecidos pelo usuário.
% Cada filtro é representado por um predicado que será aplicado aos filmes.

montar_criterios(MinAno, MaxDur, Genres, MinRat, MinOsc, Crits) :-
    ( MinAno > 0 -> C1 = [filtrar_ano(MinAno)] ; C1 = [] ),
    ( MaxDur > 0 -> C2 = [filtrar_duracao(MaxDur)] ; C2 = [] ),
    ( Genres \= [] -> C3 = [filtrar_multiplos_genero(Genres)] ; C3 = [] ),
    ( MinRat > 0 -> C4 = [filtrar_rating_imdb(MinRat)] ; C4 = [] ),
    ( MinOsc > 0 -> C5 = [filtrar_oscar(MinOsc)] ; C5 = [] ),
    append([C1,C2,C3,C4,C5], Crits).

% -------------------- Predicados de Filtro --------------------
% Estes predicados implementam os filtros usados para selecionar filmes:
% - filtrar_ano/2: Filtra filmes com base no ano mínimo.
% - filtrar_duracao/2: Filtra filmes com base na duração máxima.
% - filtrar_rating_imdb/2: Filtra filmes com base no rating mínimo do IMDb.
% - filtrar_oscar/2: Filtra filmes com base no número mínimo de Oscars ganhos.
% - filtrar_multiplos_genero/2: Filtra filmes que contenham todos os gêneros escolhidos pelo usuário.
% - filtrar_filmes/3: Aplica todos os filtros em sequência para obter a lista final de filmes.

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
% Esta seção lida com a exibição dos filmes filtrados:
% - drop/3: Remove os primeiros N elementos de uma lista (usado para paginação).
% - print_indexed/2: Imprime as informações de um filme numerado.
% - exibir_index/4: Exibe até N filmes por página e retorna o restante.
% - menu/3: Exibe opções para o usuário após cada página (mais filmes, nova busca ou sair).
% - loop_filmes/2: Controla a exibição paginada dos filmes.

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
    writeln('O que fazer agora?'),
    writeln('[1] - Mais filmes'),
    writeln('[2] - Nova busca'),
    writeln('[3] - Sair'),
    write('> '), flush_output,
    read_line_to_string(user_input, Op),
    ( Op = "1" ->
        ( Rem = [] ->
            writeln('[!]Não há mais páginas.'),
            menu(Fs, Page, Rem)
        ; P1 is Page + 1,
          loop_filmes(Fs, P1)
        )
    ; Op = "2" -> main_search
    ; Op = "3" -> writeln('Saindo...')
    ; writeln('[!]Opção inválida.'),
      menu(Fs, Page, Rem)
    ).

loop_filmes(Fs) :- loop_filmes(Fs, 0).
loop_filmes(Fs, Page) :-
    Size = 10,
    Offset is Page * Size,
    drop(Offset, Fs, ToShow),
    ( ToShow = [] ->
        writeln('[!]Não há filmes para este filtro.'), Rem = []
    ; Start is Offset + 1,
      exibir_index(ToShow, Start, Size, Rem)
    ),
    menu(Fs, Page, Rem).

% -------------------- Menu Inicial --------------------
% O predicado initial_menu/0 exibe o menu principal do sistema:
% - Opções disponíveis: nova busca, listar gêneros disponíveis ou sair.
% - Cada opção chama o predicado correspondente (e.g., main_search/0 para nova busca).

initial_menu :-
    writeln('Bem-vindo ao Movie Finder!'),
    writeln('Escolha uma opção:'),
    writeln('[1] - Nova busca'),
    writeln('[2] - Listar gêneros disponíveis'),
    writeln('[3] - Sair'),
    write('> '), flush_output,
    read_line_to_string(user_input, Op),
    initial_menu_option(Op).

initial_menu_option("1") :- main_search.
initial_menu_option("2") :- listar_generos, initial_menu.
initial_menu_option("3") :- writeln('Saindo...').
initial_menu_option(_)   :- writeln('[!]Opção inválida.'), initial_menu.

% -------------------- Ponto de Entrada --------------------
% O predicado main/0 é o ponto de entrada do programa. Ele chama o menu inicial.
% O predicado main_search/0 realiza uma busca completa, coletando critérios, filtrando filmes e exibindo os resultados.

main :-
    initial_menu.

main_search :-
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
% O predicado listar_generos/0 exibe todos os gêneros disponíveis no CSV, ordenados e sem duplicatas.
% Ele também informa o número total de gêneros disponíveis.

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
    forall(member(G, Unique), writeln(G)),
    length(Unique, Total),
    format('Total de gêneros disponíveis: ~w~n', [Total]).
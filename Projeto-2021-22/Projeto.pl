%Projeto de Jose Antonio Lopes N 103938
:-[codigo_comum].

%2.1 - Predicado extrai_ilhas_linha/3

/*
  extrai_ilhas_linha(N_L, Linha, Ilhas). N_L (inteiro positivo) correspondente ao numero de uma linha, Linha e uma linha do puzzle e Ilhas a lista ordenada cujos elementos sao as ilhas de Linha 

*/

extrai_ilhas_linha(N_L, Linha, Ilhas) :-
  extrai_ilhas_linha(N_L, Linha, Linha, Ilhas).

extrai_ilhas_linha(_, [], _, []).

extrai_ilhas_linha(N_L, [P | R], OG_L, Ilhas) :-
  P = 0,
  extrai_ilhas_linha(N_L, R, OG_L, Ilhas).

extrai_ilhas_linha(N_L, [P | R], OG_L, [ilha(P, (N_L, I)) | C]) :-
  P > 0,
  length(OG_L, L_T),
  length(R, L_C),
  I is L_T - L_C,
  extrai_ilhas_linha(N_L, R, OG_L, C).
%2.2 - Predicado ilhas/2

/*
  ilhas(Puz, Ilhas), em que Puz e o Puzzle e Ilhas a lista ordenada das ilhas de Puz.
*/
ilhas(Puz, Ilhas) :-
  ilhas(Puz, Ilhas ,0).

ilhas([],[],_).

ilhas([P | R], Ilhas_N, N) :-
  Index is N + 1,
  extrai_ilhas_linha(Index, P, L_ilhas),
  ilhas(R, Ilhas, Index),
  append(L_ilhas, Ilhas, Ilhas_N).


%2.3 - Predicado vizinhas/3

%linha_maior(Ilha_1, Ilha_2) em que a Ilha_2 se encontra numa linha superior a Ilha_1

linha_maior(ilha(_ ,(L, _)), ilha(_, (N_L, _))) :-
  N_L > L.

%coluna_maior(Ilha_1, Ilha_2) em que ilha_2 se encontra numa coluna superior a Ilha_2

coluna_maior(ilha(_ ,(_, C)), ilha(_, (_, N_C))) :-
  N_C > C.

%mesma_linha(Ilha_1, Ilha_2) em que Ilha_1 e Ilha_2 se encontram na mesma linha e em colunas diferetes

mesma_linha(ilha(_, (L, C_1)), ilha(_,(L , C_2))) :-
  C_1 =\= C_2.

%mesma_coluna(Ilha_1, Ilha_2) em que Ilha_1 e Ilha_2 se encontram na mesma coluna e em linhas diferetes.

mesma_coluna(ilha(_, (L_1, C)), ilha(_, (L_2, C))) :-
  L_1 =\= L_2.

%mesma_ilha(Ilha_1, Ilha_2) em que Ilha_1 e Ilha_2 sao a mesma ilha (ignorando o numero de pontes)

mesma_ilha(ilha(_, (L, C)), ilha(_ ,(L, C))).

/*
vizinhas(Ilhas, Ilha, Vizinhas) em que Ilhas e a lista de ilhas de um puzzle e Ilha e uma dessas ilhas, significa que Vizinhas e a lista ordenada cujos elementos sao as ilhas vizinhas de Ilha
*/

vizinhas(Ilhas, Ilha, Vizinhas) :-
  include(mesma_linha(Ilha), Ilhas, List_same_L),
  include(mesma_coluna(Ilha), Ilhas, List_same_C),

  sort(2, @>, List_same_L, Sorted_L), %DECRESCENTE
  sort(2, @<, List_same_C, Sorted_C), %CRESCENTE

  include(linha_maior(Ilha), Sorted_C, L_baixo), %Coordenadas das ilhas por baixo de Ilha
  exclude(linha_maior(Ilha), Sorted_C, L_cima), %Coordenadas das ilhas por cima de Ilha
  include(coluna_maior(Ilha), Sorted_L, C_direita), %Coordenadas das ilhas a direita de Ilha
  exclude(coluna_maior(Ilha), Sorted_L, C_esquerda), %Coordenadas das ilhas a esquerda de Ilha

  append(L_baixo, [Ilha], L_Baixo), %juntar a propria ilha no final da lista, para nao ficar vazia e para nao interferir com a ordem caso tenho ilhas la dentro
  append([Ilha], L_cima, L_Cima), %juntar a propria ilha no inicio da lista, para nao ficar vazia e para nao interferir com a ordem caso tenho ilhas la dentro
  append(C_esquerda, [Ilha], C_Esquerda), %juntar a propria ilha no final da lista, para nao ficar vazia e para nao interferir com a ordem caso tenho ilhas la dentro
  append([Ilha], C_direita, C_Direita), %juntar a propria ilha no inicio da lista, para nao ficar vazia e para nao interferir com a ordem caso tenho ilhas la dentro
  
  nth1(1, L_Baixo , Ilha_baixo), %Primeiro elemento da lista
  last(L_Cima, Ilha_cima), %Ultimo elemento da lista
  nth1(1, C_Esquerda, Ilha_esquerda), %Primeiro elemento da lista
  last(C_Direita, Ilha_direita), %Ultimo elemento da lista

  append([], [Ilha_cima, Ilha_esquerda, Ilha_direita, Ilha_baixo], L_completa),
  exclude(mesma_ilha(Ilha), L_completa, Vizinhas).

%2.4 - Predicado estado/2

/*
  estado(Ilhas, Estado) em que Ilhas e a lista de ilhas de um puzzle, significa que Estado e a lista ordenada cujos elementos sao as entradas referentes a cada uma das ilhas de Ilhas

  Uma entrada e uma lista em que:
          O 1 elemento e uma ilha
          o 2 elemento e a lista de vizinhas dessa ilha
          o 3 elemento e a lista de pontes da ilha; esta lista e vazia no estado inicial
*/

estado(Ilhas, Estado) :-
  estado(Ilhas, Ilhas, Estado).

estado([], _, []).


estado([Ilha | R_Ilhas], OG_Ilhas, [[Ilha, Vizinhas, []] | R_E]) :-
  vizinhas(OG_Ilhas, Ilha, Vizinhas),
  estado(R_Ilhas, OG_Ilhas, R_E). 
  
    
%2.5 - Predicado posicoes_entre/3

/*
  posicoes_entre(Pos1, Pos2, Posicoes) em que Pos1 e Pos2 sao posicoes, significa que Posicoes e a lista ordenada de posicoes entre Pos1 e Pos2.

  Se Pos1 e Pos2 nao pertencerem a mesma linha ou coluna, o resultado e false
*/

posicoes_entre((L, C_1), (L, C_2), Posicoes) :-
  C_2 > C_1,
  C_Menor is C_1 + 1,
  C_Maior is C_2 - 1,
  findall((L, C), between(C_Menor, C_Maior, C), Posicoes).

posicoes_entre((L, C_2), (L, C_1), Posicoes) :-
  C_2 > C_1,
  C_Menor is C_1 + 1,
  C_Maior is C_2 - 1,
  findall((L, C), between(C_Menor, C_Maior, C), Posicoes).

posicoes_entre((L_1, C), (L_2, C), Posicoes) :-
  L_2 > L_1,
  L_Menor is L_1 + 1,
  L_Maior is L_2 - 1,
  findall((L, C), between(L_Menor, L_Maior, L), Posicoes).

posicoes_entre((L_2, C), (L_1, C), Posicoes) :-
  L_2 > L_1,
  L_Menor is L_1 + 1,
  L_Maior is L_2 - 1,
  findall((L, C), between(L_Menor, L_Maior, L), Posicoes).
  
%2.6 - Predicado cria_ponte/3

/*
  cria_ponte(Pos1, Pos2, Ponte) em que Pos1 e Pos2 sao 2 posicoes, significa que Ponte e uma ponte entre essas 2 posicoes
*/

cria_ponte((L, C_1), (L, C_2), ponte((L, C_1), (L, C_2))) :-
  C_2 > C_1.

cria_ponte((L, C_1), (L, C_2), ponte((L, C_2), (L, C_1))) :-
  C_1 > C_2.

cria_ponte((L_1, C), (L_2, C), ponte((L_1, C), (L_2, C))) :-
  L_2 > L_1.

cria_ponte((L_1, C), (L_2, C), ponte((L_2, C), (L_1, C))) :-
  L_1 > L_2.

%2.7 - Predicado caminho_livre/5

/*
  caminho_livre(Pos1, Pos2, Posicoes, I, Vz) em que Pos1 e Pos2 sao posicoes, Posicoes e a lista ordenada de posicoes entre Pos1 e Pos2, I e uma ilha, e Vz uma das suas vizinhas, significa que a adicao da ponte ponte(Pos1, Pos2) nao faz com que I e Vz deixem de ser vizinhas
*/

caminho_livre(Pos1, Pos2, _, ilha(_, (Pos1)), ilha(_, (Pos2))).

caminho_livre(Pos1, Pos2, _, ilha(_, (Pos2)), ilha(_, (Pos1))).

caminho_livre(Pos1, Pos2, Posicoes, ilha(_, (L_1, C)), ilha(_, (L_2, C))) :-
  L_1 =\= L_2,
  posicoes_entre(Pos1, Pos2, Posicoes),
  posicoes_entre((L_1, C), (L_2, C), Posicoes_I_Vz),
  findall((X, Y), (member((X, Y), Posicoes), member((X, Y), Posicoes_I_Vz)), L_Same),
  length(L_Same, L_P),
  L_P =:= 0.

caminho_livre(Pos1, Pos2, Posicoes, ilha(_, (L, C_1)), ilha(_, (L, C_2))) :-
  C_1 =\= C_2,
  posicoes_entre(Pos1, Pos2, Posicoes),
  posicoes_entre((L, C_1), (L, C_2), Posicoes_I_Vz),
  findall((X, Y), (member((X, Y), Posicoes), member((X, Y), Posicoes_I_Vz)), L_Same),
  length(L_Same, L_P),
  L_P =:= 0.


%2.8 - Predicado actualiza_vizinhas_entrada/5  

/*
  actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada, Nova_Entrada) em que Pos1 e Pos2 sao as posicoes entre as quais ira ser adicionada uma ponte, Posicoes e a lista ordenada de posicoes entre Pos1 e Pos2, e Entrada e uma entrada, significa que Nova_Entrada e igual a Entrada, exceto no que diz respeito a lista de ilhas vizinhas; esta deve ser atualizada removendo as ilhas que deixara de ser vizinhas, apos a adicao da ponte
*/


actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, [ilha(P_1, (L, C)), Vizinhas, Pontes], Nova_Entrada) :-
  
  findall(ilha(P_2, (X, Y)), ((member(ilha(P_2, (X, Y)), Vizinhas)), caminho_livre(Pos1, Pos2, Posicoes, ilha(P_1, (L, C)), ilha(P_2, (X, Y)))), P),
  append([ilha(P_1, (L, C))], [P, Pontes], Nova_Entrada).


%2.9 - Predicado actualiza_vizinhas_apos_pontes/4

/*
  actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_estado), em que Estado e um estado, Pos1 e Pos2 sao as posicoes entre as quais foi adicionada uma ponte, significa que Novo_estado e o estado que se obtem de Estado apos a atualizacao das ilhas vizinhas de cada uma das suas entradas
*/

actualiza_vizinhas_apos_pontes([], _, _, []).

actualiza_vizinhas_apos_pontes([[ilha(P, (L, C)), Vizinhas, Pontes] | R], Pos1, Pos2, [Nova_entrada | R_E]) :-
  posicoes_entre(Pos1, Pos2, Posicoes),
  actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, [ilha(P, (L, C)), Vizinhas, Pontes], Nova_entrada),
  actualiza_vizinhas_apos_pontes(R, Pos1, Pos2, R_E).

%2.10 - Predicado ilhas_terminadas/2

/*
  ilhas_terminadas(Estado, Ilhas_term), em que Estado e um estado , significa que Ilhas_term e a lista de ilhas que ja tem todas as pontes associadas,
  designadas por ilhas terminadas. Se a entrada referente a uma ilha for [ilha(N_pontes, Pos), Vizinhas, Pontes], esta ilha esta terminada se N_pontes for diferente de 'X' e o comprimento da lista Pontes for N_pontes 
*/

ilhas_terminadas(Estado, Ilhas_term) :-
  findall(ilha(P, (L, C)), ((member(([ilha(P, (L, C)), _, Pontes]), Estado)), (P \= 'X'), length(Pontes,P)), Ilhas_term).

%2.11 - Predicado tira_ilhas_terminadas_entrada/3

/*
  tira_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada), em que Ilhas_term e uma lista de ilhas terminadas e Entrada e uma entrada, significa que Nova_entrada e a entrada resultante de remover as ilhas de Ilhas_term da lista de ilhas vizinhas de Entrada
*/

tira_ilhas_terminadas_entrada(Ilhas_term, [ilha(P, (L ,C)), Vizinhas, Pontes], Nova_entrada) :-
  subtract(Vizinhas, Ilhas_term, List_S_I_T),
  append([ilha(P, (L, C))], [List_S_I_T, Pontes], Nova_entrada).

%2.12 - tira_ilhas_terminadas/3

/*
  tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado) em que Estado e um estado e Ilhas_term e uma lista de ilhas terminadas, siginifica que Novo_estado e o estado resultante de aplicar o predicado tira_ilhas_terminadas_entrada a cada uma das entradas de Estado
*/

tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado) :-
  maplist(tira_ilhas_terminadas_entrada(Ilhas_term), Estado, Novo_estado).

%2.13 - Predicado marca_ilhas_terminadas_entrada/3

/*
  marca_ilhas_terminadas_entrada(Ilhas_term, Entrada,Nova_entrada), em que Ilhas_term e uma lista de ilhas terminadas e Entrada e uma entrada, significa que Nova_entrada e a entrada obtida de Entrada da seguinte forma: se a ilha de Entrada pertencer a Ilhas_term, o numero de pontes desta e substituido por 'X'; em caso contrario Nova_entrada e igual a Entrada.
*/

marca_ilhas_terminadas_entrada(Ilhas_term, [ilha(P, (L, C)), Vizinhas, Pontes], Nova_entrada) :- 
  member(ilha(P, (L, C)), Ilhas_term) -> Nova_entrada = [ilha('X', (L, C)), Vizinhas, Pontes];
  Nova_entrada = [ilha(P, (L, C)), Vizinhas, Pontes].


%2.14 - Predicado marca_ilhas_terminadas/3

/*
  marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado), em que
  Estado e um estado e Ilhas_term e uma lista de ilhas terminadas, significa que Novo_estado e o estado resultante de aplicar o predicado marca_ilhas_terminadas_entrada a cada uma das entradas de Estado.
*/

marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado) :-
  maplist(marca_ilhas_terminadas_entrada(Ilhas_term), Estado, Novo_estado).

%2.15 - Predicado trata_ilhas_terminadas/2

/*
  trata_ilhas_terminadas(Estado, Novo_estado), em que Estado e um estado significa que Novo_estado e o estado resultante de aplicar os predicados marca_ilhas_terminadas e tira_ilhas_terminadas a Estado.
*/

trata_ilhas_terminadas(Estado, Novo_estado) :-
  ilhas_terminadas(Estado, Ilhas_term),
  marca_ilhas_terminadas(Estado, Ilhas_term, Semi_estado),
  tira_ilhas_terminadas(Semi_estado, Ilhas_term, Novo_estado).
  
  
%2.16 - Predicado junta_pontes/5

%cria_n_pontes(N, Pos1, Pos2, Pontes) em que N e um inteiro positivo, Pos1 e Pos2 2 posicoes, significa que Pontes e a lista de N pontes entre Pos1 e Pos2

cria_n_pontes(N, Pos1, Pos2, Pontes) :-
  length(Pontes, N),
  cria_ponte(Pos1, Pos2, Ponte),
  maplist(=(Ponte), Pontes).

%insere_pontes_entrada(Pontes, Ilha, Entrada, Nova_Entrada) em que Pontes e uma lista de pontes, Ilha uma Ilha, Entrada uma entrada significa que Nova_Entrada e entrada obtida de Entrada introduzindo a lista Pontes se Ilha corresponder a ilha de Entrada

insere_pontes_entrada(New_Pontes, ilha(P, (L, C)), [ilha(P, (L ,C)), Vizinhas, Pontes], [ilha(P, (L ,C)), Vizinhas, Pontes_C]) :-
  append(New_Pontes, Pontes, Pontes_C).

insere_pontes_entrada(_, ilha(_, (L_1, C)), [ilha(P, (L ,C)), Vizinhas, Pontes], [ilha(P, (L ,C)), Vizinhas, Pontes]) :-
  L_1 =\= L.

insere_pontes_entrada(_, ilha(_, (L, C_1)), [ilha(P, (L ,C)), Vizinhas, Pontes], [ilha(P, (L ,C)), Vizinhas, Pontes]) :-
  C_1 =\= C.

insere_pontes_entrada(_, ilha(_, (L_1, C_1)), [ilha(P, (L ,C)), Vizinhas, Pontes], [ilha(P, (L ,C)), Vizinhas, Pontes]) :-
  L_1 =\= L,
  C_1 =\= C.

/*
  junta_pontes(Estado, Num_pontes, Ilha1, Ilha2, Novo_estado), em
  que Estado e um estado e Ilha1 e Ilha2 sao 2 ilhas, significa que Novo_estado e o estado que se obtem de Estado por adicao de Num_pontes pontes entre Ilha1 e Ilha2.

  Este predicado executa os seguintes passos:
            Cria a(s) ponte(s) enre Ilha1 e Ilha2

            Adiciona as novas pontes as entradas de Estado correspondentes a estas ilhas

            Actualiza Estado por aplicacao dos predicado actualiza_vizinhas_apos_pontes e trata_ilhas_terminadas.
*/

junta_pontes(Estado, Num_pontes, ilha(P_1, (L_1, C_1)), ilha(P_2, (L_2, C_2)), Novo_estado) :-
  cria_n_pontes(Num_pontes, (L_1, C_1), (L_2, C_2), L_pontes),
  maplist(insere_pontes_entrada(L_pontes, ilha(P_1, (L_1, C_1))), Estado, Novo_estado_C_I_1),
  maplist(insere_pontes_entrada(L_pontes, ilha(P_2, (L_2, C_2))), Novo_estado_C_I_1, Novo_estado_C_I_2),
  actualiza_vizinhas_apos_pontes(Novo_estado_C_I_2, (L_1, C_1), (L_2, C_2), Novo_estado_atualizado),
  trata_ilhas_terminadas(Novo_estado_atualizado, Novo_estado).
% Jose Antonio Lopes n:103938
:-set_prolog_flag(answer_write_options,[max_depth(0)]). % para listas completas
:- ['dados.pl'], ['keywords.pl']. % ficheiros a importar.

% Predicado 1 - eventosSemSalas

eventosSemSalas(EventoSemSala) :-
    findall(ID, evento(ID, _, _, _, semSala), EventoSemSala).

% Predicado 2 - eventosSemSalasDiaSemana

eventosSemSalasDiaSemana(DiaDaSemana, EventosSemSala) :- 
    findall(ID, (evento(ID, _, _, _, semSala), horario(ID, DiaDaSemana, _, _, _, _)), EventosSemSala).

% Precidado 3 - eventosSemSalasPeriodo

/* Predicado auxiliar adicionarSemestres(ListaPeridos, ListaPeriodosComSemestres) em que ListaPeridos eh uma lista de periodos, ListaPeriodosComSemestres eh a que contem os periodos de ListaPeriodos juntamente com o equivalente em semestres */

adicionarSemestres([], []).

adicionarSemestres([p1 | R1], [p1, p1_2 | R2]) :- 
    adicionarSemestres(R1, R2).

adicionarSemestres([p2 | R1], [p2, p1_2 | R2]) :- 
    adicionarSemestres(R1, R2).

adicionarSemestres([p3 | R1], [p3, p3_4 | R2]) :- 
    adicionarSemestres(R1, R2).

adicionarSemestres([p4 | R1], [p4, p3_4 | R2]) :- 
    adicionarSemestres(R1, R2).

eventosSemSalasPeriodo(ListaPeriodos, EventosSemSala) :- 
    adicionarSemestres(ListaPeriodos, ListaPeriodosComSemestresRepetido),
    sort(ListaPeriodosComSemestresRepetido, ListaPeriodosComSemestres),
    findall((ID, PI), (evento(ID, _, _, _, semSala), member(PI, ListaPeriodosComSemestres), horario(ID, _, _, _, _, PI)), EventosSemSalaTuple), 
    findall(ID, member((ID, _), EventosSemSalaTuple), EventosSemSala).

% Predicado 4 - organizaEventos 

organizaEventos(ListaEventos, Periodo, EventosNoPeriodo) :- 
    organizaEventos(ListaEventos, Periodo, EventosNoPeriodo, []).

organizaEventos([], _, EventosNoPeriodo, ListaAux) :- 
    sort(ListaAux, EventosNoPeriodo),
    !.

organizaEventos([P | R], Periodo, EventosNoPeriodo, ListaAux) :- 
    (Periodo == p1; Periodo == p2),
    (horario(P, _, _, _, _, Periodo); horario(P, _, _, _, _, p1_2)),
    append([ListaAux, [P]], NewLista),
    organizaEventos(R, Periodo, EventosNoPeriodo, NewLista).

organizaEventos([P | R], Periodo, EventosNoPeriodo, ListaAux) :- 
    (Periodo == p3; Periodo == p4),
    (horario(P, _, _, _, _, Periodo); horario(P, _, _, _, _, p3_4)),
    append([ListaAux, [P]], NewLista),
    organizaEventos(R, Periodo, EventosNoPeriodo, NewLista).

organizaEventos([P | R], Periodo, EventosNoPeriodo, ListaAux) :- 
    \+horario(P, _, _, _, _, Periodo),
    organizaEventos(R, Periodo, EventosNoPeriodo, ListaAux).

% Predicado 5 - eventosMenoresQue

eventosMenoresQue(Duracao, ListaEventosMenoresQue) :- 
    findall(ID, (horario(ID, _, _, _, DuracaoEvento, _), DuracaoEvento =< Duracao), ListaEventosMenoresQue).

%Predicado 6 - eventosMenoresQueBool

eventosMenoresQueBool(ID, Duracao) :- 
    horario(ID, _, _, _, DuracaoEvento, _),
    DuracaoEvento =< Duracao.

% Predicado 7 - procuraDisciplinas

procuraDisciplinas(Curso, ListaDisciplinas) :- 
    findall(Disciplina, (turno(ID, Curso, _, _), evento(ID, Disciplina, _, _, _)), ListaDisciplinasUnsorted),
    sort(ListaDisciplinasUnsorted, ListaDisciplinas).

% Predicado 8 - organizaDisciplinas


% Predicado auxiliar existeNoCurso(Disciplina, Curso) em Disciplina faz parte do Curso.
existeNoCurso(Disciplina, Curso) :- 
    evento(ID, Disciplina, _, _, _),
    turno(ID, CursoDaDisciplina, _, _),
    CursoDaDisciplina == Curso.

% Predicado auxiliar mesmaLength(L1, L2, L3) em que a soma dos comprimentos das listas L1 e L2 eh igual ao comprimento da lista L3

mesmaLength(L1, L2, L3) :-
    length(L1, LenL1),
    length(L2, LenL2),
    length(L3, LenL3),
    Soma_L2_L3 is LenL2 + LenL3,
    LenL1 =:= Soma_L2_L3.

organizaDisciplinas(ListaDisciplinas, Curso, Semestres) :-
    organizaDisciplinas(ListaDisciplinas, Curso, Semestres, [[], []], ListaDisciplinas).

organizaDisciplinas([], _, [PrimeiroSemestreSorted, SegundoSemestreSorted], [PrimeiroSemestre, SegundoSemestre], ListaDisciplinas) :-
    mesmaLength(ListaDisciplinas, PrimeiroSemestre, SegundoSemestre), 
    sort(PrimeiroSemestre, PrimeiroSemestreSorted),
    sort(SegundoSemestre, SegundoSemestreSorted).

organizaDisciplinas([P | R], Curso, Semestres, [PrimeiroSemestre, SegundoSemestre], ListaDisciplinas) :-
    existeNoCurso(P, Curso),
    evento(ID, P, _, _, _),
    turno(ID, Curso, _, _),
    horario(ID ,_, _, _, _, Periodo),
    (Periodo == p1; Periodo == p2; Periodo == p1_2),
    append([PrimeiroSemestre, [P]], NewPrimeiroSemestre),
    organizaDisciplinas(R, Curso, Semestres, [NewPrimeiroSemestre, SegundoSemestre], ListaDisciplinas).

organizaDisciplinas([P | R], Curso, Semestres, [PrimeiroSemestre, SegundoSemestre], ListaDisciplinas) :- 
    existeNoCurso(P, Curso),
    evento(ID, P, _, _, _),
    turno(ID, Curso, _, _),
    horario(ID ,_, _, _, _, Periodo),
    (Periodo == p3; Periodo == p4; Periodo == p3_4),
    append([SegundoSemestre, [P]], NewSegundoSemestre),
    organizaDisciplinas(R, Curso, Semestres, [PrimeiroSemestre, NewSegundoSemestre], ListaDisciplinas).

organizaDisciplinas([P | R], Curso, Semestres, [PrimeiroSemestre, SegundoSemestre], ListaDisciplinas) :-
    \+existeNoCurso(P, Curso),
    organizaDisciplinas(R, Curso, Semestres, [PrimeiroSemestre, SegundoSemestre], ListaDisciplinas).

% Predicado 9 - horasCurso

% Caso em que o Periodo eh o primeiro ou o segundo
horasCurso(Periodo, Curso, Ano, TotalHoras) :- 
    (Periodo == p1; Periodo == p2),
    findall((ID, Turno), turno(ID, Curso, Ano, Turno), ListaTurnosRepetidos),
    sort(1, @<, ListaTurnosRepetidos, ListaTurnos), % Remover Turnos com o mesmo ID
    findall(Duracao, (turno(ID, Curso, Ano, Turno), (horario(ID, _, _, _, Duracao, Periodo); horario(ID, _, _, _, Duracao, p1_2)), member((ID, Turno), ListaTurnos)), ListaDuracao),
    sumlist(ListaDuracao, TotalHoras).

% Caso em que o Periodo eh o terceiro ou o quarto
horasCurso(Periodo, Curso, Ano, TotalHoras) :- 
    (Periodo == p3; Periodo == p4),
    findall((ID, Turno), turno(ID, Curso, Ano, Turno), ListaTurnosRepetidos),
    sort(1, @<, ListaTurnosRepetidos, ListaTurnos), % Remover Turnos com o mesmo ID
    findall(Duracao, (turno(ID, Curso, Ano, Turno), (horario(ID, _, _, _, Duracao, Periodo); horario(ID, _, _, _, Duracao, p3_4)), member((ID, Turno), ListaTurnos)), ListaDuracao),
    sumlist(ListaDuracao, TotalHoras).

% predicado 10 - evolucaoHorasCurso

evolucaoHorasCurso(Curso, Evolucao) :- 
    evolucaoHorasCurso(Curso, Evolucao, 1, p1, []).

evolucaoHorasCurso(_, ListaAux, 4, _, ListaAux).

evolucaoHorasCurso(Curso, Evolucao, Ano, 0, ListaAux) :-
    Prox_Ano is Ano + 1, 
    evolucaoHorasCurso(Curso, Evolucao, Prox_Ano, p1, ListaAux).

evolucaoHorasCurso(Curso, Evolucao, Ano, p1, ListaAux) :- 
    horasCurso(p1, Curso, Ano, TotalHoras), 
    append([ListaAux, [(Ano, p1, TotalHoras)]], NewLista),
    evolucaoHorasCurso(Curso, Evolucao, Ano, p2, NewLista).

evolucaoHorasCurso(Curso, Evolucao, Ano, p2, ListaAux) :- 
    horasCurso(p2, Curso, Ano, TotalHoras), 
    append([ListaAux, [(Ano, p2, TotalHoras)]], NewLista),
    evolucaoHorasCurso(Curso, Evolucao, Ano, p3, NewLista).

evolucaoHorasCurso(Curso, Evolucao, Ano, p3, ListaAux) :- 
    horasCurso(p3, Curso, Ano, TotalHoras), 
    append([ListaAux, [(Ano, p3, TotalHoras)]], NewLista),
    evolucaoHorasCurso(Curso, Evolucao, Ano, p4, NewLista).    

evolucaoHorasCurso(Curso, Evolucao, Ano, p4, ListaAux) :- 
    horasCurso(p4, Curso, Ano, TotalHoras), 
    append([ListaAux, [(Ano, p4, TotalHoras)]], NewLista),
    evolucaoHorasCurso(Curso, Evolucao, Ano, 0, NewLista).

% Predicado 11 - ocupaSlot

% Caso em que o Evento contem totalmente o slot
ocupaSlot(HoraInicioDada, HoraFimDada, HoraInicioEvento, HoraFimEvento, Horas) :- 
    HoraInicioDada >= HoraInicioEvento,
    HoraFimDada =< HoraFimEvento, 
    Horas is HoraFimDada - HoraInicioDada,
    Horas > 0.

% Caso em que a sobreposicao ocorre no fim do Evento
ocupaSlot(HoraInicioDada, HoraFimDada, HoraInicioEvento, HoraFimEvento, Horas) :-
    HoraInicioDada >= HoraInicioEvento,
    HoraFimDada >= HoraFimEvento,
    Horas is HoraFimEvento - HoraInicioDada,
    Horas > 0.

% Caso em que a sobreposicao ocorre no inicio do Evento
ocupaSlot(HoraInicioDada, HoraFimDada, HoraInicioEvento, HoraFimEvento, Horas) :-
    HoraInicioDada =< HoraInicioEvento,
    HoraFimDada =< HoraFimEvento,
    Horas is HoraFimDada - HoraInicioEvento,
    Horas > 0.

% Caso em que o Evento esta contido totalmente no slot
ocupaSlot(HoraInicioDada, HoraFimDada, HoraInicioEvento, HoraFimEvento, Horas) :-
    HoraInicioDada =< HoraInicioEvento,
    HoraFimDada >= HoraFimEvento,
    Horas is HoraFimEvento - HoraInicioEvento,
    Horas > 0.

% Predicado 12 -  numHorasOcupadas

numHorasOcupadas(Periodo, TipoSala, DiaSemana, HoraInicio, HoraFim, SomaHoras) :- 
    (Periodo == p1; Periodo == p2),
    salas(TipoSala, ListaSalas),
    findall(Horas, ((horario(ID, DiaSemana, HoraInicioEvento, HoraFimEvento, _, Periodo); horario(ID, DiaSemana, HoraInicioEvento, HoraFimEvento, _, p1_2)), evento(ID, _, _, _, Sala), member(Sala, ListaSalas), ocupaSlot(HoraInicio, HoraFim, HoraInicioEvento, HoraFimEvento, Horas)), ListaHoras),
    sumlist(ListaHoras, SomaHoras).

numHorasOcupadas(Periodo, TipoSala, DiaSemana, HoraInicio, HoraFim, SomaHoras) :- 
    (Periodo == p3; Periodo == p4),
    salas(TipoSala, ListaSalas),
    findall(Horas, ((horario(ID, DiaSemana, HoraInicioEvento, HoraFimEvento, _, Periodo); horario(ID, DiaSemana, HoraInicioEvento, HoraFimEvento, _, p3_4)), evento(ID, _, _, _, Sala), member(Sala, ListaSalas), ocupaSlot(HoraInicio, HoraFim, HoraInicioEvento, HoraFimEvento, Horas)), ListaHoras),
    sumlist(ListaHoras, SomaHoras).

% Predicado 13 - ocupacaoMax

ocupacaoMax(TipoSala, HoraInicio, HoraFim, Max) :-  
    salas(TipoSala, ListaSalas),
    length(ListaSalas, NumeroSalas),
    Intervalo is HoraFim - HoraInicio,
    Max is Intervalo * NumeroSalas.

% Predicado 14 - percentagem

percentagem(SomaHoras, Max, Percentagem) :- 
    Percentagem is (SomaHoras / Max) * 100.

% Predicado 15 - ocupacaoCritica

% Predicado auxiliar percentagemArredondada(SomaHoras, Max, Percentagem) em que Percentagem eh o arredondamento da divisao de SomaHoras por Max, multiplicada por 100
percentagemArredondada(SomaHoras, Max, Percentagem) :- 
    percentagem(SomaHoras, Max, PercentagemNaoArredondada),
    Percentagem is ceiling(PercentagemNaoArredondada).

ocupacaoDiaSemana(p1, DiaSemana, TipoSala, HoraInicio, HoraFim, Ocupacao) :- 
    salas(TipoSala, ListaSalas), 
    findall(Horas, (evento(ID, _, _, _, Sala), member(Sala, ListaSalas), (horario(ID, DiaSemana, HoraInicioEvento, HoraFimEvento, _, p1); horario(ID, DiaSemana, HoraInicioEvento, HoraFimEvento, _, p1_2)), ocupaSlot(HoraInicio, HoraFim, HoraInicioEvento, HoraFimEvento, Horas)), ListaOcupacao),
    sumlist(ListaOcupacao, Ocupacao).   

ocupacaoDiaSemana(p2, DiaSemana, TipoSala, HoraInicio, HoraFim, Ocupacao) :- 
    salas(TipoSala, ListaSalas), 
    findall(Horas, (evento(ID, _, _, _, Sala), member(Sala, ListaSalas), (horario(ID, DiaSemana, HoraInicioEvento, HoraFimEvento, _, p2); horario(ID, DiaSemana, HoraInicioEvento, HoraFimEvento, _, p1_2)), ocupaSlot(HoraInicio, HoraFim, HoraInicioEvento, HoraFimEvento, Horas)), ListaOcupacao),
    sumlist(ListaOcupacao, Ocupacao).

ocupacaoDiaSemana(p3, DiaSemana, TipoSala, HoraInicio, HoraFim, Ocupacao) :- 
    salas(TipoSala, ListaSalas), 
    findall(Horas, (evento(ID, _, _, _, Sala), member(Sala, ListaSalas), (horario(ID, DiaSemana, HoraInicioEvento, HoraFimEvento, _, p3); horario(ID, DiaSemana, HoraInicioEvento, HoraFimEvento, _, p3_4)), ocupaSlot(HoraInicio, HoraFim, HoraInicioEvento, HoraFimEvento, Horas)), ListaOcupacao),
    sumlist(ListaOcupacao, Ocupacao).

ocupacaoDiaSemana(p4, DiaSemana, TipoSala, HoraInicio, HoraFim, Ocupacao) :- 
    salas(TipoSala, ListaSalas), 
    findall(Horas, (evento(ID, _, _, _, Sala), member(Sala, ListaSalas), (horario(ID, DiaSemana, HoraInicioEvento, HoraFimEvento, _, p4); horario(ID, DiaSemana, HoraInicioEvento, HoraFimEvento, _, p3_4)), ocupaSlot(HoraInicio, HoraFim, HoraInicioEvento, HoraFimEvento, Horas)), ListaOcupacao),
    sumlist(ListaOcupacao, Ocupacao).

ocupacaoCritica(HoraInicio, HoraFim, Threshold, Resultados) :- 
    findall(casosCriticos(DiaSemana, TipoSala, Percentagem), (salas(TipoSala, _), horario(_, DiaSemana, _, _, _, Periodo), ocupacaoMax(TipoSala, HoraInicio, HoraFim, Max), ocupacaoDiaSemana(Periodo, DiaSemana, TipoSala, HoraInicio, HoraFim, Ocupacao), percentagemArredondada(Ocupacao, Max, Percentagem), Percentagem > Threshold), ResultadosUnsortedRepetidos),
    sort(0, @<, ResultadosUnsortedRepetidos, Resultados).

% Predicado 16 - ocupacaoMesa

% Este predicado tem um erro. Nao tem em conta pessoas que nao estejam na lista de restriçoes.

% Predicado auxiliar testaRestricoes(Restricao, ListaPessoas) em que Restricao eh uma restricao e ListaPessoas e uma lista que cumpre essa restricao.

testaRestricoes(cab1(NomePessoa), [[_, _, _], [NomePessoa, _], [_, _, _]]).

testaRestricoes(cab2(NomePessoa), [[_, _, _], [_, NomePessoa], [_, _, _]]).

testaRestricoes(honra(NomePessoa1, NomePessoa2), [[_, _, _], [NomePessoa1, _], [NomePessoa2, _, _]]).

testaRestricoes(honra(NomePessoa1, NomePessoa2), [[_, _, NomePessoa2], [_, NomePessoa1], [_, _, _]]).

testaRestricoes(lado(NomePessoa1, NomePessoa2), [[NomePessoa1, NomePessoa2, _], [_, _], [_, _, _]]).

testaRestricoes(lado(NomePessoa1, NomePessoa2), [[NomePessoa2, NomePessoa1, _], [_, _], [_, _, _]]).

testaRestricoes(lado(NomePessoa1, NomePessoa2), [[_, NomePessoa1, NomePessoa2], [_, _], [_, _, _]]).

testaRestricoes(lado(NomePessoa1, NomePessoa2), [[_, NomePessoa2, NomePessoa1], [_, _], [_, _, _]]).

testaRestricoes(lado(NomePessoa1, NomePessoa2), [[_, _, _], [_, _], [NomePessoa1, NomePessoa2, _]]).

testaRestricoes(lado(NomePessoa1, NomePessoa2), [[_, _, _], [_, _], [NomePessoa2, NomePessoa1, _]]).

testaRestricoes(lado(NomePessoa1, NomePessoa2), [[_, _, _], [_, _], [_, NomePessoa1, NomePessoa2]]).

testaRestricoes(lado(NomePessoa1, NomePessoa2), [[_, _, _], [_, _], [_, NomePessoa2, NomePessoa1]]).

testaRestricoes(frente(NomePessoa1, NomePessoa2), [[NomePessoa1, _, _], [_, _], [NomePessoa2, _, _]]).

testaRestricoes(frente(NomePessoa1, NomePessoa2), [[_, NomePessoa1, _], [_, _], [_, NomePessoa2, _]]).

testaRestricoes(frente(NomePessoa1, NomePessoa2), [[_, _, NomePessoa1], [_, _], [_, _, NomePessoa2]]).

testaRestricoes(frente(NomePessoa1, NomePessoa2), [[NomePessoa2, _, _], [_, _], [NomePessoa1, _, _]]).

testaRestricoes(frente(NomePessoa1, NomePessoa2), [[_, NomePessoa2, _], [_, _], [_, NomePessoa1, _]]).

testaRestricoes(frente(NomePessoa1, NomePessoa2), [[_, _, NomePessoa2], [_, _], [_, _, NomePessoa1]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[NomePessoa1, _, NomePessoa2], [_, _], [_, _, _]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[NomePessoa1, _, _], [NomePessoa2, _], [_, _, _]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[NomePessoa1, _, _], [_, NomePessoa2], [_, _, _]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[NomePessoa1, _, _], [_, _], [NomePessoa2, _, _]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[NomePessoa1, _, _], [_, _], [_, NomePessoa2, _]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[NomePessoa1, _, _], [_, _], [_, _, NomePessoa2]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[_, NomePessoa1, _], [NomePessoa2, _], [_, _, _]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[_, NomePessoa1, _], [_, NomePessoa2], [_, _, _]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[_, NomePessoa1, _], [_, _], [NomePessoa2, _, _]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[_, NomePessoa1, _], [_, _], [_, NomePessoa2, _]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[_, NomePessoa1, _], [_, _], [_, _, NomePessoa2]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[NomePessoa2, _, NomePessoa1], [_, _], [_, _, _]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[_, _, NomePessoa1], [NomePessoa2, _], [_, _, _]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[_, _, NomePessoa1], [_, NomePessoa2], [_, _, _]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[_, _, NomePessoa1], [_, _], [NomePessoa2, _, _]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[_, _, NomePessoa1], [_, _], [_, NomePessoa2, _]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[_, _, NomePessoa1], [_, _], [_, _, NomePessoa2]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[NomePessoa2, _, _], [NomePessoa1, _], [_, _, _]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[_, NomePessoa2, _], [NomePessoa1, _], [_, _, _]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[_, _, NomePessoa2], [NomePessoa1, _], [_, _, _]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[_, _, _], [NomePessoa1, NomePessoa2], [_, _, _]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[_, _, _], [NomePessoa1, _], [NomePessoa2, _, _]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[_, _, _], [NomePessoa1, _], [_, NomePessoa2, _]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[_, _, _], [NomePessoa1, _], [_, _, NomePessoa2]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[NomePessoa2, _, _], [_, NomePessoa1], [_, _, _]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[_, NomePessoa2, _], [_, NomePessoa1], [_, _, _]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[_, _, NomePessoa2], [_, NomePessoa1], [_, _, _]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[_, _, _], [NomePessoa2, NomePessoa1], [_, _, _]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[_, _, _], [_, NomePessoa1], [NomePessoa2, _, _]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[_, _, _], [_, NomePessoa1], [_, NomePessoa2, _]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[_, _, _], [_, NomePessoa1], [_, _, NomePessoa2]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[NomePessoa2, _, _], [_, _], [NomePessoa1, _, _]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[_, NomePessoa2, _], [_, _], [NomePessoa1, _, _]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[_, _, NomePessoa2], [_, _], [NomePessoa1, _, _]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[_, _, _], [NomePessoa2, _], [NomePessoa1, _, _]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[_, _, _], [_, NomePessoa2], [NomePessoa1, _, _]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[_, _, _], [_, _], [NomePessoa1, _, NomePessoa2]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[NomePessoa2, _, _], [_, _], [_, NomePessoa1, _]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[_, NomePessoa2, _], [_, _], [_, NomePessoa1, _]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[_, _, NomePessoa2], [_, _], [_, NomePessoa1, _]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[_, _, _], [NomePessoa2, _], [_, NomePessoa1, _]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[_, _, _], [_, NomePessoa2], [_, NomePessoa1, _]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[NomePessoa2, _, _], [_, _], [_, _, NomePessoa1]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[_, NomePessoa2, _], [_, _], [_, _, NomePessoa1]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[_, _, NomePessoa2], [_, _], [_, _, NomePessoa1]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[_, _, _], [NomePessoa2, _], [_, _, NomePessoa1]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[_, _, _], [_, NomePessoa2], [_, _, NomePessoa1]]).

testaRestricoes(naoLado(NomePessoa1, NomePessoa2), [[_, _, _], [_, _], [NomePessoa2, _, NomePessoa1]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[NomePessoa1, NomePessoa2, _], [_, _], [_, _, _]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[NomePessoa1, _, NomePessoa2], [_, _], [_, _, _]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[NomePessoa1, _, _], [NomePessoa2, _], [_, _, _]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[NomePessoa1, _, _], [_, NomePessoa2], [_, _, _]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[NomePessoa1, _, _], [_, _], [_, NomePessoa2, _]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[NomePessoa1, _, _], [_, _], [_, _, NomePessoa2]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[NomePessoa2, NomePessoa1, _], [_, _], [_, _, _]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[_, NomePessoa1, NomePessoa2], [_, _], [_, _, _]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[_, NomePessoa1, _], [NomePessoa2, _], [_, _, _]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[_, NomePessoa1, _], [_, NomePessoa2], [_, _, _]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[_, NomePessoa1, _], [_, _], [NomePessoa2, _, _]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[_, NomePessoa1, _], [_, _], [_, _, NomePessoa2]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[NomePessoa2, _, NomePessoa1], [_, _], [_, _, _]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[_, NomePessoa2, NomePessoa1], [_, _], [_, _, _]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[_, _, NomePessoa1], [NomePessoa2, _], [_, _, _]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[_, _, NomePessoa1], [_, NomePessoa2], [_, _, _]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[_, _, NomePessoa1], [_, _], [NomePessoa2, _, _]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[_, _, NomePessoa1], [_, _], [_, NomePessoa2, _]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[NomePessoa2, _, _], [NomePessoa1, _], [_, _, _]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[_, NomePessoa2, _], [NomePessoa1, _], [_, _, _]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[_, _, NomePessoa2], [NomePessoa1, _], [_, _, _]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[_, _, _], [NomePessoa1, NomePessoa2], [_, _, _]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[_, _, _], [NomePessoa1, _], [NomePessoa2, _, _]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[_, _, _], [NomePessoa1, _], [_, NomePessoa2, _]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[_, _, _], [NomePessoa1, _], [_, _, NomePessoa2]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[NomePessoa2, _, _], [_, NomePessoa1], [_, _, _]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[_, NomePessoa2, _], [_, NomePessoa1], [_, _, _]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[_, _, NomePessoa2], [_, NomePessoa1], [_, _, _]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[_, _, _], [NomePessoa2, NomePessoa1], [_, _, _]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[_, _, _], [_, NomePessoa1], [NomePessoa2, _, _]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[_, _, _], [_, NomePessoa1], [_, NomePessoa2, _]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[_, _, _], [_, NomePessoa1], [_, _, NomePessoa2]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[_, NomePessoa2, _], [_, _], [NomePessoa1, _, _]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[_, _, NomePessoa2], [_, _], [NomePessoa1, _, _]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[_, _, _], [NomePessoa2, _], [NomePessoa1, _, _]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[_, _, _], [_, NomePessoa2], [NomePessoa1, _, _]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[_, _, _], [_, _], [NomePessoa1, NomePessoa2, _]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[_, _, _], [_, _], [NomePessoa1, _, NomePessoa2]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[NomePessoa2, _, _], [_, _], [_, NomePessoa1, _]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[_, _, NomePessoa2], [_, _], [_, NomePessoa1, _]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[_, _, _], [NomePessoa2, _], [_, NomePessoa1, _]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[_, _, _], [_, NomePessoa2], [_, NomePessoa1, _]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[_, _, _], [_, _], [NomePessoa2, NomePessoa1, _]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[_, _, _], [_, _], [_, NomePessoa1, NomePessoa2]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[NomePessoa2, _, _], [_, _], [_, _, NomePessoa1]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[_, NomePessoa2, _], [_, _], [_, _, NomePessoa1]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[_, _, _], [NomePessoa2, _], [_, _, NomePessoa1]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[_, _, _], [_, NomePessoa2], [_, _, NomePessoa1]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[_, _, _], [_, _], [NomePessoa2, _, NomePessoa1]]).

testaRestricoes(naoFrente(NomePessoa1, NomePessoa2), [[_, _, _], [_, _], [_, NomePessoa2, NomePessoa1]]).

ocupacaoMesa(ListaPessoas, ListaRestricoes, OcupacaoMesa) :- 
    ocupacaoMesa(ListaPessoas, ListaRestricoes, OcupacaoMesa, [[_, _, _], [_, _], [_, _, _]]),
    !.

ocupacaoMesa(_, [], OcupacaoMesa, OcupacaoMesa).

ocupacaoMesa(ListaPessoas, [P | R], OcupacaoMesa, ListaAux) :- 
    testaRestricoes(P, ListaAux),
    ocupacaoMesa(ListaPessoas, R, OcupacaoMesa, ListaAux).

% URL do enunciado: https://rachacuca.com.br/logica/problemas/loja-de-perfumes/

% Fatos
camiseta(amarela).
camiseta(azul).
camiseta(branca).
camiseta(verde).
camiseta(vermelha).

nome(caue).
nome(giovanni).
nome(lucas).
nome(ramon).
nome(silvio).

perfume(amadeirado).
perfume(citrico).
perfume(floral).
perfume(fresco).
perfume(oriental).

preco(100).
preco(150).
preco(200).
preco(250).
preco(300).

companhia(amiga).
companhia(esposa).
companhia(irma).
companhia(mae).
companhia(namorada).

idade(20).
idade(25).
idade(30).
idade(35).
idade(40).

% Estrutura da fila
fila([H1, H2, H3, H4, H5]) :-
    homem(H1), homem(H2), homem(H3), homem(H4), homem(H5),
    todos_diferentes([H1, H2, H3, H4, H5]).

homem(homem(Camiseta, Nome, Perfume, Preco, Companhia, Idade)) :-
    camiseta(Camiseta),
    nome(Nome),
    perfume(Perfume),
    preco(Preco),
    companhia(Companhia),
    idade(Idade).

todos_diferentes([]).
todos_diferentes([H|T]) :- not(member(H, T)), todos_diferentes(T).

% Regras baseadas nas dicas
% Dica 1: O cliente de camiseta Branca está em algum lugar entre o Ramon e o cliente de 25 anos, nessa ordem.
regra_dica1(fila([H1, H2, H3, H4, H5])) :-
    (
        (nome(H1, ramon), camiseta(H3, branca), idade(H5, 25));
        (nome(H1, ramon), camiseta(H4, branca), idade(H5, 25))
    ).

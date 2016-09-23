%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                           MEMBRI GRUPPO PROGETTO                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                              %
%    NOME/COG:  Matteo Codogno       (730620)                                  %
%                                                                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                    INIT                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Verifica esistenza di un predicato
verify(P) :- current_predicate(P), !.
verify(T, P, Args) :- current_predicate(T), !, P =.. _, apply(P, Args).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                    LIST                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TROVA IL PIU' PICCOLO ELEMENTO ALL'INTERNO DI UNA LISTA
lmin([Elem], Elem) :- !.
lmin([H, S | T], X) :- S >= H, !, lmin([H | T], X).
lmin([H, S | T], X) :- S < H, lmin([S | T], X).

% INSERISCE UN ELEMENTO IN UNA POSIZIONE SPECIFICA DI UNA LISTA
ins(Element, List, Position, Result) :- PrefixLength is Position,
  length(Prefix, PrefixLength), append(Prefix, Suffix, List),
  append(Prefix, [Element], Temp), append(Temp, Suffix, Result).

% DIVIDE TUTTI GLI ELEMENTI DI UNA LISTA PER UNA COSTANTE
ldiv([], _, []) :- !.
ldiv([H|T], Div, [Hr|Tr]) :- nonvar(T), !, nonvar(T), !, Hr is H / Div,
  ldiv(T, Div, Tr).

% SOMMA di una lista
lsum([], 0).
lsum([H|T], S) :- nonvar(H), !, nonvar(T), !, lsum(T, St), S is H + St.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                  VECTOR                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new_vector(V, Ls) :- nonvar(V), nonvar(Ls), !,
  not( verify(vector/2, vector(V), Ls) ), !, assert(vector(V, Ls)).

innerprod([X],[Y],[R]) :- nonvar(X), nonvar(Y), !, R is X*Y, !.
innerprod([H|T],[H1|T1],[R|R1]) :- innerprod(T,T1,R1), R is H*H1.

iprod([H|T],[H1|T1], S) :- innerprod([H|T],[H1|T1],[R|R1]), lsum([R|R1], S).

% NORMA
norm(V, N) :- nonvar(V), !, iprod(V, V, IP), N is sqrt(IP).

% SOMMA
vsum([], [], []) :- !.
vsum([], [H2|T2], [H2|T2]) :- !.
vsum([H1|T1], [], [H1|T1]) :- !.
vsum([H1|T1], [H2|T2], [S|RS]) :- nonvar(H1), !, nonvar(T1), !, nonvar(H2), !,
  nonvar(T2), !, S is H1 + H2, vsum(T1, T2, RS).

% DIFFERENZA
vsub([], [], []).
vsub([H1|T1], [], [H1|T1]) :- !.
vsub([H1|T1], [H2|T2], [S|RS]) :- nonvar(H1), !, nonvar(T1), !, nonvar(H2), !,
  nonvar(T2), !, S is H1 - H2, vsub(T1, T2, RS).

% DISTANZA
distance(_, [], _) :- !.
distance([], _, _) :- !.
distance(V1, V2, D) :- vsub(V1, V2, Sub), innerprod(Sub, Sub, Prod),
  lsum(Prod, Sum), D is sqrt(Sum).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                  KMEANS                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% INIT CENTROIDI
init_centroid([], _, _) :- !.
init_centroid(_, 0, []) :- !.
init_centroid(Observations, K, Centroid) :- length(Observations, Size),
  nonvar(Observations), !, nonvar(K), !,
  Size >= K, !, random(0, Size, N), nth0(N, Observations, C),
  delete(Observations, C, NewObservations), KN is K - 1,
  init_centroid(NewObservations, KN, NewCentroid),
  Centroid = [C | NewCentroid].

% DISTANZA MINIMA
distances(_, [], []) :- !.
distances(V1, [H|T], [Dist|TDist]) :- nonvar(V1), !, nonvar(H), !, nonvar(T), !,
  distances(V1, T, TDist), distance(V1, H, Dist).

% SCELTA CLUSTER
chose_centroid([_],[Centroid],Centroid):-!.
chose_centroid([Hd1,Hd2|Tdist], [Cetrnoid1,Centroid2|TCentroid], SelCentroid):-
  (Hd1 < Hd2 -> chose_centroid([Hd1|Tdist],[Cetrnoid1|TCentroid],SelCentroid) ;
  chose_centroid([Hd2|Tdist],[Centroid2|TCentroid],SelCentroid) ).

% SUDDIVISIONE DEI VETTORI IN TORNO AI CENTROIDI
partition([],_,[]) :- !.
partition([HOb|TOb],Centroid,[HOb,SelCe|TCluster]) :-
  distances(HOb,Centroid,Dist),
  chose_centroid(Dist,Centroid,SelCe),
  partition(TOb,Centroid,TCluster).

split_item([],_,[]):-!.
split_item([HCentroid|TCentroid], ClusterList, [Splited|SplitItem]):-
  spliting(HCentroid, ClusterList, Splited),
  split_item(TCentroid, ClusterList, SplitItem).

spliting(_,[],[]):-!.
spliting(Mean,[Hitem,SelMean|Titem],Splited):- spliting(Mean,Titem,TSplited),
  ( Mean = SelMean -> Splited = [Hitem|TSplited] ; Splited = TSplited ).

% CALCOLO DEI CENTROIDI DEI CLUSTER
centroid([], []) :- !.
centroid([HCluster|TCluster], [HCentroid|TCentroid]) :-
  compute_centroid(1, HCluster, HCentroid), centroid(TCluster, TCentroid).

% CALCOLA IL CENTROIDE DI UN CLUSTER
compute_centroid(N, [HCT], Centroid) :- ldiv(HCT, N, Centroid), !.
compute_centroid(N, [HCT,HCluster|TCluster], Centroid) :- Count is N + 1,
  vsum(HCT, HCluster, NewHCT),
  compute_centroid(Count, [NewHCT|TCluster], Centroid).

% KMEANS
km([], _, []) :- !.
km(Observations, K, Cluster) :- nonvar(Observations), !, nonvar(K), !,
  length(Observations, N), N >= K, !, 
  init_centroid(Observations, K, Centroid),
  compute_cluster(Observations, Centroid, Cluster).

compute_cluster([], _, []) :- !.
compute_cluster(Observations, Centroid, Cluster) :- nonvar(Observations), !,
  nonvar(Centroid), !, partition(Observations, Centroid, ClusterList),
  split_item(Centroid, ClusterList, NewCluster), centroid(NewCluster, NewCentroid),
  ( Centroid = NewCentroid -> true, !, Cluster = NewCluster ;
  compute_cluster(Observations, NewCentroid, Cluster) ), !.

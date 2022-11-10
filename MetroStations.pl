%first line

connected(new_elmarg,elmarg).
connected(elmarg,ezbet_elnakhl).
connected(ezbet_elnakhl,ain_shams).
connected(ain_shams,elmatareyya).
connected(elmatareyya,helmeyet_elzaitoun).
connected(helmeyet_elzaitoun,hadayeq_elzaitoun).
connected(hadayeq_elzaitoun,saray_elqobba).
connected(saray_elqobba,hammamat_elqobba).
connected(hammamat_elqobba,kobri_elqobba).
connected(kobri_elqobba,manshiet_elsadr).
connected(manshiet_elsadr,eldemerdash).
connected(eldemerdash,ghamra).
connected(ghamra,alshohadaa).
connected(alshohadaa,urabi).
connected(urabi,nasser).
connected(nasser,sadat).
connected(sadat,saad_zaghloul).
connected(saad_zaghloul, alsayyeda_zeinab).
connected(alsayyeda_zeinab,elmalek_elsaleh).
connected(elmalek_elsaleh,margirgis).
connected(margirgis,elzahraa).
connected(elzahraa,dar_elsalam).
connected(dar_elsalam,hadayeq_elmaadi).
connected(hadayeq_elmaadi,maadi).
connected(maadi,thakanat_elmaadi).
connected(thakanat_elmaadi,tora_elbalad).
connected(tora_elbalad,kozzika).
connected(kozzika,tora_elasmant).
connected(tora_elasmant,elmaasara).
connected(elmaasara,hadayeq_helwan).
connected(hadayeq_helwan,wadi_hof).
connected(wadi_hof,helwan_university).
connected(helwan_university,ain_helwan).
connected(ain_helwan,helwan).

%second line (elmounib line)
connected(shobra_elkheima,koliet_elzeraa).
connected(koliet_elzeraa,mezallat).
connected(mezallat,khalafawy).
connected(khalafawy,sainte_teresa).
connected(sainte_teresa,road_elfarag).
connected(road_elfarag,massara).
connected(massara,alshohadaa).
connected(alshohadaa,ataba).
connected(ataba,naguib).
connected(naguib,sadat).
connected(sadat,opera).
connected(opera,dokki).
connected(dokki,bohooth).
connected(bohooth,cairo_university).
connected(cairo_university,faisal).
connected(faisal,giza).
connected(giza,omm_elmisryeen).
connected(omm_elmisryeen,sakiat_mekki).
connected(sakiat_mekki,elmounib).

%task1
path(D,D,_,[]).
path(S,D,H,[[S,W]|P]):-
(
   (H=='any') -> connected(S,W),
   path(W,D,H,P),!;
   (H>0) ->connected(S,W),
   NewH is H-1,
   path(W,D,NewH,P),!
).
path(Start, Dest, [[Start,Dest]]) :- connected(Start, Dest).
path(Start, Dest, [[Start, Way]|Path]) :- dif(Dest, Way), connected(Start, Way), path(Way, Dest, Path).


%task2
nstations(Station, 1) :- Station = helwan,!.
nstations(Station, 1) :- Station = shobra_elkheima,!.
nstations(Station, 1) :- Station = elmounib,!.
nstations(Station, 1) :- Station = new_elmarg,!.
nstations(Station, 4) :- Station = sadat ,!.
nstations(Station, 4) :- Station = alshohadaa ,!.
nstations(Station,2)  :- connected(Station,_),!.

%task3
numStations([] , 0).
numStations([H|T],R):-
numStations(T,R1),
R is 1 + R1.
cost(Start,End,P):-
path(Start , End , L),numStations(L,N),
N>7,N<16 -> P is 5,write('Ticket:5 EGP'),!.
cost(Start,End,P):-
path(Start , End , L),numStations(L,N),
N>=16 -> P is 7,write('Ticket:7 EGP');
P is 3 , write('Ticket:3 EGP').

%task4
checkPath([[H1,T1],[H2,T2]]) :-
connected(H1,T1),
connected(H2,T2),
T1 = H2,!.

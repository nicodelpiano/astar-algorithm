/*

	TP BUSQUEDA PROLOG
	   Algoritmo A*

	Nicolas F. Del Piano 


	Independiente NO desciende.
*/

/* Ciudades y sus correspondientes indices */
ciudad(1,'Madrid').
ciudad(2,'Valladolid').
ciudad(3,'Benavente').
ciudad(4,'Burgos').
ciudad(5,'Leon').
ciudad(6,'Santander').
ciudad(7,'Oviedo').
ciudad(8,'Palencia').

/* Grafo de fronteras y pesos (distancias)
   Agregado el ciclo pedido
 */
grafo(1,2,199).
grafo(1,3,264).
grafo(1,4,237).
grafo(2,5,134).
grafo(3,5,69).
grafo(4,6,136).
grafo(6,7,207).
grafo(5,7,118).
grafo(5,8,130).
grafo(8,2,47).

/* Rutas */
ruta(1,2,'N 601').
ruta(1,3,'N VI').
ruta(1,4,'N I').
ruta(2,5,'N 601').
ruta(3,5,'N 630').
ruta(4,6,'N 623').
ruta(6,7,'N 634').
ruta(5,7,'N 630').
ruta(5,8,'N 610').
ruta(8,2,'N 620').

/* heuristica usada                  */
/* distancia en linea recta ficticia */
h(1,300).
h(2,250).
h(3,250).
h(4,100).
h(5,80).
h(6,50).
h(7,50).
h(8,200).

/* 
   encontrar_mejor_camino
   Predicado que aplica A* al grafo anteriormente definido 
   para encontrar el mejor camino entre el Origen y Destino.
   Encuentra el mejor camino (uno solo) y lo imprime primero.

   Nota: Para representar un camino voy a usar una lista
   que tiene como primer elemento el costo total del camino
   en distancia y como cola el indice de cada ciudad del camino.
   O sea, [Costo_Total|Camino_Recorrido].

*/
encontrar_mejor_camino(Origen, Destino):- 
	ciudad(C1,Origen),
	ciudad(C2,Destino),
	a_estrella([[0,C1]],C2,CaminoRev),
	reverse(CaminoRev, Camino),
	write('El mejor camino es: '), imprimir_camino(Camino,Rutas),
	write('Las rutas a recorrer son: '),imprimir_rutas(Rutas),!.

/* Cualquier otro caso */
encontrar_mejor_camino(_,_):- write('Si no viste nada, entonces no hay ningun camino che!.').

/* 
   Igual que la anterior solo que esta sigue buscando 
   y te muestra todos los caminos
*/
encontrar_todos(Origen, Destino):-
	ciudad(C1,Origen),
	ciudad(C2,Destino),
	a_estrella([[0,C1]],C2,CaminoRev),
	reverse(CaminoRev, Camino),
	write('Un camino encontrado es: '), imprimir_camino(Camino,Rutas),
	write('Las rutas a recorrer son: '),imprimir_rutas(Rutas),fail.
encontrar_todos(_,_):- write('Esos son todos!').

/* a_estrella
   Aplicamos el algoritmo de busqueda A*.
   Empezando del nodo inicial, construimos los caminos posibles
   utilizando los predicados auxiliares definidos mas abajo. 
   Primero elegimos el camino de mejor f (en el caso inicial va a tomar el nodo inicial), 
   expandimos las fronteras de este ultimo, saco el camino viejo de los caminos actuales
   y sigo aplicando el algoritmo en la lista de caminos que tiene a los caminos viejos sin el 
   borrado y los caminos nuevos expandidos, para comparar todos.

*/

a_estrella(Caminos, Dest, [C,Dest|Camino]):- 
	member([C,Dest|Camino],Caminos),
	elegir_mejorf(Caminos, [C1|_]),
	C1 == C.
a_estrella(Caminos, Destino, MejorCamino):- 
	elegir_mejorf(Caminos, MejorF),
	delete(Caminos, MejorF, CaminosAnteriores),
	expandir_frontera(MejorF, NuevosCaminos),
	append(CaminosAnteriores, NuevosCaminos, L),
	a_estrella(L, Destino, MejorCamino).

/*
   elegir_mejorf
   De todos los caminos nuevos expandidos por la frontera elegimos
   el de mejor f = h + g, es decir, la eleccion que hace el algoritmo A*.
   Comparamos cada primer elemento del camino (o sea los costos) y a eso
   le sumamos la heuristica correspondiente del penultimo elemento de 
   la lista (o sea la ciudad actual). Eso nos da f, y elegimos el camino de mejor f.
*/

elegir_mejorf([X],X):-!.
elegir_mejorf([[C1,Ci1|Y],[C2,Ci2|_]|Z], MejorF):-
	h(Ci1, H1),
	h(Ci2, H2),
	H1 +  C1 =< H2 +  C2,
	elegir_mejorf([[C1,Ci1|Y]|Z], MejorF).
elegir_mejorf([[C1,Ci1|_],[C2,Ci2|Y]|Z], MejorF):-
	h(Ci1, H1),
	h(Ci2, H2),
	H1  + C1 > H2 +  C2,
	elegir_mejorf([[C2,Ci2|Y]|Z], MejorF).


/* 
expandirFrontera.
Buscamos todos los nodos del grafo que
   sean frontera de la ciudad actual
   y lo adherimos a la lista L.
   Luego actualizamos los costos de todos los caminos nuevos
   que hay en L.

   Nota: el not member es para no pasar por nodos ya visitados y corregir el error
   de los ciclos.
*/
expandir_frontera([Costo,Ciudad|Camino],Caminos):- 
	findall([Costo,CiudadNueva,Ciudad|Camino],
		(grafo(Ciudad, CiudadNueva,_),
		not(member(CiudadNueva,Camino))),
		L),
	cambiar_costos(L, Caminos).

/* cambiar_costos 
   Actualizo los costos de cada camino, una vez agregado
   el ultimo nodo. 
   Recordar que cada camino esta representado por una lista donde el primer
   elemento es el costo total del camino y los elementos siguientes de la cola son
   los indices de las ciudades que reprensentan.
   [Costo_Total|Camino]. 
*/
cambiar_costos([],[]):-!.
cambiar_costos([[Costo_Total,Ci1,Ci2|Camino]|Y],[[NuevoCosto_Total,Ci1,Ci2|Camino]|Z]):-
	grafo(Ci2, Ci1, Distancia),
	NuevoCosto_Total is Costo_Total + Distancia,
	cambiar_costos(Y,Z).


/* 
imprime el camino en pantalla
 */
imprimir_camino([Costo],[]):- nl, write('El costo total del camino recorrido es: '), write(Costo), write(' kilometros.'),nl.
imprimir_camino([Ciudad,Costo],[]):- ciudad(Ciudad, Nombre), write(Nombre), write(' '), nl, write('El costo total del camino recorrido es: '), write(Costo), write(' kilometros.'),nl.
imprimir_camino([Ciudad,Ciudad2|Y],Rutas):-
	ciudad(Ciudad, Nombre),
	ruta(Ciudad,Ciudad2,Ruta),
	append([Ruta],R,Rutas),
	write(Nombre),write(', '),
	imprimir_camino([Ciudad2|Y],R).


/* imprimir rutas */
imprimir_rutas([X]):- write(X), nl, nl.
imprimir_rutas([X|Y]):-
	write(X),write(' - '),
	imprimir_rutas(Y).

/*Aclaracion: aca no comente mucho porque es mas de lo mismo*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*----------------- COSTO UNIFORME ----------------------------------*/
/*-------------------------------------------------------------------*/
/* Modifico la heuristica para que sea todo 0 y tengo el costo uniforme */

busqueda_costo_uniforme(Origen,Destino):-
	ciudad(C1,Origen),
	ciudad(C2,Destino),
	costo_uniforme([[0,C1]],C2,CaminoRev),
	reverse(CaminoRev, Camino),
	write('El mejor camino es: '), imprimir_camino(Camino,Rutas),
	write('Las rutas a recorrer son: '),imprimir_rutas(Rutas),!.

/* Cualquier otro caso */
busqueda_costo_uniforme(_,_):- write('Si no viste nada, entonces no hay ningun camino che!.').

costo_uniforme(Caminos, Dest, [C,Dest|Camino]):- 
	member([C,Dest|Camino],Caminos),
	elegir_mejorg(Caminos, [C1|_]),
	C1 == C.
costo_uniforme(Caminos, Destino, CaminoUniforme):- 
	elegir_mejorg(Caminos, MejorG),
	delete(Caminos, MejorG, CaminosAnteriores),
	expandir_frontera(MejorG, NuevosCaminos),
	append(CaminosAnteriores, NuevosCaminos, L),
	costo_uniforme(L, Destino, CaminoUniforme).

elegir_mejorg([X],X):-!.
elegir_mejorg([[C1|C],[C2|_]|Cola],MejorG):-
	C1 =< C2,
	elegir_mejorg([[C1|C]|Cola],MejorG).
elegir_mejorg([[C1|_],[C2|C]|Cola],MejorG):-
	C1 > C2,
	elegir_mejorg([[C2|C]|Cola],MejorG).

/*---------------------------------------------------------------*/
/*----------------- COSTO AVARA (Greedy) ------------------------------*/
/*---------------------------------------------------------------*/
/* Solo uso h y me olvido de los pesos */

busqueda_greedy(Origen,Destino):-
	ciudad(C1,Origen),
	ciudad(C2,Destino),
	busqueda_avara([[0,C1]],C2,CaminoRev),
	reverse(CaminoRev, Camino),
	write('El mejor camino es: '), imprimir_camino(Camino,Rutas),
	write('Las rutas a recorrer son: '),imprimir_rutas(Rutas),!.

/* Cualquier otro caso */
busqueda_greedy(_,_):- write('Si no viste nada, entonces no hay ningun camino che!.').

busqueda_avara(Caminos, Dest, [C,Dest|Camino]):- 
	member([C,Dest|Camino],Caminos),
	elegir_mejorh(Caminos, [C1|_]),
	C1 == C.
busqueda_avara(Caminos, Destino, CaminoGreedy):- 
	elegir_mejorh(Caminos, MejorH),
	delete(Caminos, MejorH, CaminosAnteriores),
	expandir_frontera(MejorH, NuevosCaminos),
	append(CaminosAnteriores, NuevosCaminos, L),
	busqueda_avara(L, Destino, CaminoGreedy).

elegir_mejorh([X],X):-!.
elegir_mejorh([[C1,Ci1|Y],[_,Ci2|_]|Z], MejorH):-
	h(Ci1, H1),
	h(Ci2, H2),
	H1 =< H2,
	elegir_mejorh([[C1,Ci1|Y]|Z], MejorH).
elegir_mejorh([[_,Ci1|_],[C2,Ci2|Y]|Z], MejorH):-
	h(Ci1, H1),
	h(Ci2, H2),
	H1> H2,
	elegir_mejorh([[C2,Ci2|Y]|Z], MejorH).

/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/



# RMSD

En la elaboración de este cuaderno de actividades se pide el desarrollo de una aplicación capaz de calcular el RMSD o desviación cuadrática media de una serie de residuos determinados por el usuario. 

$$
RMSD = \sqrt{\frac{\sum^{n}_{i, j}(Dist1_{i,j}-Dist2_{i,j})^2}{n}}
$$

En primer lugar, definiremos una función que calcule la distancia entre dos puntos en el espacio. 

=== "Distancia (coordenadas)"

	```pascal linenums="1"
	function distancia3D(a1, b1, c1, a2, b2, c2: real): real;
         begin
           result :=sqrt(sqr(a1-a2)+sqr(b1-b2)+sqr(c1-c2));
           end;
=== "Distancia (TPunto)"

	```pascal linenums="1"
	function distancia3D(p1, p2: Tpunto): real; overload;
         begin
           result :=sqrt(sqr(p1.X-p2.X)+sqr(p1.Y-p2.Y)+sqr(p1.Z-p1.Z));
           end;

El siguiente paso consiste en crear una función capaz de generar una matriz con las distancias internas entre todos los átomos de un residuo. 

??? example "matriz_distancias()"
	```pascal linenums="1"
	function matriz_distancias (puntos: TPuntos): ;
	var 
	matriz : MatrizDistancias;
	i, j, n : integer;
	begin
	 n:= high(puntos)+1,
	 SetLength(matriz, n, n);
	   for i:= 0 to n do  
	      for j:= 0 to n do  
		 matriz[i,j]:= distancia3D(puntos[i], puntos[j]); 
	end;



# Diagrama de Ramachandran

En la realización de este cuaderno de actividades se pide el desarrollo de una función que permita obtener los ángulos diedros de una proteína y su representación en un diagrama de Ramachandran. Para la elaboración de esta actividad se ha hecho uso del material impartido en clase en forma de apuntes. El código correspondiente a esta funcionalidad se puede encontrar en la librería [biotools/src_biotools](https://github.com/currocam/biotools_hQC/blob/master/biotools/src_biotools.pas) y la implementación en un programa con interfaz gráfica en el repositorio bajo el nombre de [ramachandran](https://github.com/currocam/biotools_hQC/tree/master/ramachandran).

## Ángulos de torsión

Los ángulos de torsión característicos de una proteína son los ángulos $\psi$ y $\phi$ y se pueden calcular para todos os residuos de una proteína excepto el primero y último. 

| ![ángulos de torsión](https://www.researchgate.net/publication/312022960/figure/fig8/AS:668976552095754@1536507847848/Dihedral-angle-representation-A-very-small-section-of-the-protein-backbone-is-displayed.png) |
|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|
| Figura 1. Representación de los ángulos de torsión.[^1]                                                                                                                                                            |

A continuación, se muestran las funciones desarrolladas `angulo()` y  `torsion()`  para calcular el ángulo diedro (según las convenciones IUPAC de signo) para 4 puntos en el espacio (definidos según el `record TPunto`. Estas funciones se basan en que podemos calcular el ángulo diedro formado por 4 puntos en el espacio, $A$, $B$, $C$ y $D$, como el ángulo que forman entre si los vectores $V_1$ y $V_2$, resultantes del producto vectorial de $\vec{BC}$ y $\vec{BA}$ respectivamente. Además, este ángulo diedro debe de corregirse después en signo para adecuarse a las convenciones IUPAC.

=== "angulo (A, B: Tpunto)"

	```pascal linenums="1"
    	function angulo (A, B: Tpunto): real;
	var
	   denominador: real;
	begin
	  denominador:= modulo(A)*modulo(B);
	if denominador > 0
	 then  result:= arccos(A*B/denominador)
	 else  result:=  maxfloat;
	end;

	```
=== "angulo(A, B, C: TPunto)"
	```pascal linenums="1"
	function angulo(A, B, C: TPunto):real; overload;
	var
	   BA,BC: TPunto;
	begin
	   BA:= A-B;
	   BC:= C-B;
	   result:= angulo( BA, BC);
	end;

	```
=== "torsion ( A, B, C, D: TPunto)"
	```pascal linenums="1"
	function torsion ( A, B, C, D: TPunto): real;
	var
	   BA, BC, CB, CD, V1, V2, P: TPunto;
	   diedro, diedro_IUPAC, denominador, cosenoGamma: real;
	begin
	     diedro_IUPAC:=0;
	   BA:= A- B;
	   BC:= C-B;
	   CB:= B-C;
	   CD:= D-C;
	   V1:= prodVectorial(BC, BA);
	   V2:= prodVectorial(CD, CB);
	   diedro:= angulo(V1, V2);
	   P:= prodVectorial(V2, V1);
	   denominador:= modulo(P)* modulo(CB);
	   if denominador >0 then
	   begin
	    cosenoGamma:= P* CB/ denominador;
	    if cosenoGamma >0 then cosenoGamma:=1 else cosenoGamma:=-1;
	   end else diedro_IUPAC:= maxfloat;
	   if diedro_IUPAC < maxfloat then diedro_IUPAC:= diedro*cosenoGamma;
	   result:=diedro_IUPAC;
	end;
	```
## Diagrama de Ramachandran
Los diagramas de Ramachandran son representaciones de los ángulos de torsión de los residuos de una proteína y son de gran interés puesto que es posible predecir la estructura secundaria de una proteína en base a los pares de valores $\psi$ y $\phi$ que tengan. Además, nos da información sobre la calidad de una estructura tridimensional, puesto que valores que se alejen de aquellas zonas que se consideran "normales" son indicativo de un posible error en la estructura. 

|![Ramachandran](https://upload.wikimedia.org/wikipedia/commons/9/90/Ramachandran_plot_general_100K.jpg)|
|:--:|
|Figura 2. Diagrama de Ramachandran.[^2]|

A continuación, se muestra la representación del Diagrama de Ramachandran calculado para la proteína 2AFW (experimental y dibujada en azul) con la proteína calculada por AlphaFold (en blanco).Como se puede observar, los resultados son altamente similares entre ambas.  

| ![Ramachandran](images/2AFW_Vs_AlphaFold_Ramachandran.jpeg)                                                                                              |
|:-------------------------------------------------------------------------------------------------------------------------------------------------------:|
| Figura 3. Representación esquemática del diagrama de Ramachandran para la proteína experimental 2AFM comparada con la proteína predecida por AlphaFold. |

En la realización de este ejercicio, se pide que se compare los valores de ángulos de torsión calculados por nuestro programa frente a los valores de referencia que obtengamos por una aplicación profesional. En nuestro caso, vamos a emplear como referencia los ángulos de torsión calculados por la aplicación [Torsion angles](https://swift.cmbi.umcn.nl/servers/html/chiang.html) para la proteína 2AFM. Como puede observarse en la Tabla 1, los valores son enormemente parecidos, diferenciándose únicamente debido a distintos criterios de redondeo. 

| Número de residue 	| Residuo 	| $\phi_{\text{ref}}$ 	| $\phi_{\text{calculado}}$ 	| $\psi_{\text{ref}}$ 	| $\psi_{\text{calculado}}$ 	|
|:---:	|:---:	|:---:	|:---:	|:---:	|:---:	|
| 34 	| SER 	| -41.80 	| -41.77 	| 138.20 	| 138.19 	|
| 35 	| ALA 	| -98.40 	| -98.42 	| 22.50 	| 22.48 	|
| 36 	| TRP 	| -59.90 	| -59.89 	| -31.10 	| -31.05 	|
| 37 	| PRO 	| -67.90 	| -67.86 	| -9.00 	| -9.03 	|
| 38 	| GLU 	| -89.60 	| -89.61 	| -10.40 	| -10.38 	|
| 39 	| GLU 	| -55.90 	| -55.90 	| -37.10 	| -37.10 	|
| 40 	| LYS 	| -56.70 	| -56.73 	| -28.00 	| -28.00 	

Tabla 1. Comparación de los ángulos de torsión calculados frente a un valor de referencia para la proteína 2AFM.
## Ejemplo de uso 

A continuación, se muestra en una animación la implementación de estas funciones en una interfaz gráfica dentro del programa `Ramachandran`. Para la realización de este programa fue necesario escribir la función `PlotXY` utilizando la clase `Canvas`. No entraremos en detalles en el funcionamiento de esta puesto que fue desarrollada en clase.No obstante, y para obtener un gráfico de mayor calidad con ejes, utilizamos también la clase `TChart` que facilita la realización de gráficos. No obstante, hemos querido mantener el gráfico original para mostrar cómo nuestro 'módulo gráfico' funciona de manera muy parecida a una clase desarrollada de manera profesional. 

| ![Interfaz gráfica para el programa Ramachandran](images/ramachandran.gif) |
|:--------------------------------------------------------------------------:|
| Figura 4. Animación del programa `Ramachandran` mostrando su uso.          |


# Referencias
[^1]: «A Generative Angular Model of Protein Structure Evolution». Molecular Biology and Evolution 34, n.º 11 (1 de noviembre de 2017): 3040-3040. https://doi.org/10.1093/molbev/msx214.
.
[^2]: Imagen elaborada por Dcrjsr y obtenida a través de Wikipedia. 


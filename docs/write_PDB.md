# WritePDB: Extraer átomos Carbono alfa

En la realización de este cuaderno de actividades se pide el desarrollo de una función que permite escribir la información relativa a un átomo de un archivo PDB en el formato adecuado. El objetivo es emplear dicha función para extraer los carbonos alfa de un fichero PDB, escribirlos en un archivo pseudo-pdb y estudiar el desorden de la proteína en base a su factor $B$. El factor $B$ representa el desplazamiento de los átomos de su posición media en la estructura cristalina (y que se observa como una disminución en al intensidad de la difracción). Puede tener dos causas: puede ser el resultado de vibraciones atómicas dependientes de la temperatura o del desorden estático en la estructura cristalina. Podemos utilizarlo, por tanto, como indicador de los residuos más desordenados de la proteína [truebloodAtomicDispacementParameter1996]. 

 El código correspondiente a esta funcionalidad se puede encontrar en la librería [biotools/src_biotools](https://github.com/currocam/biotools_hQC/blob/master/biotools/src_biotools.pas) y la implementación en un programa con interfaz gráfica en el repositorio bajo el nombre de [write_PDB](https://github.com/currocam/biotools_hQC/tree/master/write_PDB).

## WriteAtomPDB

En primer lugar, se desarrolló una función que escribiera el contenido de un `record` tipo `TAtomPDB` en una línea tipo `AnsiString` correctamente formateada. Para hacerlo, se consultó la bibliografía proporcionada para familiarizarnos con el formato PDB. Para su desarrollo se definieron también una serie de test unitarios para asegurar la función se comportaba de acuerdo a lo deseado. 

A continuación, se muestra la función `WriteAtomPDB`. Para escribirla se utilizó la función `formatfloat` para convertir los valores numéricos en cadenas de texto con los números decimales adecuados y la función `format` para alinear dentro del número de espacios que ocupa cada tipo de variable en los archivos PDB a la izquierda o a la derecha, según fuera necesario. Para ello se utiliza la sintaxis `'%d'`. Añadir, además, que, puesto que no se consideraba el factor ocupancia en la estructura `TAtomPDB`, no se ha incluido tampoco en este función (por lo que todos los átomos del pseudo-pdb tendrán una ocupancia idéntica e igual a 1.00).  

??? example "Función WriteAtomPDB"
	```pascal linenums="1"
	function WriteAtomPDB(atom: TAtomPDB): AnsiString;
	var
	  numatm, numres, X, Y, Z, R: AnsiString;
	  linea: AnsiString;
	begin
	    // Obtenemos la información del objeto atom en el formato numérico que nos interesa
	    numatm:= inttostr(atom.NumAtom);
	    numres := inttostr(atom.NumRes);
	    X  := formatfloat('0.000', atom.coor.X);
	    Y  := formatfloat('0.000', atom.coor.Y);
	    Z  := formatfloat('0.000', atom.coor.Z);
	    R  := formatfloat('0.00', atom.R);
	    linea:= Concat('ATOM  ' , // 6 char justificado a la izquierda
            format('%5s', [numatm]) ,  // 5 char justificado a la derecha
            '  ' ,
            format('%-3s', [atom.ID]) ,
            ' ' + //No incluimos altLoc
            atom.residuo,
            ' ',
            atom.subunidad,
            '  ',
            format('%-4s', [numres]), //4 char justificado a la derecha
            '  ', //No incluimos iCode
            ' ',
            format('%7s', [X]),
            ' ',
            format('%7s', [Y]),
            ' ',
            format('%7s', [Z]),
            '  ',
            format('%-5s', ['1.00']), //No incluimosla ocupancia
            format('%-6s', [R]));
            // No incluimos el resto de parámetros

	result:= linea;
	end;          
	```

## Test unitario

A continuación, se muestra a modo de ejemplo uno de los test unitarios que se escribió para comprobar el buen funcionamiento de la función `writeAtomPDB()`. 

??? example "testing_biotools.writePDB"
	```pascal linenums="1"
	procedure testing_biotools.writePDB;
	// 'ATOM      1  N   ALA A  33      -6.424 -34.116  36.857  1.00 43.50')
	var
	atm: TAtomPDB;
	str1, str2: AnsiSTring;
	begin
	  atm.NumAtom := 141;
	  atm.ID := 'CA';
	  atm.residuo:= 'SER';
	  atm.subunidad := 'A';
	  atm.NumRes:= 50;
	  atm.coor.X := -24.739;
	  atm.coor.Y := -24.229;
	  atm.coor.Z := 0.489;
	  atm.R:= 22.73 ;
	  str1 :=  WriteAtomPDB(atm);
	  str2 := 'ATOM    141  CA  SER A  50     -24.739 -24.229   0.489  1.00 22.73';
	  IF NOT SameText(trim(str1),str2) THEN
	     begin
	       Fail('El formato no es el adecuado. ');
	     end;
	end;        
	```
## Ejemplo de uso

A continuación, se muestra una pequeña animación donde se muestra la implementación en una interfaz gráfica del código anterior. Destacar que, aunque el ejercicio pedía, en un principio, un programa capaz de extraer los carbonos $\alpha$ de un archivo PDB, nos pareció más adecuado generalizarlo a un programa capaz de extraer cualquier tipo de átomo dado por el usuario. 

|![writePDB](images/write_PDB.gif)|
|:--:|
|Figura 1. Animación con ejemplo de uso de la aplicación writePDB para extraer átomos de cierto ID| 

## Extracción carbonos alfa y visualización en pyMol según el factor B

Por último, se realizó la tarea propuesta en el ejercicio. Se obtuvo un pseudo archivo PDB con los átomos correspondientes a los carbonos $\alpha$ de la proteína 2AFM y, usando el programa pyMol, se obtuvo una animación de la proteína en modo spacefill y coloreada según el factor B. En pos de la reproducibilidad, se muestra a continuación tanto la animación como el script utilizado para obtenerla. No se entrará en detalles al respecto de la obtención de la animación puesto que se ha seguido usando las mismas herramientas empleadas en el apartado de Visualización de este cuaderno de actividades. 

??? example "Visualización carbonos alfa 2AFM"
	```python linenums="1"
	set ray_opaque_background, off
	load write_PDB/data/2AFM_CA_pseudo.pdb
	spectrum b, blue_white_red, minimum=10, maximum=50
	as surface, 
	label b >60, "(%s, %s)" % (resn, resi)
	python

	import imageio
	step = 1
	images = []
	for a in range(0,180,step):
	  cmd.rotate("y", float(step)) 
	  cmd.ray(500, 500)
	  filename = "file"+str(a)+".png"
	  cmd.png(filename)
	  images.append(imageio.imread(filename))
	imageio.mimsave('pseudoPDB_b_factor.gif', images)
	python end
	```

|![writePDB](images/pseudoPDB_b_factor.gif)|
|:--:|
|Figura 2. Animación de los carbonos $\alpha$ de la proteína 2AFMen modo spacefill y coloreados según el factor B. Además, se han anotado los residuos que tuvieran un valor de B superior a 60| 

Respecto a la interpretación de la anterior imagen, cabe destacar que una de las dos subunidades que comprenden el PDB (ambas versiones de la misma proteína) una de ellas tiene valores de factores B muy superiores. 


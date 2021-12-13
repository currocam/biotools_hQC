# Parseador ficheros PDB

En la realización de este cuaderno de actividades se pide el desarrollo de una función, CargarPDB, capaz de parsear la información estructural relevante de un archivo PDB y almacenarla en una estructura matricial. Esta función ha sido realizada durante el transcurso de la asignatura y es uno de los elementos claves del entorno de trabajo que hemos desarrollado durante el semestre. A continuación se procede a mostrar de forma detallada el funcioamiento de la misma. El código correspondiente a esta funcionalidad se puede encontrar en la librería [biotools/src_biotools](https://github.com/currocam/biotools_hQC/blob/master/biotools/src_biotools.pas). Además, aclarar que para la epxlicación no se seguirá el orden cronológico en que se programó (el cuál podría consultarse siguiendo los commits del repositorio) sino el orden lógico de este. 

## Records y Arrays

En primer lugar, se definieron una serie de records que nos permitieran almacenar la información de forma ordenada y de vectores dinámicos, dynamic arrays, para almacenar estos. Un record en pascal es un tipo de dato altamente estructurado y que consiste en la agrupación de elementos de distinto tipo. Un array es un tipo que agrupa variables del mismo tipo. Utilizamos vectores dinámicos cuando no es posible conocer el número exacto de elementos necesarios, por lo será necesario definir el tamaño de las mismas durante la ejecución del programa. 

Se definió un record que contuviera la información relativa a un punto en el espacio, TPunto, y un array de puntos. 
```pascal
TPunto = record
	X,Y,Z: real;
	end;
Tpuntos = array of Tpunto;

```
Se definió un record que contuviera la información relativa a un átomo de un archivo PDB en el espacio. La información que se consideró fue la siguiente como relevante fue la siguiente. Destacar que la información relativa a las coordenadas se almacena en un TPunto. 
```pascal
TAtomPDB = record
	NumAtom: integer;     //Número de átomo
    	ID: string;           //Tipo de átomo
    	residuo: string;      //Residuo al que pertenece
    	subunidad: char;      //Subunidad a la que pertenece
    	NumRes: integer;      //Número de residuo
    	coor: Tpunto;         //Coordenadas espaciales
    	R: real;              //Factor temp
    	end;
```
Se definió un record que contuviera la información relativa a un residuo de un archivo PDB, es decir, a una serie grupo de átomos (de longitud variable) sobre los cuáles se pueden definir nuevas propiedades como ángulos diedros. 
```pascal
TResiduoPDB = record    // con los residuos
	phi,psi: real;        //Ángulos diedros
    	NumRes: integer;      //Número de residuo
    	subunidad: char;      //Subunidad a la que pertenece
    	ID3: string;          //Identificador residuo 3 letras
    	ID1: char;            //Identificador residuo 1 letra
    	Atm1, AtmN: integer;  //Número 1º y último átomo
    	N, CA, C, O: integer; //Número átomos esenciales de residuo
    	end;
```
Por último, se definieron unos record con la información relativa a una subunidad de un archivo PDB y a un archivo PDB completo. Destacar que, en este caso, se optó por almacenar información redundante con el fin de simplificar el acceso a esta luego. Estos records constan de una serie de vectores dinámicos que contienen TAtomPDB, TResiduoPDB, TSubunidadPDB y también de índices. 
```pascal
TsubunidadPDB = record
	ID: char; 
    	atm1, atmN, res1, resN: integer; 
    	atomCount, resCount: integer;
    	ResIndex: array of integer;
    	end;

 TPDB = record
       header: string;
       atm: array of TAtomPDB;          //array de records con información átomos
       res: array of TResiduoPDB;       //array de records con información residuos
       sub: array of TsubunidadPDB;     //array de records con información subunidades
       NumFichas, NumResiduos: integer; //Número de átomos y residuos
       NumSubunidades : integer;        //Número de subunidades
       subs, secuencia : string;        //Strings caracteres subunidades y secuencia
       end;
```

## Referencias
\bibliography

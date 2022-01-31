# Parseador ficheros PDB

En la realización de este cuaderno de actividades se pide el desarrollo de una función, CargarPDB, capaz de parsear la información estructural relevante de un archivo PDB y almacenarla en una estructura matricial. Esta función ha sido realizada durante el transcurso de la asignatura y es uno de los elementos claves del entorno de trabajo que hemos desarrollado durante el semestre. A continuación se procede a mostrar de forma detallada el funcioamiento de la misma. El código correspondiente a esta funcionalidad se puede encontrar en la librería [biotools/src_biotools](https://github.com/currocam/biotools_hQC/blob/master/biotools/src_biotools.pas). Además, aclarar que para la epxlicación no se seguirá el orden cronológico en que se programó (el cuál podría consultarse siguiendo los commits del repositorio) sino el orden lógico de este. 

## Records y Arrays

En primer lugar, se definieron una serie de records que nos permitieran almacenar la información de forma ordenada y de vectores dinámicos, dynamic arrays, para almacenar estos. Un record en pascal es un tipo de dato altamente estructurado y que consiste en la agrupación de elementos de distinto tipo. Un array es un tipo que agrupa variables del mismo tipo. Utilizamos vectores dinámicos cuando no es posible conocer el número exacto de elementos necesarios, por lo será necesario definir el tamaño de las mismas durante la ejecución del programa. 

1. Se definió un record que contuviera la información relativa a un punto en el espacio, TPunto, y un array de puntos. 

2. Se definió un record que contuviera la información relativa a un átomo de un archivo PDB en el espacio. La información que se consideró fue la siguiente como relevante fue la siguiente. Destacar que la información relativa a las coordenadas se almacena en un TPunto. 

3. Se definió un record que contuviera la información relativa a un residuo de un archivo PDB, es decir, a una serie grupo de átomos (de longitud variable) sobre los cuáles se pueden definir nuevas propiedades como ángulos diedros. 

4. Por último, se definieron unos record con la información relativa a una subunidad de un archivo PDB y a un archivo PDB completo. Destacar que, en este caso, se optó por almacenar información redundante con el fin de simplificar el acceso a esta luego. Estos records constan de una serie de vectores dinámicos que contienen TAtomPDB, TResiduoPDB, TSubunidadPDB y también de índices. 

=== "TPunto"

	```pascal linenums="1"
    TPunto = record
        	X,Y,Z: real;
	        end;
    Tpuntos = array of Tpunto;

	```
=== "TAtomPDB"
	```pascal linenums="1"
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
=== "TResiduoPDB"
	```pascal linenums="1"
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
=== "TsubunidadPDB"
	```pascal linenums="1"
	TsubunidadPDB = record
		ID: char; 
	    	atm1, atmN, res1, resN: integer; 
	    	atomCount, resCount: integer;
	    	ResIndex: array of integer;
	    	end;
	```
=== "TPDB"	
	```pascal linenums="1"
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

## Cargar PDB
La función cargar PDB básica que se definió en un principio, fue la siguiente. Esta función tenía como único argumento un TStrings (texto contenido en un memo) y, de forma sistemática, recorría las líneas del archivo PDB, accediendo a la información relativa a cada átomo, residuo y subunidad, guardándola en un record dentro de un array. Para hacer esto se tuvo en cuenta que los archivos .pdb son archivos que siguen unas pautas de formato muy específicas.

### Cálculo ángulos de torsión
Una vez recorrido el archivo .pdb, se recorrían cada una de las subunidades y residuos del TPDB para definir los ángulos diedros $\psi$ y $\phi$. Para ello, se definió una función llamada torsión, que calculaba el ángulo de torsión 4 puntos en el espacio. Destacar que, para facilitar el trabajo vectorial, se definieron nuevos operadores aritméticos y booleanos para el tipo TPunto. Por ejemplo, así definimos algunos de ellos: 

???+ example "Operadores geométricos src_biotools"
	```pascal linenums="1"
	Operator + (A, B : TPunto): Tpunto;
	begin
		result.X:= A.X + B.X;
	  	result.Y:= A.Y + B.Y;
	  	result.Z:= A.Z + B.Z;
	end;

	Operator * (V: TPunto; k: real):TPunto;
	begin
	 	result.X:= k*V.X;
		result.Y:= k*V.Y;
		result.Z:= k*V.Z;
	  end;

	Operator = (A, B: TPunto):boolean;
	begin
		if (A.X = B.X) and (A.Y = B.Y) and (A.Z= B.Z) then result:= True;
		else  result:=False;
	end;
	```
A continuación, se muestra la función utilizada para calcular los ángulos de torsión, teniendo en cuenta las convenciones de la IUPAC. 

??? example "Cálculo ángulos diedros"
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
A continuación, se muestra la función CargarPDB: 

??? example "Función CargarPDB"
	```pascal linenums="1"
	function CargarPDB(texto: TStrings): TPDB;    overload;
	  var
	    p: TPDB;
	    linea: string;
	    j, k, F, R, S: integer;
	    resno: integer;

	  begin

	    p.secuencia:='';
	    F:=0; R:=0; S:=0;
	    setlength(p.atm, texto.count);
	    setlength(p.res, texto.count);
	    setlength(p.sub, texto.count);

	    for j:=0 to texto.count-1 do
	    begin
	      linea:= texto[j];
	      if (copy(linea,1,6)='ATOM  ')then
	      begin
		F:= F+1;
		p.atm[F].NumAtom :=strtoint(trim(copy(linea,7,5)));
		p.atm[F].ID:= trim(copy(linea, 13, 4));
		p.atm[F].Residuo:= copy(linea, 18,3);
		p.atm[F].Subunidad:=linea[22];
		p.atm[F].NumRes:= strtoint(trim(copy(linea,23,4)));
		p.atm[F].coor.X:= strtofloat(trim(copy(linea,31,8)));
		p.atm[F].coor.Y:= strtofloat(trim(copy(linea,39,8)));
		p.atm[F].coor.Z:= strtofloat(trim(copy(linea,47,8)));
		p.atm[F].R:= strtofloat(trim(copy(linea, 61,6)));

	      //Residuo
		if p.atm[F].ID = 'N' then
		begin
		  R:= R+1;
		  p.res[R].Atm1:= F;
		  p.res[R].ID3:=p.atm[F].Residuo;
		  p.res[R].ID1:=AA3to1(p.res[R].ID3);
		  p.res[R].N:= F;
		  p.res[R].NumRes:= p.atm[F].NumRes;
		  p.res[R].Subunidad:= p.atm[F].Subunidad;
		  p.secuencia:= p.secuencia + p.res[R].ID1;

		//Subunidad
		  if pos(p.atm[F].Subunidad, p.subs)=0 then
		  begin
		    S:= S+1;
		    p.subs:= p.subs + p.atm[F].Subunidad;
		    p.sub[S].ID:=p.atm[F].Subunidad;
		    p.sub[S].atm1:=F;
		    p.sub[S].res1:=R;
		  end;
		end;
		if p.atm[F].ID='CA' then p.res[R].CA:= F;
		if p.atm[F].ID='C' then p.res[R].C:= F;
		if p.atm[F].ID='O' then p.res[R].O:= F;
		p.res[R].AtmN:= F;
		p.sub[S].atmN:= F;
		p.sub[S].resN:= R;

	      end;
	    end;
	    setlength(p.atm, F+1);
	    setlength(p.res, R+1);
	    setlength(p.sub, S+1);
	    p.NumFichas:= F;
	    p.NumResiduos:= R;
	    p.NumSubunidades:= S;

	    for j:=1 to p.NumSubunidades do with p.sub[j] do
	    begin
	      AtomCount:= atmN - atm1 + 1;
	      ResCount:= resN - res1 + 1;
	      for k:=p.sub[j].res1 + 1 to p.sub[j].resn - 1 do
	      begin
		p.res[k].phi:=torsion(p.atm[p.res[k-1].C].coor,
		                      p.atm[p.res[k].N].coor,
		                      p.atm[p.res[k].CA].coor,
		                      p.atm[p.res[k].C].coor);

		p.res[k].psi:=torsion(p.atm[p.res[k].N].coor,
		                      p.atm[p.res[k].CA].coor,
		                      p.atm[p.res[k].C].coor,
		                      p.atm[p.res[k+1].N].coor);

	      end;

	      setlength(p.sub[j].resindex, p.NumResiduos + 1);
	      for k:=1 to p.sub[j].ResCount do
	      begin
		resno:= p.sub[j].res1 + k - 1;
		p.sub[j].resindex[p.res[resno].numres]:= resno;
	      end;
	    end;

	    result:=p;
	  end
	```


## Referencias
\bibliography

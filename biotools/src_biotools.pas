unit src_biotools;

{$mode objfpc}{$H+}

interface


uses
  Classes, SysUtils, Math, Graphics, Dialogs, Forms, Controls, Menus, StdCtrls,
  ExtCtrls;  // Cargar librerías
                                                      //necesarias biotools


type
  // RECORDS DE INTERÉS
  //
  // Definimos record como punto en el espacio
  TPunto = record
    X,Y,Z: real;
    end;

  Tpuntos = array of Tpunto;

  // Definimos record con la información de un átomo de archivo PDB
  TAtomPDB = record
    NumAtom: integer;     //Número de átomo
    ID: string;           //Tipo de átomo
    residuo: string;      //Residuo al que pertenece
    subunidad: char;      //Subunidad a la que pertenece
    NumRes: integer;      //Número de residuo
    coor: Tpunto;         //Coordenadas espaciales
    R: real;              //Factor temp
    end;
  //Definimos record con la información de un residuo de un archivo PDB
  TResiduoPDB = record    // con los residuos
    phi,psi: real;        //Ángulos diedros
    NumRes: integer;      //Número de residuo
    subunidad: char;      //Subunidad a la que pertenece
    ID3: string;          //Identificador residuo 3 letras
    ID1: char;            //Identificador residuo 1 letra
    Atm1, AtmN: integer;  //Número 1º y último átomo
    N, CA, C, O: integer; //Número átomos esenciales de residuo
    end;

   //Definimos record con la información de una subunidad de un archivo PDB
   TsubunidadPDB = record
    ID: char;
    atm1, atmN, res1, resN: integer;
    atomCount, resCount: integer;

    ResIndex: array of integer;
    end;

  //Definimos record con la información de un archivo PDB
  TPDB = record
       header: string;
       atm: array of TAtomPDB;          //array de records con información átomos
       res: array of TResiduoPDB;       //array de records con información residuos
       sub: array of TsubunidadPDB;     //array de records con información subunidades
       NumFichas, NumResiduos: integer; //Número de átomos y residuos
       NumSubunidades : integer;        //Número de subunidades
       subs, secuencia : string;        //Strings caracteres subunidades y secuencia
  end;
  Matriz2D = array of array of real;
  ArrayMatrices = array of array of array of real;
  TTabladatos = array of array of real; // Disponemos los vectores en filas
   TTransformTPuntoFunc = function(a: real; X:TPunto):TPunto;
  //CONSTANTES PÚBLICAS
  const
  //Cadena de texto para cambio de código de 3 letras a 1 y viceversa
  AA = 'ALA=A#ARG=R#ASN=N#ASP=D'
     + '#CYS=C#GLN=Q#GLU=E#GLY=G#HIS=H'
     + '#ILE=I#LEU=L#LYS=K#MET=M#PHE=F'
     + '#PRO=P#SER=S#THR=T#TRP=W#TYR=Y'
     + '#VAL=V';

  //PUBLICACIÓN DE FUNCIONES
  //
  //GEOMETRÍA
  //Operadores para trabajar con vectores
  Operator + (A, B : TPunto) : Tpunto;
  Operator - (A, B : TPunto) : Tpunto;
  Operator * (V: TPunto;k: real) :TPunto;
  Operator * (A, B: TPunto):real;
  Operator = (A, B: TPunto):boolean;
  //
  //Funciones para trabajar con vectores
  function modulo(A: TPunto): real;
  function angulo(A, B: TPunto): real;
  function prodVectorial(p1, p2: TPunto): TPunto;
  function distancia3D(a1, b1, c1, a2, b2, c2: real): real;
  function distancia3D(p1, p2: Tpunto): real; overload;
  function torsion ( A, B, C, D: TPunto): real;
  //Funciones formato
  function AA3TO1(aa3: string):char;
  function AA1TO3 (aa1:char):string ;
  function isEmbl(archivo: Tstrings): boolean;
  function isGenBank(archivo: Tstrings): boolean;
  function isPDB(archivo: Tstrings): boolean;
  function isUniProt(archivo: Tstrings): boolean;
  function leerSecuenciaProteina(archivo: Tstrings): AnsiSTring;
  // Funciones Módulo gráfico
  function PlotXY(datos: TTablaDatos;
                  im: TImage; OX: integer = 0;
                  OY: integer =1; borrar: boolean = False;//borrar gráfico anterior
                  linea: boolean = False; clpluma: TColor = clyellow;
                  clrelleno: TColor = clyellow;clFondo: TColor = clBlack;
                  radio: integer = 5;borde: integer = 40;
                  marcarFin : boolean = False ; clfin : TColor =clred): boolean;
  // Funciones Esterodiagrama
  function translacion(dx, dy, dz: real; V: Tpunto): Tpunto;
  function translacion(dx, dy, dz: real; datos: Tpuntos): Tpuntos; overload;
  function GiroOX(rad: real; V: Tpunto): Tpunto;
  function GiroOY(rad: real; V: Tpunto): Tpunto;
  function GiroOZ(rad: real; V: Tpunto): Tpunto;
  function GiroOX(rad: real; datos: Tpuntos): Tpuntos; overload;
  function GiroOY(rad: real; datos: Tpuntos): Tpuntos; overload;
  function GiroOZ(rad: real; datos: Tpuntos): Tpuntos; overload;
  function GiroOX(rad: real; var p: TPDB): integer; overload;
  function GiroOY(rad: real; var p: TPDB): integer; overload;
  function GiroOZ(rad: real; var p: TPDB): integer; overload;
  function AlinearZ(puntos: Tpuntos):TPuntos;
  function girarTpuntos(rad: real; datos:Tpuntos; funcion_girar:TTransformTPuntoFunc): Tpuntos;
  //Funciones archivos PDB
  function cargarPDB (var p: TPDB): string;
  function CargarPDB(texto: TStrings): TPDB;    overload;
  function WriteAtomPDB(atom: TAtomPDB): AnsiString;
implementation

//GEOMETRÍA
//Operadores para trabajar con vectores
Operator + (A, B : TPunto): Tpunto;
begin
  result.X:= A.X + B.X;
  result.Y:= A.Y + B.Y;
  result.Z:= A.Z + B.Z;
end;

Operator - (A, B : TPunto): Tpunto;
var
   V: TPunto;
begin
   V.X:= A.X - B.X;
   V.Y:= A.Y - B.Y;
   V.Z:= A.Z - B.Z;
   result:=V;
   end;

//Operador * entre real y vector para escalar vector
Operator * (V: TPunto; k: real):TPunto;
begin
  result.X:= k*V.X;
  result.Y:= k*V.Y;
  result.Z:= k*V.Z;
  end;
//Operador * entre vectores para producto escalar
Operator * (A, B: TPunto): real;
begin
 result:= A.X*B.X + A.Y*B.Y + A.Z*B.Z;
end;
Operator = (A, B: TPunto):boolean;
begin
 if (A.X = B.X) and (A.Y = B.Y) and (A.Z= B.Z) then
begin
  result:= True;
end
else
begin
  result:=False;
end;
end;
//
//GEOMÉTRICAS
function distancia3D(a1, b1, c1, a2, b2, c2: real): real;
         begin
           result :=sqrt(sqr(a1-a2)+sqr(b1-b2)+sqr(c1-c2));
           end;
function distancia3D(p1, p2: Tpunto): real; overload;
         begin
           result :=sqrt(sqr(p1.X-p2.X)+sqr(p1.Y-p2.Y)+sqr(p1.Z-p1.Z));
           end;
function modulo(A: TPunto): real;
begin
  result:= sqrt(sqr(A.X)+sqr(A.Y)+sqr(A.Z));
  end;
function angulo (A, B: Tpunto): real;
var
   denominador: real;
begin
  denominador:= modulo(A)*modulo(B);
if denominador > 0
 then  result:= arccos(A*B/denominador)
 else  result:=  maxfloat;
end;

function angulo(A, B, C: TPunto):real; overload;
var
   BA,BC: TPunto;
begin
   BA:= A-B;
   BC:= C-B;
   result:= angulo( BA, BC);
end;
function prodVectorial(p1,p2: TPunto): Tpunto;
var
   V:TPunto;
begin
   V.X:=p1.Y*p2.Z - p1.Z*p2.Y;
   V.Y:=P1.Z*P2.X - P1.X*P2.Z;
   V.Z:=p1.X*p2.Y - p1.Y*p2.X;
   result:= V;
   end;
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
//FUNCIONES FORMATO
function AA3TO1(aa3: string):char;
 begin
   result:= AA[pos(aa3,AA)+4];
   end;
 function AA1TO3 (aa1:char):string ;
 var index : integer;
 begin
 index:=pos(AA1, AA)-1;
 result:= copy(AA,index, 3);
 end;

//
// Funciones Módulo gráfico
function PlotXY(datos: TTablaDatos;
                im: TImage; OX: integer = 0;
                OY: integer =1; borrar: boolean = False;//borrar gráfico anterior
                linea: boolean = False; clpluma: TColor = clyellow;
                clrelleno: TColor = clyellow;clFondo: TColor = clBlack;
                radio: integer = 5;borde: integer = 40;
                marcarFin : boolean = False ; clfin : TColor =clred): boolean;
var
   j:integer;
   xp, yp: integer; //Describimos coordenadas centro elipse
   xmin, xmax, ymin, ymax, rangoX, rangoY: real;
   ancho, alto: integer;
   OK : boolean;
   function Xpixel (x:real): integer ;
    begin
         result:= round(((ancho-2*borde)*(x-xmin)/rangoX)+borde);
       end;

    function Ypixel (y:real):integer;
    begin
          result:= round(alto-(((alto-2*borde)*(y-ymin)/rangoY)+borde));
       end;

begin
     xmin := minValue(datos[OX]);
     ymin := minValue(datos[OY]);
     xmax := maxValue(datos[OX]);
     ymax:= maxValue(datos[OY]);
     rangoX:=xmax-xmin;
     rangoY:= ymax-ymin;
     if (rangoX=0) or (rangoY=0) then OK:= FALSE
     else
     begin
       ancho := im.Width;
       alto := im.Height;
       //ancho := 50;
       //alto := 50;

       //Borramos dibujo previo
       if borrar then
       begin
        im.Canvas.Brush.Color:= clfondo; //aseguramos color de fondo
        im.Canvas.clear;
       end;
       im.Canvas.Pen.Color:= clpluma;
       im.Canvas.brush.color:= clrelleno;
       // Movemos el punto inicial
       Xp := XPixel(datos[OX, 0]);
       Yp := YPixel(datos[OY, 0]);
       im.Canvas.MoveTo(Xp, Yp);

       for j:= 0 to high(datos[0]) do
       begin
         Xp := XPixel(datos[OX, j]);
         Yp := YPixel(datos[OY, j]);
         im.Canvas.Ellipse(Xp-radio, Yp-radio, Xp+radio, Yp+radio);
         if linea then im.Canvas.LineTo(Xp, Yp);
       end;
       OK:= true
       end;
         if marcarFin then
         begin
           im.Canvas.Pen.Color:= clfin;
           im.Canvas.brush.color:= clfin;
           im.Canvas.Ellipse(Xp-radio, Yp-radio, Xp+radio, Yp+radio);
         end;
     result := OK;
end;
//
// Funciones Esterodiagrama

function translacion(dx, dy, dz: real; V: Tpunto): Tpunto;
var
   S: Tpunto;
begin
   S.X:=V.X +dx;
   S.Y:=V.Y +dy;
   S.Z:=V.Z +dz;
   result:= s;
end;
function translacion(dx, dy, dz: real; datos: Tpuntos): Tpuntos; overload;
var
   s: Tpuntos;
   j: integer;
begin
   setLength(s, high(datos)+1);
   for j:=0 to high(datos) do
   begin
     s[j] := translacion(dx, dy, dz, datos[j]) ;
   end;
   result := s;
end;

function translacion(dx, dy, dz: real; var p: TPDB): integer; overload;
var
   j: integer;
begin
   for j:=1 to p.NumFichas do
   begin
       p.atm[j].coor.x := p.atm[j].coor.x + dx;
       p.atm[j].coor.y := p.atm[j].coor.y + dy;
       p.atm[j].coor.z := p.atm[j].coor.z + dz;
   end;
   result:=1;
end;

function GiroOX(rad: real; V: Tpunto): Tpunto;
var
   S: Tpunto;
   seno, coseno: real;
begin
   seno:= sin(rad);
   coseno:= cos(rad);

   S.X:= V.X;
   S.Y:= V.Y*coseno - V.Z*seno;
   S.Z:= V.Y*seno + V.Z*coseno;
   result:= S;
end;

function GiroOX(rad: real; datos: Tpuntos): Tpuntos; overload;
var
   s: Tpuntos;
   j: integer;
begin
   setLength(s, high(datos)+1);
   for j:=0 to high(datos) do
   begin
     s[j] := GiroOX(rad, datos[j]) ;
   end;
   result := s;
end;

function GiroOX(rad: real; var p: TPDB): integer; overload;
var
   j: integer;
   seno, coseno: real;
begin
   for j:=1 to p.NumFichas do
   begin
       seno:= sin(rad);
       coseno:= cos(rad);
       p.atm[j].coor.y := p.atm[j].coor.y*coseno-p.atm[j].coor.z*seno;
       p.atm[j].coor.z := p.atm[j].coor.z*coseno + p.atm[j].coor.y*seno;
   end;
   result:=1;
end;

function GiroOY(rad: real; V: Tpunto): Tpunto;
var
   S: Tpunto;
   seno, coseno: real;
begin
   seno:= sin(rad);
   coseno:= cos(rad);

   S.X:= V.X*coseno + V.Z*seno;
   S.Y:= V.Y;
   S.Z:= V.Z*coseno -V.X*seno;
   result:= S;
end;

function GiroOY(rad: real; datos: Tpuntos): Tpuntos; overload;
var
   s: Tpuntos;
   j: integer;
begin
   setLength(s, high(datos)+1);
   for j:=0 to high(datos) do
   begin
     s[j] := GiroOY(rad, datos[j]) ;
   end;
   result := s;
end;


function girarTpuntos(rad: real; datos:Tpuntos; funcion_girar:TTransformTPuntoFunc): Tpuntos;
var
   s: Tpuntos;
   j: integer;
begin
     setLength(s, high(datos)+1);
     for j:=0 to high(datos) do
     begin
     s[j] := funcion_girar(rad, datos[j]) ;
     end;
     result := s;
end;

function GiroOY(rad: real; var p: TPDB): integer; overload;
var
   j: integer;
   seno, coseno: real;
begin
     seno:= sin(rad);
   coseno:= cos(rad);
   for j:=1 to p.NumFichas do
   begin
       p.atm[j].coor.X:= p.atm[j].coor.X*coseno + p.atm[j].coor.Z*seno;
       p.atm[j].coor.z := p.atm[j].coor.z*coseno - p.atm[j].coor.Z*seno;
   end;
   result:=1;
end;

function GiroOZ(rad: real; V: Tpunto): Tpunto;
var
   S: Tpunto;
   seno, coseno: real;
begin
   seno:= sin(rad);
   coseno:= cos(rad);
   S.X:= V.X*coseno - V.Y*seno;
   S.Y:= V.X*seno +V.Y*coseno;
   S.Z:= V.Z;

   result:= S;
end;
function GiroOZ(rad: real; datos: Tpuntos): Tpuntos; overload;
var
   s: Tpuntos;
   j: integer;
begin
   setLength(s, high(datos)+1);
   for j:=0 to high(datos) do
   begin
     s[j] := GiroOZ(rad, datos[j]) ;
   end;
   result := s;
end;


function GiroOZ(rad: real; var p: TPDB): integer; overload;
var
   j: integer;
   seno, coseno: real;
begin
   seno:= sin(rad);
   coseno:= cos(rad);
   for j:=1 to p.NumFichas do
   begin
       p.atm[j].coor.X:= p.atm[j].coor.X*coseno - p.atm[j].coor.Y*seno;
       p.atm[j].coor.Y := p.atm[j].coor.Y*coseno + p.atm[j].coor.X*seno;
   end;
   result:=1;
end;

// Funciones Alinear Z

function AlinearZ(puntos: Tpuntos):TPuntos;
var
   S: TPuntos;
   p1, p2: TPunto;
   a, b, c, d1, d2: real;
   alpha, phi, senoAlpha, senoPhi: real;
begin
   setLength(S, high(puntos)+1);
   //Transladar
   p1:= puntos[0];
   S:= translacion(-p1.X, -p1.Y, -p1.Z, puntos);
   //Calcular ángulos
   a:= S[high(S)].X;
   b:= S[high(S)].Y;
   c:= S[high(S)].Z;
   d1:= sqrt(sqr(b)+sqr(c));
   d2:= sqrt(sqr(a)+sqr(b)+sqr(c));
   senoPhi:= b/d1;
   senoAlpha:= a/d2;
   phi:= arcsin(senoPhi);
   alpha:= arcsin(senoAlpha);
   if c<0 then phi:= -phi else alpha:= -alpha; //Cambio signo
   //Giro sobre OX y OY
   S:= GiroOX(phi, S);
   S:= GiroOY(alpha, S);
   result:= S;
end;

// Discriminar distintos formatos.

function isEmbl(archivo: Tstrings): boolean;
begin
   if (copy(archivo[0],0,2)='ID') and (copy(archivo[1],0,2)='XX')
   then result:= TRUE else result := FALSE;
end;
function isUniProt(archivo: Tstrings): boolean;
begin
   if (copy(archivo[0],0,2)='ID') and (copy(archivo[1],0,2)='AC')
   then result:= TRUE else result := FALSE;
end;
function isGenBank(archivo: Tstrings): boolean;
begin
   if (copy(archivo[0],0,5)='LOCUS')
   then result:= TRUE else result := FALSE;
end;
function isPDB(archivo: Tstrings): boolean;
begin
   if (copy(archivo[0],0,6)= 'HEADER')
   then result:= TRUE else result := FALSE;
end;

// Calcular RMSD
function matriz_distancias (puntos: TPuntos): Matriz2D;
var 
matriz : Matriz2D;
i, j, n : integer;
begin
 n:= high(puntos)+1;
 SetLength(matriz, n, n);
   for i:= 0 to n do  
      for j:= 0 to n do  
         matriz[i,j]:= distancia3D(puntos[i], puntos[j]); 
 result:= matriz;
end;

function RMSD(distancias1, distancias2 : Matriz2D): real;
var
sumatorio : real;
i, j, n : integer;
begin
if high(distancias1 = distancias2) then
begin
sumatorio := 0;
n:= high(residuo1)+1;
   for i:= 0 to n do
   begin  
      for j:= 0 to n do
      begin  
         sumatorio:= sumatorio + sqr(distancias1[i, j] - distancias2[i, j]); 
      end;   
   end;
result := sqrt(sumatorio/n);  
end;
else result:= Inf;
end;

function RMSD(residuos : ArrayMatrices): Matriz2D; overload;
var
n : integerM
matriz : Matriz2D;
begin
 n := high(residuos)+1;
 SetLength(matriz, n, n);
 for i:= 0 to n do  
      for j:= 0 to n do
           matriz[i,j]:= RMSD(residuos[i], residuos[j]);
result:= matriz;
end;

function leerSecuenciaProteina(archivo: Tstrings): AnsiString;
	var
	   j,i: integer;
	   sec, linea: String;
	   p: TPDB;
	   check: boolean;
	begin
	   if(isPDB(archivo))then
	     begin
	     p:= CargarPDB(archivo);
	     result:= p.secuencia;
	     ShowMessage('Se ha detectado un archivo en formato PDB');
	   end
	else if(isGenBank(archivo)) then
	   begin
	     sec:='';
	     for j:= 0 to archivo.count-1 do
	     begin
	       linea:= archivo[j];
	       if copy(linea,0,6) = 'ORIGIN' then Break;
	       sec:= sec+ trim(linea);
	       if copy(linea,22,13) = '/translation=' then sec:= copy(linea, 35, 70);
	     end;
	     result:= trim(copy(sec,2,sec.Length-2));
	     ShowMessage('Se ha detectado un archivo en formato GenBank');
	   end
	   else if(isUniProt(archivo)) then
	     begin
		for j:= 0 to archivo.count-1 do
		begin
		  linea:= archivo[j];
		  if copy(linea,0,2) = '//' then Break;
		  sec:= sec+ trim(linea);
		  if copy(linea,0,2) = 'SQ' then sec:= '';
		end;
		result:= trim(sec);
		ShowMessage('Se ha detectado un archivo en formato UniProt');
	      end
	   else if(isEmbl(archivo)) then
	     begin
	       sec:='';
	       check:= FALSE;
	       for j:= 0 to archivo.count-1 do
	       begin
		 linea:= archivo[j];
		 if (copy(linea,0,2) = 'XX') AND check then Break;
		 sec:= sec+ trim(copy(linea, 22, 58));
		 if copy(linea,22,13) = '/translation=' then
		 begin
		   check:= TRUE;
		   sec:= copy(linea, 35, 39);
		   end;
	       end;
	       result:= trim(copy(sec,2,sec.Length-2));
	       ShowMessage('Se ha detectado un archivo en formato EMBL');
		 end

	   else
	      ShowMessage('Es un formato no soportado');
	end;

//
// Funciones trabajar PDB

  function cargarPDB (var p: TPDB): string;
  var
     dialogo: TOpenDialog;
     textoPDB: TStrings;
  begin
     dialogo:= TOpenDialog.create(application);
     textoPDB:= TStringlist.create;

     if dialogo.execute then
     begin
      textoPDB.loadfromfile(dialogo.filename);
      if isPDB(textoPDB) = False then ShowMessage('No es un archivo PDB');
      p:=cargarPDB(textoPDB);
      result:= dialogo.filename;

     end
     else result:= '';

     dialogo.free;
     textoPDB.free;
  end;
function CargarPDB(texto: TStrings): TPDB;    overload;
  var
    p: TPDB;
    linea: string;
    j, k, F, R, S: integer;
    resno: integer;

  begin
    if isPDB(texto) = False then ShowMessage('No es un archivo PDB');
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
  end;

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
    //ShowMessage(linea);
    result:= linea;
end;

end.


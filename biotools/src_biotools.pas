unit src_biotools;

{$mode objfpc}{$H+}

interface


uses
  Classes, SysUtils, Math, Graphics, Dialogs, Forms, Controls, Menus, StdCtrls,
  ExtCtrls, ColorBox;  // Cargar librerías
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

  TTabladatos = array of array of real; // Disponemos los vectores en filas

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
  // Funciones Módulo gráfico
  function PlotXY(datos: TTablaDatos;
                im: TImage; OX: integer = 0;
                OY: integer =1; borrar: boolean = False;//borrar gráfico anterior
                linea: boolean = False; clpluma: TColor = clyellow;
                clrelleno: TColor = clyellow;clFondo: TColor = clBlack;
                radio: integer = 5;borde: integer = 40): boolean;
  // Funciones Esterodiagrama
  function translacion(dx, dy, dz: real; V: Tpunto): Tpunto;
  function translacion(dx, dy, dz: real; datos: Tpuntos): Tpuntos; overload;
  //Funciones archivos PDB
  function cargarPDB (var p: TPDB): string;
  function CargarPDB(texto: TStrings): TPDB;    overload;
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
                radio: integer = 5;borde: integer = 40): boolean;
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
      p:=cargarPDB(textoPDB);
      result:= dialogo.filename;

     end else result:= '';

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

end.

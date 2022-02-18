# Estereodiagrama

En la realización de este cuaderno de actividades se pide el desarrollo de una aplicación capaz de crear un estereodiagrama correspondiente a un fragmento definido de la proteína.  El código correspondiente a las funciones utilizadas para transformar (traslación y giro) las coordenadas se encuentran en la librería [biotools/src_biotools](https://github.com/currocam/biotools_hQC/blob/master/biotools/src_biotools.pas) y la aplicación bajo el nombre de [estereodiagrama](https://github.com/currocam/biotools_hQC/blob/master/biotools/estereodiagrama). Este apartado se corresponde a la 7ª actividad de la relación de ejercicios.

## Funciones de transformación
En primer lugar, se desarrollaron una serie de funciones capaces de realizar transformaciones a un conjunto de coordenadas. Estas funciones, que fueron desarrolladas en clase, tienen distintas versiones sobrecargadas de manera que se pueda trasladar tanto un punto en el espacio, `TPunto`,  un array dinámico `TPuntos` como un `TPDB`. En los siguientes bloques de código se ha escogido una serie de funciones representativas de este pequeño módulo 'espacial' desarrollado.

=== "translacion() con TPunto"

	```pascal linenums="1"
    function translacion(dx, dy, dz: real; V: Tpunto): Tpunto;
    var
       S: Tpunto;
    begin
       S.X:=V.X +dx;
       S.Y:=V.Y +dy;
       S.Z:=V.Z +dz;
       result:= s;
    end;
	```
=== "translacion() con TPuntos"
	```pascal linenums="1"
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
	```
=== "translacion() con TPDB"
	```pascal linenums="1"
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
	```
Además, se realizó la siguiente propuesta de mejora al código desarrollado en clase para las funciones relativas a girar. Se trató de implementar una función más genérica llamada `girarTpuntos` que recibiera tanto un ángulo en radianes como una función capaz de girar un vector en el espacio y que aplicara dicha transformación a todos los vectores del array. Esta idea podría ser implementada de forma más genérica con una función que recibiese como argumentos el array a transformar y una función transformadora y se obtendría así un código más claro y conciso.  Para escribir esta función fue necesario definir una nueva clase llamada `TTransformTPuntoFunc`.

=== "Type TTransformTPuntoFunc"

	```pascal linenums="1"
    TTransformTPuntoFunc = function(a: real; X:TPunto):TPunto;
	```
=== "girarTpuntos()"

	```pascal linenums="1"
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
	```
=== "GiroOZ() con TPunto"
	```pascal linenums="1"
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
	```
=== "GiroOX() con TPuntos"

	```pascal linenums="1"
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
	```
=== "GiroOY() con TPuntos"
	```pascal linenums="1"
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
	```
## Estereodiagrama

Un estereodiagrama es un tipo de representación donde se muestra una imagen en dos dimensiones rotada en torno al eje Y (por defecto) un pequeño ángulo (por defecto 5º) de manera que al visualizarse las dos una al lado de la otra se aprecie cierta tridimensionalidad.

### Aplicación Free Pascal/Lazarus

Nuestra aplicación deberá tener, entonces, una interfaz que permita al usuario seleccionar un fragmento de la proteína y un ángulo de giro, así como dos paneles donde se muestre gráficamente el resultado de la transformación. Además de esto, se han llevado a cabo las siguientes mejoras sobre lo realizado en clase:

1. Opción para modificar el archivo en memoria y poder realizar así transformaciones sucesivas.
2. Opción para elegir el eje sobre el se va a girar.
3. Restricción del rango de residuos que el usuario puede elegir como fragmento según la subunidad elegida. De esta manera, cuando el archivo PDB se carga en el programa, se actualiza el rango de subunidades que se pueden elegir y, cada vez que el usuario elige una nueva subunidad, los valores mínimos y máximos de las casillas de primer y último residuo se actualizan para que solo sean elegibles residuos de dicha subunidad.

A continuación, mostramos el procedimiento empleado para transformar las coordenadas espaciales iniciales según las indicaciones del usuario (o las coordenadas espaciales resultantes de la última transformación, si así se indica). Hemos incluido este procedimiento porque nos parecía relevante mostrar un ejemplo de uso de la función `girarTpuntos()` puesto que tiene ciertas peculiaridades en la sintaxis y para poder mostrar la solución que encontramos para evitar posibles errores que podían surgir al realizar transformaciones en memoria (si se cambiaba el array de `TPuntos` inicial o si todavía no se había definido).

=== "Procedure transformación"

	```pascal linenums="1"
    Tprocedure TForm1.Button2Click(Sender: TObject);
    var
      CA1, CAn, subunidad, j: integer;
      transfunct : TTransformTPuntoFunc;
    begin
       Image1.Canvas.Clear;
       Image2.Canvas.Clear;
       CA1:= SpinEdit3.Value;
       CAn:= SpinEdit4.Value;

       setLength(V_CAInicial, CAn-CA1+1);
       setLength(V_CATrans, CAn-CA1+1);
       setLength(datos, 3, CAn-CA1+1);    //3 coordenadas

       for j:= CA1 to CAn do V_CAInicial[j-CA1]:=p.atm[p.res[j].CA].coor;
       for j:= 0 to high(V_CAInicial) do
       begin
         datos[0,j]:=  V_CAInicial[j].X;
         datos[1,j]:=  V_CAInicial[j].Y;
         datos[2,j]:=  V_CAInicial[j].Z;
       end;
       plotXY(datos, image1, SpinEdit6.Value, SpinEdit7.Value, TRUE, TRUE);

       if(SpinEdit1.Value = 0)then transfunct :=@GiroOX
      else if(SpinEdit1.Value = 1)then transfunct :=@GiroOY
      else if(SpinEdit1.Value = 2)then transfunct :=@GiroOZ;


    if CheckBox1.Checked and (high(V_CAInicial) = high(V_CATrans)) then      
       V_CATrans:= girarTpuntos(SpinEdit2.Value*pi/180,V_CATrans,  transfunct)
       else  V_CATrans:= girarTpuntos(SpinEdit2.Value*pi/180,V_CAInicial,  transfunct);
       for j:= 0 to high(V_CATrans) do
       begin
         datos[0,j]:=  V_CATrans[j].X;
         datos[1,j]:=  V_CATrans[j].Y;
         datos[2,j]:=  V_CATrans[j].Z;
       end;
       plotXY(datos, image2, SpinEdit6.Value, SpinEdit7.Value, TRUE, TRUE);
    end
	```

### Demostración de uso

A continuación, mostramos el funcionamiento del programa con un fragmento de un $\alpha$ hélice.

|![Interfaz gráfica para el programa Ramachandran](images/estereodiagrama.gif)|
|:-----------------------------------------------------------------------------:|
| Figura 1. Animación del programa `Estereodiagrama` mostrando su uso.|

### Verificación con función de visión estereoscópica

Por último, y para comprobar que nuestra representación es correcta, vamos a comparar el estereodiagrama obtenido con la herramienta de visión estereoscópica del programa PyMol. Mostramos a continuación el estereodiagrama realizado con nuestra aplicación Free Pascal/Lazarus:

|![estereodiagrama.png](images/estereodiagrama.png)|
|:-----------------------------------------------------------------------------:|
| Figura 2. Estereodiagrama de los residuos 35 a 43 de la proteína 2AFM, mostrando las coordenadas $y$ frente a $x$. La imagen de la derecha ha sido rotada 5º en el eje de la $y$ respecto a la imagen de la izquierda.|

Por último, se muestran en la siguiente imagen una representación de la cadena principal (es decir, solo de los $\text{C}_\alpha$) de los mismos residuos, 35 a 43, de la proteína 2AFM después de aplicar la opción `stereo crosseye` del programa PyMol. Como se puede observar, aunque la orientación no es exactamente la misma, se puede apreciar cómo nuestro estereodiagrama se encuentra bien construido.

|![estereodiagrama_pymol.png](images/estereodiagrama_pymol.png)|
|:-----------------------------------------------------------------------------:|
| Figura 3. Estereodiagrama realizado usando el programa PyMol para comprobar la calidad de nuestro estereodiagrama..|

unit test_biotools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  testing_biotools= class(TTestCase)
  published
  procedure OperadoresGeometricos;
    procedure FuncionesGeometricas;
    procedure CodigosAA;
  end;

implementation

procedure testing_biotools.CodigosAA;

begin
  IF AA3TO1('ALA') <> 'A' THEN
     Fail('Fail AA3To1');
  IF AA1TO3('R') <> 'ARG' THEN
     Fail(AA1TO3('R'));
  IF AA1TO3('A') <> 'ALA' THEN
     Fail(AA1TO3('A'));

end;
procedure testing_biotools.FuncionesGeometricas;
var
  V1, V2, V3, V4: TPunto;
begin
  V1.X:= 0;V1.Y:= 0;V1.Z:= 1;
  V2.X:= 2;V2.Y:= -3;V2.Z:= 1;
  V3.X:= -3;V3.Y:= 1;V3.Z:= 2;
  V4.X:= -7;V4.Y:= -7;V4.Z:= -7;
  IF modulo(V1) <> 1 THEN
     Fail('Fail modulo()');
  IF distancia3D(V1, V1) <> 0 THEN
     Fail('Fail distancia3d()');
  //IF angulo(V1, V3)*180/pi <> 180 THEN
  //   Fail('Fail angulo()');
    IF prodVectorial(V2, V3) <> V4 THEN
     Fail('Fail prodVectorial()');
end;
procedure testing_biotools.OperadoresGeometricos;
var
  V1, V2, V1Plus2, V1Minus2, V1PorV2: TPunto;
begin
  V1.X:= 1;V1.Y:= 1;V1.Z:= 1;
  V2.X:= 2;V2.Y:= 2;V2.Z:= -2;
  V1Plus2.X:= 3;V1Plus2.Y:= 3;V1Plus2.Z:= -1;
  V1Minus2.X:= -1;V1Minus2.Y:= -1;V1Minus2.Z:= 3;
  IF V1+V2 <> V1Plus2 THEN
     Fail('Fail + operator');
  IF V1-V2 <> V1Minus2 THEN
     Fail('Fail - operator');
  IF V1*1 <> V1 THEN
     Fail('Fail * vector escalar');
  IF V1*V2 <> 2 THEN
     Fail('Fail * vector vector');
end;



initialization

  RegisterTest(testing_biotools);
end.


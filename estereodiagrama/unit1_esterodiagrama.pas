unit unit1_esterodiagrama;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin, ValEdit, SpinEx, src_biotools;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    Image1: TImage;
    Image2: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    SpinEdit4: TSpinEdit;
    SpinEdit5: TSpinEdit;
    SpinEdit6: TSpinEdit;
    SpinEdit7: TSpinEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;
  p: TPDB;
  V_CAInicial, V_CATrans: TPuntos;
  datos: TTablaDatos;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenDialog1.execute then
  begin
    Memo1.Lines.LoadFromFile(OpenDialog1.FileName);
    p:= cargarPDB(Memo1.Lines);
    SpinEdit5.MaxValue:= p.NumSubunidades;
    SpinEdit5.Value:= 1; //Subunidad por defecto
    SpinEdit3.Value:= p.sub[SpinEdit5.Value].res1;
    SpinEdit4.Value:= p.sub[SpinEdit5.Value].res1+1;
    SpinEdit3.MinValue:= p.sub[SpinEdit5.Value].res1;
    SpinEdit3.MaxValue:= p.sub[SpinEdit5.Value].resN-1;
    SpinEdit4.MinValue:= p.sub[SpinEdit5.Value].res1+1;
    SpinEdit4.MaxValue:= p.sub[SpinEdit5.Value].resN;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  CA1, CAn, subunidad, j: integer;
  transfunct : TTransformTPuntoFunc;
begin
   Image1.Canvas.Clear;
   Image2.Canvas.Clear;
   CA1:= SpinEdit3.Value;
   CAn:= SpinEdit4.Value;
   //Ajustamos tamaño arrays dinámicas
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
   //ShowMessage(FloatToStr(datos[0,1])+FloatToStr(datos[1,1])+FloatToStr(datos[2,1]));
   if(SpinEdit1.Value = 0)then transfunct :=@GiroOX
  else if(SpinEdit1.Value = 1)then transfunct :=@GiroOY
  else if(SpinEdit1.Value = 2)then transfunct :=@GiroOZ;


  if CheckBox1.Checked and (high(V_CAInicial) = high(V_CATrans))then
   //Si se ha marcado como que se quieren guardar los cambios temporalmente
   V_CATrans:= girarTpuntos(SpinEdit2.Value*pi/180,V_CATrans,  transfunct)
   else  V_CATrans:= girarTpuntos(SpinEdit2.Value*pi/180,V_CAInicial,  transfunct);
   for j:= 0 to high(V_CATrans) do
   begin
     datos[0,j]:=  V_CATrans[j].X;
     datos[1,j]:=  V_CATrans[j].Y;
     datos[2,j]:=  V_CATrans[j].Z;
   end;
   //ShowMessage(FloatToStr(datos[0,1])+FloatToStr(datos[1,1])+FloatToStr(datos[2,1]));
   plotXY(datos, image2, SpinEdit6.Value, SpinEdit7.Value, TRUE, TRUE);

end;

end.


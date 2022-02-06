unit unit1_alinear_Z;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Spin, ExtDlgs, src_biotools;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Edit1: TEdit;
    Image1: TImage;
    Image2: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    SavePictureDialog1: TSavePictureDialog;
    SavePictureDialog2: TSavePictureDialog;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SpinEdit3Click(Sender: TObject);
    procedure SpinEdit3EditingDone(Sender: TObject);
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

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenDialog1.execute then
  begin
    edit1.text:= extractfilename(Opendialog1.FileName);
    Memo1.Lines.LoadFromFile(OpenDialog1.FileName);
    p:= cargarPDB(Memo1.Lines);
    SpinEdit3.MaxValue:= p.NumSubunidades;
    SpinEdit3.Value:= 1;
    SpinEdit1.Value:= p.sub[SpinEdit3.Value].res1;
    SpinEdit2.Value:= p.sub[SpinEdit3.Value].res1+1;
    SpinEdit1.MinValue:= p.sub[SpinEdit3.Value].res1;
    SpinEdit1.MaxValue:= p.sub[SpinEdit3.Value].resN;
    SpinEdit2.MinValue:= p.sub[SpinEdit3.Value].res1+1;
    SpinEdit2.MaxValue:= p.sub[SpinEdit3.Value].resN;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  CA1, CAn, subunidad, j: integer;
begin
   CA1:= SpinEdit1.Value;
   CAn:= SpinEdit2.Value;
   subunidad:= SpinEdit3.Value;
   if not (CA1 < CAn) then begin
      ShowMessage('El valor de Ca inicial debe de ser menor que el Ca final');
      exit;
   end;
   setLength(V_CAInicial, CAn-CA1+1);
   setLength(V_CATrans, CAn-CA1+1);
   setLength(datos, 2, CAn-CA1+1);
   for j:= CA1 to CAn do V_CAInicial[j-CA1]:=p.atm[p.res[j].CA].coor;
   for j:= 0 to high(V_CAInicial) do
   begin
     datos[0,j]:=  V_CAInicial[j].X;
     datos[1,j]:=  V_CAInicial[j].Y;
   end;
   plotXY(datos, image1, 0, 1, TRUE, TRUE);
   V_CATrans:= alinearZ(V_CAInicial);
   for j:= 0 to high(V_CATrans) do
   begin
     datos[0,j]:=  V_CATrans[j].X;
     datos[1,j]:=  V_CATrans[j].Y;
   end;
   plotXY(datos, image2, 0, 1, TRUE, TRUE,
                 clyellow, clyellow, clblack, 3, 40, true)
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if SavepictureDialog1.Execute then
    image2.Picture.SaveToFile( SavepictureDialog1.Filename );
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  if SavepictureDialog2.Execute then
    image1.Picture.SaveToFile( SavepictureDialog1.Filename );
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
   Image1.Canvas.Clear;
   Image2.Canvas.Clear
end;

procedure TForm1.SpinEdit3Click(Sender: TObject);
begin
   SpinEdit1.Value:= p.sub[SpinEdit3.Value].res1;
   SpinEdit2.Value:= p.sub[SpinEdit3.Value].res1+1;
   SpinEdit1.MinValue:= p.sub[SpinEdit3.Value].res1;
   SpinEdit1.MaxValue:= p.sub[SpinEdit3.Value].resN;
   SpinEdit2.MinValue:= p.sub[SpinEdit3.Value].res1+1;
   SpinEdit2.MaxValue:= p.sub[SpinEdit3.Value].resN;
end;

procedure TForm1.SpinEdit3EditingDone(Sender: TObject);
begin

end;

end.


unit unit1_alinear_Z;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Spin, src_biotools;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Image1: TImage;
    Image2: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;
  p: TPBD;
  V_CAInicial, V_CATrans: TPuntos;

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
    SpinEdit1.MaxValue:= p.NumResiduos-1;
    SpinEdit2.MaxValue:= p.NumResiduos;
    SpinEdit3.MaxValue:= p.NumSubunidades;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  CA1, CAn, subunidad: integer;
begin
   CA1:= SpinEdit1.Value;
   CAn:= SpinEdit2.Value;
   subunidad:= SpinEdit3.Value;
   if not CA1 < CAn then ShowMessage('El valor de Ca inicial debe de ser menor que el Ca final');
   setLength(V_CAInicial, CAn-CA1+1);
   setLength(V_CATrans, CAn-CA1+1);
   for j:= CA1 to CAn do V_CAInicial[j]:=p.sub[subunidad]
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
   Image1.Canvas.Clear;
   Image2.Canvas.Clear
end;

end.


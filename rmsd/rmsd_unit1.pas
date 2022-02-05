unit rmsd_unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  src_biotools, StrUtils;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    OpenDialog1: TOpenDialog;
    SpinEdit1: TSpinEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;
  p: TPDB;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button2Click(Sender: TObject);
begin
  if OpenDialog1.execute then
  begin
    Memo1.Lines.Clear;
    Memo1.Lines.LoadFromFile(OpenDialog1.FileName);
    p:= cargarPDB(Memo1.Lines);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  aa : string;
  n_res, counter, j, k, index : integer;
  res : TPuntos;
  arrayDist : ArrayMatrices;
  resultMatrix : Matriz2D;
  strMatrix: string;
begin
  Memo2.Lines.Clear;
  aa := Edit1.Text;
  if not Length(aa) = 3 then ShowMessage('Por favor, use el código de tres letras');
  n_res := SpinEdit1.Value;
  counter := 0;
  setLength(arrayDist, n_res);
  setLength(resultMatrix, n_res, n_res);
  for j:=0 to p.NumResiduos-1 do
  begin
   if counter=n_res then break;
   if (p.res[j].ID3=aa) then
   begin
     setLength(res, p.res[j].AtmN-p.res[j].atm1);
     for k := p.res[j].atm1 to p.res[j].AtmN do
     begin
       index := k - p.res[j].atm1;
       res[index]:= p.atm[k].coor;
       end;
     arrayDist[counter]:= distan_matriz(res);
     Finalize(res);
     counter := counter +1;
     end;
  end;
  for j:= 0 to n_res-1 do
  begin
   for k:= 0 to n_res-1 do
   begin
   resultMatrix[j, k]:= RMSD_distan(arrayDist[j], arrayDist[k]);
   memo2.lines.add('RMSD entre '+ inttostr(j) +' y '+ inttostr(k)+': '+ formatfloat('0.000',resultMatrix[j, k]));
   end;
  end;
  for j:= 0 to n_res-1 do
  begin
   strMatrix := '';
   for k:= 0 to n_res-1 do
   begin
   strMatrix:= strMatrix+formatfloat('0.000',resultMatrix[j, k])+'    ';
   end;
   memo2.lines.add('|'+strMatrix+'|');
  end;
end;
end.


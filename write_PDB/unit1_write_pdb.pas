unit unit1_write_PDB;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  src_biotools;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    Memo2: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1; p:TPDB;

implementation

{$R *.lfm}

{ TForm1 }


procedure TForm1.Button1Click(Sender: TObject);
var
  j: integer;
begin
  memo2.clear;
  memo2.visible:= false;
  for j:= 1 to p.NumFichas do //Se itera a lo largo del la estructura TPDB
      begin
              if p.atm[j].ID= Edit1.Text then //Se compara con el identificador del usuario
                 memo2.Lines.Add(WriteAtomPDB(p.atm[j]));
      end;
  memo2.Visible:= true;
end;


procedure TForm1.Button2Click(Sender: TObject);
begin
    if Opendialog1.execute then
  begin
    memo1.clear;
    memo1.lines.loadfromfile(OpenDialog1.FileName);
    p:= CargarPDB(memo1.lines);
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
   begin
   if SaveDialog1.Execute then
    Memo2.Lines.SaveToFile(SaveDialog1.Filename );
 end;
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin

end;

procedure TForm1.Memo1Change(Sender: TObject);
begin

end;

procedure TForm1.MenuItem2Click(Sender: TObject);
begin

end;

end.


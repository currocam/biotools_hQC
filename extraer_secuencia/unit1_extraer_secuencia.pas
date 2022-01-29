unit unit1_extraer_secuencia;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, src_biotools;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    Memo2: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    OpenDialog1: TOpenDialog;
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1; documento: TStrings; p:TPDB;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.MenuItem1Click(Sender: TObject);
begin

end;

procedure TForm1.MenuItem2Click(Sender: TObject);
begin
  if Opendialog1.execute then
  begin
    memo1.clear;
    memo1.lines.loadfromfile(OpenDialog1.FileName);
    edit1.Caption:=OpenDialog1.FileName;
    documento:= memo1.lines;
  end;
end;

procedure TForm1.MenuItem6Click(Sender: TObject);
begin
  memo2.clear;
  memo2.Lines.text:= trim(leerSecuenciaProteina(documento));
end;

end.


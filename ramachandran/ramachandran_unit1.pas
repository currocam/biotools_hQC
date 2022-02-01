unit ramachandran_unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls,
  ExtCtrls, ColorBox, ExtDlgs, TAGraph, TASeries, src_biotools, strutils;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    CheckBox1: TCheckBox;
    ColorBox1: TColorBox;
    ColorBox2: TColorBox;
    ColorBox3: TColorBox;
    ColorListBox1: TColorListBox;
    Edit1: TEdit;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    Memo2: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    OpenDialog1: TOpenDialog;
    SavePictureDialog1: TSavePictureDialog;
    SavePictureDialog2: TSavePictureDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Label1Click(Sender: TObject);

    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;
  p: TPDB;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.MenuItem5Click(Sender: TObject);
begin
    if opendialog1.execute then
  begin
     memo1.clear;
     memo1.lines.loadfromfile(opendialog1.FileName);
     edit1.text:=opendialog1.FileName;
     edit1.text:= extractFileName(opendialog1.filename);
     edit1.Visible:= True;
     Button3.Visible:= True;
     memo1.Visible:=True;

     p:= cargarPDB(memo1.lines);
end;
end;

procedure TForm1.MenuItem6Click(Sender: TObject);
begin
  memo1.clear;
  memo2.clear;
end;


procedure TForm1.Button1Click(Sender: TObject);
begin
  memo1.visible:=True;
  Button1.Visible:=False;
  Button2.Visible:=True;

end;

procedure TForm1.Button2Click(Sender: TObject);
begin
memo1.visible:=False;
Button1.Visible:=True;
Button2.Visible:=False;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  j, k, counter_col: integer;
  //linea: string;
  datos : TTabladatos;
  borrar: boolean;
begin
    memo2.clear;
  for j:= 1 to p.NumSubunidades do
  begin
     counter_col:= -1;
     borrar:= CheckBox1.Checked;
     setLength(datos, 2,  p.sub[j].resCount -2);
     memo2.visible:=false;
     Image1.Visible:=false;
     if borrar then Chart1LineSeries1.Clear();
     for k:=p.sub[j].res1+1 to p.sub[j].resn-1 do
     begin
        counter_col:= counter_col +1;
        memo2.lines.add(padright(p.res[k].ID3 + inttostr(p.res[k].NumRes) + p.res[k].subunidad, 10)
                       + padleft(formatfloat('#.##',p.res[k].phi*180/pi),10)
                       + padleft(formatfloat('#.##',p.res[k].psi*180/pi),10));

        datos[0, counter_col]:= p.res[k].phi*180/pi;
        datos[1, counter_col]:= p.res[k].psi*180/pi;
        // Añadimos a gráfico con ejes
        Chart1LineSeries1.AddXY(datos[0, counter_col], datos[1, counter_col],'', colorBox2.Selected);
     end;
     plotXY(datos, Image1,
                   0,        //OX
                   1,        //OY
                   borrar,    //borrar
                   false,    //linea
                   colorBox1.Selected,   //clpluma
                   colorBox2.Selected,   //clrelleno
                   colorBox3.Selected);  //Tcolor
     memo2.visible:=true;
     Image1.Visible:=true;
     Chart1.BottomAxis.Visible:= TRUE;
     Chart1.BottomAxis.Visible:= TRUE;
     Chart1LineSeries1.ShowPoints:=TRUE;
     Chart1LineSeries1.LineType:=ltNone;
end;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  if SavepictureDialog1.Execute then
    image1.Picture.SaveToFile( SavepictureDialog1.Filename );
end;

procedure TForm1.Button6Click(Sender: TObject);
var
  jpg: TRasterImage;
begin
  jpg := Chart1.SaveToImage(TJpegImage);
  try
    TJPegImage(jpg).CompressionQuality:=100;
    if SavepictureDialog2.Execute then
    jpg.SaveToFile(SavepictureDialog2.Filename);
  finally
    jpg.Free;
  end;
end;

procedure TForm1.CheckBox2Change(Sender: TObject);
begin

end;



procedure TForm1.FormCreate(Sender: TObject);
begin
   Canvas.Clear;
end;

procedure TForm1.Label1Click(Sender: TObject);
begin

end;

end.


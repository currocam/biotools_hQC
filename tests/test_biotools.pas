unit test_biotools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, src_biotools;

type

  testing_biotools= class(TTestCase)
  published
  procedure OperadoresGeometricos;
    procedure FuncionesGeometricas;
    procedure CodigosAA;
    procedure Embl;
    procedure GenBank;
    procedure PDB;
    procedure UniProt;
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
procedure testing_biotools.Embl;
var
  SL_embl: TStringList;
  SL_pdb: TStringList;
  SL_UniProt: TStringList;
  SL_GenBank: TStringList;
  sec1, sec2: String;
begin
  SL_embl := TStringList.Create;
  SL_pdb := TStringList.Create;
  SL_UniProt:= TStringList.Create;
  SL_GenBank := TStringList.Create;
  SL_embl.LoadFromFile('text_embl.txt');
  SL_pdb.LoadFromFile('text_PDB.txt');
  SL_UniProt.LoadFromFile('text_UniProt.txt');
  SL_GenBank.LoadFromFile('text_GenBank.txt');
  IF not isEmbl(SL_embl) THEN Fail('No se ha detectado correctamente el archivo formato EMBL');
  IF isEmbl(SL_pdb) THEN Fail('Se ha detectado erróneamente el formato EMBL con un pdb');
  IF isEmbl(SL_UniProt) THEN Fail('Se ha detectado erróneamente el formato EMBL con un UniProt');
  IF isEmbl(SL_GenBank) THEN Fail('Se ha detectado erróneamente el formato EMBL con un GenBank');
  sec1 := 'MDFIVAIFALFVISSFTITSTNAVEASTLLDIGNLSRS' +
         'SFPRGFIFGAGSSAYQFEGAVNEGGRGPSIWDTFTHKYPEKIRDGSNADITV' +
         'DQYHRYKEDVGIMKDQNMDSYRFSISWPRILPKGKLSGGINHEGIKYYNNLI' +
         'NELLANGIQPFVTLFHWDLPQVLEDEYGGFLNSGVINDFRDYTDLCFKEFGD' +
         'RVRYWSTLNEPWVFSNSGYALGTNAPGRCSASNVAKPGDSGTGPYIVTHNQI' +
         'LAHAEAVHVYKTKYQAYQKGKIGITLVSNWLMPLDDNSIPDIKAAERSLDFQ' +
         'FGLFMEQLTTGDYSKSMRRIVKNRLPKFSKFESSLVNGSFDFIGINYYSSSY' +
         'ISNAPSHGNAKPSYSTNPMTNISFEKHGIPLGPRAASIWIYVYPYMFIQEDF' +
         'EIFCYILKINITILQFSITENGMNEFNDATLPVEEALLNTYRIDYYYRHLYY' +
         'IRSAIRAGSNVKGFYAWSFLDCNEWFAGFTVRFGLNFVD';
  sec2 := leerSecuenciaProteina(SL_embl);
  IF not (sec2 = sec1) THEN Fail('No se ha leído la secuencia correctamente');
end;

procedure testing_biotools.PDB;
var
  SL_embl: TStringList;
  SL_pdb: TStringList;
  SL_UniProt: TStringList;
  SL_GenBank: TStringList;
begin
  SL_embl := TStringList.Create;
  SL_pdb := TStringList.Create;
  SL_UniProt:= TStringList.Create;
  SL_GenBank := TStringList.Create;
  SL_embl.LoadFromFile('text_embl.txt');
  SL_pdb.LoadFromFile('text_PDB.txt');
  SL_UniProt.LoadFromFile('text_UniProt.txt');
  SL_GenBank.LoadFromFile('text_GenBank.txt');
  IF isPDB(SL_embl) THEN Fail('Se ha detectado erróneamente el formato PDB con un embl');
  IF not isPDB(SL_pdb) THEN Fail('No se ha detectado el formato PDB con un pdb');
  IF isPDB(SL_UniProt) THEN Fail('Se ha detectado erróneamente el formato PDB con un uniprot');
  IF isPDB(SL_GenBank) THEN Fail('Se ha detectado erróneamente el formato PDB con un GenBank');
end;

procedure testing_biotools.UniProt;
var
  SL_embl: TStringList;
  SL_pdb: TStringList;
  SL_UniProt: TStringList;
  SL_GenBank: TStringList;
  sec1, sec2: String;
begin
  SL_embl := TStringList.Create;
  SL_pdb := TStringList.Create;
  SL_UniProt:= TStringList.Create;
  SL_GenBank := TStringList.Create;

  SL_embl.LoadFromFile('text_embl.txt');
  SL_pdb.LoadFromFile('text_PDB.txt');
  SL_UniProt.LoadFromFile('text_UniProt.txt');
  SL_GenBank.LoadFromFile('text_GenBank.txt');
  IF isUniProt(SL_embl) THEN Fail('Se ha detectado erróneamente el formato UniProt');
  IF isUniProt(SL_pdb) THEN Fail('Se ha detectado erróneamente el formato UniProt');
  IF NOT isUniProt(SL_UniProt) THEN Fail('No se ha detectado el formato UniProt');
  IF isUniProt(SL_GenBank) THEN Fail('Se ha detectado erróneamente el formato UniProt');
  sec1 := 'MSTESMIRDV ELAEEALPKK TGGPQGSRRC LFLSLFSFLI VAGATTLFCL LHFGVIGPQR' +
          'EEFPRDLSLI SPLAQAVRSS SRTPSDKPVA HVVANPQAEG QLQWLNRRAN ALLANGVELR' +
          'DNQLVVPSEG LYLIYSQVLF KGQGCPSTHV LLTHTISRIA VSYQTKVNLL SAIKSPCQRE' +
          'TPEGAEAKPW YEPIYLGGVF QLEKGDRLSA EINRPDYLDF AESGQVYFGI IAL';
  sec2 := leerSecuenciaProteina(SL_UniProt);
  IF not (sec2 = sec1) THEN Fail('No se ha leído la secuencia correctamente');
  end;

procedure testing_biotools.GenBank;
var
  SL_embl: TStringList;
  SL_pdb: TStringList;
  SL_UniProt: TStringList;
  SL_GenBank: TStringList;
  sec1, sec2: String;
begin
  SL_embl := TStringList.Create;
  SL_pdb := TStringList.Create;
  SL_UniProt:= TStringList.Create;
  SL_GenBank := TStringList.Create;
  SL_embl.LoadFromFile('text_embl.txt');
  SL_pdb.LoadFromFile('text_PDB.txt');
  SL_UniProt.LoadFromFile('text_UniProt.txt');
  SL_GenBank.LoadFromFile('text_GenBank.txt');
  IF isGenBank(SL_embl) THEN Fail('Se ha detectado erróneamente el formato GenBank');
  IF isGenBank(SL_pdb) THEN Fail('Se ha detectado erróneamente el formato GenBank');
  IF isGenBank(SL_UniProt) THEN Fail('Se ha detectado erróneamente el formato GenBank');
  IF NOT isGenBank(SL_GenBank) THEN Fail('No se ha detectado el formato GenBank');
  sec1 := 'MKKVNHWINGKNVAGNDYFLTTNPATGEVLADVASGGEAEINQA' +
          'VATAKEAFPKWANLPMKERARLMRRLGDLIDQNVPEIAAMETADTGLPIHQTKNVLIP' +
          'RASHNFEFFAEVCQQMNGKTYPVDDKMLNYTLVQPVGVCALVSPWNVPFMTATWKVAP' +
          'CLALGITAVLKMSELSPLTADRLGELALEAGIPAGVLNVVQGYGATAGDALVRHHDVR' +
          'AVSFTGGTATGRNIMKNAGLKKYSMELGGKSPVLIFEDADIERALDAALFTIFSINGE' +
          'RCTAGSRIFIQQSIYPEFVKFAERANRVRVGDPTDPNTQVGALISQQHWEKVSGYIRL' +
          'GIEEGATLLAGGPDKPSDLPAHLKGGNFLRPTVLADVDNRMRVAQEEIFGPVACLLPF' +
          'KDEAEALRLANDVEYGLASYIWTQDVSKVLRLARGIEAGMVFVNTQFVRDLRHAFGGV' +
          'KPRTGREGGGYSSKCSRK';
  sec2 := leerSecuenciaProteina(SL_GenBank);
  IF not (sec2 = sec1) THEN Fail('No se ha leído la secuencia correctamente');
end;


initialization

  RegisterTest(testing_biotools);
end.


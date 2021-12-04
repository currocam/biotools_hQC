program testing_biotools;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, test_biotools, src_biotools;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.


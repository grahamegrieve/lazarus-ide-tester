program idetester_standalone;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, idetester_tests, idetester_form, idetester_runtime, idetester_external;

{$R *.res}

var
  fn : String;
begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TTesterForm, TesterForm);
  if getCommandLineParam('test', fn) then
    TesterForm.engine := TTestEngineExternalCmdLine.create(fn);
  Application.Run;
end.


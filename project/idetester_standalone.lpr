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
  Forms, idetester_tests, idetester_form, idetester_runtime, idetester_external, idetester_example_testcase;

{$R *.res}

var
  fn : String;
begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TIdeTesterForm, IdeTesterForm);
  if getCommandLineParam('test', fn) then
    IdeTesterForm.engine := TTestEngineExternalCmdLine.create(fn);
  Application.Run;
end.


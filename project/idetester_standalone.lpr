program idetester_standalone;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  SysUtils,
  Interfaces, // this includes the LCL widgetset
  Forms, idetester_tests, idetester_base, idetester_ini, idetester_form, idetester_runtime, idetester_external, idetester_example_testcase, idetester_ide {not used, but want to compile};

{$R *.res}

var
  fn, p : String;
begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TIdeTesterForm, IdeTesterForm);
  if getCommandLineParam('test', fn) then
  begin
    IdeTesterForm.engine := TTestEngineExternalCmdLine.create(fn);
    IdeTesterForm.store := TTestIniSettingsProvider.create(fn.Replace('.exe', '')+'.testing.ini');
    if getCommandLineParam('params', p) then
      IdeTesterForm.store.save(tsmConfig, 'parameters', p);
  end;
  Application.Run;
end.


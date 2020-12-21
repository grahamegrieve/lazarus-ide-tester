program idetester_example;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Classes, Forms,
  idetester_example_testcase, idetester_runtime, idetester_tests, idetester_form;

begin
  if IsRunningIDETests then
    RunIDETests
  else
  begin
    Application.Scaled:=True;
    Application.Initialize;
    Application.CreateForm(TTesterForm, TesterForm);
    Application.Title := 'FPCUnit test runner';
    Application.Run;
    Application.Free;
  end;
end.

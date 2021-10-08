program idetester_console_test;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes,

  idetester_console, idetester_tests, idetester_example_testcase;


var
  Application: TIdeTesterConsoleRunner;

begin
  Application := TIdeTesterConsoleRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'IDEConsole Tester Consoler';
  Application.ShowProgress := true;
  Application.Sparse := true;
  Application.Run;
  Application.Free;
  Writeln('Press Enter to close');
  ReadLn;
end.

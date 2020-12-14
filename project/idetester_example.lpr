program idetester_example;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner, idetester_example_testcase, idetester_runtime,
  idetester_tests;

type

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

begin
  if IsRunningIDETests then
    RunIDETests
  else
  begin
    Application := TMyTestRunner.Create(nil);
    Application.Initialize;
    Application.Title := 'FPCUnit Console test runner';
    Application.Run;
    Application.Free;
  end;
end.

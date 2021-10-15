unit idetester_tests;

{$MODE DELPHI}

{
This is a set of tests with which to test the test runner.
This unit should only be included in a project that is testing the test runner
}
interface

uses
  Classes, SysUtils,
  FPCUnit, TestRegistry;

type
  { TTestCaseTests }

  TTestCaseTests = class (TTestCase)
  published
    procedure testPass;
    procedure testFail;
    procedure testError;
    procedure testIgnore;
  end;

implementation


{ TTestCaseTests }

procedure TTestCaseTests.testPass;
begin
  AssertTrue(true);
end;

procedure TTestCaseTests.testFail;
begin
  AssertTrue('message', false);
end;

procedure TTestCaseTests.testError;
begin
  raise Exception.create('Error');
end;

procedure TTestCaseTests.testIgnore;
begin
  ignore('testing');
end;


procedure RegisterTests;
begin
  RegisterTest('Library.TestCase tests', TTestCaseTests.Suite);
end;


initialization
  registerTests;
end.


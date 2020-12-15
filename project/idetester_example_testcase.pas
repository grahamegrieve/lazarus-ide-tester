unit idetester_example_testcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry;

type

  TTestCase1= class(TTestCase)
  published
    procedure TestHookUp;
  end;

implementation

procedure TTestCase1.TestHookUp;
begin
//  Fail('Write your own test');
//  System.Halt(1);
  sleep(600);
end;



initialization
  RegisterTest(TTestCase1);
end.


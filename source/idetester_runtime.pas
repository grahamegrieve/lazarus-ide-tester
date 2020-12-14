unit idetester_runtime;

{$MODE DELPHI}

{

# Introduction

This unit supports the IDE testing interface.

Using the IDE tsting is simple (in concept):
* register your tests in FPC test_registry
* include this unit in your .lpr, and then execute RunIDETests in the start code

e.g.

program xxxx;

uses
  // your units here, that define your tests and register them with FPC
  FPCIDETests;

begin
  RunIDETests
end.

Note that the program must be a console application (e.g. on Windows, win32 GUI Application must be false)
(and for advanced users, on windows, don't FreeConsole unless RunIDETests isn't going to be called)

You can do other things with the same program:

begin
  if IsRunningIDETests
    RunIDETests
  else
    // whatever else you want to do
end.

---------------------------------------------------------------

# Public methods:

procedure RunIDETests;

Execute the tests as requested by the IDE testing interface,
then terminate the application

function IsRunningIDETests: boolean;

return true, if the program was run from the IDE
and is going unit testing. You can use this in your
early start up testing routine to decide whether
to run the tests, or do something else

-------------------------------------------------------------
# Command line interface:

The program uses a command line interface.

the parameter -fpc4169C1B6-1D5C-4E4D-A790-8458C64CDA57 is always present - this signals that the unit has been run by the IDE

Additionally, there may be 1 of 2 other parameters:
- -run [testId] where [testId] is the id of root test to execute
- -selection [fn] where [fn] is a file that contains a list of tests to run, one testId per line

# Listing Tests

The output always starts with a list of all known tests

-- Tests --

[ ] testId = testClassName: testName

where [ ] is a series of spaces that convey nesting

## List operation:

### Input:

just the -list parameter

### Output

Writes a list of tests to std output. Each line consists of classname:testname. Lines are indented to show containment. Not all tests have a testname

## Run operation

### Input

[fn] is a test file with a list of lines, where each line is a test to run, using the same format as the list output: id: classname/testname where id is a number
This is also indented using the same output as the containment. (note: use a test file so as not to have problems overflowing input buffer?)

### Output:
Writes a list of test outcomes to std output. Each line has the following format:

id : [na|pass|fail|error] duration class : "messsage"

values:
- id: the id from the input file
- pass | fail | error - for tests: result of running the test; for suites, na, or error (if the error happens in setup/teardown
- duration - number of milliseconds (integer value)
- class - for fail/error, the class of exception that was raised
- message - for fail/error, the error message that was raised

Tests will be run in the order found in [fn]
}


interface

uses
  Classes, SysUtils, Generics.Collections,
  FPCUnit, TestRegistry, TestDecorator;

const
  FPC_MAGIC_COMMAND = 'fpc4169C1B6-1D5C-4E4D-A790-8458C64CDA57';

procedure RunIDETests;
function IsRunningIDETests: boolean;

type

  { TTestInfo }

  TTestInfo = class
  private
    FId: String;
    FStart: UInt64;
  public
    constructor Create(id : String);
    property id : String read FId write FId;
    property start : UInt64 read FStart write FStart;
  end;

  { TTestEngineRunTimeListener }

  TTestEngineRunTimeListener = class (TinterfacedObject, ITestListener)
  private
    FTests : TDictionary<TTest, TTestInfo>;
  public
    constructor Create(tests : TDictionary<TTest, TTestInfo>);

    procedure StartTest(ATest: TTest);
    procedure EndTest(ATest: TTest);
    procedure StartTestSuite(ATestSuite: TTestSuite);
    procedure EndTestSuite(ATestSuite: TTestSuite);
    procedure AddFailure(ATest: TTest; AFailure: TTestFailure);
    procedure AddError(ATest: TTest; AError: TTestFailure);
  end;

function getCommandLineParam(name : String; var res : String) : boolean; overload;
function getCommandLineParam(name : String) : String; overload;
function hasCommandLineParam(name : String) : boolean;

implementation

function getCommandLineParam(name : String; var res : String) : boolean; overload;
var
  i : integer;
begin
  result := false;
  for i := 1 to paramCount - 1 do
  begin
    if paramStr(i) = '-'+name then
    begin
      res := paramStr(i+1);
      exit(true);
    end;
  end;
end;

function getCommandLineParam(name : String) : String; overload;
var
  i : integer;
begin
  result := '';
  for i := 1 to paramCount - 1 do
  begin
    if paramStr(i) = '-'+name then
      exit(paramStr(i+1));
  end;
end;

function hasCommandLineParam(name : String) : boolean;
var
  i : integer;
begin
  result := false;
  for i := 1 to paramCount  do
  begin
    if paramStr(i) = '-'+name then
      exit(true);
  end;
end;

function IsRunningIDETests: boolean;
begin
  result := hasCommandLineParam(FPC_MAGIC_COMMAND);
end;

procedure printTests(indent : String; path : String; suite : TTestSuite; allTests : TDictionary<TTest, TTestInfo>);
var
  i: integer;
  cindent : String;
  cpath : String;
begin
  for i := 0 to suite.ChildTestCount - 1 do
  begin
    if (suite.Test[i].TestName = '') and (suite.ChildTestCount = 1) then
    begin
      cindent := indent;
      cpath := path
    end
    else
    begin
      cpath := path+'.'+inttostr(i);
      writeln(indent+cpath+' = '+suite.Test[i].className+': '+suite.Test[i].TestName);
      allTests.Add(suite.Test[i], TTestInfo.create(cpath));
      cindent := indent + ' ';
    end;

    if suite.Test[i] is TTestSuite then
      printTests(cindent, cpath, TTestSuite(suite.Test[i]), allTests)
    else if TObject(suite.Test[i]).InheritsFrom(TTestDecorator) then
      printTests(cindent, cpath, TTestSuite(TTestDecorator(suite.Test[i]).Test), allTests);
  end;
end;

procedure listTests(allTests : TDictionary<TTest, TTestInfo>);
var
  test : TTestSuite;
begin
  writeln('-- Test List ---');
  test := GetTestRegistry;
  writeln('0 = RootTest: AllTests');
  printTests(' ', '0', test, allTests);
  allTests.Add(test, TTestInfo.create('0'));
  writeln('-- End Test List ---');
end;

function findTest(allTests : TDictionary<TTest, TTestInfo>; id : String) : TTest; overload;
var
  test : TTest;
begin
  result := nil;
  for test in allTests.keys do
    if allTests[test].id = id then
      exit(test);
end;


function runTests(allTests : TDictionary<TTest, TTestInfo>; id : String) : integer;
var
  test : TTest;
  listener : ITestListener;
  tr : TTestResult;
begin
  test := findTest(allTests, id);
  if test = nil then
  begin
    writeln('Unable to find test '+id);
    result := 1;
  end
  else
  begin
    listener := TTestEngineRunTimeListener.create(allTests) as ITestListener;
    tr := TTestResult.create;
    try
      tr.AddListener(listener);
      test.Run(tr);
    finally
      tr.free;
    end;
  end;
end;

procedure RunIDETests;
var
  allTests : TDictionary<TTest, TTestInfo>;
begin
  if not IsRunningIDETests then
  begin
    Writeln('No IDE Tester Parameter found');
    exitCode := 1;
  end
  else
  begin
    allTests := TDictionary<TTest, TTestInfo>.create;
    try
      ListTests(allTests);
      if hasCommandLineParam('run') then
        exitCode := RunTests(allTests, getCommandLineParam('run'))
      else
        exitCode := 1;
    finally
      allTests.free;
    end;
    if hasCommandLineParam('pause') then
    begin
      writeln('press enter to close');
      readln;
    end;
  end;
  system.Exit;
end;

{ TTestInfo }

constructor TTestInfo.Create(id: String);
begin
  inherited Create;
  FId := id;
end;

{ TTestEngineRunTimeListener }

constructor TTestEngineRunTimeListener.Create(tests: TDictionary<TTest, TTestInfo>);
begin
  inherited create;
  FTests := tests;
end;

procedure TTestEngineRunTimeListener.StartTest(ATest: TTest);
var
  ti : TTestInfo;
begin
  ti := FTests[aTest];
  writeln(ti.id+': start');
  ti.start := GetTickCount64;
end;

procedure TTestEngineRunTimeListener.EndTest(ATest: TTest);
var
  ti : TTestInfo;
begin
  ti := FTests[aTest];
  writeln(ti.id+': end '+inttostr(GetTickCount64 - ti.start));
end;

procedure TTestEngineRunTimeListener.StartTestSuite(ATestSuite: TTestSuite);
var
  ti : TTestInfo;
begin
  ti := FTests[ATestSuite];
  writeln(ti.id+': start');
  ti.start := GetTickCount64;
end;

procedure TTestEngineRunTimeListener.EndTestSuite(ATestSuite: TTestSuite);
var
  ti : TTestInfo;
begin
  ti := FTests[ATestSuite];
  writeln(ti.id+': end '+inttostr(GetTickCount64 - ti.start));
end;

procedure TTestEngineRunTimeListener.AddFailure(ATest: TTest; AFailure: TTestFailure);
var
  ti : TTestInfo;
begin
  ti := FTests[aTest];
  writeln(ti.id+': fail '+AFailure.ExceptionClassName+' '+AFailure.ExceptionMessage);
end;

procedure TTestEngineRunTimeListener.AddError(ATest: TTest; AError: TTestFailure);
var
  ti : TTestInfo;
begin
  ti := FTests[aTest];
  writeln(ti.id+': error '+AError.ExceptionClassName+' '+AError.ExceptionMessage);
end;

end.


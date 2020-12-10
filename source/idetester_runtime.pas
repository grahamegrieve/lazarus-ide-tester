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

Additionally, there will be 1 of 2 other parameters:
- list  - return a list of tests
- run [fn] where [fn] is the name of a file that contains a list of tests to execute

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
  Classes, SysUtils;

const
  FPC_MAGIC_COMMAND = 'fpc4169C1B6-1D5C-4E4D-A790-8458C64CDA57';

procedure RunIDETests;
function IsRunningIDETests: boolean;

implementation

function getCommandLineParam(name : String; var res : String) : boolean;
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

procedure listTests;
begin
  writeln('todo: list tests');
end;

procedure runTests;
begin
  writeln('todo: run tests');
end;

procedure RunIDETests;
begin
  exitCode := 1;
  if not IsRunningIDETests then
    Writeln('No IDE Tester Parameter found')
  else if hasCommandLineParam('list') then
  begin
    exitCode := 0;
    ListTests;
  end
  else if hasCommandLineParam('run') then
  begin
    RunTests;
    exitCode := 0;
  end
  else
    writeln('No IDE Tester command found');
  system.Exit;
end;

end.


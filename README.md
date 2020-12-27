# lazarus-ide-tester

The Lazarus IDE tester is an improved set of unit testing frameworks for 
Lazarus. It still uses FPCUnit, but allows the developer to run the tests
within the lazarus IDE, for an improved workflow.

Advantages of the IDE Tester:

1. It remembers outcomes from run to run, so you can work through failing tests without rerunning all tests
1. it runs in the IDE
1. it runs on OSX
1. it lets you stop a run of tests
1. it updates while the run is happening
1. it's fast

The IDETester has been tested and works on:
* Windows
* Linux (Ubuntu 20.10)
* OSX (Big Sur)

Discussion on use at https://forum.lazarus.freepascal.org/index.php/board,13.0.html

Issue report and PRs to https://github.com/grahamegrieve/lazarus-ide-tester

# Todo

Things still to do:

* add additional views (e.g. only show non-passing tests, flatten heirarchy)


# User Documentation

The IDETester appears as a view on the View menu in Lazarus:

FPCUnit Test Cases 

This brings up the Tests view:

![Screenshot](https://github.com/grahamegrieve/lazarus-ide-tester/blob/main/doco/screenshot.png)

## Features

The view has the following features

### Buttons:

* Reload: Compile the current project, and load all tests from it (not visible when testing directly - see below)
* Run Selected Test(s) - run the currently selected test, and any children
* Run Checked Tests - Run the set of tests that are checked
* Stop Test Run - Stop the current test run 
* Debug Selected Test(s) - debug the currently selected test (only in IDE)
* Run Failed Tests - run all tests currently in failed status (including error) 
* Clear outcomes - forgot all the remembered outcomes
* Copy - copy the test outcome details to the clipboard
* Configure - Set configuration options (see below)

### Tree
This tree contains all the registered FPCUnit tests, and displays their status. The
test status can be one of:

![outcomes](https://github.com/grahamegrieve/lazarus-ide-tester/blob/main/doco/outcomes.png)

You can also check and uncheck tests in the tree

### Status Bar:

Displays the current status of the tests / testing process

### Configuration Options

![options](https://github.com/grahamegrieve/lazarus-ide-tester/blob/main/doco/options.png)

Note that the configuration options are stored in the [project].lps file, so are not committed
to version control (or should not be).

#### Test Project

Often / usually, an application will have a dedicated test project, instead of building the tests 
into the application being developed. Choose the project (.lpi) for the project, and then this 
project will be compiled and executed rather instead of the current project.

Notes:

* If you provide an alternative project, you can specify to auto-save editor files before running the tests (because unsaved code won't be tested). 
* The selected test project will need to compile using lazbuild for this option. This can be tricky with multi-platforms etc. 

#### Additional Execution Parameters

Additional parameters passed directly to the test application. 
These can be used for:

* additional parameters around start up mode, or local 
* configuration options for folders etc so testing resources can be found
* passwords for testing protected resources

#### Time to wait

The tests are run in a background process. When the tests are stopped, 
the controller waits for the currently running test to complete. If 
the test takes longer than this specified time, then the process will 
be terminated, and no clean up will happen.

# Installing the IDETester

1. Get the code from the git repository https://github.com/grahamegrieve/lazarus-ide-tester
1. Install into the Lazarus IDE
  1. Open the package /package/idetester.lpk, and compile it 
  2. Open the package /ide/idetester_dsgn.lpk. Don't compile it
  3. Choose Package.. install packages and then install idetester_dsgn from the list of uninstalled packages. In some versions of lazarus(?) if it is not displayed as an option, you need to compile it manually first

# Running Test Cases

There's multiple ways to run your test cases:

* Run them from the IDE directly 
* Run them as a GUI
* Run them as a Console Application

## Running tests from the IDE

Summary:

* Bring up the Test View in the IDE 
* Create a new project, or use an existing project 
* Add the idetester package as a dependency. 
* in the dpr, add idetester_runtime to the units list, and make the main clause of the project:

```pascal
program my_test_example;

uses
  // add your test units here 
  idetester_runtime;

begin
  RunIDETests
end.                                      
```      

* note that if the application is a console app (windows), then you can call RunIDETestsCmdLine instead
* Press "load tests" in the test view, and then run your tests 

Note that you can make running the IDE tests to be one of the options for running your program:

```pascal
if IsRunningIDETests then
  RunIDETests
else
  // do something else
```

Note that on windows, your project does have to be a console application, whatever else it is/does

Also, note that the github repo includes a project project/idetester_standalone that 
can run the test cases in that project using a ```-test {exename}``` parameter

## Run tests as a GUI

The IDETester can be used directly in place of the FPC GUITest runner (see above for the advantages it has)

To do this:

* Create a new project, or use an existing project 
* Add the idetester package as a dependency. 
* in the dpr, add idetester_form to the units list, and make the main clause of the project:

```pascal
program idetester_example;

uses
  Interfaces, // this includes the LCL widgetset
  // add your test units here 
  idetester_form;

begin
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TIdeTesterForm, IdeTesterForm);
  Application.Title := 'FPCUnit test runner';
  Application.Run;
  Application.Free;
end.               
```

## Use the standalone tester 

* You can also use a ConsoleRunner (idetester_console.pas) provided as part of IDETester
* This isn't much different from the built in FPC Console test runner, but gives you more flexibility for CI integration
* use the class in the idetester_console unit

# Mixing all 3 options

See https://github.com/grahamegrieve/delphi-markdown/blob/master/fpc/MarkdownTestProgram.lpr for an example.


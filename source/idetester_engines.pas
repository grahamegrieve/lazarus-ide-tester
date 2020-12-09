unit idetester_engines;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  TTestCheckState = (tcsUnchecked, tcsChecked, tcsMixed);
  TTestOutcome = (toUnknown, toNotRun, toRunning, toChildRunning, toPass, toFail, toError, toSomePass);

  TTestNodeList = class;

  { TTestNode }

  TTestNode = class abstract (TObject)
  private
    FDuration: Int64;
    FExceptionClassName: string;
    FExceptionMessage: string;
    FLineNumber: longint;
    FParent : TTestNode;
    FChildren : TTestNodeList;
    FSourceUnitName: string;
    FOutcome : TTestOutcome;
    FStartTime : UInt64;
    FData : Pointer;

    FExecute : boolean;
    function GetCheckState: TTestCheckState;
    procedure SetCheckState(AValue: TTestCheckState);
    procedure SetDuration(AValue: Int64);
    procedure SetOutcome(AValue: TTestOutcome);

    function testCount : cardinal;
  protected
    function testName : String; virtual; abstract;
    function testClassName : String; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    property data : Pointer read FData write FData;
    property checkState : TTestCheckState read GetCheckState write SetCheckState;
    property outcome : TTestOutcome read FOutcome write SetOutcome;
    property execute : Boolean read FExecute write FExecute;
    property parent : TTestNode read FParent;
    property duration : Int64 read FDuration write SetDuration;

    property ExceptionMessage: string read FExceptionMessage write FExceptionMessage;
    property ExceptionClassName: string read FExceptionClassName write FExceptionClassName;
    property SourceUnitName: string read FSourceUnitName write FSourceUnitName;
    property LineNumber: longint read FLineNumber write FLineNumber;

    function description : String;
    function details(indent : String) : String;
    procedure start;
    procedure finish;
  end;

  { TTestNodeList }

  TTestNodeList = class (TObjectList<TTestNode>)
  end;

  TSettingsProvider = class abstract (TObject)
  private
  public
    function readString(name, defValue : String) : String; virtual; abstract;
    procedure saveString(name, value : String); virtual; abstract;
  end;

  { TTestError }

  TTestError = class (TObject)
  private
    FExceptionClass: String;
    FExceptionMessage: String;
  public
    property ExceptionClass : String read FExceptionClass write FExceptionClass;
    property ExceptionMessage : String read FExceptionMessage write FExceptionMessage;
  end;

  TTestListener = class abstract (TObject)
  private
  public
    procedure StartTest(test: TTestNode); virtual; abstract;
    procedure EndTest(test: TTestNode); virtual; abstract;
    procedure TestFailure(ATest: TTestNode; AFailure: TTestError); virtual; abstract;
    procedure TestError(ATest: TTestNode; AError: TTestError); virtual; abstract;
    procedure StartTestSuite(ATest: TTestNode); virtual; abstract;
    procedure EndTestSuite(ATest: TTestNode); virtual; abstract;
    procedure EndRun(ATest: TTestNode); virtual; abstract;
  end;

  TTestEngineThreadMode = (ttmMainThread, ttmOtherThread, ttmEither);

  TTestSession = class (TObject)
  end;

  { TTestEngine }

  TTestEngine = class abstract (TObject)
  private
    FListener: TTestListener;
  public
    property listener : TTestListener read FListener write FListener;

    procedure loadAllTests(list : TTestNodeList); virtual; abstract; // get a list of tests
    function threadMode : TTestEngineThreadMode; virtual; abstract;
    function canTerminate : boolean; virtual; abstract; // true if it's ok to call terminateTests

    function prepareToRunTests : TTestSession; virtual; abstract; // get ready to run tests - do whatever is requred (e.g. compile in the ide)

    procedure runTest(session : TTestSession; node : TTestNode; list : TTestNodeList); virtual; abstract; // run the named test, and any sub tests that are checked
    procedure terminateTests; virtual; abstract; // terminate the tests without waiting for clean up. called from a different thread to runTest, which will still be in progress
    procedure finishTestRun(session : TTestSession); virtual; abstract; // clean up after a test run (must free session)
  end;

implementation

end.


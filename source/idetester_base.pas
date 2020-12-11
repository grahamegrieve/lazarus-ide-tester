unit idetester_base;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, Generics.Collections,
  laz.VirtualTrees,
  idetester_strings;

type
  TTestCheckState = (tcsUnchecked, tcsChecked, tcsMixed);
  TTestOutcome = (toUnknown, toNotRun, toRunning, toChildRunning, toPass, toFail, toError, toSomePass);

  TTestNode = class;
  TTestNodeList = class;

  TTestNodeUpdateEvent = procedure (node : TTestNode) of object;

  { TTestNode }

  TTestNode = class (TObject)
  private
    FDuration: Int64;
    FExceptionClassName: string;
    FExceptionMessage: string;
    FLineNumber: longint;
    FOwnsData: boolean;
    FParent : TTestNode;
    FChildren : TTestNodeList;
    FSourceUnitName: string;
    FOutcome : TTestOutcome;
    FStartTime : UInt64;
    FNode : PVirtualNode;
    FData : TObject;

    FExecute : boolean;
    FOnUpdate : TTestNodeUpdateEvent;
    FTestClassName: String;
    FTestName: String;
    function GetCheckState: TTestCheckState;
    function GetHasChildren: boolean;
    procedure SetCheckState(AValue: TTestCheckState);
    procedure SetDuration(AValue: Int64);
    procedure SetOutcome(AValue: TTestOutcome);

    function testCount : cardinal;
  public
    constructor Create(parent : TTestNode);
    destructor Destroy; override;

    property Data : TObject read FData write FData;
    property ownsData : boolean read FOwnsData write FOwnsData;
    property node : PVirtualNode read FNode write FNode;
    property checkState : TTestCheckState read GetCheckState write SetCheckState;
    property outcome : TTestOutcome read FOutcome write SetOutcome;
    property execute : Boolean read FExecute write FExecute;
    property parent : TTestNode read FParent;
    property duration : Int64 read FDuration write SetDuration;
    property OnUpdate : TTestNodeUpdateEvent read FOnUpdate write FOnUpdate;
    property children : TTestNodeList read FChildren;
    property hasChildren : boolean read GetHasChildren;
    property testName : String read FTestName write FTestName;
    property testClassName : String read FTestClassName write FTestClassName;

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

  TTestSettingsProvider = class abstract (TObject)
  private
  public
    function read(name, defValue : String) : String; virtual; abstract;
    procedure save(name, value : String); virtual; abstract;
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
    procedure TestFailure(test: TTestNode; fail: TTestError); virtual; abstract;
    procedure TestError(test: TTestNode; error: TTestError); virtual; abstract;
    procedure StartTestSuite(test: TTestNode); virtual; abstract;
    procedure EndTestSuite(test: TTestNode); virtual; abstract;
    procedure EndRun(test: TTestNode); virtual; abstract;
  end;

  TTestEngineThreadMode = (ttmMainThread, ttmOtherThread, ttmEither);

  TTestSession = class (TObject)
  public
    procedure skipTest(node : TTestNode); virtual; abstract; // set up a list of tests to skip
  end;

  TNodeFactory = function (parent : TTestNode) : TTestNode of object;

  { TTestEngine }

  TTestEngine = class abstract (TObject)
  private
    FListener: TTestListener;
  public
    property listener : TTestListener read FListener write FListener;

    procedure loadAllTests(factory : TNodeFactory); virtual; abstract; // get a list of tests
    function threadMode : TTestEngineThreadMode; virtual; abstract;

    function canTerminate : boolean; virtual; abstract; // true if it's ok to call terminateTests
    function canDebug : boolean; virtual; abstract;
    function doesReload : boolean; virtual; abstract;

    function prepareToRunTests : TTestSession; virtual; abstract; // get ready to run tests - do whatever is requred (e.g. compile in the ide)

    procedure runTest(session : TTestSession; node : TTestNode; debug : boolean); virtual; abstract; // run the named test, and any sub tests that are checked. All tests is provided because test engines may need to access the entire list of tests
    procedure terminateTests; virtual; abstract; // terminate the tests without waiting for clean up. called from a different thread to runTest, which will still be in progress
    procedure finishTestRun(session : TTestSession); virtual; abstract; // clean up after a test run (must free session)
  end;

implementation

{ TTestNode }

constructor TTestNode.Create(parent: TTestNode);
begin
  inherited Create;
  FParent := parent;
  FChildren := TTestNodeList.create;
  FChildren.OwnsObjects := false;
  FDuration := -1;
end;

destructor TTestNode.Destroy;
begin
  if FOwnsData then
    FData.Free;
  FChildren.Free;
  inherited Destroy;
end;

procedure TTestNode.SetOutcome(AValue: TTestOutcome);
begin
  FOutcome := AValue;
  FOnUpdate(self);
end;

function TTestNode.testCount: cardinal;
var
  child : TTestNode;
begin
  result := 0;
  for child in FChildren do
    if (not child.hasChildren) then
      inc(result)
    else
      inc(result, child.testCount);
end;

function TTestNode.description: String;
begin
  if FParent = nil then
    result := rsAllTests
  else
    result := testName;

  if (FDuration > -1) or (FChildren.Count > 0) then
  begin
    result := result + ' (';
    if (FChildren.Count > 0) then
      result := result + inttostr(testCount)+' tests';
    if (FDuration > -1) and (FChildren.Count > 0) then
      result := result + ', ';
    if FDuration > -1 then
      result := result + inttostr(FDuration)+'ms';
    result := result + ')';
  end;
  if ExceptionMessage <> '' then
    result := result +': '+ExceptionMessage;
end;

function TTestNode.details(indent : String): String;
var
  b : TStringBuilder;
  child : TTestNode;
  tn : String;
begin
  if FChildren.Count = 0 then
  begin
    result := TestName;
    if (FDuration > -1) or (FChildren.Count > 0) then
    begin
      result := result + ' (';
      if (FChildren.Count > 0) then
        result := result + inttostr(testCount)+' tests';
      if (FDuration > -1) and (FChildren.Count > 0) then
        result := result + ', ';
      if FDuration > -1 then
        result := result + inttostr(FDuration)+'ms';
      result := result + ')';
    end;
    if FExceptionMessage <> '' then
      if FSourceUnitName <> '' then
        result := result + '. '+FExceptionClassName+': '+FExceptionMessage+' (@'+FSourceUnitName+'#'+inttostr(FLineNumber)
      else
        result := result + '. '+FExceptionClassName+': '+FExceptionMessage;
    result := result + #13#10;
  end
  else
  begin
    b := TStringBuilder.create;
    try
      if FParent = nil then
        tn := rsAllTests
      else
        tn := TestName;
      b.append(indent+'-- '+tn+' Starts ---------------------------------'+#13#10);
      for child in FChildren do
        b.append(child.details(indent+'  '));
      b.append(indent+'-- '+tn+' Ends -----------------------------------'+#13#10);
      result := b.toString;
    finally
      b.free;
    end;
  end;
end;

procedure TTestNode.start;
begin
  FStartTime := GetTickCount64;
end;

procedure TTestNode.finish;
begin
  if FStartTime > 0 then
  begin
    Duration := GetTickCount64 - FStartTime;
    FStartTime := 0;
  end;
end;

procedure TTestNode.SetCheckState(AValue: TTestCheckState);
begin
  case AValue of
    tcsUnchecked : FNode.CheckState := csUncheckedNormal;
    tcsChecked : FNode.CheckState := csCheckedNormal;
    tcsMixed : FNode.CheckState := csMixedNormal;
  end;
  FOnUpdate(self);
end;

function TTestNode.GetCheckState: TTestCheckState;
begin
  case FNode.CheckState of
    csUncheckedNormal, csUncheckedPressed : result := tcsUnchecked;
    csCheckedNormal, csCheckedPressed : result := tcsChecked;
    csMixedNormal, csMixedPressed : result := tcsMixed;
  end;
end;

function TTestNode.GetHasChildren: boolean;
begin
  result := FChildren.count > 0;
end;

procedure TTestNode.SetDuration(AValue: Int64);
begin
  FDuration := AValue;
  // don't do this - we're probably in the test thread, and update will be called later...   FOnUpdate(self);
end;

//{ TTestNodeList }
//
//function TTestNodeList.forTest(test: TTest): TTestNode;
//var
//  ti : TTestNode;
//begin
//  result := nil;
//  for ti in self do
//    if ti.test = test then
//      exit(ti);
//end;

end.


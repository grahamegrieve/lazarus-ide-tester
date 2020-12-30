unit idetester_base;

{$MODE DELPHI}

interface

uses
  SysUtils, Classes, Generics.Collections,
  laz.VirtualTrees,
  idetester_strings;

type
  TTestCheckState = (tcsUnchecked, tcsChecked, tcsMixed);
  TTestOutcome = (toUnknown, toNotRun, toRunning, toChildRunning, toPass, toFail, toError, toSomePass, toHalt);

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
    FSourceUnit: String;
    FSourceUnitError: string;
    FOutcome : TTestOutcome;
    FStartTime : UInt64;
    FCheckState : TTestCheckState;
    FNodePtr : PVirtualNode;
    FData : TObject;

    FExecute : boolean;
    FOnUpdate : TTestNodeUpdateEvent;
    FTestClassName: String;
    FTestName: String;
    function GetHasChildren: boolean;
    procedure SetCheckState(AValue: TTestCheckState);
    procedure SetDuration(AValue: Int64);
    procedure SetNodePtr(AValue: PVirtualNode);
    procedure SetOutcome(AValue: TTestOutcome);

    function testCount : cardinal;
  public
    constructor Create(parent : TTestNode);
    destructor Destroy; override;

    property Data : TObject read FData write FData;
    property ownsData : boolean read FOwnsData write FOwnsData;
    property node : PVirtualNode read FNodePtr write SetNodePtr;
    property checkState : TTestCheckState read FCheckState write SetCheckState;
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
    property SourceUnit : String read FSourceUnit write FSourceUnit;
    property SourceUnitError: string read FSourceUnitError write FSourceUnitError;
    property LineNumber: longint read FLineNumber write FLineNumber;

    function description : String;
    function sortName : string;
    function descriptionFull : String;
    function details(indent : String) : String;
    procedure start;
    procedure finish;
  end;

  { TTestNodeList }

  TTestNodeList = class (TObjectList<TTestNode>)
  end;

  TTestSettingsMode = (tsmConfig, tsmStatus);

  { TTestSettingsProvider }

  TTestSettingsProvider = class abstract (TObject)
  private
  public
    function read(mode : TTestSettingsMode; name, defValue : String) : String; virtual; abstract;
    procedure save(mode : TTestSettingsMode; name, value : String); virtual; abstract;

    function parameters : String;
  end;

  { TTestError }

  TTestError = class (TObject)
  private
    FExceptionClass: String;
    FExceptionMessage: String;
    FLineNumber: Integer;
    FSourceUnit: String;
  public
    property ExceptionClass : String read FExceptionClass write FExceptionClass;
    property ExceptionMessage : String read FExceptionMessage write FExceptionMessage;
    property SourceUnit : String read FSourceUnit write FSourceUnit;
    property LineNumber : Integer read FLineNumber write FLineNumber;
  end;

  TTestListener = class abstract (TObject)
  private
  public
    procedure StartTest(test: TTestNode); virtual; abstract;
    procedure EndTest(test: TTestNode); virtual; abstract;
    procedure TestFailure(test: TTestNode; fail: TTestError); virtual; abstract;
    procedure TestError(test: TTestNode; error: TTestError); virtual; abstract;
    procedure TestHalt(test: TTestNode); virtual; abstract; // testing process died completely
    procedure StartTestSuite(test: TTestNode); virtual; abstract;
    procedure EndTestSuite(test: TTestNode); virtual; abstract;
    procedure EndRun(test: TTestNode); virtual; abstract;
  end;

  TTestEngineThreadMode = (ttmMainThread, ttmOtherThread, ttmEither);

  TTestSession = class (TObject)
  public
    procedure skipTest(node : TTestNode); virtual; abstract; // set up a list of tests to skip
  end;

  TLogEvent = procedure (sender : TObject; msg : String) of object;

  { TTestEngine }

  TTestEngine = class abstract (TObject)
  private
    FListener: TTestListener;
    FOnReinitialise, FOnUpdateStatus: TNotifyEvent;
    FOnStatusMessage: TLogEvent;
    FSettings : TTestSettingsProvider;
  protected
    procedure setStatusMessage(s : String);
  public
    property listener : TTestListener read FListener write FListener;
    property settings : TTestSettingsProvider read FSettings write FSettings;

    property OnReinitialise : TNotifyEvent read FOnReinitialise write FOnReinitialise;
    property OnStatusMessage: TLogEvent read FOnStatusMessage write FOnStatusMessage;
    property OnUpdateStatus: TNotifyEvent read FOnUpdateStatus write FOnUpdateStatus;

    procedure loadAllTests(testList : TTestNodeList; manual : boolean); virtual; abstract; // get a list of tests
    function threadMode : TTestEngineThreadMode; virtual; abstract;

    function canTerminate : boolean; virtual; abstract; // true if it's ok to call terminateTests
    function canDebug : boolean; virtual; abstract;
    function canStart : boolean; virtual; abstract;
    function canStop : boolean; virtual;
    function doesReload : boolean; virtual; abstract;
    function canParameters : boolean; virtual;
    function canTestProject : boolean; virtual;

    function prepareToRunTests : TTestSession; virtual; abstract; // get ready to run tests - do whatever is requred (e.g. compile in the ide)
    function setUpDebug(session : TTestSession; node : TTestNode) : boolean; virtual;

    procedure runTest(session : TTestSession; node : TTestNode); virtual; abstract; // run the named test, and any sub tests that are checked.
    procedure terminateTests(session: TTestSession); virtual; abstract; // terminate the tests without waiting for clean up. called from a different thread to runTest, which will still be in progress
    procedure finishTestRun(session : TTestSession); virtual; abstract; // clean up after a test run (must free session)

    procedure openSource(test : TTestNode); virtual;
  end;

procedure readLocation(loc : String; out srcUnit : String; out line : integer);

implementation

procedure readLocation(loc : String; out srcUnit : String; out line : integer);
var
  p : TStringArray;
begin
  p := loc.split([' ']);
  srcUnit := '';
  line := 0;
  if (length(p) > 0) then
  begin
    srcUnit := p[length(p)-1];
    if (length(p) > 2) then
      line := StrToIntDef(p[length(p)-3], 0);
  end;
end;

{ TTestSettingsProvider }

function TTestSettingsProvider.parameters: String;
begin
  result := read(tsmConfig, 'parameters', '');
end;

{ TTestEngine }

procedure TTestEngine.setStatusMessage(s: String);
begin
  FOnStatusMessage(self, s);
end;

function TTestEngine.canStop: boolean;
begin
  result := true;
end;

function TTestEngine.canParameters: boolean;
begin
  result := false;
end;

function TTestEngine.canTestProject: boolean;
begin
  result := false;
end;

function TTestEngine.setUpDebug(session: TTestSession; node: TTestNode) : boolean;
begin
  result := false;
end;

procedure TTestEngine.openSource(test: TTestNode);
begin
  // nothing
end;

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
  if assigned(FOnUpdate) then
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
    result := rs_IdeTester_Msg_AllTests
  else
    result := testName;

  if (FDuration > -1) or (FChildren.Count > 0) then
  begin
    result := result + ' (';
    if (FChildren.Count > 0) then
      result := result + inttostr(testCount)+' '+rs_IdeTester_SBar_Tests;
    if (FDuration > -1) and (FChildren.Count > 0) then
      result := result + ', ';
    if FDuration > -1 then
      result := result + inttostr(FDuration)+'ms';
    result := result + ')';
  end;
  if ExceptionMessage <> '' then
    result := result +': '+ExceptionMessage;
end;

function TTestNode.sortName: string;
begin
  if testClassName <> '' then
    result := testClassName
  else
    result := testName;
end;

function TTestNode.descriptionFull: String;
begin
  result := description;
  if SourceUnitError <> '' then
    result := result + ' @ '+SourceUnitError+'#'+inttostr(LineNumber);
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
        result := result + inttostr(testCount)+' '+rs_IdeTester_SBar_Tests;
      if (FDuration > -1) and (FChildren.Count > 0) then
        result := result + ', ';
      if FDuration > -1 then
        result := result + inttostr(FDuration)+'ms';
      result := result + ')';
    end;
    if FExceptionMessage <> '' then
      if FSourceUnitError <> '' then
        result := result + '. '+FExceptionClassName+': '+FExceptionMessage+' (@'+FSourceUnitError+'#'+inttostr(FLineNumber)
      else
        result := result + '. '+FExceptionClassName+': '+FExceptionMessage;
    result := result + #13#10;
  end
  else
  begin
    b := TStringBuilder.create;
    try
      if FParent = nil then
        tn := rs_IdeTester_Msg_AllTests
      else
        tn := TestName;
      b.append(indent+'-- '+tn+' '+rs_IdeTester_Msg_Starts+' ---------------------------------'+#13#10);
      for child in FChildren do
        b.append(child.details(indent+'  '));
      b.append(indent+'-- '+tn+' '+rs_IdeTester_Msg_Ends+' -----------------------------------'+#13#10);
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
  FCheckState := aValue;
  if FNodePtr <> nil then
  begin
    case FCheckState of
      tcsUnchecked : FNodePtr.CheckState := csUncheckedNormal;
      tcsChecked : FNodePtr.CheckState := csCheckedNormal;
      tcsMixed : FNodePtr.CheckState := csMixedNormal;
    end;
  end;
  if assigned(FOnUpdate) then
    FOnUpdate(self);
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

procedure TTestNode.SetNodePtr(AValue: PVirtualNode);
begin
  FNodePtr:=AValue;
  if FNodePtr <> nil then
  begin
    case FCheckState of
      tcsUnchecked : FNodePtr.CheckState := csUncheckedNormal;
      tcsChecked : FNodePtr.CheckState := csCheckedNormal;
      tcsMixed : FNodePtr.CheckState := csMixedNormal;
    end;
  end;
end;

end.


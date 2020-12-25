unit idetester_direct;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils,
  FPCUnit, testregistry, testdecorator,
  idetester_base, idetester_strings;

type
  { TTestSessionDirect }

  TTestSessionDirect = class (TTestSession)
  private
    FTestResult : TTestResult;
  public
    constructor Create;
    destructor Destroy; override;
    procedure skipTest(node : TTestNode); override;
  end;

  { TTestEngineDirectListener }

  TTestEngineDirectListener = class (TinterfacedObject, ITestListener)
  private
    FListener : TTestListener;
    FRoot : TTestNode;
    function findTestInNode(test : TTest; node : TTestNode) : TTestNode;
    function findNode(test : TTest; optional : boolean = false) : TTestNode;
    function makeError(err : TTestFailure) : TTestError;
  public
    constructor create(listener : TTestListener; root : TTestNode);

    procedure StartTest(ATest: TTest);
    procedure EndTest(ATest: TTest);
    procedure StartTestSuite(ATestSuite: TTestSuite);
    procedure EndTestSuite(ATestSuite: TTestSuite);
    procedure AddFailure(ATest: TTest; AFailure: TTestFailure);
    procedure AddError(ATest: TTest; AError: TTestFailure);
  end;

  { TTestEngineDirect }

  TTestEngineDirect = class (TTestEngine)
  private
    Function registerTestNode(factory : TNodeFactory; parent : TTestNode; test : TTest) : TTestNode;
    procedure BuildTree(factory : TNodeFactory; rootTest: TTestNode; aSuite: TTestSuite);
  public
    procedure loadAllTests(factory : TNodeFactory; manual : boolean); override;
    function threadMode : TTestEngineThreadMode; override;
    function canTerminate : boolean; override;
    function doesReload : boolean; override;
    function canDebug : boolean; override;

    function paramsForTest(test : TTestNode) : String; override;
    function paramsForCheckedTests(test : TTestNode; session : TTestSession) : String; override;
    function paramsForLoad() : String; override;
    function executableName() : String; override;

    function prepareToRunTests : TTestSession; override;
    procedure runTest(session : TTestSession; node : TTestNode; debug : boolean); override;
    procedure terminateTests(session: TTestSession); override;
    procedure finishTestRun(session : TTestSession); override;
  end;


implementation

{ TTestEngineDirectListener }

constructor TTestEngineDirectListener.create(listener: TTestListener; root: TTestNode);
begin
  inherited Create;
  FListener := listener;
  FRoot := root;
end;

function TTestEngineDirectListener.findTestInNode(test: TTest; node: TTestNode): TTestNode;
var
  child, t : TTestNode;
begin
  if node.Data = test then
    result := node
  else
  begin
    result := nil;
    for child in node.children do
    begin
      t := findTestInNode(test, child);
      if (t <> nil) then
        exit(t);
    end;
  end;
end;

function TTestEngineDirectListener.findNode(test : TTest; optional : boolean) : TTestNode;
begin
  result := findTestInNode(test, FRoot);
  if (result = nil) and not optional then
    raise Exception.create(Format(rs_IdeTester_Err_Node_Not_Found, [test.TestName])); // this really shouldn't happen
end;

function TTestEngineDirectListener.makeError(err : TTestFailure) : TTestError;
var
  src : String;
  line : integer;
begin
  result := TTestError.create;
  result.ExceptionClass := err.ExceptionClass.ClassName;
  result.ExceptionMessage := err.ExceptionMessage;
  readLocation(err.LocationInfo, src, line);
  result.SourceUnit := src;
  result.LineNumber := line;
end;

procedure TTestEngineDirectListener.StartTest(ATest: TTest);
begin
  FListener.StartTest(findNode(ATest));
end;

procedure TTestEngineDirectListener.EndTest(ATest: TTest);
begin
  FListener.EndTest(findNode(ATest));
end;

procedure TTestEngineDirectListener.StartTestSuite(ATestSuite: TTestSuite);
var
  node : TTestNode;
begin
  node := findNode(ATestSuite, true);
  if (node <> nil) then
    FListener.StartTestSuite(node);
end;

procedure TTestEngineDirectListener.EndTestSuite(ATestSuite: TTestSuite);
var
  node : TTestNode;
begin
  node := findNode(ATestSuite, true);
  if (node <> nil) then
    FListener.EndTestSuite(node);
end;

procedure TTestEngineDirectListener.AddFailure(ATest: TTest; AFailure: TTestFailure);
var
  err : TTestError;
begin
  err := makeError(aFailure);
  try
    FListener.TestFailure(findNode(ATest), err);
  finally
    err.Free;
  end;
end;

procedure TTestEngineDirectListener.AddError(ATest: TTest; AError: TTestFailure);
var
  err : TTestError;
begin
  err := makeError(aError);
  try
    FListener.TestError(findNode(ATest), err);
  finally
    err.Free;
  end;
end;

{ TTestSessionDirect }

constructor TTestSessionDirect.Create;
begin
  inherited Create;
  FTestResult := TTestResult.Create;
end;

destructor TTestSessionDirect.Destroy;
begin
  inherited Destroy;
end;

procedure TTestSessionDirect.skipTest(node: TTestNode);
begin
 if (node.data is TTestCase) then
   FTestResult.AddToSkipList(node.data as TTestCase);
end;

{ TTestEngineDirect }

procedure TTestEngineDirect.loadAllTests(factory : TNodeFactory; manual : boolean);
var
  test : TTestSuite;
  node : TTestNode;
begin
  test := GetTestRegistry;
  node := registerTestNode(factory, nil, test);
  BuildTree(factory, node, test);
end;

function TTestEngineDirect.registerTestNode(factory : TNodeFactory; parent: TTestNode; test: TTest): TTestNode;
begin
  result := factory(parent);
  if (parent <> nil) then
    parent.Children.add(result);
  result.Data := test;
  result.testName := test.TestName;
  result.SourceUnit := test.UnitName;
  result.testClassName := test.ClassName;
  result.checkState := tcsUnchecked;
  result.outcome := toNotRun;
end;

procedure TTestEngineDirect.BuildTree(factory : TNodeFactory; rootTest: TTestNode; aSuite: TTestSuite);
var
  test: TTestNode;
  i: integer;
begin
  for i := 0 to ASuite.ChildTestCount - 1 do
  begin
    if (ASuite.Test[i].TestName = '') and (ASuite.ChildTestCount = 1) then
      test := rootTest
    else
      test := registerTestNode(factory, rootTest, ASuite.Test[i]);

    if ASuite.Test[i] is TTestSuite then
      BuildTree(factory, test, TTestSuite(ASuite.Test[i]))
    else if TObject(ASuite.Test[i]).InheritsFrom(TTestDecorator) then
      BuildTree(factory, test, TTestSuite(TTestDecorator(ASuite.Test[i]).Test));
  end;
end;

function TTestEngineDirect.threadMode: TTestEngineThreadMode;
begin
  result := ttmEither;
end;

function TTestEngineDirect.canTerminate: boolean;
begin
  result := false;
end;

function TTestEngineDirect.doesReload: boolean;
begin
  result := false;
end;

function TTestEngineDirect.canDebug: boolean;
begin
  result := false;
end;

function TTestEngineDirect.paramsForTest(test: TTestNode): String;
begin
  result := 'n/a';
end;

function TTestEngineDirect.paramsForCheckedTests(test: TTestNode; session : TTestSession): String;
begin
  result := 'n/a';
end;

function TTestEngineDirect.paramsForLoad(): String;
begin
  result := 'n/a';
end;

function TTestEngineDirect.executableName(): String;
begin
  result := 'n/a';
end;

function TTestEngineDirect.prepareToRunTests: TTestSession;
begin
  result := TTestSessionDirect.Create;
end;

procedure TTestEngineDirect.runTest(session: TTestSession; node: TTestNode; debug : boolean);
var
  sess : TTestSessionDirect;
  listenerProxy : ITestListener;
begin
  listenerProxy := TTestEngineDirectListener.create(listener, node) as ITestListener;

  sess := session as TTestSessionDirect;
  try
    sess.FTestResult.AddListener(listenerProxy);
    if (node.data is TTestSuite) then
      listener.StartTestSuite(node);
    try
      (node.data as TTest).Run(sess.FTestResult);
    finally
      if (node.data is TTestSuite) then
        listener.EndTestSuite(node);
    end;
    sess.FTestResult.RemoveListener(listenerProxy);
  finally
    listener.EndRun(node);
  end;
end;

procedure TTestEngineDirect.terminateTests(session: TTestSession);
begin
  raise Exception.create(rs_IdeTester_Msg_NOT_SUPPORTED);
end;

procedure TTestEngineDirect.finishTestRun(session: TTestSession);
begin
  session.free;
end;

end.


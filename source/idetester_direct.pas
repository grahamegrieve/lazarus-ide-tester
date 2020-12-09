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
    FAllTests : TTestNodeList;
  public
    constructor create(listener : TTestListener; allTests : TTestNodeList);
    property listener : TTestListener read FListener write FListener;

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
    Function registerTestNode(factory : TNodeFactory; parent : TTestNode; test : TTest; name : String) : TTestNode;
    procedure BuildTree(factory : TNodeFactory; rootTest: TTestNode; aSuite: TTestSuite);
  public
    procedure loadAllTests(factory : TNodeFactory); override;
    function threadMode : TTestEngineThreadMode; override;
    function canTerminate : boolean; override;
    function doesReload : boolean; override;

    function prepareToRunTests : TTestSession; override;
    procedure runTest(session : TTestSession; node : TTestNode; allTests : TTestNodeList); override;
    procedure terminateTests; override;
    procedure finishTestRun(session : TTestSession); override;
  end;


implementation

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

procedure TTestEngineDirect.loadAllTests(factory : TNodeFactory);
var
  test : TTestSuite;
  node : TTestNode;
begin
  test := GetTestRegistry;
  node := registerTestNode(factory, nil, test, rsAllTests);
  BuildTree(factory, node, test);
end;

function TTestEngineDirect.registerTestNode(factory : TNodeFactory; parent: TTestNode; test: TTest; name: String): TTestNode;
begin
  result := factory(parent);
  if (parent <> nil) then
    parent.Children.add(result);
  result.Data := test;
  result.testName := test.TestName;
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
      test := registerTestNode(factory, rootTest, ASuite.Test[i], ASuite.Test[i].TestName);

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

function TTestEngineDirect.prepareToRunTests: TTestSession;
begin
  result := TTestSessionDirect.Create;
end;

procedure TTestEngineDirect.runTest(session: TTestSession; node: TTestNode; allTests : TTestNodeList);
var
  sess : TTestSessionDirect;
  listenerProxy : ITestListener;
begin
  listenerProxy := TTestEngineDirectListener.create(listener, allTests) as ITestListener;

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

procedure TTestEngineDirect.terminateTests;
begin
  raise Exception.create('not supported here');
end;

procedure TTestEngineDirect.finishTestRun(session: TTestSession);
begin
  session.free;
end;

end.


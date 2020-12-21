unit idetester_external;

{$MODE DELPHI}
interface

uses
  Classes, SysUtils, Process, Generics.Collections,
  idetester_base, idetester_runtime;

const
  BUF_SIZE = 2048; // Buffer size for reading the output in chunks

type
  { TTestNodeId }
  TTestNodeId = class (TObject)
  private
    FId: String;
    FRunning: boolean;
    FTestClassName: String;
    FTestName: String;
  public
    property id : String read FId write FId;
    property testClassName : String read FTestClassName write FTestClassName;
    property testName : String read FTestName write FTestName;
    property running : boolean read FRunning write FRunning;
  end;

  { TTestEngineExternalSession }

  TTestEngineExternalSession = class (TTestSession)
  private
    FProcess: TProcess;
    FSkipList : TTestNodeList;

    function saveSkipList : String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure skipTest(node : TTestNode); override;
    property Process : TProcess read FProcess write FProcess;
  end;

  TTesterTestEntry = class;
  TTesterTestEntryList = TObjectList<TTesterTestEntry>;

  { TTesterTestEntry }

  TTesterTestEntry = class (TObject)
  private
    FChildren : TTesterTestEntryList;
    FClassName : String;
    FClssName: String;
    FId: String;
    FTestName : String;
  public
    destructor Destroy; override;
    function addChild : TTesterTestEntry;
    property id : String read FId write FId;
    property clssName : String read FClssName write FClassName;
    property testName : String read FTestname write FTestName;
  end;

  { TTesterOutputProcessor }

  TTesterOutputProcessLineEvent = procedure (line : String) of object;

  TTesterOutputProcessor = class (TObject)
  private
    FProcess : TProcess;
    FEvent : TTesterOutputProcessLineEvent;
    FCarry : String;
    procedure processOutput(text : String);
  public
    constructor Create(process : TProcess; event : TTesterOutputProcessLineEvent);

    procedure Process;
    property Event : TTesterOutputProcessLineEvent read FEvent write FEvent;
  end;

  TTestEngineExternalOutputMode = (teomStarting, teomListing, teomRunning, teomDone);

  { TTestEngineExternal }


  TTestEngineExternal = class abstract (TTestEngine)
  private
    FOutputMode : TTestEngineExternalOutputMode;
    FIndent : integer;
    FTests : TTesterTestEntry;
    FTestStack : TTesterTestEntryList;

    FRunningTest : TTestNode;

    procedure processLine(line : String);
    procedure processEntry(line : String);
    procedure processRun(line : String);

    procedure clearTests;
    Function registerTestNode(factory : TNodeFactory; parent : TTestNode; test : TTesterTestEntry) : TTestNode;
    procedure BuildTree(factory : TNodeFactory; rootTest: TTestNode; suite: TTesterTestEntry);

    function findTestInNode(id : String; node : TTestNode) : TTestNode;
    function findNode(id : String) : TTestNode;
    procedure markTestHalted(node : TTestNode);
    procedure addBaseParams(st : TStringList);
  protected
    function runProgram(params : TStringList; debug : boolean) : TProcess; virtual; abstract;
    function autoLoad : boolean; virtual;
  public
    procedure loadAllTests(factory : TNodeFactory; manual : boolean); override;
    function threadMode : TTestEngineThreadMode; override;
    function canTerminate : boolean; override;
    function doesReload : boolean; override;
    function canDebug : boolean; override;
    function hasParameters : boolean; override;

    function prepareToRunTests : TTestSession; override;
    procedure runTest(session : TTestSession; node : TTestNode; debug : boolean); override;
    procedure terminateTests(session: TTestSession); override;
    procedure finishTestRun(session : TTestSession); override;
  end;

  { TTestEngineExternalCmdLine }

  TTestEngineExternalCmdLine = class (TTestEngineExternal)
  private
    FExecutable : String;
  protected
    function runProgram(params : TStringList; debug : boolean) : TProcess; override;
  public
    constructor Create(executable : String);
  end;

implementation

Function StringSplit(Const sValue, sDelimiter : String; Var sLeft, sRight: String) : Boolean;
Var
  iIndex : Integer;
  sA, sB : String;
Begin
  // Find the delimiter within the source string
  iIndex := Pos(sDelimiter, sValue);
  Result := iIndex <> 0;

  If Not Result Then
  Begin
    sA := sValue;
    sB := '';
  End
  Else
  Begin
    sA := Copy(sValue, 1, iIndex - 1);
    sB := Copy(sValue, iIndex + Length(sDelimiter), MaxInt);
  End;

  sLeft := sA;
  sRight := sB;
End;

function countIndent(s : String) : integer;
begin
  result := 0;
  while (result < length(s)) and (s[result+1] = ' ') do
    inc(result);
end;

{ TTesterOutputProcessor }

procedure TTesterOutputProcessor.processOutput(text: String);
var
  curr, s : String;
begin
  curr := FCarry + text;
  while curr.contains(#13#10) do
  begin
    StringSplit(curr, #13#10, s, curr);
    event(s);
  end;
  FCarry := curr;
end;

constructor TTesterOutputProcessor.Create(process: TProcess; event : TTesterOutputProcessLineEvent);
begin
  inherited Create;
  FEvent := event;
  FProcess := process;
end;

procedure TTesterOutputProcessor.Process;
var
  BytesRead    : longint;
  Buffer       : TBytes;
  s : String;
begin
  repeat
    SetLength(Buffer, BUF_SIZE);
    BytesRead := FProcess.Output.Read(Buffer, BUF_SIZE);
    processOutput(TEncoding.UTF8.GetString(Buffer, 0, BytesRead));
  until BytesRead = 0;
end;

{ TTestEngineExternalSession }

function TTestEngineExternalSession.saveSkipList: String;
var
  f : System.Text;
  n : TTestNode;
begin
  result := IncludeTrailingPathDelimiter(GetTempDir)+'skip-list.txt';
  assignFile(f, result);
  rewrite(f);
  for n in FSkipList do
    writeln(f, (n.data as TTestNodeId).id);
  CloseFile(f);
end;

constructor TTestEngineExternalSession.Create;
begin
  inherited create;
  FSkipList := TTestNodeList.create(false);
end;

destructor TTestEngineExternalSession.Destroy;
begin
  FSkipList.free;
  inherited Destroy;
end;

procedure TTestEngineExternalSession.skipTest(node: TTestNode);
begin
  FSkipList.add(node);
end;

{ TTesterTestEntry }

destructor TTesterTestEntry.Destroy;
begin
  FChildren.free;
  inherited Destroy;
end;

function TTesterTestEntry.addChild: TTesterTestEntry;
begin
  if FChildren = nil then
    FChildren := TTesterTestEntryList.create(true);
  result := TTesterTestEntry.create;
  FChildren.add(result);
end;

{ TTestEngineExternal }

procedure TTestEngineExternal.processLine(line: String);
begin
  case FOutputMode of
    teomStarting : if line = '-- Test List ---' then FOutputMode := teomListing;
    teomListing : ProcessEntry(line);
    teomRunning : ProcessRun(line);
    teomDone : // ignore;
  end;
end;

procedure TTestEngineExternal.processEntry(line: String);
var
  l, r, id : String;
  i : integer;
  te : TTesterTestEntry;
begin
  if (line = '-- End Test List ---') then
  begin
    FOutputMode := teomRunning;
    FreeAndNil(FTestStack);
  end
  else
  begin
    StringSplit(line, ':', l, r);
    StringSplit(l, '=', id, l);
    i := countIndent(id);
    id := id.trim;
    l := l.trim;
    r := r.trim;
    if (FIndent = 0) and (i = 0) then
    begin
      te := TTesterTestEntry.create;
      FTests := te;
      FTestStack := TTesterTestEntryList.create(false);
    end
    else
    begin
      while (i < FIndent) do
      begin
        FTestStack.delete(FTestStack.count - 1);
        dec(FIndent);
      end;
      te := FTestStack[FTestStack.count - 1].addChild;
    end;
    FIndent := i + 1;
    FTestStack.Add(te);
    te.id := id;
    te.clssName := l;
    te.testName := r;
  end;
end;

procedure TTestEngineExternal.processRun(line: String);
var
  l, r, id : String;
  test : TTestNode;
  err : TTestError;
begin
  StringSplit(line, ':', id, r);
  test := findNode(id);
  StringSplit(r.trim, ' ', l, r);
  if (l = 'start') then
  begin
    (test.Data as TTestNodeId).running := true;
    if test.hasChildren then
      listener.StartTestSuite(test)
    else
      listener.StartTest(test);

  end
  else if (l = 'end') then
  begin
    (test.Data as TTestNodeId).running := false;
    if test.hasChildren then
      listener.EndTestSuite(test)
    else
      listener.EndTest(test);
  end
  else if (l = 'fail') then
  begin
    StringSplit(r.trim, ' ', l, r);
    err := TTestError.create;
    try
      err.ExceptionClass := l;
      err.ExceptionMessage := r;
      listener.TestFailure(test, err);
    finally
      err.free;
    end;
  end
  else if (l = 'error') then
  begin
    StringSplit(r.trim, ' ', l, r);
    err := TTestError.create;
    try
      err.ExceptionClass := l;
      err.ExceptionMessage := r;
      listener.TestError(test, err);
    finally
      err.free;
    end;
  end
  // else ignore it
end;

procedure TTestEngineExternal.clearTests;
begin
  FOutputMode := teomStarting;
  FIndent := 0;
  FreeAndNil(FTests);
  FreeAndNil(FTestStack);

end;

procedure TTestEngineExternal.loadAllTests(factory: TNodeFactory; manual : boolean);
var
  params : TStringList;
  process : TProcess;
  p : TTesterOutputProcessor;
  node : TTestNode;
begin
  if manual or autoLoad then
  begin
    clearTests;
    params := TStringList.create;
    try
      addBaseParams(params);
      params.add('-'+FPC_MAGIC_COMMAND);
      process := runProgram(params, false);
      try
        if process <> nil then
        begin
          p := TTesterOutputProcessor.create(process, processLine);
          try
            p.process;
            node := registerTestNode(factory, nil, FTests);
            BuildTree(factory, node, FTests);
          finally
            p.free;
          end;
        end;
      finally
        process.free;
      end;
    finally
      params.free;
    end;
  end;
end;

function TTestEngineExternal.registerTestNode(factory : TNodeFactory; parent: TTestNode; test: TTesterTestEntry): TTestNode;
var
  nid : TTestNodeId;
begin
  result := factory(parent);
  if (parent <> nil) then
    parent.Children.add(result);
  nid := TTestNodeId.create;
  nid.id := test.id;
  nid.testClassName := test.clssName;
  nid.testName := test.testName;
  result.Data := nid;
  result.ownsData := true;
  result.testName := test.TestName;
  result.testClassName := test.clssName;
  result.checkState := tcsUnchecked;
  result.outcome := toNotRun;
end;

procedure TTestEngineExternal.BuildTree(factory : TNodeFactory; rootTest: TTestNode; suite: TTesterTestEntry);
var
  test: TTestNode;
  entry : TTesterTestEntry;
begin
  if suite.FChildren <> nil then
    for entry in suite.FChildren do
    begin
      test := registerTestNode(factory, rootTest, entry);
      BuildTree(factory, test, entry);
  end;
end;

function TTestEngineExternal.findTestInNode(id: String; node: TTestNode): TTestNode;
var
  child, t : TTestNode;
begin
  if (node.Data as TTestNodeId).id = id then
    result := node
  else
  begin
    result := nil;
    for child in node.children do
    begin
      t := findTestInNode(id, child);
      if (t <> nil) then
        exit(t);
    end;
  end;
end;

function TTestEngineExternal.findNode(id: String): TTestNode;
begin
  result := findTestInNode(id, FRunningTest);
  if (result = nil) then
    raise Exception.create('Test "'+id+'" not found!'); // this really shouldn't happen
end;

procedure TTestEngineExternal.markTestHalted(node: TTestNode);
var
  child : TTestNode;
begin
  if not node.hasChildren then
  begin
    if (node.Data as TTestNodeId).running then
      listener.TestHalt(node);
  end
  else
  begin
    for child in node.children do
      markTestHalted(child);
    if (node.Data as TTestNodeId).running then
      listener.EndTestSuite(node);
  end;
end;

procedure TTestEngineExternal.addBaseParams(st: TStringList);
var
  l , r : String;
begin
  r := parameters;
  while (r <> '') do
  begin
    StringSplit(r, ' ', l, r);
    l := l.trim;
    if (l <> '') then
      st.add(l);
  end;
end;

function TTestEngineExternal.autoLoad: boolean;
begin
  result := true;
end;

function TTestEngineExternal.threadMode: TTestEngineThreadMode;
begin
  result := ttmOtherThread;
end;

function TTestEngineExternal.canTerminate: boolean;
begin
  result := true;
end;

function TTestEngineExternal.doesReload: boolean;
begin
  result := true;

end;

function TTestEngineExternal.canDebug: boolean;
begin
  result := false;
end;

function TTestEngineExternal.hasParameters: boolean;
begin
  Result := true;
end;

function TTestEngineExternal.prepareToRunTests: TTestSession;
begin
  result := TTestEngineExternalSession.create; // this will hold the external process
end;

procedure TTestEngineExternal.runTest(session: TTestSession; node: TTestNode; debug: boolean);
var
  sess : TTestEngineExternalSession;
  params : TStringList;
  p : TTesterOutputProcessor;
begin
  try
    clearTests;
    FRunningTest := node;
    params := TStringList.create;
    try
      addBaseParams(params);
      params.add('-'+FPC_MAGIC_COMMAND);
      params.add('-run');
      params.add((node.Data as TTestNodeId).id);
      sess := session as TTestEngineExternalSession;
      if sess.FSkipList.count > 0 then
      begin
        params.add('-skip');
        params.add(sess.saveSkipList);
      end;
      sess.Process := runProgram(params, debug);
      try
        p := TTesterOutputProcessor.create(Sess.Process, processLine);
        try
          p.Process;
        finally
          p.free;
        end;
        // todo: check that the list is in sync?
        // todo: clean up in case of termination
      finally
        sess.Process.free;
        sess.Process := nil;
      end;
    finally
      params.free;
    end;
    // any tests still running, mark them as halted.
    markTestHalted(node);
  finally
    listener.EndRun(node);
  end;
end;

procedure TTestEngineExternal.terminateTests(session: TTestSession);
var
  sess : TTestEngineExternalSession;
begin
  sess := session as TTestEngineExternalSession;
  if sess.Process <> nil then
    sess.Process.Terminate(2);
end;

procedure TTestEngineExternal.finishTestRun(session: TTestSession);
begin

end;

{ TTestEngineExternalCmdLine }

constructor TTestEngineExternalCmdLine.Create(executable: String);
begin
  inherited Create;
  FExecutable := executable;
end;

function TTestEngineExternalCmdLine.runProgram(params: TStringList; debug : boolean): TProcess;
begin
  result := TProcess.create(nil);
  result.Executable := FExecutable;
  result.Parameters := params;
  result.ShowWindow := swoHIDE;
  result.Options := [poUsePipes];
  result.Execute;
end;

end.


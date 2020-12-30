unit idetester_external;

{$MODE DELPHI}
interface

uses
  Classes, SysUtils, Process, Generics.Collections,
  simpleipc,
  Forms, Dialogs,
  idetester_strings, idetester_base, idetester_runtime;

const
  CONSOLE_TIMEOUT = 4000; // how long we wait for input to start
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

  { TTesterOutputListener }

  TTesterOutputListenerEvent = procedure (line : String; var finished : boolean) of object;

  TTesterOutputListener = class (TObject)
  private
    FEvent : TTesterOutputListenerEvent;
    FServer : TSimpleIPCServer;
  public
    constructor Create(event : TTesterOutputListenerEvent);
    destructor Destroy; override;

    function id : String;
    procedure listen(process: TProcess);
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
    FSourceUnit: String;
    FTestName : String;
  public
    destructor Destroy; override;
    function addChild : TTesterTestEntry;
    property id : String read FId write FId;
    property clssName : String read FClssName write FClassName;
    property testName : String read FTestname write FTestName;
    property sourceUnit : String read FSourceUnit write FSourceUnit;
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

    procedure processLine(line : String; var finished : boolean);
    procedure processEntry(line : String);
    procedure processRun(line : String);

    procedure clearTests;
    Function registerTestNode(testList : TTestNodeList; parent : TTestNode; test : TTesterTestEntry) : TTestNode;
    procedure BuildTree(testList : TTestNodeList; rootTest: TTestNode; suite: TTesterTestEntry);

    function findTestInNode(id : String; node : TTestNode) : TTestNode;
    function findNode(id : String) : TTestNode;
    procedure markTestHalted(node : TTestNode);
    procedure addBaseParams(st : TStringList);
  protected
    FIPCServer : TTesterOutputListener;
    function runProgram(session : TTestEngineExternalSession; params : TStringList) : TProcess; virtual; abstract;
    function makeSession : TTestEngineExternalSession; virtual;
    function autoLoad : boolean; virtual;
    procedure FinishTests; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure loadAllTests(testList : TTestNodeList; manual : boolean); override;
    function threadMode : TTestEngineThreadMode; override;
    function canTerminate : boolean; override;
    function doesReload : boolean; override;
    function canParameters : boolean; override;

    function prepareToRunTests : TTestSession; override;
    procedure runTest(session : TTestSession; node : TTestNode); override;
    procedure terminateTests(session: TTestSession); override;
    procedure finishTestRun(session : TTestSession); override;
  end;

  { TTestEngineExternalCmdLine }

  TTestEngineExternalCmdLine = class (TTestEngineExternal)
  private
    FExecutable : String;
  protected
    function runProgram(session : TTestEngineExternalSession; params : TStringList) : TProcess; override;
  public
    constructor Create(executable : String);
    function canDebug : boolean; override;
    function canStart : boolean; override;
  end;

Function StringSplit(Const sValue, sDelimiter : String; Var sLeft, sRight: String) : Boolean;

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

{ TTesterOutputListener }

constructor TTesterOutputListener.Create(event : TTesterOutputListenerEvent);
begin
  inherited Create;
  FEvent := event;
  FServer := TSimpleIPCServer.create(nil);
  FServer.Threaded := true;
  FServer.Global := true;
  FServer.ServerID := 'fpc-'+IntToStr(GetTickCount64);
  FServer.Active := true;
end;

destructor TTesterOutputListener.Destroy;
begin
  FServer.active := false;
  FServer.Free;
  inherited Destroy;
end;

function TTesterOutputListener.id: String;
begin
  result := FServer.ServerID;
end;

procedure TTesterOutputListener.listen(process: TProcess);
var
  fin : boolean;
begin
  fin := false;
  while process.Active and not fin do
  begin
    while FServer.PeekMessage(5, true) do
    begin
      FEvent(FServer.StringMessage, fin);
      if (fin) then
      begin
        break;
      end;
    end;
  end;
  process.Terminate(1);
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

procedure TTestEngineExternal.processLine(line: String; var finished : boolean);
begin
  case FOutputMode of
    teomStarting : if line = '-- Test List ---' then FOutputMode := teomListing;
    teomListing : ProcessEntry(line);
    teomRunning : ProcessRun(line);
    teomDone : finished := true;
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
    if te.testName.contains('@@') then
    begin
      te.SourceUnit := te.testName.Substring(te.testName.IndexOf('@@')+2).trim;
      te.testName := te.testName.Substring(0, te.testName.IndexOf('@@')).trim;
    end;
  end;
end;

procedure TTestEngineExternal.processRun(line: String);
var
  l, r, id, src, s : String;
  ln : integer;
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
      if err.ExceptionMessage.contains('@@') then
      begin
        s := err.ExceptionMessage;
        readLocation(s.Substring(s.IndexOf('@@')+2).trim, src, ln);
        err.SourceUnit := src;
        err.LineNumber := ln;
        err.ExceptionMessage := err.ExceptionMessage.Substring(0, err.ExceptionMessage.IndexOf('@@')).trim;
      end;
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
      if err.ExceptionMessage.contains('@@') then
      begin
        readLocation(err.ExceptionMessage.Substring(err.ExceptionMessage.IndexOf('@@')+2).trim, src, ln);
        err.SourceUnit := src;
        err.LineNumber := ln;
        err.ExceptionMessage := err.ExceptionMessage.Substring(0, err.ExceptionMessage.IndexOf('@@')).trim;
      end;
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

procedure TTestEngineExternal.loadAllTests(testList : TTestNodeList; manual : boolean);
var
  params : TStringList;
  process : TProcess;
  session : TTestEngineExternalSession;
  node : TTestNode;
begin
  if manual or autoLoad then
  begin
    clearTests;
    session := makeSession;
    try
      params := TStringList.create;
      try
        addBaseParams(params);
        params.add('-'+FPC_MAGIC_COMMAND);
        params.add('-server');
        params.add(FIPCServer.id);
        process := runProgram(session, params);
        try
          if process <> nil then
          begin
            FIPCServer.listen(process);
            if FTests <> nil then
            begin
              node := registerTestNode(testList, nil, FTests);
              BuildTree(testList, node, FTests);
            end
            else
              ShowMessage(format(rs_IdeTester_Err_No_Load_Tests, [helpUrl]));
          end;
        finally
          process.free;
        end;
      finally
        params.free;
      end;
    finally
      session.Free;
    end;
  end;
end;

function TTestEngineExternal.registerTestNode(testList : TTestNodeList; parent: TTestNode; test: TTesterTestEntry): TTestNode;
var
  nid : TTestNodeId;
begin
  result := TTestNode.create(parent);
  testList.add(result);
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
  result.SourceUnit := test.sourceUnit;
  result.checkState := tcsUnchecked;
  result.outcome := toNotRun;
end;

procedure TTestEngineExternal.BuildTree(testList : TTestNodeList; rootTest: TTestNode; suite: TTesterTestEntry);
var
  test: TTestNode;
  entry : TTesterTestEntry;
begin
  if suite.FChildren <> nil then
    for entry in suite.FChildren do
    begin
      test := registerTestNode(testList, rootTest, entry);
      BuildTree(testList, test, entry);
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
    raise Exception.create(Format(rs_IdeTester_Err_Node_Not_Found, [id])); // this really shouldn't happen
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
  r := settings.parameters;
  while (r <> '') do
  begin
    StringSplit(r, ' ', l, r);
    l := l.trim;
    if (l <> '') then
      st.add(l);
  end;
end;

function TTestEngineExternal.makeSession: TTestEngineExternalSession;
begin
  result := TTestEngineExternalSession.create;
end;

function TTestEngineExternal.autoLoad: boolean;
begin
  result := true;
end;

procedure TTestEngineExternal.FinishTests;
begin
  // nothing
end;

constructor TTestEngineExternal.Create;
begin
  inherited create;
  FIPCServer := TTesterOutputListener.create(processLine);
end;

destructor TTestEngineExternal.Destroy;
begin
  FIPCServer.Free;
  inherited Destroy;
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

function TTestEngineExternal.canParameters: boolean;
begin
  Result := true;
end;

function TTestEngineExternal.prepareToRunTests: TTestSession;
begin
  result := TTestEngineExternalSession.create; // this will hold the external process
end;

procedure TTestEngineExternal.runTest(session: TTestSession; node: TTestNode);
var
  sess : TTestEngineExternalSession;
  params : TStringList;
begin
  sess := session as TTestEngineExternalSession;
  try
    clearTests;
    FRunningTest := node;
    params := TStringList.create;
    try
      addBaseParams(params);
      params.add('-'+FPC_MAGIC_COMMAND);
      params.add('-server');
      params.add(FIPCServer.id);
      params.add('-run');
      params.add((node.Data as TTestNodeId).id);
      if sess.FSkipList.count > 0 then
      begin
        params.add('-skip');
        params.add(sess.saveSkipList);
      end;
      sess.Process := runProgram(sess, params);
      try
        if sess.process <> nil then
          FIPCServer.listen(sess.process);
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
    self.listener.EndRun(node);
  end;
  FinishTests;
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
  //  nothing here
end;

{ TTestEngineExternalCmdLine }

constructor TTestEngineExternalCmdLine.Create(executable: String);
begin
  inherited Create;
  FExecutable := executable;
end;

function TTestEngineExternalCmdLine.runProgram(session : TTestEngineExternalSession; params: TStringList): TProcess;
begin
  result := TProcess.create(nil);
  result.Executable := FExecutable;
  result.CurrentDirectory := ExtractFileDir(FExecutable);
  result.Parameters := params;
  result.ShowWindow := swoHIDE;
  result.Options := [];
  result.Execute;
end;

function TTestEngineExternalCmdLine.canDebug: boolean;
begin
  result := false;
end;

function TTestEngineExternalCmdLine.canStart: boolean;
begin
  result := FExecutable <> '';
end;

end.


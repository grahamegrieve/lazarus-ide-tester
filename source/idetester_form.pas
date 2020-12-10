unit idetester_form;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, ActnList, Menus, LCLIntf, laz.VirtualTrees, ClipBrd,
  Generics.Collections,
  idetester_options, idetester_base, idetester_strings, idetester_direct, idetester_ini;

const
  KILL_TIME_DELAY = 5000;
  CAN_CONFIGURE = true;

type
  // for the virtual tree

  { TTestNodeData }
  TTestNodeData = record
    node : TTestNode;
  end;
  PTestNodeData = ^TTestNodeData;

  TTestEventKind = (tekStartSuite, tekFinishSuite, tekStart, tekEnd, tekFail, tekError, tekEndRun);

  { TTestEvent }

  TTestEvent = class (TObject)
  private
    node : TTestNode;
    event : TTestEventKind;
    excMessage : String;
    excClass : String;
  end;
  TTestEventQueue = class (TObjectList<TTestEvent>);

  TTesterForm = class;

  { TTestThread }

  TTestThread = class (TThread)
  private
    FTester : TTesterForm;
  protected
    procedure execute; override;
  public
    constructor Create(tester : TTesterForm);
  end;

  { TTesterFormListener }

  TTesterFormListener  = class (TTestListener)
  private
    FTester : TTesterForm;
  public
    constructor Create(tester : TTesterForm);

    procedure StartTest(test: TTestNode); override;
    procedure EndTest(test: TTestNode); override;
    procedure TestFailure(test: TTestNode; fail: TTestError); override;
    procedure TestError(test: TTestNode; error: TTestError); override;
    procedure StartTestSuite(test: TTestNode); override;
    procedure EndTestSuite(test: TTestNode); override;
    procedure EndRun(test: TTestNode); override;
  end;

  { TTesterForm }
  TTesterForm = class(TForm)
    actionTestConfigure: TAction;
    actionTestReload: TAction;
    actionTestSelectAll: TAction;
    actionTestUnselectAll: TAction;
    actionTestReset: TAction;
    actionTestCopy: TAction;
    actionTestStop: TAction;
    actionTestRunFailed: TAction;
    actTestRunAll1: TAction;
    actTestRunSelected: TAction;
    ActionList1: TActionList;
    ilMain: TImageList;
    ilOutcomes: TImageList;
    lblStatus: TLabel;
    MenuItem4: TMenuItem;
    Panel2: TPanel;
    Timer1: TTimer;
    tbBtnReload: TToolButton;
    tbBtnReloadSep: TToolButton;
    tbBtnConfigureSep: TToolButton;
    tbBtnConfigure: TToolButton;
    tvTests: TLazVirtualStringTree;
    MenuItem3: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    pbBar: TPaintBox;
    Panel1: TPanel;
    pmTests: TPopupMenu;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure actionTestConfigureExecute(Sender: TObject);
    procedure actionTestCopyExecute(Sender: TObject);
    procedure actionTestResetExecute(Sender: TObject);
    procedure actionTestRunFailedExecute(Sender: TObject);
    procedure actionTestSelectAllExecute(Sender: TObject);
    procedure actionTestStopExecute(Sender: TObject);
    procedure actionTestUnselectAllExecute(Sender: TObject);
    procedure actTestRunAll1Execute(Sender: TObject);
    procedure actTestRunSelectedExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pbBarPaint(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure tvTestsAddToSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvTestsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure tvTestsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure tvTestsRemoveFromSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
  private
    FEngine : TTestEngine;
    FStore : TTestSettingsProvider;

    FTestInfo : TTestNodeList;
    FRunningTest : TTestNode;
    FTestsTotal, FTestsCount, FFailCount, FErrorCount : cardinal;
    FWantStop : boolean;
    FSelectedNode : TTestNode;
    FSession : TTestSession;
    FLock : TRTLCriticalSection;
    FIncoming : TTestEventQueue;
    FShuttingDown : boolean;
    FThread : TTestThread;
    FKillTime : cardinal;

    // -- utils ----
    function tn(p: PVirtualNode): TTestNode;
    procedure UpdateTotals;

    // -- init ----
    procedure LoadTree;
    function nodeFactory(parent : TTestNode) : TTestNode;
    procedure refreshNode(test : TTestNode);
    procedure loadState;
    procedure saveState;

    // -- checkbox mgmt ----
    procedure setTestState(node: TTestNode; state : TTestCheckState);
    procedure checkStateOfChildren(node: TTestNode);

    // -- running tests ----
    procedure setDoExecute(node: TTestNode);
    procedure setDoExecuteParent(node: TTestNode);
    procedure setActionStatus(running : boolean);
    procedure StartTestRun;
    procedure DoExecuteTests; // in alternative thread
    procedure FinishTestRun;
    procedure queueEvent(test: TTestNode; event : TTestEventKind; msg, clssName : String);
    procedure killRunningTests;
    procedure EndTestSuite(ATest: TTestNode);
  public
    // these two must be set before the Form is shown
    property engine : TTestEngine read FEngine write FEngine;
    property store : TTestSettingsProvider read FStore write FStore; // either an ini in AppConfig, or stored in the project settings somewhere?
  end;

var TesterForm : TTesterForm;

implementation

{$R *.lfm}

{ TTesterFormListener }

constructor TTesterFormListener.Create(tester: TTesterForm);
begin
  inherited Create;
  FTester := tester;
end;

procedure TTesterFormListener.StartTest(test: TTestNode);
begin
  test.start;
  FTester.queueEvent(test, tekStart, '', '');
end;

procedure TTesterFormListener.EndTest(test: TTestNode);
begin
  test.finish;
  FTester.queueEvent(test, tekEnd, '', '');
end;

procedure TTesterFormListener.TestFailure(test: TTestNode; fail: TTestError);
begin
  FTester.queueEvent(test, tekFail, fail.ExceptionMessage, fail.ExceptionClass);
end;

procedure TTesterFormListener.TestError(test: TTestNode; error: TTestError);
begin
  FTester.queueEvent(test, tekError, error.ExceptionMessage, error.ExceptionClass);
end;

procedure TTesterFormListener.StartTestSuite(test: TTestNode);
begin
  FTester.queueEvent(test, tekStartSuite, '', '');
end;

procedure TTesterFormListener.EndTestSuite(test: TTestNode);
begin
  FTester.queueEvent(test, tekFinishSuite, '', '');
end;

procedure TTesterFormListener.EndRun(test: TTestNode);
begin
  FTester.queueEvent(test, tekEndRun, '', '');
end;

{ TTestThread }

procedure TTestThread.execute;
begin
  try
    FTester.DoExecuteTests;
  except
  end;
end;

constructor TTestThread.Create(tester: TTesterForm);
begin
  FTester := tester;
  inherited Create(false);
end;

{ TTesterForm }

procedure TTesterForm.FormCreate(Sender: TObject);
begin
  FTestInfo := TTestNodeList.create;
  FTestInfo.OwnsObjects := true;
  InitCriticalSection(FLock);
  FIncoming := TTestEventQueue.create;
  FIncoming.OwnsObjects := false;

  pbBarPaint(pbBar);
  setActionStatus(false);
end;

procedure TTesterForm.FormDestroy(Sender: TObject);
begin
  FShuttingDown := true;
  saveState;
  FTestInfo.Free;
  FIncoming.Free;
  DoneCriticalSection(FLock);
  FEngine.Free;
  FStore.Free;
end;

procedure TTesterForm.FormShow(Sender: TObject);
begin
  if store = nil then
    store := TTestIniSettingsProvider.create(IncludeTrailingPathDelimiter(getAppConfigDir(true))+'fhir-tests-settings.ini');
  if engine = nil then
    engine := TTestEngineDirect.create;

  engine.listener := TTesterFormListener.create(self);
  if not engine.doesReload then
  begin
    tbBtnReload.visible := false;
    tbBtnReloadSep.visible := false;
  end;
  if not CAN_CONFIGURE then // todo - should this be the engine that decides this?
  begin
    tbBtnConfigure.visible := false;
    tbBtnConfigureSep.visible := false;
  end;

  LoadTree;
  LoadState;
  UpdateTotals;
end;

procedure TTesterForm.setActionStatus(running: boolean);
begin
  actionTestCopy.Enabled := not running;
  actionTestSelectAll.Enabled := not running;
  actionTestUnselectAll.Enabled := not running;
  actionTestReset.Enabled := not running;
  actionTestStop.Enabled := running;
  actionTestRunFailed.Enabled := not running;
  actTestRunAll1.Enabled := not running;
  actTestRunSelected.Enabled := not running;
end;

// -- Tree Management ----------------------------------------------------------

function TTesterForm.tn(p : PVirtualNode) : TTestNode;
var
  pd : PTestNodeData;
begin
  pd := tvTests.GetNodeData(p);
  result := pd.node;
end;

procedure TTesterForm.UpdateTotals;
var
  tc, pc, ec, fc, nr, cc : cardinal;
  tn : TTestNode;
  s : String;
begin
  if ((FRunningTest <> nil) or FShuttingDown) then
    exit;

  tc := 0;
  ec := 0;
  fc := 0;
  nr := 0;
  cc := 0;
  pc := 0;
  for tn in FTestInfo do
  begin
    if not tn.hasChildren then
    begin
      inc(tc);
      if tn.checkState = tcsChecked then
        inc(cc);
      case tn.outcome of
        toPass : inc(pc);
        toFail : inc(ec);
        toError : inc(fc);
      else //  toNotRun
        inc(nr);
      end;
    end;
  end;

  s := inttostr(tc)+ ' Tests: ';
  s := s + inttostr(cc)+ ' checked';
  s := s + ', '+inttostr(pc)+' passed';
  if (fc + ec > 0) then
    s := s + ', '+inttostr(fc+ec)+' failed';
  if (ec > 0) then
    s := s + ' ('+inttostr(ec)+' errors)';
  if (nr > 0) then
    s := s + ', '+inttostr(nr)+' not run';
  if (ec + fc + nr = 0) then
    s := s + ' - All OK✓';
  lblStatus.caption := s;
end;

procedure TTesterForm.tvTestsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
begin
  ImageIndex := ord(tn(node).outcome);
end;

procedure TTesterForm.tvTestsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
begin
  CellText := tn(node).description;
end;

function TTesterForm.nodeFactory(parent : TTestNode) : TTestNode;
var
  p : PTestNodeData;
begin
  result := TTestNode.create(parent);
  FTestInfo.add(result);

  if (parent <> nil) then
    result.Node := tvTests.AddChild(parent.Node)
  else
    result.Node := tvTests.AddChild(nil);

  p := tvTests.GetNodeData(result.Node);
  p.Node := result;
  result.Node.CheckType := ctTriStateCheckBox;
  result.Node.CheckState := csUncheckedNormal;
  result.OnUpdate := refreshNode;
end;

procedure TTesterForm.refreshNode(test: TTestNode);
begin
  tvTests.InvalidateNode(test.node);
end;

procedure TTesterForm.LoadTree;
begin
  tvTests.NodeDataSize := sizeof(pointer);
  engine.loadAllTests(nodeFactory);
  tvTests.Selected[FTestInfo[0].node] := true;
  tvTests.Expanded[FTestInfo[0].node] := true;
end;

// --- Test Selection Management -----------------------------------------------

procedure TTesterForm.tvTestsAddToSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  FSelectedNode := tn(node);
  if FSelectedNode.hasChildren then
  begin
    actTestRunSelected.caption := 'Run these tests';
    actionTestSelectAll.caption := 'Check these tests';
    actionTestUnselectAll.caption := 'Uncheck these tests';
    actionTestSelectAll.hint := 'Check selected tests + children';
    actionTestUnselectAll.hint := 'Uncheck selected tests + children';
    actionTestCopy.hint := 'Copy results to clipboard for selected tests';
    actTestRunSelected.caption := 'Run Selected Test + children';
  end
  else
  begin
    actTestRunSelected.caption := 'Run this test';
    actionTestSelectAll.caption := 'Check this test';
    actionTestUnselectAll.caption := 'Uncheck this test';
    actionTestSelectAll.hint := 'Check selected test';
    actionTestUnselectAll.hint := 'Uncheck selected test';
    actionTestCopy.hint := 'Copy results to clipboard for selected test';
    actTestRunSelected.caption := 'Run Selected Test';
  end;
  UpdateTotals;
end;

procedure TTesterForm.tvTestsRemoveFromSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  FSelectedNode := nil;
  UpdateTotals;
end;


procedure TTesterForm.loadState;
var
  ss, so : String;
  i : integer;
  ti : TTestNode;
begin
  tvTests.BeginUpdate;

  if (store.read('Count', '0') = inttostr(FTestInfo.Count)) then
  begin
    ss := store.Read('States', '');
    so := store.Read('Outcomes', '');
    for i := 0 to FTestInfo.count - 1 do
    begin
      ti := FTestInfo[i];
      if i < length(ss) then
        ti.checkState := TTestCheckState(StrToInt(ss[i+1]))
      else
        ti.checkState := tcsUnchecked;

      if i < length(so) then
      begin
        ti.outcome := TTestOutcome(StrToInt(so[i+1]));
        if not (ti.outcome in [toSomePass, toPass, toFail, toError]) then
          ti.outcome := toNotRun;
      end
      else
        ti.outcome := toNotRun;
    end;
  end;

  tvTests.EndUpdate;
end;

procedure TTesterForm.saveState;
var
  bs, bo : TStringBuilder;
  ti : TTestNode;
begin
  bs := TStringBuilder.create;
  bo := TStringBuilder.create;
  try
    for ti in FTestInfo do
    begin
      bs.append(inttostr(ord(ti.checkState)));
      bo.append(inttostr(ord(ti.outcome)));
    end;
    store.save('Count', inttostr(FTestInfo.Count));
    store.save('States', bs.toString);
    store.save('Outcomes', bo.toString);
  finally
    bo.Free;
    bs.Free;
  end;
end;

procedure TTesterForm.setTestState(node: TTestNode; state: TTestCheckState);
var
  ti : TTestNode;
begin
  node.checkState := state;
  for ti in node.Children do
    setTestState(ti, state);
end;

procedure TTesterForm.checkStateOfChildren(node: TTestNode);
var
  state : TTestCheckState;
  ti : TTestNode;
begin
  if node <> nil then
  begin
    ti := node.Children[0]; // will never be empty
    state := ti.checkState;
    for ti in node.Children do;
      if state <> ti.checkState then
        state := tcsMixed;
    node.checkState := state;
    checkStateOfChildren(node.Parent);
  end;
end;

procedure TTesterForm.actionTestSelectAllExecute(Sender: TObject);
var
  node : TTestNode;
begin
  node := FSelectedNode;
  if (node <> nil) then
  begin
    setTestState(node, tcsChecked);
    checkStateOfChildren(node.Parent);
  end;
  UpdateTotals;
end;

procedure TTesterForm.actionTestUnselectAllExecute(Sender: TObject);
var
  node : TTestNode;
begin
  node := FSelectedNode;
  if (node <> nil) then
  begin
    setTestState(node, tcsUnchecked);
    checkStateOfChildren(node.Parent);
  end;
  UpdateTotals;
end;

// ---- Test Execution Control -------------------------------------------------

procedure TTesterForm.actTestRunAll1Execute(Sender: TObject);
var
  ti : TTestNode;
begin
  FTestsTotal := 0;
  for ti in FTestInfo do
  begin
    ti.execute := ti.CheckState <> tcsUnchecked;
    if (ti.execute) and (not ti.hasChildren) then
      inc(FTestsTotal);
  end;

  if FTestsTotal > 0 then
  begin
    FRunningTest := FTestInfo[0];
    StartTestRun;
    FThread := TTestThread.create(self);
    FThread.FreeOnTerminate := true;
  end
  else
    ShowMessage('No Tests Checked');
end;

procedure TTesterForm.actTestRunSelectedExecute(Sender: TObject);
var
  node, ti : TTestNode;
begin
  for ti in FTestInfo do
    ti.execute := false;
  Node := FSelectedNode;
  if node <> nil then
  begin
    setDoExecute(node);
    setDoExecuteParent(node.Parent);

    FTestsTotal := 0;
    for ti in FTestInfo do
      if ti.execute and (not ti.hasChildren) then
        inc(FTestsTotal);
    FRunningTest := node;
    StartTestRun;
    FThread := TTestThread.create(self);
    FThread.FreeOnTerminate := true;
  end;
end;

procedure TTesterForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FRunningTest = nil;
end;

procedure TTesterForm.actionTestRunFailedExecute(Sender: TObject);
var
  ti : TTestNode;
begin
  for ti in FTestInfo do
  begin
    ti.execute := (ti.outcome in [toFail, toError]) and (ti.CheckState <> tcsUnchecked);
    if ti.execute then
      setDoExecuteParent(ti.parent);
  end;

  FTestsTotal := 0;
  for ti in FTestInfo do
    if ti.execute and (not ti.hasChildren) then
      inc(FTestsTotal);

  if FTestsTotal > 0 then
  begin
    FRunningTest := FTestInfo[0];
    StartTestRun;
    FThread := TTestThread.create(self);
    FThread.FreeOnTerminate := true;
  end
  else
    ShowMessage('No Failed Tests');
end;

procedure TTesterForm.actionTestResetExecute(Sender: TObject);
var
  ti : TTestNode;
begin
  for ti in FTestInfo do
  begin
    if ti.outcome <> toNotRun then
      ti.outcome := toNotRun;
  end;
  UpdateTotals;
end;

procedure TTesterForm.actionTestStopExecute(Sender: TObject);
begin
  FWantStop := true;
  if engine.canTerminate then
  begin
    FKillTime := GetTickCount64 + KILL_TIME_DELAY;
    actionTestStop.ImageIndex := 14;
  end;
end;

procedure TTesterForm.setDoExecute(node : TTestNode);
var
  child : TTestNode;
begin
  node.execute := true;

  for child in node.Children do
    setDoExecute(child);
end;

procedure TTesterForm.setDoExecuteParent(node: TTestNode);
begin
  if node <> nil then
  begin
    node.execute := true;
    setDoExecuteParent(node.parent);
  end;
end;

// -- Actually executing the tests ---------------------------------------------

procedure TTesterForm.pbBarPaint(Sender: TObject);
var
  msg: string;
  OldStyle: TBrushStyle;
begin
  with (Sender as TPaintBox) do
  begin
    Canvas.Lock;
    Canvas.Brush.Color := clSilver;
    Canvas.Rectangle(0, 0, Width, Height);
    Canvas.Font.Color := clWhite;
    if FTestsTotal > 0 then
    begin
      if FErrorCount > 0 then
        Canvas.Brush.Color := clRed
      else if FFailCount > 0 then
        Canvas.Brush.Color := clMaroon
      else
        Canvas.Brush.Color := clGreen;
      Canvas.Rectangle(0, 0, round(FTestsCount / (FTestsTotal) * Width), Height);
      msg := Format(rsRuns, [IntToStr(FTestsCount), IntToStr(FTestsTotal)]);
      msg := Format(rsErrors, [msg, IntToStr(FErrorCount)]);
      msg := Format(rsFailures, [msg, IntToStr(FFailCount)]);
      OldStyle := Canvas.Brush.Style;
      Canvas.Brush.Style := bsClear;
      Canvas.Textout(10, 4,  msg);
      Canvas.Brush.Style := OldStyle;
    end;
    Canvas.UnLock;
  end;
end;

procedure TTesterForm.Timer1Timer(Sender: TObject);
var
  list : TTestEventQueue;
  ev : TTestEvent;
  ti : TTestNode;
begin
  list := TTestEventQueue.create;
  try
    list.OwnsObjects := true;
    EnterCriticalSection(FLock);
    try
      for ev in FIncoming do
        list.add(ev);
      FIncoming.Clear;
    finally
      LeaveCriticalSection(FLock);
    end;
    for ev in list do
    begin
      case ev.event of
        tekStartSuite:
          begin
            ev.node.outcome := toRunning;
            ti := ev.node.parent;
            while (ti <> nil) do
            begin
              ti.outcome := toChildRunning;
              ti := ti.parent;
            end;
          end;
        tekFinishSuite:
          begin
            EndTestSuite(ev.node);
          end;
        tekStart:
          begin
            ev.node.outcome := toRunning;
            ev.node.parent.outcome := toChildRunning;
            lblStatus.caption := 'Running Test '+ev.node.testName;
          end;
        tekEnd:
          begin
            if (ev.node.outcome = toRunning) then
              ev.node.outcome := toPass;
            Inc(FTestsCount);
            lblStatus.caption := '';
          end;
        tekFail:
          begin
            ev.node.ExceptionMessage := ev.excMessage;
            ev.node.ExceptionClassName := ev.excClass;
            ev.node.outcome := toFail;
            Inc(FFailCount);
            lblStatus.caption := '';
          end;
        tekError:
          begin
            ev.node.ExceptionMessage := ev.excMessage;
            ev.node.ExceptionClassName := ev.excClass;
            ev.node.outcome := toError;
            Inc(FErrorCount);
            lblStatus.caption := '';
          end;
        tekEndRun:
          begin
            ti := ev.node;
            if (ti <> nil) then
            begin
              ti := ti.parent;
              while (ti <> nil) do
              begin
                EndTestSuite(ti);
                ti := ti.parent;
              end;
            end;
            FinishTestRun;
          end;
      end;
    end;
    pbbar.Refresh;
    Application.ProcessMessages;
  finally
    list.free;
  end;
  if (FKillTime > 0) and (GetTickCount64 > FKillTime) then
    killRunningTests;
end;

procedure TTesterForm.EndTestSuite(ATest: TTestNode);
var
  outcome : TTestOutcome;
  child : TTestNode;
begin
  outcome := toUnknown;
  for child in ATest.Children do
  begin
    case child.outcome of
      toSomePass : if outcome = toUnknown then outcome := toSomePass else if outcome in [toPass, toNotRun] then outcome := toSomePass;
      toNotRun : if outcome = toUnknown then outcome := toNotRun else if outcome = toPass then outcome := toSomePass;
      toPass : if outcome = toUnknown then outcome := toPass else if outcome = toUnknown then outcome := toSomePass;
      toFail: if outcome <> toError then outcome := toFail;
      toError: outcome := toError;
    else
    end;
  end;
  ATest.outcome := outcome;
  ATest.finish;
end;

procedure TTesterForm.StartTestRun;
var
  ti : TTestNode;
begin
  saveState;
  FWantStop := false;
  FTestsCount := 0;
  FFailCount := 0;
  FErrorCount := 0;
  pbBar.Invalidate;
  setActionStatus(true);
  timer1.Enabled := true;

  FSession := engine.prepareToRunTests;
  for ti in FTestInfo do
    if not ti.execute then
      FSession.skipTest(ti);
end;

procedure TTesterForm.FinishTestRun;
begin
  FThread := nil;
  FRunningTest := nil;
  FKillTime := 0;
  actionTestStop.ImageIndex := 3;
  engine.finishTestRun(FSession);
  FSession := nil;
  Timer1.Enabled := false;
  setActionStatus(false);
  saveState;
  UpdateTotals;
end;

procedure TTesterForm.actionTestCopyExecute(Sender: TObject);
begin
  Clipboard.AsText := FSelectedNode.details('');
end;

procedure TTesterForm.actionTestConfigureExecute(Sender: TObject);
begin
  IDETesterSettings := TIDETesterSettings.create(self);
  try
    IDETesterSettings.showModal;
  finally
    IDETesterSettings.free;
  end;
end;

// -- test thread  - no UI access ----------------------------------------------

procedure TTesterForm.DoExecuteTests;
begin
  engine.runTest(FSession, FRunningTest);
end;

procedure TTesterForm.queueEvent(test: TTestNode; event : TTestEventKind; msg, clssName : String);
var
  ev : TTestEvent;
begin
  ev := TTestEvent.create;
  ev.node := test;
  ev.event := event;
  ev.excMessage := msg;
  ev.excClass := clssName;
  EnterCriticalSection(FLock);
  try
    FIncoming.add(ev);
  finally
    LeaveCriticalSection(FLock);
  end;
  if FWantStop then
    Abort;
end;

procedure TTesterForm.killRunningTests;
begin
  KillThread(FThread.Handle);
  if (not FRunningTest.hasChildren) then
    EndTestSuite(FRunningTest);
  queueEvent(FRunningTest, tekEndRun, '', '');
end;

end.


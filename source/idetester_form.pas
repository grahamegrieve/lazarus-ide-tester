unit idetester_form;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, ActnList, Menus, LCLIntf, laz.VirtualTrees, ClipBrd,
  Generics.Collections,
  idetester_base, idetester_strings, idetester_direct, idetester_ini,
  idetester_options, idetester_debug_form;

const
  DEFAULT_KILL_TIME_DELAY = 5000;

type
  // for the virtual tree

  { TTestNodeData }
  TTestNodeData = record
    node : TTestNode;
  end;
  PTestNodeData = ^TTestNodeData;

  TTestEventKind = (tekStartSuite, tekFinishSuite, tekStart, tekEnd, tekFail, tekError, tekHalt, tekEndRun);

  { TTestEvent }

  TTestEvent = class (TObject)
  private
    node : TTestNode;
    event : TTestEventKind;
    excMessage : String;
    excClass : String;
  end;
  TTestEventQueue = class (TObjectList<TTestEvent>);

  TIdeTesterForm = class;

  { TTestThread }

  TTestThread = class (TThread)
  private
    FTester : TIdeTesterForm;
    FDebug : boolean;
  protected
    procedure execute; override;
  public
    constructor Create(tester : TIdeTesterForm; debug : boolean);
  end;

  { TTesterFormListener }

  TTesterFormListener  = class (TTestListener)
  private
    FTester : TIdeTesterForm;
  public
    constructor Create(tester : TIdeTesterForm);

    procedure StartTest(test: TTestNode); override;
    procedure EndTest(test: TTestNode); override;
    procedure TestFailure(test: TTestNode; fail: TTestError); override;
    procedure TestError(test: TTestNode; error: TTestError); override;
    procedure TestHalt(test: TTestNode); override; // testing process died completely
    procedure StartTestSuite(test: TTestNode); override;
    procedure EndTestSuite(test: TTestNode); override;
    procedure EndRun(test: TTestNode); override;
  end;

  { TIdeTesterForm }
  TIdeTesterForm = class(TForm)
    actTestDebugSelected: TAction;
    actTestConfigure: TAction;
    actTestReload: TAction;
    actTestSelectAll: TAction;
    actTestUnselectAll: TAction;
    actTestReset: TAction;
    actTestCopy: TAction;
    actTestStop: TAction;
    actTestRunFailed: TAction;
    actTestRunChecked: TAction;
    actTestRunSelected: TAction;
    ActionList1: TActionList;
    ilMain: TImageList;
    ilOutcomes: TImageList;
    lblStatus: TLabel;
    MenuItem4: TMenuItem;
    mnuDebug: TMenuItem;
    Panel2: TPanel;
    Timer1: TTimer;
    tbBtnReload: TToolButton;
    tbBtnReloadSep: TToolButton;
    tbBtnConfigure: TToolButton;
    tbBtnDebug: TToolButton;
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
    procedure actTestConfigureExecute(Sender: TObject);
    procedure actTestCopyExecute(Sender: TObject);
    procedure actTestReloadExecute(Sender: TObject);
    procedure actTestDebugSelectedExecute(Sender: TObject);
    procedure actTestResetExecute(Sender: TObject);
    procedure actTestRunFailedExecute(Sender: TObject);
    procedure actTestSelectAllExecute(Sender: TObject);
    procedure actTestStopExecute(Sender: TObject);
    procedure actTestUnselectAllExecute(Sender: TObject);
    procedure actTestRunCheckedExecute(Sender: TObject);
    procedure actTestRunSelectedExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pbBarPaint(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure tvTestsAddToSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvTestsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvTestsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure tvTestsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure tvTestsRemoveFromSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
  private
    FEngine : TTestEngine;
    FStore : TTestSettingsProvider;

    FTestInfo : TTestNodeList;
    FRunningTest : TTestNode;
    FTestsTotal, FTestsCount, FFailCount, FErrorCount : cardinal;
    FStartTime, FEndTime : UInt64;
    FWantStop : boolean;
    FSelectedNode : TTestNode;
    FSession : TTestSession;
    FLock : TRTLCriticalSection;
    FIncoming : TTestEventQueue;
    FShuttingDown : boolean;
    FThread : TTestThread;
    FKillTime : cardinal;
    FLoading, FShown : boolean;

    // -- utils ----
    procedure SetEngine(AValue: TTestEngine);
    function tn(p: PVirtualNode): TTestNode;
    procedure UpdateTotals;

    // -- init ----
    procedure doReinitialise(sender : TObject);
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
    procedure DoExecuteTests(debug : boolean); // in alternative thread
    procedure FinishTestRun;
    procedure queueEvent(test: TTestNode; event : TTestEventKind; msg, clssName : String);
    procedure killRunningTests;
    procedure EndTestSuite(ATest: TTestNode);
  public
    // these two must be set before the Form is shown
    property engine : TTestEngine read FEngine write SetEngine;
    property store : TTestSettingsProvider read FStore write FStore; // either an ini in AppConfig, or stored in the project settings somewhere?
  end;

var IdeTesterForm : TIdeTesterForm;

implementation

{$R *.lfm}

{ TTesterFormListener }

constructor TTesterFormListener.Create(tester: TIdeTesterForm);
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

procedure TTesterFormListener.TestHalt(test: TTestNode);
begin
  FTester.queueEvent(test, tekHalt, '', '');
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
    FTester.DoExecuteTests(FDebug);
  except
  end;
end;

constructor TTestThread.Create(tester: TIdeTesterForm; debug : boolean);
begin
  FTester := tester;
  FDebug := debug;
  inherited Create(false);
end;

{ TIdeTesterForm }

procedure TIdeTesterForm.FormCreate(Sender: TObject);
begin
  FTestInfo := TTestNodeList.create(true);
  InitCriticalSection(FLock);
  FIncoming := TTestEventQueue.create(false);
  tvTests.NodeDataSize := sizeof(pointer);
  FLoading := true;

  pbBarPaint(pbBar);
  setActionStatus(false);

  actTestDebugSelected.Caption := rs_IdeTester_Caption_DebugSelected_NODE;
  actTestConfigure.Caption := rs_IdeTester_Caption_Configure;
  actTestReload.Caption := rs_IdeTester_Caption_Reload;
  actTestSelectAll.Caption := rs_IdeTester_Caption_SelectAll_NODE;
  actTestUnselectAll.Caption := rs_IdeTester_Caption_UnselectAll_NODE;
  actTestReset.Caption := rs_IdeTester_Caption_Reset_NODE;
  actTestCopy.Caption := rs_IdeTester_Caption_Copy_NODE;
  actTestStop.Caption := rs_IdeTester_Caption_Stop;
  actTestRunFailed.Caption := rs_IdeTester_Caption_RunFailed;
  actTestRunChecked.Caption := rs_IdeTester_Caption_RunChecked;
  actTestRunSelected.Caption := rs_IdeTester_Caption_RunSelected_NODE;

  actTestDebugSelected.Hint := rs_IdeTester_Hint_DebugSelected;
  actTestConfigure.Hint := rs_IdeTester_Hint_Configure;
  actTestReload.Hint := rs_IdeTester_Hint_Reload;
  actTestSelectAll.Hint := rs_IdeTester_Hint_SelectAll;
  actTestUnselectAll.Hint := rs_IdeTester_Hint_UnselectAll;
  actTestReset.Hint := rs_IdeTester_Hint_Reset;
  actTestCopy.Hint := rs_IdeTester_Hint_Copy;
  actTestStop.Hint := rs_IdeTester_Hint_Stop;
  actTestRunFailed.Hint := rs_IdeTester_Hint_RunFailed;
  actTestRunChecked.Hint := rs_IdeTester_Hint_RunChecked;
  actTestRunSelected.Hint := rs_IdeTester_Hint_RunSelected;
end;

procedure TIdeTesterForm.FormDestroy(Sender: TObject);
begin
  FShuttingDown := true;
  saveState;
  FTestInfo.Free;
  FIncoming.Free;
  DoneCriticalSection(FLock);
  FEngine.Free;
  FStore.Free;
end;

procedure TIdeTesterForm.FormShow(Sender: TObject);
begin
  if not FShown then
  begin
    FShown := true;
    FLoading := true;
    if store = nil then
      store := TTestIniSettingsProvider.create(IncludeTrailingPathDelimiter(getAppConfigDir(false))+'fhir-tests-settings.ini');
    if engine = nil then
      engine := TTestEngineDirect.create;

    engine.listener := TTesterFormListener.create(self);
    engine.OnReinitialise := doReinitialise;
    if engine.hasParameters then
      engine.parameters := store.read('parameters', '');
    if not engine.doesReload then
    begin
      tbBtnReload.visible := false;
      tbBtnReloadSep.visible := false;
    end;
    if not (engine.canTerminate or engine.hasParameters) then
    begin
      tbBtnConfigure.visible := false;
    end;
    if not engine.canDebug then
    begin
      tbBtnDebug.visible := false;
      mnuDebug.visible := false;
    end;
    actTestReloadExecute(self);
    pbBarPaint(pbBar);
    FLoading := false;
  end;
end;

procedure TIdeTesterForm.setActionStatus(running: boolean);
begin
  actTestCopy.Enabled := not running;
  actTestSelectAll.Enabled := not running;
  actTestReload.Enabled := not running;
  actTestConfigure.Enabled := not running;
  actTestUnselectAll.Enabled := not running;
  actTestReset.Enabled := not running;
  actTestStop.Enabled := running;
  actTestRunFailed.Enabled := not running;
  actTestRunChecked.Enabled := not running;
  actTestRunSelected.Enabled := not running;
  actTestDebugSelected.Enabled := not running;
end;

// -- Tree Management ----------------------------------------------------------

function TIdeTesterForm.tn(p : PVirtualNode) : TTestNode;
var
  pd : PTestNodeData;
begin
  pd := tvTests.GetNodeData(p);
  result := pd.node;
end;

procedure TIdeTesterForm.SetEngine(AValue: TTestEngine);
begin
  FEngine := AValue;
  FEngine.OnReinitialise := doReinitialise;
end;

procedure TIdeTesterForm.UpdateTotals;
var
  tc, pc, ec, fc, nr, cc : cardinal;
  tn : TTestNode;
  res : boolean;
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
  res := false;
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
        toError, toHalt : inc(fc);
      else //  toNotRun
        inc(nr);
      end;
      res := res or not (tn.outcome in [toUnknown, toNotRun]);
    end;
  end;

  s := inttostr(tc)+ ' '+rs_IdeTester_SBar_Tests+': ';
  s := s + inttostr(cc)+ ' '+rs_IdeTester_SBar_Checked;
  s := s + ', '+inttostr(pc)+' '+rs_IdeTester_SBar_Passed;
  if (fc + ec > 0) then
    s := s + ', '+inttostr(fc+ec)+' '+rs_IdeTester_SBar_Failed;
  if (ec > 0) then
    s := s + ' ('+inttostr(ec)+' '+rs_IdeTester_SBar_Errors+')';
  if (nr > 0) then
    s := s + ', '+inttostr(nr)+' '+rs_IdeTester_SBar_NotRun;
  if (ec + fc + nr = 0) then
    s := s + ' - '+rs_IdeTester_SBar_All_OK;
  lblStatus.caption := s;

  actTestRunSelected.enabled := tc > 0;
  actTestDebugSelected.enabled := tc > 0;
  actTestSelectAll.enabled := tc > 0;
  actTestUnselectAll.enabled := tc > 0;
  actTestSelectAll.enabled := tc > 0;
  actTestUnselectAll.enabled := tc > 0;
  actTestCopy.enabled := tc > 0;
  actTestRunChecked.enabled := cc > 0;
  actTestRunFailed.enabled := fc + ec > 0;
  actTestReset.enabled := res;
end;

procedure TIdeTesterForm.doReinitialise(sender: TObject);
begin
  tvTests.Clear;
  FTestInfo.Clear;
  tvTests.Refresh;
  FTestsTotal := 0;
  UpdateTotals;
  pbBarPaint(pbBar);
  if engine.hasParameters then
    engine.parameters := store.read('parameters', '');
  setActionStatus(false);
end;

procedure TIdeTesterForm.tvTestsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
begin
  ImageIndex := ord(tn(node).outcome);
end;

procedure TIdeTesterForm.tvTestsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
begin
  CellText := tn(node).description;
end;

function TIdeTesterForm.nodeFactory(parent : TTestNode) : TTestNode;
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

procedure TIdeTesterForm.refreshNode(test: TTestNode);
begin
  tvTests.InvalidateNode(test.node);
end;

procedure TIdeTesterForm.LoadTree;
begin
  tvTests.Clear;
  FTestInfo.Clear;
  tvTests.Refresh;
  engine.loadAllTests(nodeFactory, not FLoading);
  if FTestInfo.Count > 0 then
  begin

    tvTests.Selected[FTestInfo[0].node] := true;
    tvTests.Expanded[FTestInfo[0].node] := true;
  end
  else
  begin
    tvTestsRemoveFromSelection(nil, nil);
  end;
end;

// --- Test Selection Management -----------------------------------------------

procedure TIdeTesterForm.tvTestsAddToSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  FSelectedNode := tn(node);
  if FSelectedNode.hasChildren then
  begin
    actTestRunSelected.caption := 'Run these tests';
    actTestDebugSelected.caption := 'Debug these tests';
    actTestSelectAll.caption := 'Check these tests';
    actTestUnselectAll.caption := 'Uncheck these tests';
    actTestSelectAll.hint := 'Check selected tests + children';
    actTestUnselectAll.hint := 'Uncheck selected tests + children';
    actTestCopy.hint := 'Copy results to clipboard for selected tests';
    actTestRunSelected.caption := 'Run Selected Test + children';
    actTestDebugSelected.caption := 'Debug Selected Test + children';
  end
  else
  begin
    actTestRunSelected.caption := 'Run this test';
    actTestRunSelected.caption := 'Debug this test';
    actTestSelectAll.caption := 'Check this test';
    actTestUnselectAll.caption := 'Uncheck this test';
    actTestSelectAll.hint := 'Check selected test';
    actTestUnselectAll.hint := 'Uncheck selected test';
    actTestCopy.hint := 'Copy results to clipboard for selected test';
    actTestRunSelected.caption := 'Run Selected Test';
    actTestDebugSelected.caption := 'Debug Selected Test';
  end;
  UpdateTotals;
end;

procedure TIdeTesterForm.tvTestsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  UpdateTotals;
end;

procedure TIdeTesterForm.tvTestsRemoveFromSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  FSelectedNode := nil;
  UpdateTotals;
end;


procedure TIdeTesterForm.loadState;
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
        if not (ti.outcome in [toSomePass, toPass, toFail, toError, toHalt]) then
          ti.outcome := toNotRun;
      end
      else
        ti.outcome := toNotRun;
    end;
  end;

  tvTests.EndUpdate;
end;

procedure TIdeTesterForm.saveState;
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

procedure TIdeTesterForm.setTestState(node: TTestNode; state: TTestCheckState);
var
  ti : TTestNode;
begin
  node.checkState := state;
  for ti in node.Children do
    setTestState(ti, state);
end;

procedure TIdeTesterForm.checkStateOfChildren(node: TTestNode);
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

procedure TIdeTesterForm.actTestSelectAllExecute(Sender: TObject);
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

procedure TIdeTesterForm.actTestUnselectAllExecute(Sender: TObject);
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

procedure TIdeTesterForm.actTestRunCheckedExecute(Sender: TObject);
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
    FThread := TTestThread.create(self, false);
    FThread.FreeOnTerminate := true;
  end
  else
    ShowMessage(rs_IdeTester_Err_No_Tests);
end;

procedure TIdeTesterForm.actTestRunSelectedExecute(Sender: TObject);
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
    FThread := TTestThread.create(self, false);
    FThread.FreeOnTerminate := true;
  end;
end;

procedure TIdeTesterForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FRunningTest = nil;
end;

procedure TIdeTesterForm.actTestRunFailedExecute(Sender: TObject);
var
  ti : TTestNode;
begin
  for ti in FTestInfo do
  begin
    ti.execute := (ti.outcome in [toFail, toError, toHalt]) and (ti.CheckState <> tcsUnchecked);
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
    FThread := TTestThread.create(self, false);
    FThread.FreeOnTerminate := true;
  end
  else
    ShowMessage(rs_IdeTester_Err_No_FailedTests);
end;

procedure TIdeTesterForm.actTestResetExecute(Sender: TObject);
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

procedure TIdeTesterForm.actTestStopExecute(Sender: TObject);
begin
  FWantStop := true;
  if engine.canTerminate then
  begin
    FKillTime := GetTickCount64 + StrToInt(store.read('killtime', inttostr(DEFAULT_KILL_TIME_DELAY)));
    actTestStop.ImageIndex := 14;
  end;
end;

procedure TIdeTesterForm.setDoExecute(node : TTestNode);
var
  child : TTestNode;
begin
  node.execute := true;

  for child in node.Children do
    setDoExecute(child);
end;

procedure TIdeTesterForm.setDoExecuteParent(node: TTestNode);
begin
  if node <> nil then
  begin
    node.execute := true;
    setDoExecuteParent(node.parent);
  end;
end;

// -- Actually executing the tests ---------------------------------------------

procedure TIdeTesterForm.pbBarPaint(Sender: TObject);
var
  msg: string;
  OldStyle: TBrushStyle;
begin
  with (Sender as TPaintBox) do
  begin
    Canvas.Lock;
    if FRunningTest <> nil then
      Canvas.Pen.Color := clBlue
    else
      Canvas.Pen.Color := clBlack;
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
      msg := Format(rs_IdeTester_PBar_Runs, [IntToStr(FTestsCount), IntToStr(FTestsTotal)]);
      msg := Format(rs_IdeTester_PBar_Failures, [msg, IntToStr(FFailCount)]);
      msg := Format(rs_IdeTester_PBar_Errors, [msg, IntToStr(FErrorCount)]);
      if FStartTime <> 0 then
      begin
        if FEndTime <> 0 then
          msg := Format(rs_IdeTester_PBar_TimeDone, [msg, IntToStr((FEndTime - FStartTime) div 1000)])
        else
          msg := Format(rs_IdeTester_PBar_Time, [msg, IntToStr((GetTickCount64 - FStartTime) div 1000)]);
      end;
      OldStyle := Canvas.Brush.Style;
      Canvas.Brush.Style := bsClear;
      Canvas.Textout(6, 2,  msg);
      Canvas.Brush.Style := OldStyle;
    end;
    Canvas.UnLock;
  end;
end;

procedure TIdeTesterForm.Timer1Timer(Sender: TObject);
var
  list : TTestEventQueue;
  ev : TTestEvent;
  ti : TTestNode;
begin
  list := TTestEventQueue.create(true);
  try
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
            lblStatus.caption := rs_IdeTester_Msg_Running_Test + ev.node.testName;
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
        tekHalt:
          begin
            ev.node.ExceptionMessage := rs_IdeTester_Msg_Process_Terminated;
            ev.node.outcome := toHalt;
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
    if FRunningTest <> nil then
      pbbar.Refresh;
    Application.ProcessMessages;
  finally
    list.free;
  end;
  if (FKillTime > 0) and (GetTickCount64 > FKillTime) then
    killRunningTests;
end;

procedure TIdeTesterForm.EndTestSuite(ATest: TTestNode);
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
      toError, toHalt: outcome := toError;
    else
    end;
  end;
  ATest.outcome := outcome;
  ATest.finish;
end;

procedure TIdeTesterForm.StartTestRun;
var
  ti : TTestNode;
begin
  saveState;
  FStartTime := 0;
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
  FStartTime := GetTickCount64;
  FEndTime := 0;
end;

procedure TIdeTesterForm.FinishTestRun;
begin
  FThread := nil;
  FRunningTest := nil;
  FKillTime := 0;
  FEndTime := GetTickCount64;
  actTestStop.ImageIndex := 3;
  engine.finishTestRun(FSession);
  FSession := nil;
  Timer1.Enabled := false;
  setActionStatus(false);
  saveState;
  UpdateTotals;
  pbBarPaint(pbBar);
end;

procedure TIdeTesterForm.actTestCopyExecute(Sender: TObject);
begin
  Clipboard.AsText := FSelectedNode.details('');
end;

procedure TIdeTesterForm.actTestReloadExecute(Sender: TObject);
begin
  LoadTree;
  LoadState;
  UpdateTotals;
end;

procedure TIdeTesterForm.actTestDebugSelectedExecute(Sender: TObject);
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
  end;

  // we don't support debugging yet, so we just allow users to copy parameters
  IDETesterDebugForm := TIDETesterDebugForm.create(self);
  try
    if node <> nil then
    begin
      IDETesterDebugForm.edtParamsRunSelected.text := engine.paramsForTest(FRunningTest);
      FSession := engine.prepareToRunTests;
      try
        for ti in FTestInfo do
          if not ti.execute then
            FSession.skipTest(ti);
        IDETesterDebugForm.edtParamsChecked.text := engine.paramsForCheckedTests(FRunningTest, FSession);
      finally
        FreeAndNil(FSession);
      end;
    end
    else
    begin
      IDETesterDebugForm.edtParamsChecked.text := '';
      IDETesterDebugForm.edtParamsRunSelected.text := '';
    end;

    IDETesterDebugForm.edtParamsLoad.text := engine.paramsForLoad;
    IDETesterDebugForm.edtExecutable.text := engine.executableName;
    IDETesterDebugForm.ShowModal;
  finally
    FreeAndNil(IDETesterDebugForm);
  end;
  FRunningTest := nil;
    //StartTestRun;
    //FThread := TTestThread.create(self, true);
    //FThread.FreeOnTerminate := true;
end;

procedure TIdeTesterForm.actTestConfigureExecute(Sender: TObject);
begin
  IDETesterSettings := TIDETesterSettings.create(self);
  try
    IDETesterSettings.pnlKillTime.visible := engine.canTerminate;
    IDETesterSettings.pnlParameters.visible := engine.hasParameters;
    IDETesterSettings.editWaitTime.Text := store.read('killtime', inttostr(DEFAULT_KILL_TIME_DELAY));
    IDETesterSettings.edtParameters.text := store.read('parameters', '');
    if IDETesterSettings.showModal = mrOk then
    begin
      store.save('parameters', IDETesterSettings.edtParameters.text);
      engine.parameters := IDETesterSettings.edtParameters.text;
      store.save('killtime', IDETesterSettings.editWaitTime.Text);
    end;

  finally
    IDETesterSettings.free;
  end;
end;

// -- test thread  - no UI access ----------------------------------------------

procedure TIdeTesterForm.DoExecuteTests(debug : boolean);
begin
  engine.runTest(FSession, FRunningTest, debug);
end;

procedure TIdeTesterForm.queueEvent(test: TTestNode; event : TTestEventKind; msg, clssName : String);
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

procedure TIdeTesterForm.killRunningTests;
begin
  if FSession <> nil then
    engine.terminateTests(FSession);
end;

end.


unit idetester_form;

{
Copyright (c) 2011+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.
 * Neither the name of HL7 nor the names of its contributors may be used to
   endorse or promote products derived from this software without specific
   prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
}

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, ActnList, Menus, LCLIntf, laz.VirtualTrees, ClipBrd,
  Generics.Collections,
  idetester_base, idetester_strings, idetester_direct, idetester_ini,
  idetester_options;

const
  DEFAULT_KILL_TIME_DELAY = 5000;
  IMG_INDEX_STOP = 4;
  IMG_INDEX_WAIT = 5;

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
    sourceUnitName  : string;
    lineNumber: longint;
  end;
  TTestEventQueue = class (TObjectList<TTestEvent>);

  TIdeTesterForm = class;

  { TTestThread }

  TTestThread = class (TThread)
  private
    FTester : TIdeTesterForm;
  protected
    procedure execute; override;
  public
    constructor Create(tester : TIdeTesterForm);
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

  TViewMode = (vmAll, vmFlat, vmIssues, vmNotRun);

  { TIdeTesterForm }
  TIdeTesterForm = class(TForm)
    actTestGoSource: TAction;
    actTestGoError: TAction;
    actTestCopyName: TAction;
    actTestCopyPath: TAction;
    actTestViewFlat: TAction;
    actTestViewIssues: TAction;
    actTestViewUnrun: TAction;
    actTestViewAll: TAction;
    actTestDebugSelected: TAction;
    actTestConfigure: TAction;
    actTestReload: TAction;
    actTestReset: TAction;
    actTestCopyResults: TAction;
    actTestStop: TAction;
    actTestRunFailed: TAction;
    actTestRunChecked: TAction;
    actTestRunSelected: TAction;
    ActionList1: TActionList;
    ilMain: TImageList;
    ilOutcomes: TImageList;
    lblStatus: TLabel;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    mnuGo: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    mnuDebug: TMenuItem;
    Panel2: TPanel;
    pmView: TPopupMenu;
    Timer1: TTimer;
    tbBtnReload: TToolButton;
    tbBtnReloadSep: TToolButton;
    tbBtnConfigure: TToolButton;
    tbBtnDebug: TToolButton;
    btnView: TToolButton;
    tvTests: TLazVirtualStringTree;
    MenuItem3: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    pbBar: TPaintBox;
    Panel1: TPanel;
    pmTests: TPopupMenu;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    procedure actTestConfigureExecute(Sender: TObject);
    procedure actTestCopyNameExecute(Sender: TObject);
    procedure actTestCopyPathExecute(Sender: TObject);
    procedure actTestCopyResultsExecute(Sender: TObject);
    procedure actTestGoErrorExecute(Sender: TObject);
    procedure actTestGoSourceExecute(Sender: TObject);
    procedure actTestReloadExecute(Sender: TObject);
    procedure actTestDebugSelectedExecute(Sender: TObject);
    procedure actTestResetExecute(Sender: TObject);
    procedure actTestRunFailedExecute(Sender: TObject);
    procedure actTestSelectAllExecute(Sender: TObject);
    procedure actTestStopExecute(Sender: TObject);
    procedure actTestUnselectAllExecute(Sender: TObject);
    procedure actTestRunCheckedExecute(Sender: TObject);
    procedure actTestRunSelectedExecute(Sender: TObject);
    procedure actTestViewAllExecute(Sender: TObject);
    procedure actTestViewFlatExecute(Sender: TObject);
    procedure actTestViewIssuesExecute(Sender: TObject);
    procedure actTestViewUnrunExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pbBarPaint(Sender: TObject);
    procedure pmTestsPopup(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure tvTestsAddToSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvTestsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvTestsDblClick(Sender: TObject);
    procedure tvTestsGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: String);
    procedure tvTestsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure tvTestsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure tvTestsRemoveFromSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
  private
    FEngine : TTestEngine;
    FStore : TTestSettingsProvider;

    FViewMode : TViewMode;
    FTestInfo : TTestNodeList;
    FRunningTest : TTestNode;
    FTestsTotal, FTestsCount, FFailCount, FErrorCount : cardinal;
    FStartTime, FEndTime : UInt64;
    FRunning, FWantStop : boolean;
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
    procedure SetStore(AValue: TTestSettingsProvider);
    function tn(p: PVirtualNode): TTestNode;
    procedure UpdateTotals;

    // -- init ----
    procedure doReinitialise(sender : TObject);
    procedure doEngineUpdateStatus(sender : TObject);
    procedure LoadTree(full : boolean);
    function InView(test : TTestNode): boolean;
    function parentForTest(test : TTestNode): PVirtualNode;
    procedure AddNodeToTree(test: TTestNode);
    procedure buildTreeView;
    procedure refreshNode(test : TTestNode);
    procedure loadState;
    procedure saveState;

    // -- checkbox mgmt ----
    procedure setTestState(node: TTestNode; state : TTestCheckState);
    procedure checkStateOfChildren(node: TTestNode);

    // -- running tests ----
    procedure doEngineStatusMessage(sender : TObject; msg : String);
    procedure setDoExecute(node: TTestNode);
    procedure setDoExecuteParent(node: TTestNode);
    procedure setActionStatus(tc, fc, cc : integer);
    procedure StartTestRun(debug : boolean);
    procedure DoExecuteTests; // in alternative thread
    procedure FinishTestRun;
    procedure queueEvent(test: TTestNode; event : TTestEventKind; msg, clssName, srcUnit : String; line : integer);
    procedure killRunningTests;
    procedure EndTestSuite(ATest: TTestNode);
  public
    // these two must be set before the Form is shown
    property engine : TTestEngine read FEngine write SetEngine;
    property store : TTestSettingsProvider read FStore write SetStore; // either an ini in AppConfig, or stored in the project settings somewhere?
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
  FTester.queueEvent(test, tekStart, '', '', '', 0);
end;

procedure TTesterFormListener.EndTest(test: TTestNode);
begin
  test.finish;
  FTester.queueEvent(test, tekEnd, '', '', '', 0);
end;

procedure TTesterFormListener.TestFailure(test: TTestNode; fail: TTestError);
begin
  FTester.queueEvent(test, tekFail, fail.ExceptionMessage, fail.ExceptionClass, fail.SourceUnit, fail.LineNumber);
end;

procedure TTesterFormListener.TestError(test: TTestNode; error: TTestError);
begin
  FTester.queueEvent(test, tekError, error.ExceptionMessage, error.ExceptionClass, error.SourceUnit, error.LineNumber);
end;

procedure TTesterFormListener.TestHalt(test: TTestNode);
begin
  FTester.queueEvent(test, tekHalt, '', '', '', 0);
end;

procedure TTesterFormListener.StartTestSuite(test: TTestNode);
begin
  FTester.queueEvent(test, tekStartSuite, '', '', '', 0);
end;

procedure TTesterFormListener.EndTestSuite(test: TTestNode);
begin
  FTester.queueEvent(test, tekFinishSuite, '', '', '', 0);
end;

procedure TTesterFormListener.EndRun(test: TTestNode);
begin
  FTester.queueEvent(test, tekEndRun, '', '', '', 0);
end;

{ TTestThread }

procedure TTestThread.execute;
begin
  try
    FTester.DoExecuteTests();
  except
  end;
end;

constructor TTestThread.Create(tester: TIdeTesterForm);
begin
  FTester := tester;
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
  UpdateTotals;

  actTestDebugSelected.Caption := rs_IdeTester_Caption_DebugSelected_NODE;
  actTestConfigure.Caption := rs_IdeTester_Caption_Configure;
  actTestReload.Caption := rs_IdeTester_Caption_Reload;
  actTestReset.Caption := rs_IdeTester_Caption_Reset_NODE;
  actTestCopyResults.Caption := rs_IdeTester_Caption_Copy_NODE;
  actTestStop.Caption := rs_IdeTester_Caption_Stop;
  actTestRunFailed.Caption := rs_IdeTester_Caption_RunFailed;
  actTestRunChecked.Caption := rs_IdeTester_Caption_RunChecked;
  actTestRunSelected.Caption := rs_IdeTester_Caption_RunSelected_NODE;
  actTestViewFlat.Caption := rs_IdeTester_Caption_ViewFlat;
  actTestViewIssues.Caption := rs_IdeTester_Caption_ViewIssues;
  actTestViewUnrun.Caption := rs_IdeTester_Caption_ViewUnrun;
  actTestViewAll.Caption := rs_IdeTester_Caption_ViewAll;

  actTestDebugSelected.Hint := rs_IdeTester_Hint_DebugSelected;
  actTestConfigure.Hint := rs_IdeTester_Hint_Configure;
  actTestReload.Hint := rs_IdeTester_Hint_Reload;
  actTestReset.Hint := rs_IdeTester_Hint_Reset;
  actTestCopyResults.Hint := rs_IdeTester_Hint_Copy;
  actTestStop.Hint := rs_IdeTester_Hint_Stop;
  actTestRunFailed.Hint := rs_IdeTester_Hint_RunFailed;
  actTestRunChecked.Hint := rs_IdeTester_Hint_RunChecked;
  actTestRunSelected.Hint := rs_IdeTester_Hint_RunSelected;
  actTestViewFlat.Hint := rs_IdeTester_Hint_ViewFlat;
  actTestViewIssues.Hint := rs_IdeTester_Hint_ViewIssues;
  actTestViewUnrun.Hint := rs_IdeTester_Hint_ViewUnrun;
  actTestViewAll.Hint := rs_IdeTester_Hint_ViewAll;
end;

procedure TIdeTesterForm.FormDestroy(Sender: TObject);
begin
  if FRunning and engine.canTerminate then
    killRunningTests;
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
    if not engine.doesReload then
      tbBtnReload.visible := false;
    if not (engine.canTerminate or engine.canParameters or engine.canTestProject) then
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
  if PixelsPerInch > 96 then
  begin
    ToolBar1.ImagesWidth := 32;
    tvTests.ImagesWidth := 32
  end;
end;

procedure TIdeTesterForm.setActionStatus(tc, fc, cc : integer);
var
  hasTests : boolean;
begin
  hasTests := tc > 0;
  if (engine = nil) or (store = nil) then
  begin
    actTestCopyResults.Enabled := false;
    actTestReload.Enabled := false;
    actTestConfigure.Enabled := false;
    actTestReset.Enabled := false;
    actTestStop.Enabled := false;
    actTestRunFailed.Enabled := false;
    actTestRunChecked.Enabled := false;
    actTestRunSelected.Enabled := false;
    actTestDebugSelected.Enabled := false;
  end
  else
  begin
    actTestCopyResults.Enabled := not FRunning and hasTests;
    actTestReload.Enabled := not FRunning and engine.canStart;
    actTestConfigure.Enabled := not FRunning;
    actTestReset.Enabled := not FRunning and hasTests;
    actTestStop.Enabled := FRunning and engine.canStop;
    actTestRunFailed.Enabled := not FRunning and (fc > 0) and engine.canStart;
    actTestRunChecked.Enabled := not FRunning and (cc > 0) and hasTests and engine.canStart;
    actTestRunSelected.Enabled := not FRunning and hasTests and engine.canStart;
    actTestDebugSelected.Enabled := engine.canDebug and not FRunning and hasTests and (store.read(tsmConfig, 'testproject', '') = '') and engine.canStart;
  end;
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
  FEngine.OnStatusMessage := doEngineStatusMessage;
  FEngine.OnUpdateStatus := doEngineUpdateStatus;
  FEngine.settings := store;
end;

procedure TIdeTesterForm.SetStore(AValue: TTestSettingsProvider);
begin
  if FStore = AValue then Exit;
  FStore := AValue;
  if engine <> nil then
    engine.settings := store;
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

  if tc = 0 then
    s := inttostr(tc)+ ' '+rs_IdeTester_SBar_Tests
  else
  begin
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
  end;
  lblStatus.caption := s;

  setActionStatus(tc, fc + ec, cc);
end;

procedure TIdeTesterForm.doReinitialise(sender: TObject);
begin
  tvTests.Clear;
  FTestInfo.Clear;
  tvTests.Refresh;
  FTestsTotal := 0;
  UpdateTotals;
  pbBarPaint(pbBar);
end;

procedure TIdeTesterForm.doEngineUpdateStatus(sender: TObject);
begin
  if FRunning then
    setActionStatus(FTestsCount, 0, 0)
  else
    UpdateTotals;
end;

procedure TIdeTesterForm.tvTestsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
begin
  ImageIndex := ord(tn(node).outcome);
end;

procedure TIdeTesterForm.tvTestsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
begin
  if FViewMode = vmFlat then
    CellText := tn(node).sortName
  else
    CellText := tn(node).description;
end;

procedure TIdeTesterForm.refreshNode(test: TTestNode);
begin
  tvTests.InvalidateNode(test.node);
end;

procedure TIdeTesterForm.LoadTree(full : boolean);
begin
  if (full) then
  begin
    FTestInfo.Clear;
    engine.loadAllTests(FTestInfo, not FLoading);
  end;
  buildTreeView;
  if tvTests.ChildCount[nil] > 0 then
  begin
    tvTests.Selected[FTestInfo[0].node] := true;
    if (FViewMode in [vmAll, vmNotRun]) then
      tvTests.Expanded[FTestInfo[0].node] := true
    else if (FViewMode in [vmIssues]) then
      tvTests.FullExpand;
  end
  else
  begin
    tvTestsRemoveFromSelection(nil, nil);
  end;
end;

function hasLeafChildren(test : TTestNode) : boolean;
var
  child : TTestNode;
begin
  if not test.hasChildren then
    result := false
  else
  begin
    result := false;
    for child in test.children do
      if not child.hasChildren then
        exit(true);
  end;
end;

function hasFailedDescendents(test : TTestNode) : boolean;
var
  child : TTestNode;
begin
  if test.outcome in [toFail, toError, toHalt] then
    result := true
  else
  begin
    result := false;
    for child in test.children do
      if hasFailedDescendents(child) then
        exit(true);
  end;
end;

function hasUnrunDescendents(test : TTestNode) : boolean;
var
  child : TTestNode;
begin
  if test.outcome in [toNotRun] then
    result := true
  else
  begin
    result := false;
    for child in test.children do
      if hasUnrunDescendents(child) then
        exit(true);
  end;
end;

function TIdeTesterForm.InView(test : TTestNode): boolean;
begin
  case FViewMode of
    vmAll : result := true;
    vmFlat : result := hasLeafChildren(test);
    vmIssues : result := hasFailedDescendents(test);
    vmNotRun : result := hasUnrunDescendents(test);
  else
    result := true;
  end;
end;

function TIdeTesterForm.parentForTest(test : TTestNode): PVirtualNode;
var
  parent : TTestNode;
begin
  result := nil;
  parent := test.parent;
  while (parent <> nil) do
  begin
    if (parent.node <> nil) then
      exit(parent.node);
    parent := parent.parent;
  end;
end;

procedure TIdeTesterForm.AddNodeToTree(test : TTestNode);
var
  p : PTestNodeData;
begin
  test.Node := tvTests.AddChild(parentForTest(test));
  p := tvTests.GetNodeData(test.Node);
  p.Node := test;
  test.Node.CheckType := ctTriStateCheckBox;
  test.OnUpdate := refreshNode;
end;

procedure TIdeTesterForm.buildTreeView;
var
  test, child : TTestNode;
  ts : TStringList;
  i : integer;
begin
  // remove everything from the tree
  for test in FTestInfo do
  begin
    test.node := nil;
    test.OnUpdate := nil;
  end;
  tvTests.BeginUpdate;
  tvTests.Clear;
  if FViewMode = vmFlat then
  begin
    ts := TStringList.create;
    try
      for test in FTestInfo do
        if InView(test) then
          ts.AddObject(test.sortName, test);
      ts.sort;
      for i := 0 to ts.count - 1 do
      begin
        test := ts.objects[i] as TTestNode;
        AddNodeToTree(test);
        for child in test.children do
          AddNodeToTree(child);
      end;
    finally
      ts.free;
    end;
  end
  else
  begin
    for test in FTestInfo do
      if InView(test) then
        AddNodeToTree(test);
  end;
  tvTests.EndUpdate;
end;

// --- Test Selection Management -----------------------------------------------

procedure TIdeTesterForm.tvTestsAddToSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  FSelectedNode := tn(node);
  if FSelectedNode.hasChildren then
  begin
    actTestRunSelected.caption := rs_IdeTester_Caption_RunSelected_NODE;
    actTestDebugSelected.caption := rs_IdeTester_Caption_DebugSelected_NODE;
    actTestReset.caption := rs_IdeTester_Caption_Reset_NODE;
    actTestCopyResults.caption := rs_IdeTester_Caption_Copy_NODE;
  end
  else
  begin
    actTestRunSelected.caption := rs_IdeTester_Caption_RunSelected_LEAF;
    actTestDebugSelected.caption := rs_IdeTester_Caption_DebugSelected_LEAF;
    actTestReset.caption := rs_IdeTester_Caption_Reset_LEAF;
    actTestCopyResults.caption := rs_IdeTester_Caption_Copy_LEAF;
  end;
  actTestGoError.enabled := engine.doesSource and (FSelectedNode.SourceUnitError <> '');
  actTestGoSource.enabled := engine.doesSource and (FSelectedNode.SourceUnit <> '');
  UpdateTotals;
end;

procedure TIdeTesterForm.tvTestsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  test : TTestNode;
begin
  test := tn(node);
  case node.CheckState of
    csUncheckedNormal : test.checkState := tcsUnchecked;
    csCheckedNormal : test.checkState := tcsChecked;
    csMixedNormal : test.checkState := tcsMixed;
  else
    ;
  end;
  UpdateTotals;
end;

procedure TIdeTesterForm.tvTestsDblClick(Sender: TObject);
begin
  if FSelectedNode <> nil then
    engine.openSource(FSelectedNode, osmNull);
end;

procedure TIdeTesterForm.tvTestsGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: String);
begin
  HintText := tn(node).descriptionFull;
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

  if (store.read(tsmStatus, 'Count', '0') = inttostr(FTestInfo.Count)) then
  begin
    ss := store.Read(tsmStatus, 'States', '');
    so := store.Read(tsmStatus, 'Outcomes', '');
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
    store.save(tsmStatus, 'Count', inttostr(FTestInfo.Count));
    store.save(tsmStatus, 'States', bs.toString);
    store.save(tsmStatus, 'Outcomes', bo.toString);
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

procedure TIdeTesterForm.doEngineStatusMessage(sender: TObject; msg: String);
begin
  lblStatus.caption := msg;
  lblStatus.Update;
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
    StartTestRun(false);
    FThread := TTestThread.create(self);
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
    StartTestRun(false);
    FThread := TTestThread.create(self);
    FThread.FreeOnTerminate := true;
  end;
end;

procedure TIdeTesterForm.actTestViewAllExecute(Sender: TObject);
begin
  FViewMode := vmAll;
  LoadTree(false);
  btnView.ImageIndex := actTestViewAll.ImageIndex;
end;

procedure TIdeTesterForm.actTestViewFlatExecute(Sender: TObject);
begin
  FViewMode := vmFlat;
  LoadTree(false);
  btnView.ImageIndex := actTestViewFlat.ImageIndex;
end;

procedure TIdeTesterForm.actTestViewIssuesExecute(Sender: TObject);
begin
  FViewMode := vmIssues;
  LoadTree(false);
  btnView.ImageIndex := actTestViewIssues.ImageIndex;
end;

procedure TIdeTesterForm.actTestViewUnrunExecute(Sender: TObject);
begin
  FViewMode := vmNotRun;
  LoadTree(false);
  btnView.ImageIndex := actTestViewUnrun.ImageIndex;
end;

procedure TIdeTesterForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := not FRunning;
end;

procedure TIdeTesterForm.actTestRunFailedExecute(Sender: TObject);
var
  ti : TTestNode;
begin
  for ti in FTestInfo do
  begin
    ti.execute := (ti.outcome in [toFail, toError, toHalt]);
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
    StartTestRun(false);
    FThread := TTestThread.create(self);
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
    FKillTime := GetTickCount64 + StrToInt(store.read(tsmConfig, 'killtime', inttostr(DEFAULT_KILL_TIME_DELAY)));
    actTestStop.ImageIndex := IMG_INDEX_WAIT;
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

procedure TIdeTesterForm.pmTestsPopup(Sender: TObject);
begin
  mnuGo.visible := engine.doesSource;
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
            ev.node.ExceptionClassName := '';
            ev.node.ExceptionMessage := '';
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
            ev.node.SourceUnitError := ev.SourceUnitName;
            ev.node.LineNumber := ev.LineNumber;
            ev.node.outcome := toFail;
            Inc(FFailCount);
            lblStatus.caption := '';
          end;
        tekError:
          begin
            ev.node.ExceptionMessage := ev.excMessage;
            ev.node.ExceptionClassName := ev.excClass;
            ev.node.SourceUnitError := ev.sourceUnitName;
            ev.node.LineNumber := ev.LineNumber;
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

procedure TIdeTesterForm.StartTestRun(debug : boolean);
var
  ti : TTestNode;
begin
  saveState;
  FStartTime := 0;
  FRunning := true;
  FWantStop := false;
  FTestsCount := 0;
  FFailCount := 0;
  FErrorCount := 0;
  pbBar.Invalidate;
  setActionStatus(1, 0, 0);
  timer1.Enabled := true;

  FSession := engine.prepareToRunTests;
  for ti in FTestInfo do
    if not ti.execute then
      FSession.skipTest(ti);
  if debug then
    if not engine.SetUpDebug(FSession, FRunningTest) then
      abort;
  FStartTime := GetTickCount64;
  FEndTime := 0;
end;

procedure TIdeTesterForm.FinishTestRun;
begin
  FThread := nil;
  FRunningTest := nil;
  FKillTime := 0;
  FEndTime := GetTickCount64;
  actTestStop.ImageIndex := IMG_INDEX_STOP;
  engine.finishTestRun(FSession);
  FSession := nil;
  Timer1.Enabled := false;
  saveState;
  FRunning := false;
  UpdateTotals;
  pbBarPaint(pbBar);
end;

procedure TIdeTesterForm.actTestCopyResultsExecute(Sender: TObject);
begin
  Clipboard.AsText := FSelectedNode.details('');
end;

procedure TIdeTesterForm.actTestGoErrorExecute(Sender: TObject);
begin
  if FSelectedNode <> nil then
    engine.openSource(FSelectedNode, osmError);
end;

procedure TIdeTesterForm.actTestGoSourceExecute(Sender: TObject);
begin
  if FSelectedNode <> nil then
    engine.openSource(FSelectedNode, osmDefinition);
end;

procedure TIdeTesterForm.actTestReloadExecute(Sender: TObject);
begin
  LoadTree(true);
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

    StartTestRun(true);
    FThread := TTestThread.create(self);
    FThread.FreeOnTerminate := true;
  end;
end;

procedure TIdeTesterForm.actTestConfigureExecute(Sender: TObject);
begin
  IDETesterSettings := TIDETesterSettings.create(self);
  try
    IDETesterSettings.pnlKillTime.visible := engine.canTerminate;
    IDETesterSettings.pnlParameters.visible := engine.canParameters;
    IDETesterSettings.pnlTester.visible := engine.canTestProject;
    IDETesterSettings.editWaitTime.Text := store.read(tsmConfig, 'killtime', inttostr(DEFAULT_KILL_TIME_DELAY));
    IDETesterSettings.edtParameters.text := store.read(tsmConfig, 'parameters', '');
    IDETesterSettings.edtTester.text := store.read(tsmConfig, 'testproject', '');
    IDETesterSettings.chkAutoSave.Checked :=  store.read(tsmConfig, 'autosave', '0') = '1';
    if IDETesterSettings.showModal = mrOk then
    begin
      store.save(tsmConfig, 'parameters', IDETesterSettings.edtParameters.text);
      store.save(tsmConfig, 'testproject', IDETesterSettings.edtTester.text);
      store.save(tsmConfig, 'killtime', IDETesterSettings.editWaitTime.Text);
      if IDETesterSettings.chkAutoSave.Checked then
        store.save(tsmConfig, 'autosave', '1')
      else
        store.save(tsmConfig, 'autosave', '0');
      UpdateActions;
    end;

  finally
    IDETesterSettings.free;
  end;
end;

procedure TIdeTesterForm.actTestCopyNameExecute(Sender: TObject);
begin
  Clipboard.AsText := FSelectedNode.testName;
end;

procedure TIdeTesterForm.actTestCopyPathExecute(Sender: TObject);
var
  s : String;
  n : TTestNode;
begin
  n := FSelectedNode;
  s := '';
  while n <> nil do
  begin
    s := n.testName + ' / ' + s;
    n := n.parent;
  end;
  Clipboard.AsText := s.Substring(0, s.length-3);
end;

// -- test thread  - no UI access ----------------------------------------------

procedure TIdeTesterForm.DoExecuteTests;
begin
  engine.runTest(FSession, FRunningTest);
end;

procedure TIdeTesterForm.queueEvent(test: TTestNode; event : TTestEventKind; msg, clssName, srcUnit : String; line : integer);
var
  ev : TTestEvent;
begin
  ev := TTestEvent.create;
  ev.node := test;
  ev.event := event;
  ev.excMessage := msg;
  ev.excClass := clssName;
  ev.sourceUnitName := srcUnit;
  ev.lineNumber := line;
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


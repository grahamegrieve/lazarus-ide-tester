unit idetester_intf;

{$mode Delphi}

interface

uses
  SysUtils, Classes,
  // LCL
  LCLType, Forms,
  // IdeIntf
  MenuIntf, IDECommands, ToolBarIntf, IDEWindowIntf, LazLoggerBase, LazIDEIntf,
  idetester_strings, idetester_form, idetester_ide;

procedure Register;

implementation

var
  IDETesterCreator : TIDEWindowCreator;

procedure IDEMenuSectionClicked(Sender: TObject);
begin
  IDEWindowCreators.ShowForm(IDETesterCreator.FormName,true);
end;

procedure CreateIDETester(Sender: TObject; aFormName: string; var AForm: TCustomForm; DoDisableAutoSizing: boolean);
var
  engine : TTestEngineIDE;
begin
  // sanity check to avoid clashing with another package that has registered a window with the same name
  if CompareText(aFormName,'IDETester') <> 0 then begin
    DebugLn(['ERROR: CreateIDETester: there is already a form with the name "IDETester"']);
    exit;
  end;
  IDEWindowCreators.CreateForm(AForm, TIdeTesterForm, true, LazarusIDE.OwningComponent);
  AForm.Name := aFormName;
  IdeTesterForm := AForm as TIdeTesterForm;
  if not DoDisableAutoSizing then
    AForm.EnableAutoSizing;

  // ide tester specific stuff
  IdeTesterForm.caption := rs_IdeTester_Caption_View;
  IdeTesterForm.store := TTestSettingsIDEProvider.create;
  engine := TTestEngineIDE.create;
  IdeTesterForm.engine := engine;
  LazarusIDE.AddHandlerOnProjectOpened(engine.openProject, false);
end;


procedure Register;
var
  IDEShortCutX: TIDEShortCut;
  IDECommandCategory: TIDECommandCategory;
  IDECommand: TIDECommand;
begin
  IDEShortCutX := IDEShortCut(VK_T, [ssCtrl, ssAlt, ssShift], VK_UNKNOWN, []);
  IDECommandCategory := IDECommandList.FindCategoryByName(CommandCategoryViewName);
  IDECommand := nil;
  if IDECommandCategory <> nil then
  begin
    IDECommand := RegisterIDECommand(IDECommandCategory, rs_IdeTester_Name, rs_IdeTester_Caption, IDEShortCutX, nil, @IDEMenuSectionClicked);
    if IDECommand <> nil then
      RegisterIDEButtonCommand(IDECommand);
  end;
  RegisterIDEMenuCommand(itmViewMainWindows, rs_IdeTester_Name, rs_IdeTester_Caption, nil, @IDEMenuSectionClicked, IDECommand);
  RegisterIDEMenuCommand(ComponentPalettePageDropDownExtraEntries, rs_IdeTester_Name, rs_IdeTester_Caption, nil, @IDEMenuSectionClicked, nil);

  // register dockable Window
  // default place at left=200, top=100, right=400, bottom=400
  IDETesterCreator := IDEWindowCreators.Add('IDETester', @CreateIDETester,nil, '200','100','400','400');
end;

end.


unit idetester_intf;

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
  LazarusIDE.AddHandlerOnRunDebug(engine.startRun, false);
  LazarusIDE.AddHandlerOnRunFinished(engine.endRun, false);
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


unit idetester_ide;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process,
  UITypes, Forms, Dialogs,
  ProjectIntf, LazIDEIntf, MacroIntf, CompOptsIntf,
  idetester_strings, idetester_base, idetester_external;

type
  { TTestEngineIDE }

  TTestEngineIDE = class (TTestEngineExternal)
  protected
    function runProgram(params : TStringList; debug : boolean) : TProcess; override;
    function autoLoad : boolean; override;
  public
    function OpenProject(Sender: TObject; AProject: TLazProject): TModalResult;
  end;

  { TTestSettingsProjectProvider }

  TTestSettingsProjectProvider = class (TTestSettingsProvider)
  private
  public
    function read(name, defValue : String) : String; override;
    procedure save(name, value : String); override;
  end;

implementation

{ TTestSettingsProjectProvider }

function TTestSettingsProjectProvider.read(name, defValue: String): String;
begin
  if (LazarusIDE <> nil) and (LazarusIDE.ActiveProject <> nil) and LazarusIDE.ActiveProject.CustomSessionData.Contains('idetester.'+name) then
    result := LazarusIDE.ActiveProject.CustomSessionData['idetester.'+name]
  else
    result := defValue;
end;

procedure TTestSettingsProjectProvider.save(name, value: String);
begin
  LazarusIDE.ActiveProject.CustomSessionData['idetester.'+name] := value;
end;

function nameForType(p : TProjectExecutableType) : String;
begin
  case p of
    petNone : result := 'None';
    petProgram : result := 'Program';
    petLibrary : result := 'Library';
    petPackage : result := 'Package';
    petUnit : result := 'Unit';
  else
    result := '??';
  end;
end;

{ TTestEngineIDE }

function TTestEngineIDE.runProgram(params: TStringList; debug : boolean): TProcess;
var
  exeName : String;
begin
  result := nil;
  if (LazarusIDE = nil) or (LazarusIDE.ActiveProject = nil) then
    ShowMessage(rsLazarusIDETester_Err_No_Project)
  else if (LazarusIDE.ActiveProject.ExecutableType <> petProgram) then
    ShowMessage(rsLazarusIDETester_Err_Project_Type+' ('+nameForType(LazarusIDE.ActiveProject.ExecutableType)+')')
  else
  begin
    if LazarusIDE.DoBuildProject(crCompile, []) = mrOk then
    begin
      exeName := '$(TargetFile)';
      if not IDEMacros.SubstituteMacros(exeName) then
        ShowMessage(rsLazarusIDETester_Err_Project_Target)
      else
      begin
        result := TProcess.create(nil);
        result.Executable := exeName;
        result.Parameters := params;
        result.ShowWindow := swoHIDE;
        result.Options := [poUsePipes];
        result.Execute;
      end;
    end;
  end;
end;

function TTestEngineIDE.autoLoad: boolean;
begin
  Result := false;
end;

function TTestEngineIDE.OpenProject(Sender: TObject; AProject: TLazProject): TModalResult;
begin
  OnClearTests(self);
  result := mrOk;
end;

end.


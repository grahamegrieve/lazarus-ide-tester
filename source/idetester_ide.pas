unit idetester_ide;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process,
  UITypes, Forms, Dialogs,
  ProjectIntf, LazIDEIntf, MacroIntf, CompOptsIntf,
  idetester_strings, idetester_base, idetester_external;

type
  { TTestEngineIDESession }

  TTestEngineIDESession = class (TTestEngineExternalSession)
  private
    FExeName : String;
  public
    function compile : boolean;
    property exeName : String read FExeName write FExeName;
  end;

  { TTestEngineIDE }

  TTestEngineIDE = class (TTestEngineExternal)
  protected
    function runProgram(session : TTestEngineExternalSession; params : TStringList; debug : boolean) : TProcess; override;
    function autoLoad : boolean; override;
  public
    function prepareToRunTests : TTestSession; override;
    function OpenProject(Sender: TObject; AProject: TLazProject): TModalResult;
    function executableName() : String; override;
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
  if (LazarusIDE <> nil) and (LazarusIDE.ActiveProject <> nil) then
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

function TTestEngineIDE.runProgram(session : TTestEngineExternalSession; params: TStringList; debug : boolean): TProcess;
var
  sess : TTestEngineIDESession;
begin
  if session = nil then
    sess := TTestEngineIDESession.create
  else
    sess := session as TTestEngineIDESession;
  try
    if sess.compile then
    begin
      result := TProcess.create(nil);
      result.Executable := sess.FExeName;
      result.Parameters := params;
      result.ShowWindow := swoHIDE;
      result.Options := [poUsePipes];
      result.Execute;
    end;
  finally
    if session = nil then
      sess.Free;
  end;
end;

function TTestEngineIDE.autoLoad: boolean;
begin
  Result := false;
end;

function TTestEngineIDE.prepareToRunTests: TTestSession;
begin
  Result := TTestEngineIDESession.create;
  (result as TTestEngineIDESession).compile;
end;

function TTestEngineIDE.OpenProject(Sender: TObject; AProject: TLazProject): TModalResult;
begin
  if assigned(OnReinitialise) then
    OnReinitialise(self);
  result := mrOk;
end;

function TTestEngineIDE.executableName(): String;
var
  en : String;
begin
  result := '';
  if (LazarusIDE <> nil) and (LazarusIDE.ActiveProject <> nil) then
  begin
    en := '$(TargetFile)';
    IDEMacros.SubstituteMacros(en);
    result := en;
  end;
end;

{ TTestEngineIDESession }

function TTestEngineIDESession.compile : boolean;
var
  en : String;
begin
  if FExeName <> '' then
    exit(true);

  result := false;
  if (LazarusIDE = nil) or (LazarusIDE.ActiveProject = nil) then
    ShowMessage(rs_IdeTester_Err_No_Project)
  else if LazarusIDE.DoBuildProject(crCompile, []) = mrOk then
  begin
    en := '$(TargetFile)';
    if not IDEMacros.SubstituteMacros(en) then
      ShowMessage(rs_IdeTester_Err_Project_Target)
    else if (LazarusIDE.ActiveProject.ExecutableType <> petProgram) then
      ShowMessage(Format(rs_IdeTester_Err_Project_Type, [nameForType(LazarusIDE.ActiveProject.ExecutableType)]))
    else
    begin
      result := true;
      FExeName := en;
    end;
  end;
end;

end.



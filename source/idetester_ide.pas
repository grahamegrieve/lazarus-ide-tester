unit idetester_ide;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, IniFiles,
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
    function hasTestProject: boolean; override;
  end;

  { TTestSettingsIDEProvider }

  TTestSettingsIDEProvider = class (TTestSettingsProvider)
  private
    function GetStatusFileName : String;
  public
    function read(mode : TTestSettingsMode; name, defValue : String) : String; override;
    procedure save(mode : TTestSettingsMode; name, value : String); override;
  end;

implementation

function nameForType(p : TProjectExecutableType) : String;
begin
  case p of
    petNone : result := 'None';
    petProgram : result := 'Program';
    petLibrary : result := 'Library';
    petPackage : result := 'Package';
    petUnit : result := 'Unit';
  end;
end;

{ TTestSettingsIDEProvider }

function TTestSettingsIDEProvider.GetStatusFileName: String;
var
  tp : String;
begin
  tp := read(tsmConfig, 'testproject', '');
  if tp = '' then
    tp := LazarusIDE.ActiveProject.ProjectInfoFile;
  result := tp.Replace('.lpi', '') + '.testing.ini';
end;

function TTestSettingsIDEProvider.read(mode : TTestSettingsMode; name, defValue: String): String;
var
  ini : TIniFile;
begin
  // config settings are stored in the project file
  // status settings are stored next to the target project
  if (LazarusIDE = nil) or (LazarusIDE.ActiveProject = nil) then
    exit(defValue);

  result := defValue;
  if mode = tsmConfig then
  begin
    if LazarusIDE.ActiveProject.CustomSessionData.Contains('idetester.'+name) then
      result := LazarusIDE.ActiveProject.CustomSessionData['idetester.'+name]
  end
  else
  begin
    ini := TIniFile.create(GetStatusFileName);
    try
      result := ini.ReadString('Status', name, defValue)
    finally
      ini.free;
    end;
  end;
end;

procedure TTestSettingsIDEProvider.save(mode : TTestSettingsMode; name, value: String);
var
  ini : TIniFile;
begin
  if (LazarusIDE <> nil) and (LazarusIDE.ActiveProject <> nil) then
  begin
    if mode = tsmConfig then
      LazarusIDE.ActiveProject.CustomSessionData['idetester.'+name] := value
    else
    begin
      ini := TIniFile.create(GetStatusFileName);
      try
        ini.WriteString('Status', name, value)
      finally
        ini.free;
      end;
    end;
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
      result.CurrentDirectory := ExtractFileDir(sess.FExeName);
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

// only called when testProject = '' (debugging form)
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

function TTestEngineIDE.hasTestProject: boolean;
begin
  Result := true;
end;

{ TTestEngineIDESession }

function TTestEngineIDESession.compile : boolean;
var
  en : String;
begin
  // we've already compiled successfully
  if FExeName <> '' then
    exit(true);

  // now we compile. If test project is '', we compile the current project.
  // else we use lazbuild to build the specified test project. SaveAll first?
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



unit idetester_ide;

{$mode delphi}

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
    FOutput : TStringList;
    function isErrorLine(s : string; var fn, line, err : String) : boolean;
    procedure processLine(line : String; var stop : boolean);
    function compileCurrentProject: boolean;
    function compileProject(lpi: String): boolean;
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
    procedure openSource(test : TTestNode); override;
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
  ok : boolean;
begin
  if session = nil then
    sess := TTestEngineIDESession.create
  else
    sess := session as TTestEngineIDESession;
  try
    if sess.FExeName = '' then
    begin
      setStatus('Compiling '+LazarusIDE.ActiveProject.CustomSessionData['idetester.testproject']);
      if not sess.compile then
        exit;
      setStatus('Loading');
    end;
    result := TProcess.create(nil);
    result.Executable := sess.FExeName;
    result.CurrentDirectory := ExtractFileDir(sess.FExeName);
    result.Parameters := params;
    result.ShowWindow := swoHIDE;
    result.Options := [poUsePipes];
    result.Execute;
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
  setStatus('Compiling '+LazarusIDE.ActiveProject.CustomSessionData['idetester.testproject']);
  (result as TTestEngineIDESession).compile;
  setStatus('Running');
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

procedure TTestEngineIDE.openSource(test: TTestNode);
var
  pn : String;
  point : TPoint;
begin
  if (LazarusIDE <> nil) and (LazarusIDE.ActiveProject <> nil) and (test.SourceUnitName <> '') then
  begin
    pn := LazarusIDE.ActiveProject.CustomSessionData['idetester.testproject'];
    if pn = '' then
      pn := LazarusIDE.ActiveProject.ProjectInfoFile;
    pn := ExpandFileName(IncludeTrailingPathDelimiter(ExtractFileDir(pn))+test.SourceUnitName);
    point.x := 0;
    point.y := test.LineNumber;
    LazarusIDE.DoOpenFileAndJumpToPos(pn, point, test.LineNumber, -1, -1, [ofRegularFile]);
  end;
end;

{ TTestEngineIDESession }

function TTestEngineIDESession.compile : boolean;
begin
  // we've already compiled successfully
  if FExeName <> '' then
    exit(true);

  // now we compile. If test project is '', we compile the current project.
  // else we use lazbuild to build the specified test project. SaveAll first?
  result := false;
  if (LazarusIDE = nil) or (LazarusIDE.ActiveProject = nil) then
    ShowMessage(rs_IdeTester_Err_No_Project)
  else if LazarusIDE.ActiveProject.CustomSessionData['idetester.testproject'] <> '' then
    result  := compileProject(LazarusIDE.ActiveProject.CustomSessionData['idetester.testproject'])
  else
    result := compileCurrentProject;
end;

function TTestEngineIDESession.compileProject(lpi : String) : boolean;
var
  lb, tfn, s, err, line, fn : String;
  params : TStringList;
  pp : TProcess;
  p : TTesterOutputProcessor;
  point : TPoint;
begin
  if LazarusIDE.ActiveProject.CustomSessionData['idetester.autosave'] = '1' then
    if LazarusIDE.DoSaveAll([sfCanAbort]) <> mrOK then
      exit(false);

  lb := IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0)))+'lazbuild.exe';
  if not FileExists(lpi) then
    ShowMessage(Format(rs_IdeTester_Err_Project_Not_Found, [lpi]))
  else if not FileExists(lb) then
    ShowMessage(Format(rs_IdeTester_Err_LazBuild_Not_Found, [lb]))
  else
  begin
    FOutput := TStringList.create;
    try
      params := TStringList.create;
      try
        pp := TProcess.create(nil);
        try
          pp.Executable := lb;
          pp.CurrentDirectory := ExtractFileDir(lpi);
          params.add(lpi);
          pp.Parameters := params;
          pp.ShowWindow := swoHIDE;
          pp.Options := [poUsePipes];
          pp.Execute;
          p := TTesterOutputProcessor.create(pp, processLine);
          try
            p.process;
          finally
            p.free;
          end;
          result := pp.ExitCode = 0;
        finally
          process.free;
        end;
      finally
        params.free;
      end;

      tfn := IncludeTrailingPathDelimiter(GetTempDir(false))+'idetester-lazbuild-output.log';
      FOutput.SaveToFile(tfn);

      if not result then
      begin
        for s in FOutput do
        begin
          if isErrorLine(s, fn, line, err) then
          begin
            point.x := 0;
            point.y := StrToIntDef(line, 1);
            if LazarusIDE.DoOpenFileAndJumpToPos(fn, point, StrToIntDef(line, 1), -1, -1, [ofRegularFile]) = mrOk then
            begin
              ShowMessage(Format(rs_IdeTester_Err_LazBuild_Error, [err, line, tfn]));
              exit;
            end;
          end;
        end;
        ShowMessage(Format(rs_IdeTester_Err_LazBuild_Failed, [tfn]))
      end
      else
      begin
        for s in FOutput do
          if s.contains(') Linking ') then
            FExeName := s.Substring(s.IndexOf(') Linking ')+10).trim;

        result := false;
        if FExeName = '' then
          ShowMessage(Format(rs_IdeTester_Err_LazBuild_No_ExeName, [tfn]))
        else if not FileExists(FExeName) then
          ShowMessage(Format(rs_IdeTester_Err_LazBuild_No_Exe, [FExeName, tfn]))
        else
          result := true;
      end;
    finally
      FOutput.free;
    end;
  end;
end;

function TTestEngineIDESession.isErrorLine(s: string; var fn, line, err: String): boolean;
var
  p : TStringArray;
begin
  result := s.Contains(') Error: ') or s.Contains(') Fatal: ') ;
  if result then
  begin
    p := s.Split(['(', ')', ',']);
    fn := p[0];
    line := p[1];
    if s.Contains(') Error: ') then
      err := s.Substring(s.IndexOf('Error:')+6).trim
    else
      err := s.Substring(s.IndexOf('Fatal:')+6).trim;
    result := FileExists(fn);
  end;
end;

procedure TTestEngineIDESession.processLine(line: String; var stop: boolean);
begin
  FOutput.add(line);
  stop := false;
end;

function TTestEngineIDESession.compileCurrentProject : boolean;
var
  en : String;
begin
  result := false;
  if LazarusIDE.DoBuildProject(crCompile, []) = mrOk then
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



unit idetester_ide;

{$mode delphi}

interface

uses
  Classes, SysUtils, Process, IniFiles,
  UITypes, Forms, Dialogs,
  ProjectIntf, LazIDEIntf, MacroIntf, CompOptsIntf, IDEIntf,
  idetester_strings, idetester_runtime, idetester_base, idetester_external;

const
  EOL = {$IFDEF MSWINDOWS} #13#10 {$ELSE} #10 {$ENDIF};

type
  { TTestEngineIDESession }

  TTestEngineIDESession = class (TTestEngineExternalSession)
  private
    FDebugging: boolean;
    FExeName : String;
    FOutput : TStringList;
    function isErrorLine(s : string; var fn, line, err : String) : boolean;
    procedure processLine(line : String);
    function compileCurrentProject: boolean;
    function compileProject(lpi: String): boolean;
  public
    function compile : boolean;
    property exeName : String read FExeName write FExeName;
    property debugging : boolean read FDebugging write FDebugging;
  end;

  { TTestEngineIDE }

  TTestEngineIDE = class (TTestEngineExternal)
  private
    FDebuggingSession : TTestEngineIDESession;
  protected
    function runProgram(session : TTestEngineExternalSession; params : TStringList) : TProcess; override;
    function autoLoad : boolean; override;
    function makeSession : TTestEngineExternalSession; override;
    procedure FinishTests; override;
  public
    function prepareToRunTests : TTestSession; override;
    function OpenProject(Sender: TObject; AProject: TLazProject): TModalResult;
    procedure endRun(Sender: TObject);
    function hasTestProject: boolean; override;
    procedure openSource(test : TTestNode); override;
    function canDebug : boolean; override;
    function setUpDebug(session : TTestSession; node : TTestNode) : boolean; override;
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

type
  { TConsoleOutputProcessor }

  TConsolerOutputProcessLineEvent = procedure (line : String) of object;

  TConsoleOutputProcessor = class (TObject)
  private
    FProcess : TProcess;
    FEvent : TConsolerOutputProcessLineEvent;
    FCarry : String;
    procedure processOutput(text : String);
  public
    constructor Create(process : TProcess; event : TConsolerOutputProcessLineEvent);

    procedure Process;
    property Event : TConsolerOutputProcessLineEvent read FEvent write FEvent;
  end;

procedure TConsoleOutputProcessor.processOutput(text: String);
var
  curr, s : String;
begin
  curr := FCarry + text;
  while curr.contains(EOL) do
  begin
    StringSplit(curr, EOL, s, curr);
    event(s);
  end;
  FCarry := curr;
end;

constructor TConsoleOutputProcessor.Create(process: TProcess; event : TConsolerOutputProcessLineEvent);
begin
  inherited Create;
  FEvent := event;
  FProcess := process;
end;

procedure TConsoleOutputProcessor.Process;
var
  BytesRead, total : longint;
  Buffer           : TBytes;
  start : UInt64;
begin
  start := GetTickCount64;
  total := 0;
  repeat
    SetLength(Buffer, BUF_SIZE);
    if FProcess.Output.NumBytesAvailable > 0 then
    begin
      BytesRead := FProcess.Output.Read(Buffer, BUF_SIZE);
      total := total + BytesRead;
      processOutput(TEncoding.UTF8.GetAnsiString(Buffer, 0, BytesRead));
    end;
    if (total = 0) and (GetTickCount64 - start > CONSOLE_TIMEOUT) then
      exit;
  until not FProcess.Active;
  if FProcess.Active then
    FProcess.Terminate(1);
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

function TTestEngineIDE.runProgram(session : TTestEngineExternalSession; params: TStringList): TProcess;
var
  sess : TTestEngineIDESession;
  ok : boolean;
begin
  sess := session as TTestEngineIDESession;
  if sess.debugging then
  begin
    // we already started the debugging session; we're going to create a fake process so we can terminate it.
    result := TProcess.create(nil);
    result.active := true;
  end
  else
  begin
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
    result.Options := [];
    result.Execute;
  end;
end;

function TTestEngineIDE.autoLoad: boolean;
begin
  Result := false;
end;

function TTestEngineIDE.makeSession: TTestEngineExternalSession;
begin
  Result := TTestEngineIDESession.create;
end;

procedure TTestEngineIDE.FinishTests;
begin
  FDebuggingSession := nil;
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

procedure TTestEngineIDE.endRun(Sender: TObject);
begin
  if (FDebuggingSession <> nil) and (FDebuggingSession.Process <> nil) then
    FDebuggingSession.Process.active := false;
end;

function TTestEngineIDE.hasTestProject: boolean;
begin
  Result := true;
end;

function firstLineMention(src : String; clss, test : String) : integer;
var
  ts : TStringList;
  i : integer;
  s : String;
begin
  result := 0;
  ts := TStringList.create;
  try
    ts.LoadFromFile(src);
    for i := 0 to ts.count - 1 do
    begin
      s := ts[i].Trim().Replace(' ', '');
      if s.contains(clss+'.'+test) then
        exit(i+1);
    end;
    for i := 0 to ts.count - 1 do
    begin
      s := ts[i].Trim().Replace(' ', '');
      if s.contains(clss+'=') then
        exit(i+1);
    end;
    for i := 0 to ts.count - 1 do
    begin
      s := ts[i];
      if s.contains(clss) then
        exit(i+1);
    end;
  finally
    ts.free;
  end;
end;

procedure TTestEngineIDE.openSource(test: TTestNode);
var
  pn : String;
  point : TPoint;
begin
  if (LazarusIDE <> nil) and (LazarusIDE.ActiveProject <> nil) and (test.SourceUnitError <> '') then
  begin
    pn := LazarusIDE.ActiveProject.CustomSessionData['idetester.testproject'];
    if pn = '' then
      pn := LazarusIDE.ActiveProject.ProjectInfoFile;
    pn := ExpandFileName(IncludeTrailingPathDelimiter(ExtractFileDir(pn))+test.SourceUnitError);
    point.x := 0;
    point.y := test.LineNumber;
    LazarusIDE.DoOpenFileAndJumpToPos(pn, point, test.LineNumber, -1, -1, [ofRegularFile]);
  end
  else if (LazarusIDE <> nil) and (LazarusIDE.ActiveProject <> nil) and (test.SourceUnit <> '') and (test.SourceUnit <> 'fpcunit') then
  begin
    pn := LazarusIDE.FindUnitFile(test.sourceUnit);
    if pn <> '' then
    begin
      point.x := 0;
      point.y := firstLineMention(pn, test.testClassName, test.testName);
      LazarusIDE.DoOpenFileAndJumpToPos(pn, point, point.y, -1, -1, [ofRegularFile]);
    end;
  end
end;

function TTestEngineIDE.canDebug: boolean;
begin
  result := true;
end;

function TTestEngineIDE.setUpDebug(session: TTestSession; node: TTestNode) : boolean;
var
  params, mode : String;
  pm : TAbstractRunParamsOptionsMode;
begin
  result := false;
  if (LazarusIDE = nil) or (LazarusIDE.ActiveProject = nil) then
    ShowMessage(rs_IdeTester_Err_No_Project)
  else
  begin
    // 1. determine the correct parameters
    params := '-'+FPC_MAGIC_COMMAND+' -server '+FIPCServer.id+' -run '+(node.Data as TTestNodeId).id;
    if settings.parameters <> '' then
      params := params + ' '+settings.parameters;

    //  2. set the parameters in the run mode 'test'
    pm := LazarusIDE.ActiveProject.RunParameters.Find('Test');
    if pm = nil then
      pm := LazarusIDE.ActiveProject.RunParameters.Add('Test');
    pm.HostApplicationFilename := '';
    pm.CmdLineParams := params;
    pm.UseLaunchingApplication := false;
    pm.LaunchingApplicationPathPlusParams := '';
    pm.WorkingDirectory := ExtractFileDir((session as TTestEngineIDESession).FExeName);
    pm.UseDisplay := false;
    pm.Display := '';
    pm.IncludeSystemVariables := true;

    //  3. set the mode 'test' as active
    LazarusIDE.ActiveProject.RunParameters.ActiveModeName := 'Test';

    //  4. ask the IDE to debug
    if LazarusIDE.DoRunProject = mrOK then
    begin
      result := true;
      FDebuggingSession := (session as TTestEngineIDESession);
      FDebuggingSession.debugging := true; // ok, all good. Now, remember that we're debugging..
    end;

    // 5. restore the mode
    LazarusIDE.ActiveProject.RunParameters.ActiveModeName := mode;

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
  p : TConsoleOutputProcessor;
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
          p := TConsoleOutputProcessor.create(pp, processLine);
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

procedure TTestEngineIDESession.processLine(line: String);
begin
  FOutput.add(line);
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



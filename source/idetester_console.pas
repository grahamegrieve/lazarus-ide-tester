unit idetester_console;

{
This is adapted/modified from the base FPCUnit ConsoleTestRunner. The purpose
of the modifications is to give the host more control over how the the tests
are run - e.g. to fit in with a CI Build framework
}

{
Modifications (not many) are Copyright (c) 2011+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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
  Classes, SysUtils, Contnrs, dateutils, custapp, dom, inifiles,
  fpcunit, testutils, testregistry, testdecorator,
  fpcunitreport, latextestreport, xmltestreport, plaintestreport;

type
  TFormat = (fPlain, fLatex, fXML, fPlainNoTiming, fSimple);

var
  DefaultFormat : TFormat = fSimple;

type

  { TSuiteInfo }

  TSuiteInfo = class
  private
    index : integer;
    total : integer;
    ignored : integer;
    errors : integer;
    fails : integer;
    procedure add(info : TSuiteInfo);
  public
    constructor create(index : integer);
  end;

  { TSimpleResultsWriter }

  TSimpleResultsWriter = class(TCustomResultsWriter)
  private
    FTestResultOptions : TTestResultOptions;
    FDoc: TStringList;
    FSuites : TObjectList;
    FTempFailure: TTestFailure;
    function TimeFormat(ATiming: TDateTime): String;
  protected
    procedure SetSkipAddressInfo(AValue: Boolean); override;
    procedure SetSparse(AValue: Boolean); override;
    procedure WriteTestHeader(ATest: TTest; ALevel: integer; ACount: integer); override;
    procedure WriteTestFooter(ATest: TTest; ALevel: integer; ATiming: TDateTime); override;
    procedure WriteSuiteHeader(ATestSuite: TTestSuite; ALevel: integer); override;
    procedure WriteSuiteFooter(ATestSuite: TTestSuite; ALevel: integer; ATiming: TDateTime; ANumRuns: integer; ANumErrors: integer; ANumFailures: integer; ANumIgnores: integer); override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure WriteHeader; override;
    procedure WriteResult(aResult: TTestResult); override;
    procedure AddFailure(ATest: TTest; AFailure: TTestFailure); override;
    procedure AddError(ATest: TTest; AError: TTestFailure); override;
  end;



  { TTestRunner }

  TIdeTesterConsoleRunner = class(TCustomApplication)
  private
    FShowProgress: boolean;
    FFileName: string;
    FFormat: TFormat;
    FSkipTiming : Boolean;
    FSParse: Boolean;
    FSkipAddressInfo : Boolean;
    FSuite: String;
  protected
    procedure RunSuite; virtual;
    procedure DoTestRun(ATest: TTest); virtual;
    procedure ExtendXmlDocument(Doc: TXMLDocument); virtual;
    function GetResultsWriter: TCustomResultsWriter; virtual;
    procedure DoRun; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property ShowProgress: boolean read FShowProgress write FShowProgress;
    property FileName: string read FFileName write FFileName;
    property Format: TFormat read FFormat write FFormat;
    property SkipTiming : Boolean read FSkipTiming write FSkipTiming;
    property Sparse: Boolean read FSparse write FSparse;
    property SkipAddressInfo : Boolean read FSkipAddressInfo write FSkipAddressInfo;
  end;


implementation

{ TProgressWriter }

Type
  TTestDecoratorClass = Class of TTestDecorator;

  { TDecoratorTestSuite }

  TDecoratorTestSuite = Class(TTestSuite)
  public
    Destructor Destroy; override;
  end;

  TProgressWriter= class(TNoRefCountObject, ITestListener)
  private
    FTotal : Integer;
    FFailed: Integer;
    FIgnored : Integer;
    FErrors : Integer;
    FQuiet : Boolean;
    fcount : integer;
    FSuccess : Boolean;
    procedure WriteChar(c: char);
  public
    Constructor Create(AQuiet : Boolean);
    destructor Destroy; override;
    Function GetExitCode : Integer;
    { ITestListener interface requirements }
    procedure AddFailure(ATest: TTest; AFailure: TTestFailure);
    procedure AddError(ATest: TTest; AError: TTestFailure);
    procedure StartTest(ATest: TTest);
    procedure EndTest(ATest: TTest);
    procedure StartTestSuite(ATestSuite: TTestSuite);
    procedure EndTestSuite(ATestSuite: TTestSuite);
    Property Total : Integer Read FTotal;
    Property Failed : Integer Read FFailed;
    Property Errors : Integer Read FErrors;
    Property Ignored : Integer Read FIgnored;
    Property Quiet : Boolean Read FQuiet;
  end;

{ TSuiteInfo }

procedure TSuiteInfo.add(info: TSuiteInfo);
begin
  total := total + info.total;
  ignored := ignored + info.ignored;
  errors := errors + info.errors;
  fails := fails + info.fails;
end;

constructor TSuiteInfo.create(index: integer);
begin
  inherited create;
  self.index := index;
end;

{ TDecoratorTestSuite }

destructor TDecoratorTestSuite.Destroy;
begin
  OwnsTests:=False;
  inherited Destroy;
end;

{ TProgressWriter }

procedure TProgressWriter.WriteChar(c: char);
begin
  write(c);
  // flush output, so that we see the char immediately, even it is written to file
  Flush(output);
end;

constructor TProgressWriter.Create(AQuiet: Boolean);

begin
  FQuiet:=AQuiet;
end;

destructor TProgressWriter.Destroy;
begin
  // on descruction, just write the missing line ending
  writeln;
  inherited Destroy;
end;

function TProgressWriter.GetExitCode: Integer;

begin
  Result:=Ord(Failed<>0); // Bit 0 indicates fails
  if Errors<>0 then
    Result:=Result or 2;  // Bit 1 indicates errors.
end;

procedure TProgressWriter.AddFailure(ATest: TTest; AFailure: TTestFailure);
begin
  FSuccess:=False;
  If AFailure.IsIgnoredTest then
    Inc(FIgnored)
  else
    Inc(FFailed);
  If Not Quiet then
    writechar('F');
end;

procedure TProgressWriter.AddError(ATest: TTest; AError: TTestFailure);
begin
  FSuccess:=False;
  Inc(FErrors);
  if not Quiet then
    writechar('E');
end;

procedure TProgressWriter.StartTest(ATest: TTest);
begin
FSuccess := true; // assume success, until proven otherwise
end;

procedure TProgressWriter.EndTest(ATest: TTest);
begin
if FSuccess and not Quiet then
  writechar('.');
end;

procedure TProgressWriter.StartTestSuite(ATestSuite: TTestSuite);
begin
// do nothing
  inc(fcount);
end;

procedure TProgressWriter.EndTestSuite(ATestSuite: TTestSuite);
begin
// do nothing
  dec(fcount);
  if fCount = 0 then
  begin
    writeln('!');
    writeln;
  end;
end;

{ TIdeTesterConsoleRunner }

constructor TIdeTesterConsoleRunner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  StopOnException := True;
  FFormat := DefaultFormat;
end;

destructor TIdeTesterConsoleRunner.Destroy;
begin
  inherited Destroy;
end;

procedure TIdeTesterConsoleRunner.DoRun;
var
  S : string;
begin
  Terminate;
  DoTestRun(GetTestRegistry);
end;

procedure TIdeTesterConsoleRunner.DoTestRun(ATest: TTest);
var
  ResultsWriter: TCustomResultsWriter;
  ProgressWriter: TProgressWriter;
  TestResult: TTestResult;

begin
  ProgressWriter:=Nil;
  ResultsWriter:=Nil;
  TestResult := TTestResult.Create;
  try
    ProgressWriter:=TProgressWriter.Create(Not ShowProgress);
    TestResult.AddListener(ProgressWriter);
    ResultsWriter:=GetResultsWriter;
    ResultsWriter.Filename := FileName;
    TestResult.AddListener(ResultsWriter);
    ATest.Run(TestResult);
    ResultsWriter.WriteResult(TestResult);
  finally
    if Assigned(ProgressWriter) then
      ExitCode:=ProgressWriter.GetExitCode;
    TestResult.Free;
    ResultsWriter.Free;
    ProgressWriter.Free;
  end;
end;


procedure TIdeTesterConsoleRunner.RunSuite;
var
  I,P : integer;
  S,TN : string;
  TS : TDecoratorTestSuite;
  T : TTest;
begin
  S := FSuite;
  if S = '' then
    for I := 0 to GetTestRegistry.ChildTestCount - 1 do
      writeln(GetTestRegistry[i].TestName)
  else
    begin
      TS:=TDecoratorTestSuite.Create('SuiteList');
      try
      while Not(S = '') Do
        begin
        P:=Pos(',',S);
        If P=0 then
          P:=Length(S)+1;
        TN:=Copy(S,1,P-1);
        Delete(S,1,P);
        if (TN<>'') then
          begin
          T:=GetTestRegistry.FindTest(TN);
          if Assigned(T) then
            TS.AddTest(T);
          end;
        end;
        if (TS.CountTestCases>1) then
          DoTestRun(TS)
        else if TS.CountTestCases=1 then
          DoTestRun(TS[0])
        else
          Writeln('No tests selected.');
      finally
        TS.Free;
      end;
    end;
end;

function TIdeTesterConsoleRunner.GetResultsWriter: TCustomResultsWriter;
begin
  case Format of
    fLatex:         Result := TLatexResultsWriter.Create(nil);
    fPlain:         Result := TPlainResultsWriter.Create(nil);
    fPlainNotiming: Result := TPlainResultsWriter.Create(nil);
    fSimple:        Result := TSimpleResultsWriter.Create(nil);
  else
    begin
      Result := TXmlResultsWriter.Create(nil);
      ExtendXmlDocument(TXMLResultsWriter(Result).Document);
    end;
  end;
  Result.SkipTiming:= FSkipTiming or (format=fPlainNoTiming);
  Result.Sparse:= FSparse;
  Result.SkipAddressInfo := FSkipAddressInfo;
end;


procedure TIdeTesterConsoleRunner.ExtendXmlDocument(Doc: TXMLDocument);
var
  n: TDOMElement;
begin
  n := Doc.CreateElement('Title');
  n.AppendChild(Doc.CreateTextNode(Title));
  Doc.FirstChild.AppendChild(n);
end;


{TSimpleResultsWriter}

constructor TSimpleResultsWriter.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FDoc := TStringList.Create;
  FSuites := TObjectList.create(true);
  FTempFailure := nil;
end;

destructor  TSimpleResultsWriter.Destroy;
begin
  FDoc.Free;
  FSuites.Free;
  inherited Destroy;
end;

procedure TSimpleResultsWriter.WriteHeader;
begin
end;

procedure TSimpleResultsWriter.WriteResult(aResult: TTestResult);
var
  f: text;
begin
  system.Assign(f, FileName);
  rewrite(f);
  writeln(f, FDoc.Text);
  close(f);
end;

procedure TSimpleResultsWriter.AddFailure(ATest: TTest; AFailure: TTestFailure);
begin
  inherited AddFailure(ATest, AFailure);
  FTempFailure := AFailure;
  inc((FSuites[0] as TSuiteInfo).fails);
end;

procedure TSimpleResultsWriter.AddError(ATest: TTest; AError: TTestFailure);
begin
  inherited AddError(ATest, AError);
  FTempFailure := AError;
  inc((FSuites[0] as TSuiteInfo).errors);
end;

procedure TSimpleResultsWriter.WriteTestHeader(ATest: TTest; ALevel: integer; ACount: integer);
begin
  inherited;
end;

function presentLoc(loc : String) : String;
var
  p : TArray<String>;
  u, l : String;
  i : integer;
begin
  l := '?';
  u := '?';
  p := loc.Split([' ']);

  for i := 0 to length(p) - 2 do
    if p[i] = 'line' then
      l := p[i+1];

  for i := 0 to length(p) - 1 do
    if p[i].endsWith('.pas') or p[i].endsWith('.inc') or p[i].endsWith('.dpr') or p[i].endsWith('.lpr') or p[i].endsWith('.pp') then
      u := ExtractFileName(p[i]);

  result := u+'#'+l;
end;

procedure TSimpleResultsWriter.WriteTestFooter(ATest: TTest; ALevel: integer; ATiming: TDateTime);
Var
  S : String;
begin
  inherited;
  inc((FSuites[0] as TSuiteInfo).total);

  S:='  ' + StringOfChar(' ',ALevel*2);
  S:= S + ATest.TestName+' ';
  if Not SkipTiming then
    S := S + '['+FormatDateTime(TimeFormat(ATiming), ATiming) + '] ';

  if not Assigned(FTempFailure) then
    s := s + 'pass'
  else if not FTempFailure.IsFailure then
    s := s + 'error ' + FTempFailure.ExceptionClassName + '@'+ presentLoc(FTempFailure.LocationInfo)+': '+FTempFailure.ExceptionMessage
  else if FTempFailure.IsIgnoredTest then
    s := s + 'ignore'
  else
    s := s + 'fail @'+ presentLoc(FTempFailure.LocationInfo)+': '+FTempFailure.ExceptionMessage;

  if Assigned(FTempFailure) or (not Sparse) then
    FDoc.Add(S);

  FTempFailure := nil;
end;

function TSimpleResultsWriter.TimeFormat(ATiming: TDateTime): String;

Var
  M : Int64;

begin
  Result:='ss.zzz';
  M:=MinutesBetween(ATiming,0);
  if M>60 then
    Result:='hh:mm:'+Result
  else if M>1 then
   Result:='mm:'+Result;
end;

procedure TSimpleResultsWriter.SetSkipAddressInfo(AValue: Boolean);
begin
  inherited SetSkipAddressInfo(AValue);
  if AValue then
    Include(FTestResultOptions,ttoSkipAddress)
  else
    Exclude(FTestResultOptions,ttoSkipAddress);
end;

procedure TSimpleResultsWriter.SetSparse(AValue: Boolean);
begin
  inherited SetSparse(AValue);
  if AValue then
    FTestResultOptions:=FTestResultOptions+[ttoSkipExceptionMessage,ttoErrorsOnly]
  else
    FTestResultOptions:=FTestResultOptions-[ttoSkipExceptionMessage,ttoErrorsOnly];
end;

procedure TSimpleResultsWriter.WriteSuiteFooter(ATestSuite: TTestSuite; ALevel: integer; ATiming: TDateTime; ANumRuns: integer; ANumErrors: integer; ANumFailures: integer; ANumIgnores: integer);
var
  info : TSuiteInfo;
  S: String;
begin
  inherited;
  info := FSuites[0] as TSuiteInfo;

  if Not SkipTiming then
    s := ' ['+ FormatDateTime(TimeFormat(ATiming), ATiming)+'] ';
  if info.errors + info.fails + info.ignored = 0 then
    s := s + ': '+inttostr(info.total)+' ok'
  else
  begin
    s := s + ': '+inttostr(info.total - (info.ignored + info.errors + info.fails))+' ok';
    if info.ignored > 0 then
      s := s + ', '+inttostr(info.ignored)+' ignored';
    if info.errors > 0 then
      s := s + ', '+inttostr(info.errors)+' errors';
    if info.fails > 0 then
      s := s + ', '+inttostr(info.fails)+' failed';
  end;
  if sparse and (info.errors + info.fails + info.ignored = 0) then
    FDoc.delete(info.index)
  else
    FDoc[info.index] := FDoc[info.index]+S;

  if FSuites.count > 1 then
    (FSuites[1] as TSuiteInfo).add(info);
  FSuites.delete(0);
end;

procedure TSimpleResultsWriter.WriteSuiteHeader(ATestSuite: TTestSuite; ALevel: integer);
begin
  inherited;
  FSuites.insert(0, TSuiteInfo.create(FDoc.Count));
  if (Alevel = 0) then
    FDoc.Add('All Tests')
  else
    FDoc.Add(StringOfChar(' ',ALevel*2) + ATestSuite.TestName);
end;

end.


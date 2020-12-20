unit idetester_ide;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process,
  UITypes, Forms,
  ProjectIntf, LazIDEIntf,
  idetester_base, idetester_external;

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

{ TTestEngineIDE }

function TTestEngineIDE.runProgram(params: TStringList; debug : boolean): TProcess;
begin
  // todo - compile and run, inside IDE
  raise Exception.create('Not done yet');
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


unit idetester_ide;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process,
  idetester_base, idetester_external;

type
  { TTestEngineIDE }

  TTestEngineIDE = class (TTestEngineExternal)
  protected
    function runProgram(params : TStringList; debug : boolean) : TProcess; override;
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
  // todo: read setting from current lps file
end;

procedure TTestSettingsProjectProvider.save(name, value: String);
begin
  // todo: write setting to current lps file
end;

{ TTestEngineIDE }

function TTestEngineIDE.runProgram(params: TStringList; debug : boolean): TProcess;
begin
  // todo - compile and run, inside IDE
end;

end.


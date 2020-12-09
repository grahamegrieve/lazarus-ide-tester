unit idetester_ini;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles,
  idetester_base;

type

  { TTestIniSettingsProvider }

  TTestIniSettingsProvider = class (TTestSettingsProvider)
  private
    FIni : TIniFile;
  public
    constructor Create(path : String);
    destructor Destroy; override;

    function read(name, defValue : String) : String; override;
    procedure save(name, value : String); override;
  end;

implementation

{ TTestIniSettingsProvider }

constructor TTestIniSettingsProvider.Create(path: String);
begin
  inherited Create;
  FIni := TIniFile.create(path);
end;

destructor TTestIniSettingsProvider.Destroy;
begin
  FIni.Free;
  inherited Destroy;
end;

function TTestIniSettingsProvider.read(name, defValue: String): String;
begin
  result := FIni.ReadString('Tests', name, defValue);
end;

procedure TTestIniSettingsProvider.save(name, value: String);
begin
  FIni.WriteString('Tests', name, value);
end;

end.


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

    function read(mode : TTestSettingsMode; name, defValue : String) : String; override;
    procedure save(mode : TTestSettingsMode; name, value : String); override;
  end;

implementation

{ TTestIniSettingsProvider }

constructor TTestIniSettingsProvider.Create(path: String);
var
  folder : String;
begin
  inherited Create;
  FIni := TIniFile.create(path);
  folder := ExtractFileDir(path);
  if not DirectoryExists(folder) then
    CreateDir(folder);
end;

destructor TTestIniSettingsProvider.Destroy;
begin
  FIni.Free;
  inherited Destroy;
end;

function TTestIniSettingsProvider.read(mode : TTestSettingsMode; name, defValue: String): String;
begin
  case mode of
    tsmConfig : result := FIni.ReadString('Config', name, defValue);
    tsmStatus : result := FIni.ReadString('Status', name, defValue)
  end;
end;

procedure TTestIniSettingsProvider.save(mode : TTestSettingsMode; name, value: String);
begin
  case mode of
    tsmConfig : FIni.WriteString('Config', name, value);
    tsmStatus : FIni.WriteString('Status', name, value)
  end;
end;

end.



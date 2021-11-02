unit idetester_ini;

{
Copyright (c) 2011+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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



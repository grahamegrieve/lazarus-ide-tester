{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit idetester_dsgn;

{$warn 5023 off : no warning about unused units}
interface

uses
  idetester_intf, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('idetester_intf', @idetester_intf.Register);
end;

initialization
  RegisterPackage('idetester_dsgn', @Register);
end.

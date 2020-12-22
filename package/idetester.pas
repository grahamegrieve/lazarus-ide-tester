{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit idetester;

{$warn 5023 off : no warning about unused units}
interface

uses
  idetester_runtime, idetester_base, idetester_direct, idetester_form, 
  idetester_ini, idetester_options, idetester_strings, idetester_tests, 
  idetester_external, idetester_ide, idetester_console, idetester_debug_form, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('idetester', @Register);
end.

{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit zeos_ex;

{$warn 5023 off : no warning about unused units}
interface

uses
  ZMacroQuery, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ZMacroQuery', @ZMacroQuery.Register);
end;

initialization
  RegisterPackage('zeos_ex', @Register);
end.

(*
  Brook Exception unit.

  Copyright (C) 2013 Silvio Clecio.

  http://brookframework.org

  All contributors:
  Plase see the file CONTRIBUTORS.txt, included in this
  distribution.

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit BrookException;

{$i brook.inc}

interface

uses
  BrookConsts, SysUtils;

type
  { Is a metaclass for @link(EBrook) class. }
  EBrookClass = class of EBrook;

  { Handles general exception in Brook. }
  EBrook = class(Exception)
  public
    { Creates an instance of @code(EBrook). }
    constructor Create(AInstance: TObject; const AMsg: string); overload;
    { Creates an instance of @code(EBrook). }
    constructor Create(AClass: TClass; const AMsg: string); overload;
    { Creates an instance of @code(EBrook). }
    constructor Create(AName: string; const AMsg: string); overload;
    { Creates an instance of @code(EBrook) with a formated message. }
    constructor CreateFmt(AInstance: TObject; const AMsg: string;
      const AArgs: array of const); overload;
    { Creates an instance of @code(EBrook) with a formated message. }
    constructor CreateFmt(AClass: TClass; const AMsg: string;
      const AArgs: array of const); overload;
    { Creates an instance of @code(EBrook) with a formated message. }
    constructor CreateFmt(AName: string; const AMsg: string;
      const AArgs: array of const); overload;
  end;

  { Handles the exception for 404 error. }
  EBrookHTTP404 = class(EBrook);

  { Handles the exception for 500 error. }
  EBrookHTTP500 = class(EBrook);

implementation

constructor EBrook.Create(AInstance: TObject; const AMsg: string);
begin
  inherited CreateFmt(BROOK_ERROR_MASK, [AInstance.ClassName, AMsg]);
end;

constructor EBrook.Create(AClass: TClass; const AMsg: string);
begin
  inherited CreateFmt(BROOK_ERROR_MASK, [AClass.ClassName, AMsg]);
end;

constructor EBrook.Create(AName: string; const AMsg: string);
begin
  inherited CreateFmt(BROOK_ERROR_MASK, [AName, AMsg]);
end;

constructor EBrook.CreateFmt(AInstance: TObject; const AMsg: string;
  const AArgs: array of const);
begin
  inherited CreateFmt(Format(BROOK_ERROR_MASK,
    [AInstance.ClassName, AMsg]), AArgs);
end;

constructor EBrook.CreateFmt(AClass: TClass; const AMsg: string;
  const AArgs: array of const);
begin
  inherited CreateFmt(Format(BROOK_ERROR_MASK,
    [AClass.ClassName, AMsg]), AArgs);
end;

constructor EBrook.CreateFmt(AName: string; const AMsg: string;
  const AArgs: array of const);
begin
  inherited CreateFmt(Format(BROOK_ERROR_MASK, [AName, AMsg]), AArgs);
end;

end.

(*
  Brook framework, Logger Interface

  Copyright (C) 2014 Silvio Clecio

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit BrookLogger;

{$i brook.inc}

interface

uses
  BrookClasses, BrookException, BrookMessages, BrookConsts, SysUtils;

type
  { Handles exceptions for logger. }
  EBrookLogger = class(EBrook);

  { Defines an enumerator to represent the logger output kind. }
  TBrookLogOutput = (loFile, loSystem);

type
  { Interface of the logger. }
  IBrookLogger = interface(IBrookInterface)[BROOK_LOGGER_GUID]
    { Set logger output kind. }
    procedure SetOutput(const AValue: TBrookLogOutput);
    { Get the logger output kind. }
    function GetOutput: TBrookLogOutput;
    { Returns the instance of broker logger. }
    function Instance: TObject;
    { Writes a custom log. }
    procedure Custom(const S: string; const ACode: Word);
    { Writes an information log. }
    procedure Info(const S: string);
    { Writes a warning log. }
    procedure Warn(const S: string);
    { Writes a debug log. }
    procedure Debug(const S: string);
    { Writes an error log. }
    procedure Error(const S: string; E: Exception = nil);
    { The logger output kind. }
    property Output: TBrookLogOutput read GetOutput write SetOutput;
  end;

{ Returns the logger instance. }
function BrookLog: IBrookLogger;
{ Register the logger. }
procedure BrookRegisterLog(ALog: IBrookLogger);
{ Unregister the logger. }
procedure BrookUnregisterLog;

implementation

var
  _BrookLogService: IBrookLogger = nil;

function BrookLog: IBrookLogger;
begin
  if not Assigned(_BrookLogService) then
    raise EBrookLogger.Create('BrookLog', SBrookNoLoggerRegisteredError);
  Result := _BrookLogService;
end;

procedure BrookRegisterLog(ALog: IBrookLogger);
begin
  if Assigned(_BrookLogService) then
    raise EBrookLogger.Create('BrookRegisterLog',
      SBrookLoggerAlreadyRegisteredError);
  _BrookLogService := ALog;
end;

procedure BrookUnregisterLog;
begin
  _BrookLogService := nil;
end;

end.


(*
  Brook framework, Logger Classes

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
  BrookClasses, BrookException, BrookMessages, SysUtils;

type
  { Handles exceptions for @link(TBrookLogger). }
  EBrookLogger = class(EBrook);

  { Is a metaclass for @link(TBrookLogger) class. }
  TBrookLoggerClass = class of TBrookLogger;

  { Defines an enumerator to represent the logger output kind. }
  TBrookLogOutput = (loFile, loSystem);

  { Provides features for the application logging. }
  TBrookLogger = class(TBrookComponent)
  private
    FActive: Boolean;
    FFileName: TFileName;
    FOutput: TBrookLogOutput;
  protected
    function GetActive: Boolean; virtual;
    procedure SetActive(const AValue: Boolean); virtual;
    function GetFileName: TFileName; virtual;
    procedure SetFileName(const AValue: TFileName); virtual;
    procedure SetOutput(const AValue: TBrookLogOutput); virtual;
    function GetOutput: TBrookLogOutput; virtual;
  public
    { Return the service class provided by this class. }
    class function GetServiceClass: TBrookLoggerClass;
    { Registers the service provided by this class. }
    class procedure RegisterService;
    { Unregisters the service  provided by this class. }
    class procedure UnregisterService;
    { Return an instance of this class. }
    class function Service: TBrookLogger;
    { Writes a custom log. }
    procedure Custom(const S: string; const ACode: Word); virtual; abstract;
    { Writes an information log. }
    procedure Info(const S: string); virtual; abstract;
    { Writes a warning log. }
    procedure Warn(const S: string); virtual; abstract;
    { Writes a debug log. }
    procedure Debug(const S: string); virtual; abstract;
    { Writes an error log. }
    procedure Error(const S: string; E: Exception = nil); virtual; abstract;
    { Enables or disables the logger. }
    property Active: Boolean read GetActive write SetActive;
    { Defines the name of the log file. }
    property FileName: TFileName read GetFileName write SetFileName;
    { The logger output kind. }
    property Output: TBrookLogOutput read GetOutput write SetOutput;
  end;

implementation

var
  _BrookLoggerService: TBrookLogger = nil;
  _BrookLoggerServiceClass: TBrookLoggerClass = nil;

{ TBrookLogger }

class function TBrookLogger.GetServiceClass: TBrookLoggerClass;
begin
  Result := _BrookLoggerServiceClass;
end;

class procedure TBrookLogger.RegisterService;
begin
  if Assigned(_BrookLoggerServiceClass) then
    raise EBrookLogger.Create(Self, SBrookLoggerServiceAlreadyRegisteredError);
  _BrookLoggerServiceClass := Self;
end;

class procedure TBrookLogger.UnregisterService;
begin
  FreeAndNil(_BrookLoggerService);
  _BrookLoggerServiceClass := nil;
end;

class function TBrookLogger.Service: TBrookLogger;
begin
  if not Assigned(_BrookLoggerService) then
  begin
    if not Assigned(_BrookLoggerServiceClass) then
      raise EBrookLogger.Create(Self, SBrookNoLoggerServiceRegisteredError);
    _BrookLoggerService := _BrookLoggerServiceClass.Create(nil);
  end;
  Result := _BrookLoggerService;
end;

function TBrookLogger.GetActive: Boolean;
begin
  Result := FActive;
end;

procedure TBrookLogger.SetActive(const AValue: Boolean);
begin
  if AValue <> FActive then
    FActive := AValue;
end;

function TBrookLogger.GetFileName: TFileName;
begin
  Result := FFileName;
end;

procedure TBrookLogger.SetFileName(const AValue: TFileName);
begin
  if AValue <> FFileName then
    FFileName := AValue;
end;

procedure TBrookLogger.SetOutput(const AValue: TBrookLogOutput);
begin
  if AValue <> FOutput then
    FOutput := AValue;
end;

function TBrookLogger.GetOutput: TBrookLogOutput;
begin
  Result := FOutput;
end;

end.


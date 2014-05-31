(*
  Brook framework, FCL EventLog Broker

  Copyright (C) 2014 Silvio Clecio

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit BrookFCLEventLogBroker;

{$mode objfpc}{$H+}

interface

uses
  BrookLogger, BrookApplication, BrookClasses, BrookUtils, CustWeb, EventLog,
  SysUtils;

type

  { TBrookAppLogger }

  TBrookAppLogger = class(TBrookInterfacedObject, IBrookLogger)
  private
    FLogger: TEventLog;
    FOutput: TBrookLogOutput;
    procedure SetOutput(const AValue: TBrookLogOutput);
    function GetOutput: TBrookLogOutput;
  protected
    procedure InternalLog(const L: TEventType; const S: string;
      const ACode: Word; const E: Exception);
    function Logger: TEventLog;
  public
    constructor Create; virtual;
    function Instance: TObject;
    procedure Custom(const S: string; const ACode: Word);
    procedure Info(const S: string);
    procedure Warn(const S: string);
    procedure Debug(const S: string);
    procedure Error(const S: string; E: Exception = nil);
    property Output: TBrookLogOutput read GetOutput write SetOutput;
  end;

implementation

{ TBrookAppLogger }

constructor TBrookAppLogger.Create;
begin
  inherited Create;
  SetOutput(loFile);
end;

function TBrookAppLogger.Logger: TEventLog;
begin
  if not Assigned(FLogger) then
  begin
    if Assigned(BrookApp) and Assigned(BrookApp.Instance) and
      (BrookApp.Instance is TCustomWebApplication) then
      FLogger := (BrookApp.Instance as TCustomWebApplication).EventLog;
    if not Assigned(FLogger) then
      Exit(nil);
    FLogger.Active := False;
    case Self.Output of
      loFile: FLogger.LogType := EventLog.ltFile;
      loSystem: FLogger.LogType := EventLog.ltSystem;
    end;
    FLogger.FileName := BrookSettings.LogFile;
    FLogger.RaiseExceptionOnError := False;
    FLogger.AppendContent := True;
    FLogger.Active := True;
  end;
  Result := FLogger;
end;

procedure TBrookAppLogger.SetOutput(const AValue: TBrookLogOutput);
begin
  if AValue <> FOutput then
    FOutput := AValue;
end;

function TBrookAppLogger.GetOutput: TBrookLogOutput;
begin
  Result := FOutput;
end;

procedure TBrookAppLogger.InternalLog(const L: TEventType; const S: string;
  const ACode: Word; const E: Exception);
var
  X: string;
begin
  if not BrookSettings.LogActive then
    Exit;
  X := S;
  if Assigned(E) then
    X += '<Error>' + LineEnding +
      Format('%s exception was raised with the following message: %s',
      [E.ClassName, E.Message]) + LineEnding +
      BrookDumpStack(LineEnding) + LineEnding +
      BrookDumpStackTrace(LineEnding) + '</Error>';
  case L of
    etCustom:
      begin
        Logger.CustomLogType := ACode;
        Logger.Log(etCustom, X);
      end;
    etInfo: Logger.Log(etInfo, X);
    etWarning: Logger.Log(etWarning, X);
    etError: Logger.Log(etError, X);
    etDebug: Logger.Log(etDebug, X);
  end;
end;

function TBrookAppLogger.Instance: TObject;
begin
  Result := FLogger;
end;

procedure TBrookAppLogger.Custom(const S: string; const ACode: Word);
begin
  InternalLog(etCustom, S, ACode, nil);
end;

procedure TBrookAppLogger.Info(const S: string);
begin
  InternalLog(etInfo, S, 0, nil);
end;

procedure TBrookAppLogger.Warn(const S: string);
begin
  InternalLog(etWarning, S, 0, nil);
end;

procedure TBrookAppLogger.Debug(const S: string);
begin
  InternalLog(etDebug, S, 0, nil);
end;

procedure TBrookAppLogger.Error(const S: string; E: Exception);
begin
  InternalLog(etError, S, 0, E);
end;

initialization
  BrookRegisterLog(TBrookAppLogger.Create);

end.


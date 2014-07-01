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
  BrookLogger, BrookUtils, EventLog, SysUtils, Classes;

type

  { TBrookFCLEventLog }

  TBrookFCLEventLog = class(TBrookLogger)
  private
    FConfigured: Boolean;
    FLogger: TEventLog;
  protected
    procedure SetActive(const AValue: Boolean); override;
    procedure SetFileName(const AValue: TFileName); override;
    procedure SetOutput(const AValue: TBrookLogOutput); override;
    procedure InternalLog(const L: TEventType; const S: string;
      const ACode: Word; const E: Exception); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Configure; virtual;
    procedure Unconfigure; virtual;
    procedure Custom(const S: string; const ACode: Word); override;
    procedure Info(const S: string); override;
    procedure Warn(const S: string); override;
    procedure Debug(const S: string); override;
    procedure Error(const S: string; E: Exception = nil); override;
  end;

implementation

{ TBrookFCLEventLog }

constructor TBrookFCLEventLog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLogger := TEventLog.Create(nil);
  FLogger.Identification := ApplicationName;
end;

destructor TBrookFCLEventLog.Destroy;
begin
  FreeAndNil(FLogger);
  inherited Destroy;
end;

procedure TBrookFCLEventLog.SetActive(const AValue: Boolean);
begin
  inherited SetActive(AValue);
  Unconfigure;
end;

procedure TBrookFCLEventLog.SetFileName(const AValue: TFileName);
begin
  inherited SetFileName(AValue);
  Unconfigure;
end;

procedure TBrookFCLEventLog.SetOutput(const AValue: TBrookLogOutput);
begin
  inherited SetOutput(AValue);
  Unconfigure;
end;

procedure TBrookFCLEventLog.InternalLog(const L: TEventType; const S: string;
  const ACode: Word; const E: Exception);
var
  X: string;
begin
  if not FConfigured then
    Configure;
  if FLogger.Active then
  begin
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
          FLogger.CustomLogType := ACode;
          FLogger.Log(etCustom, X);
        end;
      etInfo: FLogger.Log(etInfo, X);
      etWarning: FLogger.Log(etWarning, X);
      etError: FLogger.Log(etError, X);
      etDebug: FLogger.Log(etDebug, X);
    end;
  end;
end;

procedure TBrookFCLEventLog.Configure;
begin
  FConfigured := True;
  FLogger.Active := False;
  case Output of
    loFile: FLogger.LogType := EventLog.ltFile;
    loSystem: FLogger.LogType := EventLog.ltSystem;
  end;
  if FileName = '' then
    FLogger.FileName := BrookSettings.LogFile
  else
    FLogger.FileName := FileName;
  FLogger.RaiseExceptionOnError := False;
  FLogger.AppendContent := True;
  if Active then
    FLogger.Active := Active
  else
    FLogger.Active := BrookSettings.LogActive;
end;

procedure TBrookFCLEventLog.Unconfigure;
begin
  FConfigured := False;
end;

procedure TBrookFCLEventLog.Custom(const S: string; const ACode: Word);
begin
  InternalLog(etCustom, S, ACode, nil);
end;

procedure TBrookFCLEventLog.Info(const S: string);
begin
  InternalLog(etInfo, S, 0, nil);
end;

procedure TBrookFCLEventLog.Warn(const S: string);
begin
  InternalLog(etWarning, S, 0, nil);
end;

procedure TBrookFCLEventLog.Debug(const S: string);
begin
  InternalLog(etDebug, S, 0, nil);
end;

procedure TBrookFCLEventLog.Error(const S: string; E: Exception);
begin
  InternalLog(etError, S, 0, E);
end;

initialization
  TBrookFCLEventLog.RegisterService;

finalization
  TBrookFCLEventLog.UnregisterService;

end.


(*
  Brook FCL HTTP Daemon Broker unit.

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

unit BrookFCLHTTPDaemonBroker;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF MSWINDOWS}
  ServiceManager,
{$ENDIF}
  BrookFCLHTTPAppBroker, BrookApplication, BrookConsts, DaemonApp, Classes,
  SysUtils;

type

  { TBrookDaemonApplication }

  TBrookDaemonApplication = class(TInterfacedObject, IBrookApplication)
  private
    function GetTerminated: Boolean;
  public
    constructor Create;
    function Instance: TObject;
    procedure Run;
    procedure Terminate;
    property Terminated: Boolean read GetTerminated;
  end;

  { TBrookDaemonThread }

  TBrookDaemonThread = class(TThread)
  public
    constructor Create; virtual;
    procedure Execute; override;
  end;

  { TBrookHTTPDaemon }

  TBrookHTTPDaemon = class(TCustomDaemon)
  private
    FThread: TThread;
  public
    function Install: Boolean; override;
    function Uninstall: Boolean; override;
    function Start: Boolean; override;
    function Stop: Boolean; override;
    procedure Log(const AMsg: string);
  end;

  { TBrookHTTPDaemonMapper }

  TBrookHTTPDaemonMapper = class(TCustomDaemonMapper)
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

var
  _BrookHTTPApp: IBrookApplication = nil;

function BrookHTTPApp: IBrookApplication;
begin
  if not Assigned(_BrookHTTPApp) then
    _BrookHTTPApp := TBrookApplication.Create;
  Result := _BrookHTTPApp;
end;

{ TBrookDaemonApplication }

function TBrookDaemonApplication.GetTerminated: Boolean;
begin
  Result := Application.Terminated;
end;

constructor TBrookDaemonApplication.Create;
begin
  Application.Title := 'Brook HTTP daemon application';
end;

function TBrookDaemonApplication.Instance: TObject;
begin
  Result := BrookHTTPApp.Instance;
end;

procedure TBrookDaemonApplication.Run;
begin
  Application.Run;
end;

procedure TBrookDaemonApplication.Terminate;
begin
  Application.Terminate;
end;

{ TBrookDaemonThread }

constructor TBrookDaemonThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
end;

procedure TBrookDaemonThread.Execute;
begin
  BrookHTTPApp.Run;
end;

{ TBrookHTTPDaemon }

function TBrookHTTPDaemon.Start: Boolean;
begin
  Result := inherited Start;
  FThread := TBrookDaemonThread.Create;
  FThread.Start;
  Log('Start.');
end;

function TBrookHTTPDaemon.Stop: Boolean;
begin
  Result := inherited Stop;
  FThread.Terminate;
  Log('Stop.');
end;

function TBrookHTTPDaemon.Install: Boolean;
{$IFDEF MSWINDOWS}
var
  VSM: TServiceManager;
{$ENDIF}
begin
  Result := inherited Install;
{$IFDEF MSWINDOWS}
  VSM := TServiceManager.Create(nil);
  try
    VSM.Connect;
    if VSM.Connected then
      VSM.StartService(BROOK_HTTP_DAEMON_NAME, nil);
    VSM.Disconnect;
  finally
    VSM.Free;
  end;
{$ENDIF}
  Log('Install.');
  WriteLn('Service installed.');
end;

function TBrookHTTPDaemon.Uninstall: Boolean;
{$IFDEF MSWINDOWS}
var
  VSM: TServiceManager;
{$ENDIF}
begin
  Result := inherited Uninstall;
{$IFDEF MSWINDOWS}
  VSM := TServiceManager.Create(nil);
  try
    VSM.Connect;
    if VSM.Connected then
      VSM.StopService(BROOK_HTTP_DAEMON_NAME, True);
    VSM.Disconnect;
  finally
    VSM.Free;
  end;
{$ENDIF}
  Log('Uninstall.');
  WriteLn('Service uninstalled.');
end;

procedure TBrookHTTPDaemon.Log(const AMsg: string);
begin
  Application.Log(etCustom, ClassName + ': ' + AMsg);
end;

{ TBrookHTTPDaemonMapper }

constructor TBrookHTTPDaemonMapper.Create(AOwner: TComponent);
var
  VDaemonDef: TDaemonDef;
begin
  inherited Create(AOwner);
  VDaemonDef := DaemonDefs.Add as TDaemonDef;
  VDaemonDef.Description := BROOK_HTTP_DAEMON_DESCRIPTION;
  VDaemonDef.DisplayName := BROOK_HTTP_DAEMON_DISPLAYNAME;
  VDaemonDef.Name := BROOK_HTTP_DAEMON_NAME;
  VDaemonDef.DaemonClassName := BROOK_HTTP_DAEMON_CLASSNAME;
  VDaemonDef.WinBindings.ServiceType := stWin32;
end;

initialization
  RegisterDaemonClass(TBrookHTTPDaemon);
  RegisterDaemonMapper(TBrookHTTPDaemonMapper);
  BrookUnregisterApp;
  BrookRegisterApp(TBrookDaemonApplication.Create);

end.

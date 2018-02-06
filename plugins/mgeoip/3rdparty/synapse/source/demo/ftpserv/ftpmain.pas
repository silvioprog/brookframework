unit ftpmain;

{$IFDEF FPC}
  {$mode delphi}
{$endif}

interface

uses
{$IFDEF LINUX}
  Libc,
{$ELSE}
  Windows,
{$ENDIF}
  Classes, SysUtils, ftpthrd, blcksock, synsock;

type
  TServiceThread = class(TThread)
  private
    { Private declarations }
  protected
    procedure Execute; override;
  public
    constructor Create;
  end;


implementation

{==============================================================================}
{ TServiceThread }

constructor TServiceThread.create;
begin
  inherited create(false);
  FreeOnTerminate := false;
//  Priority := tpNormal;
end;

procedure TServiceThread.Execute;
var
  ClientSock: TSocket;
  sock: TTCPBlockSocket;
begin
  sock := TTCPBlockSocket.Create;
  try
    sock.bind('0.0.0.0','21');
    sock.setLinger(true, 10000);
    sock.listen;
    if sock.LastError <> 0 then
      exit;
    while not terminated do
    begin
      if sock.canread(1000) then
      begin
        ClientSock := sock.accept;
        if sock. lastError = 0 then
          TFtpServerThread.create(ClientSock);
      end;
    end;
  finally
    sock.Free;
  end;
end;

{==============================================================================}
end.

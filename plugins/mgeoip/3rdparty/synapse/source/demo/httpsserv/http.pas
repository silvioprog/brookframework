unit http;

{ Marked some code with !!!}

interface

uses
  Classes, blcksock, winsock, Synautil, ssl_openssl, SysUtils, Dialogs;

type
  TTCPHttpDaemon = class(TThread)
  private
    Sock:TTCPBlockSocket;
  public
    Constructor Create;
    Destructor Destroy; override;
    procedure Execute; override;
  end;

  TTCPHttpThrd = class(TThread)
  private
    Sock:TTCPBlockSocket;
  public
    Headers: TStringList;
    InputData, OutputData: TMemoryStream;
    Constructor Create (hsock:tSocket);
    Destructor Destroy; override;
    procedure Execute; override;
    function ProcessHttpRequest(Request, URI: string): integer;
  end;

implementation

{ TTCPHttpDaemon }

Constructor TTCPHttpDaemon.Create;
begin
  sock:=TTCPBlockSocket.create;
  FreeOnTerminate:=true;
  inherited create(false);
end;

Destructor TTCPHttpDaemon.Destroy;
begin
  Sock.free;
  inherited Destroy;
end;

procedure TTCPHttpDaemon.Execute;
var
  ClientSock:TSocket;
begin
  with sock do
    begin
      CreateSocket;
      setLinger(true,10000);
      bind('0.0.0.0','443');
      listen;
      repeat
        if terminated then break;
        if canread(1000) then
          begin
            ClientSock:=accept;
            if lastError=0 then TTCPHttpThrd.create(ClientSock);
          end;
      until false;
    end;
end;

{ TTCPHttpThrd }

Constructor TTCPHttpThrd.Create(Hsock:TSocket);
begin
  sock:=TTCPBlockSocket.create;
  Headers := TStringList.Create;
  InputData := TMemoryStream.Create;
  OutputData := TMemoryStream.Create;
  Sock.socket:=HSock;
  FreeOnTerminate:=true;
  inherited create(false);
end;

Destructor TTCPHttpThrd.Destroy;
begin
  Sock.free;
  Headers.Free;
  InputData.Free;
  OutputData.Free;
  inherited Destroy;
end;

procedure TTCPHttpThrd.Execute;
var
  timeout: integer;
  s: string;
  method, uri, protocol: string;
  size: integer;
  x, n: integer;
  resultcode: integer;
begin
  timeout := 120000;

  // Note: There's no need for installing a client certificate in the
  //       webbrowser. The server asks the webbrowser to send a certificate but
  //       if nothing is installed the software will work because the server
  //       doesn't check to see if a client certificate was supplied. If you
  //       want you can install:
  //
  //       file: c_cacert.p12
  //       password: c_cakey
  //
  Sock.SSL.CertCAFile := ExtractFilePath(ParamStr(0)) + 's_cabundle.pem';
  Sock.SSL.CertificateFile := ExtractFilePath(ParamStr(0)) + 's_cacert.pem';
  Sock.SSL.PrivateKeyFile := ExtractFilePath(ParamStr(0)) + 's_cakey.pem';
  Sock.SSL.KeyPassword := 's_cakey';
  Sock.SSL.verifyCert := True;

  try
    if (not Sock.SSLAcceptConnection) or
       (Sock.SSL.LastError <> 0) then
    begin
      MessageDlg('Error while accepting SSL connection: ' + Sock.SSL.LastErrorDesc, mtError, [mbAbort], 0);
      Exit;
    end;
  except
    MessageDlg('Exception while accepting SSL connection', mtError, [mbAbort], 0);
    Exit;
  end;

  
  //read request line
  s := sock.RecvString(timeout);
  if sock.lasterror <> 0 then
    Exit;
  if s = '' then
    Exit;
  method := fetch(s, ' ');
  if (s = '') or (method = '') then
    Exit;
  uri := fetch(s, ' ');
  if uri = '' then
    Exit;
  protocol := fetch(s, ' ');
  headers.Clear;
  size := -1;
  //read request headers
  if protocol <> '' then
  begin
    if pos('HTTP/', protocol) <> 1 then
      Exit;
    repeat
      s := sock.RecvString(Timeout);
      if sock.lasterror <> 0 then
        Exit;
      if s <> '' then
        Headers.add(s);
      if Pos('CONTENT-LENGTH:', Uppercase(s)) = 1 then
        Size := StrToIntDef(SeparateRight(s, ' '), -1);
    until s = '';
  end;
  //recv document...
  InputData.Clear;
  if size >= 0 then
  begin
    InputData.SetSize(Size);
    x := Sock.RecvBufferEx(InputData.Memory, Size, Timeout);
    InputData.SetSize(x);
    if sock.lasterror <> 0 then
      Exit;
  end;
  OutputData.Clear;
  ResultCode := ProcessHttpRequest(method, uri);
  sock.SendString('HTTP/1.0 ' + IntTostr(ResultCode) + CRLF);
  if protocol <> '' then
  begin
    headers.Add('Content-length: ' + IntTostr(OutputData.Size));
    headers.Add('Connection: close');
    headers.Add('Date: ' + Rfc822DateTime(now));
    headers.Add('Server: Synapse HTTP server demo');
    headers.Add('');
    for n := 0 to headers.count - 1 do
      sock.sendstring(headers[n] + CRLF);
  end;
  if sock.lasterror <> 0 then
    Exit;
  Sock.SendBuffer(OutputData.Memory, OutputData.Size);
end;

function TTCPHttpThrd.ProcessHttpRequest(Request, URI: string): integer;
var
  l: TStringlist;
begin
//sample of precessing HTTP request:
// InputData is uploaded document, headers is stringlist with request headers.
// Request is type of request and URI is URI of request
// OutputData is document with reply, headers is stringlist with reply headers.
// Result is result code
  result := 504;
  if request = 'GET' then
  begin
    headers.Clear;
    headers.Add('Content-type: Text/Html');
    l := TStringList.Create;
    try
      l.Add('<html>');
      l.Add('<head></head>');
      l.Add('<body>');
      l.Add('Request Uri: ' + uri);
      l.Add('<br>');
      l.Add('This document is generated by Synapse HTTPS server demo!');
      if Sock.SSL.GetPeerName = '' then
        l.Add('No client certificate received')
      else
        l.Add('Client certificate received from ' + Sock.SSL.GetPeerName);
      l.Add('</body>');
      l.Add('</html>');
      l.SaveToStream(OutputData);
    finally
      l.free;
    end;
    Result := 200;
  end;
end;

end.

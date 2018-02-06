unit ProxyThrd;

interface

uses
  Classes, SyncObjs, SysUtils, windows,
  blcksock, synsock, synautil;

type
  TServiceThread = class(TThread)
  protected
    procedure Execute; override;
  public
    constructor Create;
  end;

  TLogRec = record
    ip: string;
    dt: TDateTime;
    req: string;
    stat: string;
    len: integer;
    ref: string;
    agent: string;
  end;

  TTCPHttpThrd = class(TThread)
  private
    csock: TSocket;
  public
    timeout: integer;
    Headers: TStringList;
    ProxyHeaders: TStringList;
    IdStr: string;
    LogRec: TLogRec;
    Constructor Create (hsock:tSocket);
    Destructor Destroy; override;
    procedure Execute; override;
    function RelayTCP(const fsock, dsock: TTCPBlockSocket): boolean;
    function RelaySock(const fsock, dsock: TTCPBlockSocket; Size: integer): boolean;
    procedure ReturnHTML(const sock: TTCPBlockSocket; const value, stat: string);
    procedure Return502(const sock: TTCPBlockSocket; host, port: string);
    procedure WriteAccessLog(const LogRec: TLogRec);
  end;


procedure InitService;
procedure DestroyService;
procedure Writelog(value: string);

var
  CS: TCriticalSection;

implementation

{==============================================================================}

procedure InitService;
begin
  CS := TCriticalSection.create;
end;

procedure DestroyService;
begin
  cs.free;
end;

procedure Writelog(value: string);
var
  f: textFile;
  s: string;
begin
  CS.Enter;
  s := Value;
  s := extractfilepath(ParamStr(0)) + 'access.log';
  assignfile(f, s);
  if fileexists(s)
    then append(f)
  else rewrite(f);
  try
    writeln(f, Value);
  finally
    Closefile(f);
    CS.Leave;
  end;
end;

{==============================================================================}
{ TServiceThread }

constructor TServiceThread.create;
begin
  FreeOnTerminate := false;
  inherited create(false);
end;

procedure TServiceThread.Execute;
var
  sock: TTCPBlockSocket;
  ClientSock: TSocket;
begin
  sock := TTCPBlockSocket.Create;
  try
    sock.bind('0.0.0.0','3128');
    if sock.LastError <> 0 then
    begin
      WriteLog('!!! BIND failed !!!');
      Exit;
    end;
    sock.setLinger(true,10000);
    sock.listen;
    repeat
      if terminated then
        break;
      if sock.canread(1000) then
      begin
        //new connection... launch TTCPHttpThrd
        ClientSock := sock.accept;
        if sock.lastError = 0 then
          TTCPHttpThrd.create(ClientSock);
      end;
    until false;
  finally
    sock.free;
  end;
end;

{==============================================================================}

{ TTCPHttpThrd }

Constructor TTCPHttpThrd.Create(Hsock:TSocket);
begin
  csock := hsock;
  Headers := TStringList.Create;
  ProxyHeaders := TStringList.Create;
  FreeOnTerminate:=true;
  inherited create(false);
end;

Destructor TTCPHttpThrd.Destroy;
begin
  Headers.Free;
  Proxyheaders.Free;
  inherited Destroy;
end;

//do both direction TCP proxy tunnel. (used by CONNECT method for https proxying)
function TTCPHttpThrd.RelayTCP(const fsock, dsock: TTCPBlockSocket): boolean;
var
  n: integer;
  buf: string;
  ql, rl: TList;
  fgsock, dgsock: TTCPBlockSocket;
  FDSet: TFDSet;
  FDSetSave: TFDSet;
  TimeVal: PTimeVal;
  TimeV: TTimeVal;
begin
  result := false;
  //buffer maybe contains some pre-readed datas...
  if fsock.LineBuffer <> '' then
  begin
    buf := fsock.RecvPacket(timeout);
    if fsock.LastError <> 0 then
      Exit;
    dsock.SendString(buf);
  end;
  //begin relaying of TCP
  ql := TList.Create;
  rl := Tlist.create;
  try
    TimeV.tv_usec := (Timeout mod 1000) * 1000;
    TimeV.tv_sec := Timeout div 1000;
    TimeVal := @TimeV;
    if Timeout = -1 then
      TimeVal := nil;
    FD_ZERO(FDSetSave);
    FD_SET(fsock.Socket, FDSetSave);
    FD_SET(dsock.Socket, FDSetSave);
    FDSet := FDSetSave;
    while synsock.Select(65535, @FDSet, nil, nil, TimeVal) > 0 do
    begin
      rl.clear;
      if FD_ISSET(fsock.Socket, FDSet) then
        rl.Add(fsock);
      if FD_ISSET(dsock.Socket, FDSet) then
        rl.Add(dsock);
      for n := 0 to rl.Count - 1 do
      begin
        fgsock := TTCPBlockSocket(rl[n]);
        if fgsock = fsock then
          dgsock := dsock
        else
          dgsock := fsock;
        if fgsock.WaitingData > 0 then
        begin
          buf := fgsock.RecvPacket(0);
          dgsock.SendString(buf);
          if dgsock.LastError <> 0 then
            exit;
        end
        else
          exit;
      end;
      FDSet := FDSetSave;
    end;
  finally
    rl.free;
    ql.free;
  end;
  result := true;
end;

//transmit X bytes from fsock to dsock
function TTCPHttpThrd.RelaySock(const fsock, dsock: TTCPBlockSocket; Size: integer): boolean;
var
  sh, sl: integer;
  n: integer;
  buf: string;
begin
  result := false;
  sh := size div c64k;
  sl := size mod c64k;
  for n := 1 to sh do
  begin
    buf := fsock.RecvBufferStr(c64k, timeout);
    if fsock.LastError <> 0 then
      Exit;
    dsock.SendString(buf);
    if dsock.LastError <> 0 then
      Exit;
  end;
  if sl > 0 then
  begin
    buf := fsock.RecvBufferStr(sl, timeout);
    if fsock.LastError <> 0 then
      Exit;
    dsock.SendString(buf);
    if dsock.LastError <> 0 then
      Exit;
  end;
  result := true;
end;

//core of proxy
procedure TTCPHttpThrd.Execute;
var
  Sock: TTCPBlockSocket;
  QSock: TTCPBlockSocket;
  s: string;
  method, uri, protocol: string;
  size: integer;
  Prot, User, Pass, Host, Port, Path, Para: string;
  chunked: boolean;
  status: integer;
  proxykeep: boolean;
  lasthost: String;
  rprotocol: String;
begin
  idstr := inttostr(self.handle) + ' ';
  sock:=TTCPBlockSocket.create;
  Qsock:=TTCPBlockSocket.create;
  try
    Sock.socket:=CSock;
    timeout := 120000;
    lasthost := '';
    qsock.ConvertLineEnd := true;
    sock.ConvertLineEnd := true;

    repeat
      //read request line
      headers.Clear;
      proxyheaders.Clear;
      proxykeep := false;
      LogRec.ip := sock.GetRemoteSinIP;
      repeat
        s := sock.RecvString(timeout);
        if sock.lasterror <> 0 then
          Exit;
        LogRec.dt := now;
        LogRec.req := s;
        Logrec.stat := '';
        LogRec.len := 0;
        Logrec.Ref := '';
        Logrec.Agent := '';
      until s <> '';
      if s = '' then
        Exit;
      method := fetch(s, ' ');
      if (s = '') or (method = '') then
        Exit;
      uri := fetch(s, ' ');
      if uri = '' then
        Exit;
      protocol := fetch(s, ' ');
      size := 0;
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
          begin
            if pos('PROXY-', uppercase(s)) = 1 then
              proxyHeaders.add(s)
            else
              Headers.add(s);
          end;
          if Pos('CONTENT-LENGTH:', Uppercase(s)) = 1 then
            Size := StrToIntDef(SeparateRight(s, ' '), 0);
          if Pos('PROXY-CONNECTION:', Uppercase(s)) = 1 then
            if Pos('KEEP', Uppercase(s)) > 0 then
            begin
              proxykeep := true;
            end;
          if Pos('REFERER:', Uppercase(s)) = 1 then
            LogRec.ref := Trim(SeparateRight(s, ' '));
          if Pos('USER-AGENT:', Uppercase(s)) = 1 then
            LogRec.agent := Trim(SeparateRight(s, ' '));
        until s = '';
      end;

      if proxykeep then
        headers.add('Connection: keep-alive')
      else
        headers.add('Connection: close');

      s := ParseURL(uri, Prot, User, Pass, Host, Port, Path, Para);
      Headers.Insert(0, method + ' ' + s + ' ' + protocol);

      if lasthost <> host then
        qsock.closesocket;
      if qsock.Socket = INVALID_SOCKET then
      begin
        qsock.Connect(host, port);
        if qsock.LastError <> 0 then
        begin
          return502(sock, host, port);
          exit;
        end;
        lasthost := host;
      end;

      if method = 'CONNECT' then
      begin
        sock.SendString(protocol + ' 200 Connection established' + CRLF + CRLF);
        LogRec.stat := '200';
        WriteAccesslog(Logrec);
        RelayTCP(sock, qsock);
        Exit;
      end;
      qsock.SendString(headers.text + CRLF);

      //upload data from client to server if needed.
      if size > 0 then
      begin
        if not RelaySock(sock, qsock, size) then
          exit;
      end;

      //read response line
      repeat
        headers.Clear;
        s := qsock.RecvString(timeout);
        if qsock.lasterror <> 0 then
          Exit;
        if s = '' then
          Exit;
        headers.Add(s);
        rprotocol := fetch(s, ' ');
        status := StrToIntDef(separateleft(s, ' '), 0);
        if status = 100 then
        begin
          sock.SendString(rprotocol + ' ' + s + CRLF);
          repeat
            s := qSock.RecvString(Timeout);
            if qSock.LastError = 0 then
              sock.SendString(s + CRLF);
          until (s = '') or (qSock.LastError <> 0);
        end;
      until status <> 100;


      //read response headers
      if pos('HTTP/', rprotocol) <> 1 then
        Exit;
      LogRec.stat := IntToStr(status);
      size := -1;
      chunked := false;
      //read response headers
      repeat
        s := qsock.RecvString(Timeout);
        if qsock.lasterror <> 0 then
          Exit;
        if s <> '' then
          Headers.add(s);
        if Pos('CONTENT-LENGTH:', Uppercase(s)) = 1 then
          Size := StrToIntDef(SeparateRight(s, ' '), 0);
        if Pos('TRANSFER-ENCODING:', uppercase(s)) = 1 then
          chunked:=Pos('CHUNKED', uppercase(s)) > 0;
        if Pos('CONNECTION:', uppercase(s)) = 1 then
          if Pos('CLOSE', uppercase(s)) > 0 then
            proxyKeep := False;
      until s = '';

      if (not(chunked)) and (size = -1) then
        proxyKeep := false;

      if proxykeep and (protocol <> 'HTTP/1.1') then
        proxykeep := false;

      sock.SendString(headers.text +  CRLF);

      if method = 'HEAD' then
      begin
        LogRec.len := 0;
      end
      else
      begin
        if size > 0 then
        begin
          //identity kodovani
          if not RelaySock(qsock, sock, size) then
            exit;
          LogRec.len := Size;
        end
        else
        begin
          if chunked then
          begin
            repeat
              repeat
                s := qSock.RecvString(Timeout);
                if qSock.LastError = 0 then
                  sock.SendString(s + CRLF);
              until (s <> '') or (qSock.LastError <> 0);
              if qSock.LastError <> 0 then
                Break;
              s := Trim(SeparateLeft(s, ' '));
              s := Trim(SeparateLeft(s, ';'));
              Size := StrToIntDef('$' + s, 0);
              LogRec.len := LogRec.len + Size;
              if Size = 0 then
              begin
                repeat
                  s := qSock.RecvString(Timeout);
                  if qSock.LastError = 0 then
                    sock.SendString(s + CRLF);
                until (s = '') or (qSock.LastError <> 0);
                Break;
              end;
              if not RelaySock(qsock, sock, size) then
                break;
            until False;
          end
          else
          begin
            if size = -1 then
              if method = 'GET' then
                if (status div 100) = 2 then
                  begin
                    while qsock.LastError = 0 do
                    begin
                      s := qsock.RecvPacket(timeout);
                      if qsock.LastError = 0 then
                        sock.SendString(s);
                      LogRec.len := LogRec.len + length(s);
                    end;
                  end;
          end;
        end;
      end;
      //done
      WriteAccesslog(Logrec);
      if (qsock.LastError <> 0) or (sock.LastError <> 0) then
        Exit;
      sleep(1);
    until not proxykeep;
    //finish with connection
  finally
    sock.Free;
    Qsock.Free;
  end;
end;

procedure TTCPHttpThrd.ReturnHTML(const sock: TTCPBlockSocket; const value, stat: string);
begin
  sock.sendstring('HTTP/1.0 ' + stat + CRLF);
  sock.sendstring('Content-type: text/html' + CRLF);
  sock.sendstring('Content-length: ' + Inttostr(length(value)) + CRLF);
  sock.sendstring('proxy-Connection: close' + CRLF);
  sock.sendstring(CRLF);
  sock.sendstring(value);
end;

procedure TTCPHttpThrd.Return502(const sock: TTCPBlockSocket; host, port: string);
var
  l: TStringlist;
begin
  l := TStringList.Create;
  try
    l.Add('<html>');
    l.Add('<head><title>Bad address!</title></head>');
    l.Add('<body>');
    l.Add('<H1>Bad address!</H1>');
    l.Add('<P>');
    l.Add('Unable to connect with: ' + host + ':' + port);
    l.Add('<P>');
    l.Add('Requested address is bad, or server is not accessible now.');
    l.Add('<P>');
    l.Add('<H2>Error 502</H2>');
    l.Add('<P>');
    l.Add('</body>');
    l.Add('</html>');
    ReturnHTML(sock, l.text, '502');
  finally
    l.free;
  end;
end;

//write Apache compatible access log
procedure TTCPHttpThrd.WriteAccessLog(const LogRec: TLogRec);
var
  day, month, year: word;
  s: string;
const
  MNames: array[1..12] of string =
    ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
     'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
begin
  Decodedate(LogRec.dt,year, month, day);
  s := Format('%.2d', [day]) + '/' + MNames[month] + '/' + IntToStr(year);
  s := '[' + s + FormatDateTime(':hh:nn:ss', LogRec.dt) + ' ' + TimeZone + ']';
  s := LogRec.ip + ' - - ' + s + ' "' + LogRec.req + '"';
  if LogRec.stat = '' then
    s := s + ' -'
  else
    s := s + ' ' + LogRec.Stat;
  if LogRec.len = 0 then
    s := s + ' -'
  else
    s := s + ' ' + IntToStr(LogRec.len);
  if LogRec.Ref = '' then
    s := s + ' "-"'
  else
    s := s + ' "' + LogRec.Ref + '"';
  if LogRec.Agent = '' then
    s := s + ' "-"'
  else
    s := s + ' "' + LogRec.Agent + '"';
  Writelog(s);
end;

end.

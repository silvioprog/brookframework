unit FtpThrd;

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
  Classes, SysUtils, blcksock, synsock, synautil, filectrl;

type
  TFtpServerThread = class(TThread)
  private
    clients: TSocket;
    FDataIP, FDataPort: string;
  protected
    procedure Execute; override;
    procedure send(const sock: TTcpBlocksocket; value: string);
    procedure ParseRemote(Value: string);
    function buildname(dir, value: string): string;
    function buildrealname(value: string): string;
    function buildlist(value: string): string;
  public
    constructor Create(sock: TSocket);
  end;

implementation

const
  timeout = 60000;
  MyMonthNames: array[1..12] of AnsiString =
    ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
     'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');


{==============================================================================}
{ TFtpServerThread }

constructor TFtpServerThread.create(sock: TSocket);
begin
  inherited create(false);
  FreeOnTerminate := true;
  clients := sock;
//  Priority := tpNormal;
end;

procedure TFtpServerThread.send(const sock: TTcpBlocksocket; value: string);
begin
  sock.SendString(value + CRLF);
end;

procedure TFtpServerThread.ParseRemote(Value: string);
var
  n: integer;
  nb, ne: integer;
  s: string;
  x: integer;
begin
  Value := trim(Value);
  nb := Pos('(',Value);
  ne := Pos(')',Value);
  if (nb = 0) or (ne = 0) then
  begin
    nb:=RPos(' ',Value);
    s:=Copy(Value, nb + 1, Length(Value) - nb);
  end
  else
  begin
    s:=Copy(Value,nb+1,ne-nb-1);
  end;
  for n := 1 to 4 do
    if n = 1 then
      FDataIP := Fetch(s, ',')
    else
      FDataIP := FDataIP + '.' + Fetch(s, ',');
  x := StrToIntDef(Fetch(s, ','), 0) * 256;
  x := x + StrToIntDef(Fetch(s, ','), 0);
  FDataPort := IntToStr(x);
end;

function TFtpServerThread.buildname(dir, value: string): string;
begin
  if value = '' then
  begin
    result := dir;
    exit;
  end;
  if value[1] = '/' then
    result := value
  else
    if (dir <> '') and (dir[length(dir)] = '/') then
      Result := dir + value
    else
      Result := dir + '/' + value;
end;

function TFtpServerThread.buildrealname(value: string): string;
begin
  value := replacestring(value, '..', '.');
  value := replacestring(value, '/', '\');
  result := '.\data' + value;
end;

function fdate(value: integer): string;
var
  st: tdatetime;
  wYear, wMonth, wDay: word;
begin
  st := filedatetodatetime(value);
  DecodeDate(st, wYear, wMonth, wDay);
  Result:= Format('%d %s %d', [wday, MyMonthNames[wMonth], wyear]);
end;

function TFtpServerThread.buildlist(value: string): string;
var
  SearchRec: TSearchRec;
  r: integer;
  s: string;
begin
  result := '';
  if value = '' then
    exit;
  if value[length(value)] <> '\' then
    value := value + '\';
  R := FindFirst(value + '*.*', faanyfile, SearchRec);
  while r = 0 do
  begin
    if ((searchrec.Attr and faHidden) = 0)
      and ((searchrec.Attr and faSysFile) = 0)
      and ((searchrec.Attr and faVolumeID) = 0) then
    begin
      s := '';
      if (searchrec.Attr and faDirectory) > 0 then
      begin
        if (searchrec.Name <> '.') and (searchrec.Name <> '..') then
        begin
          s := s + 'drwxrwxrwx   1 root     root         1   ';
          s := s + fdate(searchrec.time) + '  ';
          s := s + searchrec.name;
        end;
      end
      else
      begin
        s := s + '-rwxrwxrwx   1 root     other        ';
        s := s + inttostr(searchrec.Size) + ' ';
        s := s + fdate(searchrec.time) + '  ';
        s := s + searchrec.name;
      end;
      if s <> '' then
        Result := Result + s + CRLF;
    end;
    r := findnext(SearchRec);
  end;
  Findclose(searchrec);
end;

procedure TFtpServerThread.Execute;
var
  sock, dsock: TTCPBlockSocket;
  s, t: string;
  authdone: boolean;
  user: string;
  cmd, par: string;
  pwd: string;
  st: TFileStream;
begin
  sock := TTCPBlockSocket.Create;
  dsock := TTCPBlockSocket.Create;
  try
    sock.Socket := clients;
    send(sock, '220 welcome ' + sock.GetRemoteSinIP + '!');
    authdone := false;
    user := '';
    repeat
      s := sock.RecvString(timeout);
      cmd := uppercase(separateleft(s, ' '));
      par := separateright(s, ' ');
      if sock.lasterror <> 0 then
        exit;
      if terminated then
        exit;
      if cmd = 'USER' then
      begin
        user := par;
        send(sock, '331 Please specify the password.');
        continue;
      end;
      if cmd = 'PASS' then
      begin
        //user verification...
        if ((user = 'username') and (par = 'password'))
          or (user = 'anonymous') then
        begin
          send(sock, '230 Login successful.');
          authdone := true;
          continue;
        end;
      end;
      send(sock, '500 Syntax error, command unrecognized.');
    until authdone;

    pwd := '/';
    repeat
      s := sock.RecvString(timeout);
      cmd := uppercase(separateleft(s, ' '));
      par := separateright(s, ' ');
      if par = s then
        par := '';
      if sock.lasterror <> 0 then
        exit;
      if terminated then
        exit;
      if cmd = 'QUIT' then
      begin
        send(sock, '221 Service closing control connection.');
        break;
      end;
      if cmd = 'NOOP' then
      begin
        send(sock, '200 tjadydadydadydaaaaa!');
        continue;
      end;
      if cmd = 'PWD' then
      begin
        send(sock, '257 ' + Quotestr(pwd, '"'));
        continue;
      end;
      if cmd = 'CWD' then
      begin
        t := unquotestr(par, '"');
        t := buildname(pwd, t);
        if directoryexists(Buildrealname(t)) then
        begin
          pwd := t;
          send(sock, '250 OK ' + t);
        end
        else
          send(sock, '550 Requested action not taken.');
        continue;
      end;
      if cmd = 'MKD' then
      begin
        t := unquotestr(par, '"');
        t := buildname(pwd, t);
        if CreateDir(Buildrealname(t)) then
        begin
          pwd := t;
          send(sock, '257 "' + t + '" directory created');
        end
        else
          send(sock, '521 "' + t + '" Requested action not taken.');
        continue;
      end;
      if cmd = 'CDUP' then
      begin
        pwd := '/';
        send(sock, '250 OK');
        continue;
      end;
      if (cmd = 'TYPE')
        or (cmd = 'ALLO')
        or (cmd = 'STRU')
        or (cmd = 'MODE') then
      begin
        send(sock, '200 OK');
        continue;
      end;
      if cmd = 'PORT' then
      begin
        Parseremote(par);
        send(sock, '200 OK');
        continue;
      end;
      if cmd = 'LIST' then
      begin
        t := unquotestr(par, '"');
        t := buildname(pwd, t);
        dsock.CloseSocket;
        dsock.Connect(Fdataip, Fdataport);
        if dsock.LastError <> 0 then
          send(sock, '425 Can''t open data connection.')
        else
        begin
          send(sock, '150 OK ' + t);
          dsock.SendString(buildlist(buildrealname(t)));
          send(sock, '226 OK ' + t);
        end;
        dsock.CloseSocket;
        continue;
      end;
      if cmd = 'RETR' then
      begin
        t := unquotestr(par, '"');
        t := buildname(pwd, t);
        if fileexists(buildrealname(t)) then
        begin
          dsock.CloseSocket;
          dsock.Connect(Fdataip, Fdataport);
          dsock.SetLinger(true, 10000);
          if dsock.LastError <> 0 then
            send(sock, '425 Can''t open data connection.')
          else
          begin
            send(sock, '150 OK ' + t);
            try
              st := TFileStream.Create(buildrealname(t), fmOpenRead or fmShareDenyWrite);
              try
                dsock.SendStreamRaw(st);
              finally
                st.free;
              end;
              send(sock, '226 OK ' + t);
            except
              on exception do
                send(sock, '451 Requested action aborted: local error in processing.');
            end;
          end;
          dsock.CloseSocket;
        end
        else
          send(sock, '550 File unavailable. ' + t);
        continue;
      end;
      if cmd = 'STOR' then
      begin
        t := unquotestr(par, '"');
        t := buildname(pwd, t);
        if directoryexists(extractfiledir(buildrealname(t))) then
        begin
          dsock.CloseSocket;
          dsock.Connect(Fdataip, Fdataport);
          dsock.SetLinger(true, 10000);
          if dsock.LastError <> 0 then
            send(sock, '425 Can''t open data connection.')
          else
          begin
            send(sock, '150 OK ' + t);
            try
              st := TFileStream.Create(buildrealname(t), fmCreate or fmShareDenyWrite);
              try
                dsock.RecvStreamRaw(st, timeout);
              finally
                st.free;
              end;
              send(sock, '226 OK ' + t);
            except
              on exception do
                send(sock, '451 Requested action aborted: local error in processing.');
            end;
          end;
          dsock.CloseSocket;
        end
        else
          send(sock, '553 Directory not exists. ' + t);
        continue;
      end;
      send(sock, '500 Syntax error, command unrecognized.');
    until false;

  finally
    dsock.free;
    sock.free;
  end;
end;

{==============================================================================}
end.

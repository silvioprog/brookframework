unit mailchck;

interface

uses
  dnssend, smtpsend, synautil, classes, synamisc;

function mailcheck(email:string):integer;

implementation

{
0 - address exists
1 - address may exists
2 - your DNS nannot working (cannot check!)
3 - your DNS is not defined (cannot check!)
4 - cannot contact any MX servers (cannot check!);
5 - domain not have MX record
6 - address not exists
7 - address is bad!
}

function mailcheck(email:string):integer;
var
  smtp:TSMTPsend;
  domain:string;
  user: string;
  mailservers:tstringlist;
  dnsservers:tstringlist;
  x: integer;
  n, m: integer;
  b: boolean;
begin
  result:=7;
  email:=getemailaddr(email);
  x := pos('@', email);
  if x <= 0 then
    exit;   //invalid address format
  domain:=separateright(email,'@');
  user:=separateLeft(email,'@');
  if (domain = '') or (user = '') then
    exit;  //invalid address format
  smtp:=tsmtpsend.create;
  mailservers:=tstringlist.create;
  dnsservers:=tstringlist.create;
  try
    dnsservers.CommaText := GetDNS;
    result := 3;
    if dnsservers.Count = 0 then
      Exit; // not DNS servers defined
    result := 2;
    b := false;
    for n := 0 to dnsservers.Count -1 do
      if GetMailServers(dnsservers[n], domain, mailservers) then
      begin
        b := true;
        break;
      end;
    if not b then
      Exit;  // DNS cannot be contacted
    result := 5;
    if mailservers.Count = 0 then
      exit;   // not defined MX record for requested domain
    b := false;
    for n := 0 to mailservers.count - 1 do
    begin
      smtp.TargetHost := mailservers[n];
      if not smtp.Login then
        Continue;
      b := true;
      if smtp.Verify(email) then
      begin
        if smtp.ResultCode < 252 then
        begin
          Result := 0; // user address confirmed!
          break;
        end;
      end
      else
        if smtp.ResultCode = 551 then
        begin
          Result := 6; // user address not confirmed!
          break;
        end;
      if not smtp.MailFrom('mailcheck@somewhere.com', 100) then
        Continue;
      if not smtp.MailTo(email) then
      begin
        Result := 6;  // user address not confirmed!
        break;
      end
      else
      begin
        Result := 1;  // address MAY exists
        break;
      end;
    end;
    if not b then
      result := 4;   //cannot contact any mailserver;
  finally
    dnsservers.free;
    mailservers.free;
    smtp.free;
  end;
end;

end.

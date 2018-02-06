{$MODE DELPHI}

Program testdns;

uses
  dnssend, synamisc, classes;

var
  l: tstringlist;
  s: string;
begin
  l := TStringList.create;
  try
    s := GetDNS;
    writeln('DNS servers: ', s);
    l.commatext := s;
    if l.count > 0 then
    begin
      s := l[0];
      GetMailServers(s, paramstr(1), l);
      Writeln('MX records for domain ', paramstr(1), ':');
      writeln(l.text);
    end;
  finally
    l.free;
  end;
end.


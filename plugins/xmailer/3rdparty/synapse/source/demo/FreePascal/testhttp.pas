{$MODE DELPHI}

Program testhttp;

uses
  httpsend, classes;

var
  HTTP: THTTPSend;
  l: tstringlist;
begin
  HTTP := THTTPSend.Create;
  l := TStringList.create;
  try
    if not HTTP.HTTPMethod('GET', Paramstr(1)) then
      begin
	writeln('ERROR');
        writeln(Http.Resultcode);
      end
    else
      begin
        writeln(Http.Resultcode, ' ', Http.Resultstring);
        writeln;
        writeln(Http.headers.text);
        writeln;
        l.loadfromstream(Http.Document);
        writeln(l.text);
     end;
  finally
    HTTP.Free;
    l.free;
  end;
end.


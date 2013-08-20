unit test;

{$mode objfpc}{$H+}

interface

uses
  BrookAction;

type
  TTest = class(TBrookAction)
  public
    procedure Get; override;
  end;

implementation

procedure TTest.Get;
begin
  Write(Values.AsJSON);
end;

initialization
  // call http://localhost/cgi-bin/cgi1/user/John/a/b, and it returns { "name" : "John", "path" : "a\/b" }
  TTest.Register('/user/:name/*path');

end.

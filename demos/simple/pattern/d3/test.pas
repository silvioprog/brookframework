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
  Write('Canonical URL.');
end;

initialization
  // call http://localhost/cgi-bin/cgi1, and it redirect to http://localhost/cgi-bin/cgi1/
  TTest.Register('*/');

end.

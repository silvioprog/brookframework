unit A;

{$mode objfpc}{$H+}

interface

uses
  BrookAction;

type
  TA = class(TBrookAction)
  public
    procedure Get; override;
  end;

implementation

procedure TA.Get;
begin
  Write('A');
end;

initialization
  // call http://localhost/cgi-bin/cgi1/a
  TA.Register('/a');

end.

unit B;

{$mode objfpc}{$H+}

interface

uses
  BrookAction;

type
  TB = class(TBrookAction)
  public
    procedure Get; override;
  end;

implementation

procedure TB.Get;
begin
  Write('B');
end;

initialization
  // call http://localhost/cgi-bin/cgi1/b
  TB.Register('/b');

end.

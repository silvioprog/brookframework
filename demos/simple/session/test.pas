unit Test;

{$mode objfpc}{$H+}

interface

uses
  BrookAction, BrookSession, SysUtils;

type

  { TVisit }

  TVisit = class
  private
    FCount: Integer;
  published
    property Count: Integer read FCount write FCount;
  end;

  { TMyAction }

  TMyAction = class(TBrookAction)
  private
    FSession: TBrookSession;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Get; override;
  end;

implementation

{ TMyAction }

constructor TMyAction.Create;
begin
  inherited Create;
  FSession := TBrookSession.Create;
end;

destructor TMyAction.Destroy;
begin
  FSession.Free;
  inherited Destroy;
end;

procedure TMyAction.Get;
var
  VVisit: TVisit;
begin
  VVisit := TVisit.Create;
  try
    FSession.Start(TheRequest);
    Write('<!DOCTYPE HTML>');
    Write('<html lang="en-US">');
    Write('<head>');
    Write('	<meta charset="UTF-8">');
    Write('	<title>Visits</title>');
    Write('</head>');
    Write('<body>');
    if FSession.Fields.Count = 0 then
    begin
      Write('Use F5 to show created session.<br />');
      FSession.Fields.Add('count=1');
    end
    else
    begin
      FSession.GetFields(VVisit);
      Write('Visit count: %d', [VVisit.Count]);
      VVisit.Count := VVisit.Count + 1;
      FSession.Fields.Values['count'] := IntToStr(VVisit.Count);
    end;
    Write('</body>');
    Write('</html>');
    FSession.Finish(TheResponse);
  finally
    VVisit.Free;
  end;
end;

initialization
  TMyAction.Register('*');

end.

unit Test;

{$mode objfpc}{$H+}

interface

uses
  BrookAction, BrookSession, BrookConsts, HTTPDefs;

type
  TMyAction = class(TBrookAction)
  private
    FSession: TBrookSession;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Request({%H-}ARequest: TRequest;
      {%H-}AResponse: TResponse); override;
  end;

implementation

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

procedure TMyAction.Request(ARequest: TRequest; AResponse: TResponse);
var
  I: Integer;
begin
  FSession.Start(ARequest);
  Write('<!DOCTYPE HTML>');
  Write('<html lang="en-US">');
  Write('<head>');
  Write('	<meta charset="UTF-8">');
  Write('	<title>Sessions</title>');
  Write('</head>');
  Write('<body>');
  if FSession.Fields.Count = 0 then
  begin
    Write('Creating session ...' + BR);
    Write('Use F5 to show created session.' + BR);
    FSession.Fields.Add('session1', 'ABC');
    FSession.Fields.Add('session2', 123);
    FSession.Fields.Add('session3', 1.5);
    FSession.Fields.Add('session4', True);
  end
  else
  begin
    Write('Created session:' + BR);
    for I := 0 to Pred(FSession.Fields.Count) do
      Write(FSession.Fields.Items[I].AsString + BR);
  end;
  Write('</body>');
  Write('</html>');
  FSession.Finish(AResponse);
end;

initialization
  TMyAction.Register('*');

end.

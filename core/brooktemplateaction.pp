unit BrookTemplateAction;

{$mode objfpc}{$H+}

interface

uses
  HTTPDefs,BrookAction;

type

  { TTemplateAction }

  TTemplateAction = class(TBrookAction)
  protected
    procedure Before; virtual;
    procedure After; virtual;
  public
    procedure Request(ARequest: TRequest; AResponse: TResponse); override;
  end;

implementation

{ TTemplateAction }

procedure TTemplateAction.Request(ARequest: TRequest; AResponse: TResponse);
begin
  Before;
  inherited Request(ARequest,AResponse);
  After;
end;

procedure TTemplateAction.Before;
begin
  // this method is intentionally left blank
end;

procedure TTemplateAction.After;
begin
  // this method is intentionally left blank
end;

end.


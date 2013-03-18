unit Actions;

{$mode objfpc}{$H+}

interface

uses
  BrookAction, HTTPDefs, Classes;

type

  { TTopLevelAction }

  TTopLevelAction = class(TBrookAction)
  private
    FTitle: string;
    FContent: string;
  protected
    procedure DoBeforeRequest(ARequest: TRequest; AResponse: TResponse); override;
    procedure DoAfterRequest(ARequest: TRequest; AResponse: TResponse); override;
  public
    property Title: string write FTitle;
    property Content: string write FContent;
  end;

  { TLevel1Action }

  TLevel1Action = class(TTopLevelAction)
  private
    FInnerContent: string;
  protected
    procedure DoBeforeRequest(ARequest: TRequest; AResponse: TResponse); override;
    procedure DoAfterRequest(ARequest: TRequest; AResponse: TResponse); override;
  public
    property InnerContent: string write FInnerContent;
  end;

  { TLevel2Action1 }

  TLevel2Action1 = class(TLevel1Action)
  public
    procedure Get; override;
  end;

  { TLevel2Action2 }

  TLevel2Action2 = class(TLevel1Action)
  public
    procedure Get; override;
  end;

implementation

{ TTopLevelAction }

procedure TTopLevelAction.DoBeforeRequest(ARequest: TRequest;
  AResponse: TResponse);
begin
  inherited DoBeforeRequest(ARequest, AResponse);
  FContent := '';
end;

procedure TTopLevelAction.DoAfterRequest(ARequest: TRequest;
  AResponse: TResponse);
var
  VHtml: TStringList;
begin
  VHtml := TStringList.Create;
  try
    VHtml.Add('<!DOCTYPE html>');
    VHtml.Add('<html>');
    VHtml.Add('<head>');
    VHtml.Add('<title>' + FTitle + '</title>');
    VHtml.Add('</head>');
    VHtml.Add('<body>');
    VHtml.Add(FContent);
    VHtml.Add('</body>');
    VHtml.Add('</html>');
    Write(VHtml);
  finally
    VHtml.Free;
  end;
  inherited DoAfterRequest(ARequest, AResponse);
end;

{ TLevel1Action }

procedure TLevel1Action.DoBeforeRequest(ARequest: TRequest;
  AResponse: TResponse);
begin
  inherited DoBeforeRequest(ARequest, AResponse);
  FInnerContent := '';
end;

procedure TLevel1Action.DoAfterRequest(ARequest: TRequest;
  AResponse: TResponse);
var
  VHtml: TStringList;
begin
  VHtml := TStringList.Create;
  try
    VHtml.Add('<p>== Level 1 Content ==</p>');
    VHtml.Add(FInnerContent);
    VHtml.Add('<p>== Level 1 Content ==</p>');
    Content := VHtml.Text;
  finally
    VHtml.Free;
  end;
  inherited DoAfterRequest(ARequest, AResponse);
end;

{ TLevel2Action1 }

procedure TLevel2Action1.Get;
begin
  Title := 'Level 2 Action 1';
  InnerContent := 'Get: Level 2 Action 1';
end;

{ TLevel2Action2 }

procedure TLevel2Action2.Get;
begin
  Title := 'Level 2 Action 2';
  InnerContent := 'Get: Level 2 Action 2';
end;

initialization
  TLevel2Action1.Register('/action1');
  TLevel2Action2.Register('/action2');

end.

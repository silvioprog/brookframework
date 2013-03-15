unit Actions;

{$mode objfpc}{$H+}

interface

uses
  BrookTemplateAction;

type

  { TTopLevelAction }

  TTopLevelAction = class(TTemplateAction)
  private
    FTitle: String;
    FContent: String;
  protected
    procedure Before; override;
    procedure After; override;
  public
    property Title: String write FTitle;
    property Content: String write FContent;
  end;

  { TLevel1Action }

  TLevel1Action = class(TTopLevelAction)
  private
    FInnerContent: String;
  protected
    procedure Before; override;
    procedure After; override;
  public
    property InnerContent: String write FInnerContent;
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

uses
  Classes,SysUtils;

{ TTopLevelAction }

procedure TTopLevelAction.Before;
begin
  inherited Before;
  FContent := '';
end;

procedure TTopLevelAction.After;
begin
  with TStringList.Create do
    try
      Add('<!DOCTYPE html>');
      Add('<html>');
      Add('  <head>');
      Add('    <title>' + FTitle + '</title>');
      Add('  </head>');
      Add('  <body>');
      Add(FContent);
      Add('  </body>');
      Add('</html>');

      Write(Text);
    finally
      Free;
    end;
  inherited After;
end;

{ TLevel1Action }

procedure TLevel1Action.Before;
begin
  inherited Before;
  FInnerContent := '';
end;

procedure TLevel1Action.After;
begin
  with TStringList.Create do
    try
      Add('<p>== Level 1 Content ==</p>');
      Add(FInnerContent);
      Add('<p>== Level 1 Content ==</p>');

      Content := Text;
    finally
      Free;
    end;
  inherited After;
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

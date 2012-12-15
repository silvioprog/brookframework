unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  BrookAction, BrookConsts, XMailer;

type
  TMail = class(TBrookAction)
  public
    procedure Get; override;
    procedure Post; override;
  end;

const
  _FORM = {$i form.inc};

implementation

procedure TMail.Get;
begin
  Write(_FORM);
end;

procedure TMail.Post;
var
  Mail: TSendMail;
begin
  Mail := TSendMail.Create;
  try
    // Mail
    Mail.Sender := 'sender@host.com';
    Mail.Receivers.Add(Fields['email'].AsString);
    Mail.Subject := 'Test.';
    Mail.Message.Add('Test OK.');
    // SMTP
    Mail.Smtp.UserName := 'user@host.com';
    Mail.Smtp.Password := 'abc123';
    Mail.Smtp.Host := 'smtp.host.com';
    Mail.Smtp.Port := '465';
    Mail.Smtp.SSL := True;
    Mail.Smtp.TLS := True;
    Mail.Send;
    Write('E-mail sent successfully!');
  finally
    Mail.Free;
  end;
end;

initialization
  TMail.Register('*');

end.

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  SNTPsend, StdCtrls;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Label1: TLabel;
    Button1: TButton;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
var
  sntp:TSntpSend;
begin
  sntp:=TSntpSend.Create;
  try
    sntp.TargetHost:=Edit1.Text;
    if sntp.GetSNTP
      then label2.Caption:=Datetimetostr(sntp.NTPTime)+' UTC'
      else label2.Caption:='Not contacted!';
  finally
    sntp.Free;
  end;
end;

end.


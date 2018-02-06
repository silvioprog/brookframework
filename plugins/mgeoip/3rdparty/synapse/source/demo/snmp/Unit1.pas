unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  SNMPsend, StdCtrls;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Label1: TLabel;
    Button1: TButton;
    Label2: TLabel;
    Label3: TLabel;
    Edit2: TEdit;
    Label4: TLabel;
    Edit3: TEdit;
    Memo1: TMemo;
    Memo2: TMemo;
    Label5: TLabel;
    Label6: TLabel;
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
  response,s:string;
  n:integer;
begin
  if SNMPget(Edit3.Text,Edit2.Text,Edit1.Text,response)
    then
      begin
        label2.Caption:='Success...';
        memo1.Lines.Clear;
        memo2.Lines.Clear;
        memo1.Lines.Text:=response;
        s:='';
        for n:=0 to Length(response) do
          begin
            s:=s+'$'+IntToHex(Ord(response[n]),2)+' ';
          end;
        memo2.Lines.Text:=s;
      end
    else
      begin
        label2.Caption:='Not contacted... (Check SNMP host, community or MIB OID!)';
        memo1.Lines.Clear;
        memo2.Lines.Clear;
      end;
end;

end.


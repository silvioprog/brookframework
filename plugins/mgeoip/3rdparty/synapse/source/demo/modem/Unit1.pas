unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  SynaSer, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Edit2: TEdit;
    Edit3: TEdit;
    Label3: TLabel;
    Button1: TButton;
    Bevel1: TBevel;
    Memo1: TMemo;
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
  ser:TBlockSerial;
begin
  ser:=TBlockSerial.Create;
  ser.RaiseExcept:=True;
  try
    ser.Connect(Edit1.Text, StrToIntDef(Edit2.Text, 9600),8,'N',0,false,false);
    memo1.lines.text:=ser.ATCommand(Edit3.Text);
  finally
    ser.free;
  end;
end;

end.
 
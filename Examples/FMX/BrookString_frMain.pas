unit BrookString_frMain;

interface

uses
  System.SysUtils,
  System.Classes,
  FMX.Types,
  FMX.StdCtrls,
  FMX.Controls,
  FMX.Controls.Presentation,
  FMX.Dialogs,
  FMX.Forms,
  BrookString;

type
  TfrMain = class(TForm)
    lbDesc: TLabel;
    btAddNow: TButton;
    btShowContent: TButton;
    btClear: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btAddNowClick(Sender: TObject);
    procedure btShowContentClick(Sender: TObject);
    procedure btClearClick(Sender: TObject);
  private
    FString: TBrookString;
  end;

var
  frMain: TfrMain;

implementation

{$R *.fmx}

procedure TfrMain.FormCreate(Sender: TObject);
begin
  FString := TBrookString.Create(nil);
end;

procedure TfrMain.FormDestroy(Sender: TObject);
begin
  FString.Free;
end;

procedure TfrMain.btAddNowClick(Sender: TObject);
var
  VBytes: TBytes;
begin
  VBytes := BytesOf(FormatDateTime(Concat('hh:nn:ss.zzz', sLineBreak), Now));
  FString.Write(VBytes, Length(VBytes));
end;

procedure TfrMain.btShowContentClick(Sender: TObject);
begin
  ShowMessage(Concat('All clicks:', sLineBreak, sLineBreak,
    StringOf(FString.Content)));
end;

procedure TfrMain.btClearClick(Sender: TObject);
begin
  FString.Clear;
end;

end.
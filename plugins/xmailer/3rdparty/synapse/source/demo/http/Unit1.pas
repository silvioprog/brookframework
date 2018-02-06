unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  HTTPSend, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Memo1: TMemo;
    Memo2: TMemo;
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    Label2: TLabel;
    Label3: TLabel;
    Edit2: TEdit;
    Button2: TButton;
    Label4: TLabel;
    Label5: TLabel;
    Edit3: TEdit;
    Label6: TLabel;
    Edit4: TEdit;
    Label7: TLabel;
    Label8: TLabel;
    Edit5: TEdit;
    Button3: TButton;
    Label9: TLabel;
    Edit6: TEdit;
    Edit7: TEdit;
    Label10: TLabel;
    Panel5: TPanel;
    Label11: TLabel;
    Label12: TLabel;
    Edit8: TEdit;
    Edit9: TEdit;
    Label13: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

function ProxyHttpPostURL(const URL, URLData: string; const Data: TStream): Boolean;
var
  HTTP: THTTPSend;
begin
  HTTP := THTTPSend.Create;
  try
    HTTP.ProxyHost := Form1.Edit8.Text;
    HTTP.ProxyPort := Form1.Edit9.Text;
    HTTP.Document.Write(Pointer(URLData)^, Length(URLData));
    HTTP.MimeType := 'application/x-www-form-urlencoded';
    Result := HTTP.HTTPMethod('POST', URL);
    Data.CopyFrom(HTTP.Document, 0);
  finally
    HTTP.Free;
  end;
end;

function ProxyHttpPostFile(const URL, FieldName, FileName: string;
  const Data: TStream; const ResultData: TStrings): Boolean;
const
  CRLF = #$0D + #$0A;
var
  HTTP: THTTPSend;
  Bound, s: string;
begin
  Bound := IntToHex(Random(MaxInt), 8) + '_Synapse_boundary';
  HTTP := THTTPSend.Create;
  try
    HTTP.ProxyHost := Form1.Edit8.Text;
    HTTP.ProxyPort := Form1.Edit9.Text;
    s := '--' + Bound + CRLF;
    s := s + 'content-disposition: form-data; name="' + FieldName + '";';
    s := s + ' filename="' + FileName +'"' + CRLF;
    s := s + 'Content-Type: Application/octet-string' + CRLF + CRLF;
    HTTP.Document.Write(Pointer(s)^, Length(s));
    HTTP.Document.CopyFrom(Data, 0);
    s := CRLF + '--' + Bound + '--' + CRLF;
    HTTP.Document.Write(Pointer(s)^, Length(s));
    HTTP.MimeType := 'multipart/form-data, boundary=' + Bound;
    Result := HTTP.HTTPMethod('POST', URL);
    ResultData.LoadFromStream(HTTP.Document);
  finally
    HTTP.Free;
  end;
end;


procedure TForm1.Button1Click(Sender: TObject);
var
  HTTP: THTTPSend;
begin
  HTTP := THTTPSend.Create;
  try
    HTTP.ProxyHost := Edit8.Text;
    HTTP.ProxyPort := Edit9.Text;
    HTTP.HTTPMethod('GET', Edit1.text);
    Memo1.Lines.Assign(HTTP.Headers);
    Memo2.Lines.LoadFromStream(HTTP.Document);
  finally
    HTTP.Free;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  st: TMemoryStream;
begin
  st:=TMemoryStream.Create;
  try
    ProxyHTTPpostURL(Edit2.Text, Edit3.Text + '=' + Edit4.Text, st);
    st.Seek(0,soFromBeginning);
    Memo2.Lines.LoadFromStream(st);
  finally
    st.Free;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  st: TFileStream;
begin
  st := TFileStream.Create(Edit7.Text, fmOpenRead	or fmShareDenyWrite);
  try
    ProxyHTTPPostFile(Edit5.Text, Edit6.Text, ExtractFilename(Edit7.Text), st, TStringList(memo2.Lines));
  finally
    st.Free;
  end;
end;

end.

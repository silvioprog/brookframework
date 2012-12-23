unit frmMain;

{$mode objfpc}{$H+}

interface

uses
  Forms, StdCtrls, Grids, Buttons, LJGridUtils, HttpUtils, SysUtils, IniFiles, Classes;

type

  { TfrMain }

  TfrMain = class(TForm)
    btUpdateContacts: TBitBtn;
    btAddPhone: TBitBtn;
    btAddContact: TBitBtn;
    btDeleteContact: TBitBtn;
    btEditContact: TBitBtn;
    btDeletePhone: TBitBtn;
    btEditPhone: TBitBtn;
    btUpdateContacts1: TBitBtn;
    lbContacts: TLabel;
    lbPhones: TLabel;
    grContacts: TStringGrid;
    grPhones: TStringGrid;
    procedure btAddContactClick(Sender: TObject);
    procedure btAddPhoneClick(Sender: TObject);
    procedure btDeleteContactClick(Sender: TObject);
    procedure btDeletePhoneClick(Sender: TObject);
    procedure btEditContactClick(Sender: TObject);
    procedure btEditPhoneClick(Sender: TObject);
    procedure btUpdateContacts1Click(Sender: TObject);
    procedure btUpdateContactsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure grContactsDblClick(Sender: TObject);
    procedure grContactsSelection(Sender: TObject;{%H-}aCol,{%H-}aRow: Integer);
    procedure grPhonesDblClick(Sender: TObject);
  private
    FHost : String;
    FPort : Integer;
    FAppName : String;
    procedure StartAppIni;
    function url : String;
  protected
    procedure UpdateContacts;
    procedure UpdatePhones;
  end;

var
  frMain: TfrMain;

implementation

{$R *.lfm}

uses
  frmCustomEdit, frmContactEdit, frmPhoneEdit;

const
  URL_ROOT = 'http://%s/cgi-bin/%s/';

{ TfrMain }

procedure TfrMain.StartAppIni;
var
  FAppPath : String;
  FApp : String;
begin
  FApp         := ExtractFileName( ChangeFileExt( Application.ExeName, '.cfg') );
  FAppPath     := format('%s%s',[ ExtractFilePath( Application.ExeName ), FApp]);
  with TIniFile.Create(FAppPath) do
  begin
   try
    FHost    := ReadString('Server', 'Host', '');
    if (FHost = '') then
    begin
     FHost    := '127.0.0.1';
     WriteString('Server', 'Host', FHost);
    end;

    FPort    := ReadInteger('Server', 'Port', 0);
    if FPort = 0 then
    begin
     FPort   := 80;
     WriteInteger('Server', 'Port', FPort);
    end;

    FAppName  := ReadString('Server', 'AppName', '');

    if FAppName = '' then
    begin
     FAppName := 'addressbook';
     WriteString('Server', 'AppName', FAppName);
    end;

   finally
    Destroy;
   end;
  end;
end;

function TfrMain.url : String;
begin
  result  := format(URL_ROOT,[ FHost, FAppName]);
  if FPort <> 80 then
  result := format(URL_ROOT,[ format('%s:%d',[FHost,FPort]), FAppName]);
end;

procedure TfrMain.FormShow(Sender: TObject);
begin
  UpdateContacts;
  UpdatePhones;
end;

procedure TfrMain.grContactsDblClick(Sender: TObject);
begin
  btEditContactClick(Sender);
end;

procedure TfrMain.grPhonesDblClick(Sender: TObject);
begin
  btEditPhoneClick(Sender);
end;

procedure TfrMain.grContactsSelection(Sender: TObject; aCol, aRow: Integer);
begin
  UpdatePhones;
end;

procedure TfrMain.btAddContactClick(Sender: TObject);
begin
  if TfrContactEdit.Add(url + 'contacts') then
    UpdateContacts;
end;

procedure TfrMain.btAddPhoneClick(Sender: TObject);
begin
  if TfrPhoneEdit.Add(url + 'contacts/:id/phones',
    GetSelectedRow(grContacts)) then
    UpdatePhones;
end;

procedure TfrMain.btEditContactClick(Sender: TObject);
begin
  if TfrContactEdit.Edit(url + 'contacts/:id',
    GetSelectedRow(grContacts)) then
    UpdateContacts;
end;

procedure TfrMain.btEditPhoneClick(Sender: TObject);
begin
  if TfrPhoneEdit.Edit(url + 'contacts/:contactid/phones/:id',
    GetSelectedRow(grPhones)) then
    UpdatePhones;
end;

procedure TfrMain.btUpdateContacts1Click(Sender: TObject);
begin
  UpdatePhones;
end;

procedure TfrMain.btUpdateContactsClick(Sender: TObject);
begin
  UpdateContacts;
end;

procedure TfrMain.FormCreate(Sender: TObject);
begin
   StartAppIni;
end;

procedure TfrMain.btDeleteContactClick(Sender: TObject);
begin
  if TfrContactEdit.Delete(url + 'contacts/:id', 'Delete contact?',
    GetSelectedRow(grContacts)) then
    UpdateContacts;
end;

procedure TfrMain.btDeletePhoneClick(Sender: TObject);
begin
  if TfrPhoneEdit.Delete(url + 'contacts/:contactid/phones/:id',
    'Delete phone?', GetSelectedRow(grPhones)) then
    UpdatePhones;
end;

procedure TfrMain.UpdateContacts;
begin
  TfrCustomEdit.Refresh(grContacts, url + 'contacts');
end;

procedure TfrMain.UpdatePhones;
begin
  TfrCustomEdit.Refresh(grPhones, url + 'contacts/:id/phones',
    GetSelectedRow(grContacts));
end;

initialization
  HttpSettings.Redirection := False;

end.


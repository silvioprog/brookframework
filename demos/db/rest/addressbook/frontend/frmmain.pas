unit frmMain;

{$mode objfpc}{$H+}

interface

uses
  Forms, StdCtrls, Grids, Buttons, Classes, LJGridUtils, BrookConfigurator;

type
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
    procedure FormShow(Sender: TObject);
    procedure grContactsDblClick(Sender: TObject);
    procedure grContactsSelection(Sender: TObject;{%H-}aCol,{%H-}aRow: Integer);
    procedure grPhonesDblClick(Sender: TObject);
  private
    FConfigurator: TBrookConfigurator;
    FRootUrl: string;
  protected
    procedure UpdateContacts;
    procedure UpdatePhones;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Configurator: TBrookConfigurator read FConfigurator;
  published
    property RootUrl: string read FRootUrl write FRootUrl;
  end;

var
  frMain: TfrMain;

implementation

{$R *.lfm}

uses
  frmCustomEdit, frmContactEdit, frmPhoneEdit;

{ TfrMain }

constructor TfrMain.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConfigurator := TBrookConfigurator.Create('addressbookclient.cfg');
  FConfigurator.Target := Self;
  FConfigurator.Configure;
end;

destructor TfrMain.Destroy;
begin
  FConfigurator.Free;
  inherited Destroy;
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
  if TfrContactEdit.Add(RootUrl + 'contacts') then
    UpdateContacts;
end;

procedure TfrMain.btAddPhoneClick(Sender: TObject);
begin
  if TfrPhoneEdit.Add(RootUrl + 'contacts/:id/phones',
    GetSelectedRow(grContacts)) then
    UpdatePhones;
end;

procedure TfrMain.btEditContactClick(Sender: TObject);
begin
  if TfrContactEdit.Edit(RootUrl + 'contacts/:id',
    GetSelectedRow(grContacts)) then
    UpdateContacts;
end;

procedure TfrMain.btEditPhoneClick(Sender: TObject);
begin
  if TfrPhoneEdit.Edit(RootUrl + 'contacts/:contactid/phones/:id',
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

procedure TfrMain.btDeleteContactClick(Sender: TObject);
begin
  if TfrContactEdit.Delete(RootUrl + 'contacts/:id', 'Delete contact?',
    GetSelectedRow(grContacts)) then
    UpdateContacts;
end;

procedure TfrMain.btDeletePhoneClick(Sender: TObject);
begin
  if TfrPhoneEdit.Delete(RootUrl + 'contacts/:contactid/phones/:id',
    'Delete phone?', GetSelectedRow(grPhones)) then
    UpdatePhones;
end;

procedure TfrMain.UpdateContacts;
begin
  TfrCustomEdit.Refresh(grContacts, RootUrl + 'contacts');
end;

procedure TfrMain.UpdatePhones;
begin
  TfrCustomEdit.Refresh(grPhones, RootUrl + 'contacts/:id/phones',
    GetSelectedRow(grContacts));
end;

end.


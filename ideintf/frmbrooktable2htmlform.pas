(*
  Brook Table2HTMLForm unit.

  Copyright (C) 2013 Silvio Clecio.

  http://brookframework.org

  All contributors:
  Plase see the file CONTRIBUTORS.txt, included in this
  distribution.

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit frmBrookTable2HTMLForm;

{$mode objfpc}{$H+}

interface

uses
  BrookDataBase, BrookTable, BrookSQLdbBroker, BrookDBUtils, BrookConsts, Forms,
  Classes, SysUtils, Dialogs, StdCtrls, ExtCtrls, CheckLst, Buttons, Controls,
  XMLPropStorage, ListFilterEdit, SQLdb, DB, StrUtils;

type

  { TfrBrookTable2HTMLForm }

  TfrBrookTable2HTMLForm = class(TForm)
    btDelete: TBitBtn;
    btDown: TBitBtn;
    btOpen: TBitBtn;
    btSave: TBitBtn;
    btCancel: TBitBtn;
    btUp: TBitBtn;
    chLineBreak: TCheckBox;
    chFullHTML: TCheckBox;
    edConnection: TEdit;
    lbFilterTables: TLabel;
    lbInfo: TLabel;
    lbConnection: TLabel;
    lbFields: TLabel;
    lbTables: TLabel;
    liFields: TCheckListBox;
    edFilterTables: TListFilterEdit;
    liTables: TListBox;
    pnFilterTable: TPanel;
    pnConnection: TPanel;
    pnAjFields: TPanel;
    pnFields: TPanel;
    pnTable: TPanel;
    pnData: TPanel;
    pnClient: TPanel;
    pnBottom: TPanel;
    sp1: TSplitter;
    xml: TXMLPropStorage;
    procedure btDeleteClick(Sender: TObject);
    procedure btDownClick(Sender: TObject);
    procedure btOpenClick(Sender: TObject);
    procedure btUpClick(Sender: TObject);
    procedure btSaveClick(Sender: TObject);
    procedure edConnectionEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure liFieldsClick(Sender: TObject);
    procedure liFieldsItemClick(Sender: TObject;{%H-}Index: integer);
    procedure liTablesSelectionChange(Sender: TObject;{%H-}User: boolean);
  public
    t: TBrookTable;
    d: TBrookDataBase;
    conn: TSQLConnection;
    class procedure Execute;
    procedure UpdateView;
    procedure Setup;
    procedure OpenTable;
  end;

implementation

{$R *.lfm}

uses
  BrookProjectIntf;

{ TfrBrookTable2HTMLForm }

procedure TfrBrookTable2HTMLForm.edConnectionEditingDone(Sender: TObject);
begin
  Setup;
end;

procedure TfrBrookTable2HTMLForm.FormCreate(Sender: TObject);
begin
  xml.FileName := BrookGetExpertsConfigFileName;
end;

procedure TfrBrookTable2HTMLForm.FormShow(Sender: TObject);
begin
  edConnection.SetFocus;
  edConnection.SelectAll;
end;

procedure TfrBrookTable2HTMLForm.liFieldsClick(Sender: TObject);
begin
  UpdateView;
end;

procedure TfrBrookTable2HTMLForm.liFieldsItemClick(Sender: TObject;
  Index: integer);
begin
  UpdateView;
end;

procedure TfrBrookTable2HTMLForm.btSaveClick(Sender: TObject);
var
  i: Integer;
  form: string;
  fle: TFileStream;
  dlg: TSaveDialog;
begin
  dlg := TSaveDialog.Create(nil);
  try
    dlg.Options := dlg.Options + [ofOverwritePrompt];
    dlg.DefaultExt := '.html';
    dlg.FileName := 'form.html';
    if dlg.Execute then
    begin
      OpenTable;
      for i := 0 to Pred(liFields.Count) do
        t.FieldDefs.Find(liFields.Items[i]).Required := liFields.Checked[i];
      form :=
        IfThen(
          chFullHTML.Checked,
          Format(
            '<!DOCTYPE HTML>'+LF+
            '<html lang="en-US">'+LF+
            '<head>'+LF+
            '	<meta charset="UTF-8">'+LF+
            '	<title></title>'+LF+
            '</head>'+LF+
            '<body>'+LF+
            '%s'+LF+
            '</body>'+LF+
            '</html>', [BrookFieldDefsToHTMLForm(t.FieldDefs, ES, ES,
              chLineBreak.Checked)]),
          BrookFieldDefsToHTMLForm(t.FieldDefs, ES, ES, chLineBreak.Checked));
      fle := TFileStream.Create(dlg.FileName, fmCreate);
      try
        fle.Write(Pointer(form)^, Length(form));
        ShowMessage('HTML form successfully created.');
      finally
        fle.Free;
      end;
    end;
  finally
    dlg.Free;
  end;
end;

procedure TfrBrookTable2HTMLForm.btUpClick(Sender: TObject);
var
  idx: Integer;
begin
  idx := liFields.ItemIndex;
  liFields.Items.Move(idx, idx - 1);
  liFields.Selected[idx - 1] := True;
  UpdateView;
end;

procedure TfrBrookTable2HTMLForm.btDeleteClick(Sender: TObject);
var
  i: Integer;
begin
  if MessageDlg('Delete field(s)?', mtConfirmation, mbYesNo, 0) <> mrYes then
    Exit;
  for i := Pred(liFields.Count) downto 0 do
    if liFields.Selected[i] then
      liFields.Items.Delete(i);
  UpdateView;
end;

procedure TfrBrookTable2HTMLForm.btDownClick(Sender: TObject);
var
  idx: Integer;
begin
  idx := liFields.ItemIndex;
  liFields.Items.Move(idx, idx + 1);
  liFields.Selected[idx + 1] := True;
  UpdateView;
end;

procedure TfrBrookTable2HTMLForm.btOpenClick(Sender: TObject);
var
  dlg: TOpenDialog;
begin
  dlg := TOpenDialog.Create(nil);
  try
    if dlg.Execute then
    begin
      edConnection.Text := dlg.FileName;
      edConnection.SelectAll;
      Setup;
    end;
  finally
    dlg.Free;
  end;
end;

procedure TfrBrookTable2HTMLForm.liTablesSelectionChange(Sender: TObject;
  User: boolean);
begin
  conn.GetFieldNames(liTables.GetSelectedText, liFields.Items);
  liFields.CheckAll(cbChecked);
  UpdateView;
end;

class procedure TfrBrookTable2HTMLForm.Execute;
begin
  with Self.Create(nil) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TfrBrookTable2HTMLForm.UpdateView;
var
  av: Boolean;
  i: integer = 0;
  field: TFieldDef;
  idx, count: Integer;
begin
  btSave.Enabled := False;
  count := liFields.Count;
  idx := liFields.ItemIndex;
  av := idx > -1;
  btUp.Enabled := (count > 0) and (idx > 0);
  btDown.Enabled := (count > 0) and av and (idx < (count - 1));
  btDelete.Enabled := av;
  lbInfo.Caption := SP;
  if av then
  begin
    OpenTable;
    idx := t.FieldDefs.IndexOf(liFields.GetSelectedText);
    if idx <> -1 then
    begin
      field := t.FieldDefs[idx];
      lbInfo.Caption := Format('Type: %s, MaxLength: %d',
        [FieldTypeNames[field.DataType], field.Size]);
    end;
  end;
  for i := 0 to Pred(count) do
    if liFields.Checked[i] then
    begin
      btSave.Enabled := True;
      Exit;
    end;
end;

procedure TfrBrookTable2HTMLForm.Setup;
var
  dbs: TBrookDataBases;
begin
  dbs := TBrookDataBases.Service;
  if FileExists(edConnection.Text) then
    dbs.Configurator.Configuration := edConnection.Text
  else
    dbs.Configurator.Configuration := 'library=sqldb;' + edConnection.Text;
  edFilterTables.Clear;
  liTables.Clear;
  liFields.Clear;
  if edConnection.Text = ES then
    Exit;
  if not Assigned(d) then
  begin
    d := TBrookDataBase.Create;
    d.Disconnect;
    conn := d.Connection as TSQLConnection;
  end;
  if not Assigned(t) then
    t := TBrookTable.Create(d);
  try
    dbs.Configurator.Configure;
    conn.GetTableNames(liTables.Items);
    edFilterTables.
{$IF FPC_FULLVERSION >= 20701}
      Items
{$ELSE}
      Data
{$ENDIF}
      .Assign(liTables.Items);
  except
    edConnection.SelectAll;
    raise;
  end;
end;

procedure TfrBrookTable2HTMLForm.OpenTable;
begin
  t.Name := liTables.GetSelectedText;
  t.Reset.Select(liFields.Items.CommaText).Open;
end;

end.


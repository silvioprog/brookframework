(*
  Brook New Project unit.

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

unit frmBrookNewProject;

{$i brook.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, ExtCtrls, Buttons, StdCtrls,
  ComCtrls, LCLType, EditBtn, XMLPropStorage;

type
  TfrBrookNewProject = class(TForm)
    btAddAct: TBitBtn;
    btEditAct: TBitBtn;
    btDeleteAct: TBitBtn;
    btPatternsHelp: TBitBtn;
    btOK: TBitBtn;
    btPrior: TBitBtn;
    btNext: TBitBtn;
    btCancel: TBitBtn;
    edAppName: TEdit;
    edPubHTMLDir: TDirectoryEdit;
    lbAppName: TLabel;
    lbInfo2: TLabel;
    lbInfo1: TLabel;
    lbPubHTMLDir: TLabel;
    lbActions: TLabel;
    lbInfo3: TLabel;
    lvActions: TListView;
    pnActsBottom: TPanel;
    pcWizard: TPageControl;
    pnBottom: TPanel;
    rgCharset: TRadioGroup;
    rgAppType: TRadioGroup;
    tsFinish: TTabSheet;
    tsActions: TTabSheet;
    tsApp: TTabSheet;
    xml: TXMLPropStorage;
    procedure btAddActClick(Sender: TObject);
    procedure btDeleteActClick(Sender: TObject);
    procedure btEditActClick(Sender: TObject);
    procedure btNextClick(Sender: TObject);
    procedure btPatternsHelpClick(Sender: TObject);
    procedure btPriorClick(Sender: TObject);
    procedure edAppNameContextPopup(Sender: TObject;{%H-}MousePos: TPoint;
      var Handled: Boolean);
    procedure edAppNameKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: Word;{%H-}Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure lvActionsDblClick(Sender: TObject);
    procedure lvActionsSelectItem(Sender: TObject;{%H-}Item: TListItem;
      Selected: Boolean);
    procedure pcWizardChange(Sender: TObject);
    procedure pcWizardChanging(Sender: TObject; var AllowChange: Boolean);
    procedure tsFinishShow(Sender: TObject);
  private
    class var FInstante: TfrBrookNewProject;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ValidateData(const AName, APattern: string;
      const ADefault: Boolean);
    class property Instante: TfrBrookNewProject read FInstante;
  end;

procedure OnlyAlphaNumeric(var Key: Char);

implementation

{$R *.lfm}

uses
  frmBrookActEdit, BrookProjectIntf;

procedure OnlyAlphaNumeric(var Key: Char);
begin
  if not (Key in ['a'..'z', 'A'..'Z', '0'..'9', #8, #13, #27]) then
    Key := #0;
end;

constructor TfrBrookNewProject.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  xml.FileName := BrookGetExpertsConfigFileName;
  FInstante := Self;
end;

procedure TfrBrookNewProject.btNextClick(Sender: TObject);
begin
  pcWizard.PageIndex := Succ(pcWizard.PageIndex);
end;

procedure TfrBrookNewProject.btPatternsHelpClick(Sender: TObject);
begin
  //
end;

procedure TfrBrookNewProject.btAddActClick(Sender: TObject);
var
  VItem: TListItem;
  VDefault: Boolean;
  VName, VPattern: string;
begin
  VName := '';
  VPattern := '';
  VDefault := False;
  if TfrBrookActEdit.Execute('Add', VName, VPattern, VDefault) then
  begin
    VItem := lvActions.Items.Add;
    VItem.Caption := VName;
    VItem.SubItems.Add(VPattern);
    VItem.SubItems.Add(BoolToStr(VDefault, True));
  end;
end;

procedure TfrBrookNewProject.btEditActClick(Sender: TObject);
var
  VItem: TListItem;
  VDefault: Boolean;
  VName, VPattern: string;
begin
  VItem := lvActions.Selected;
  if Assigned(VItem) then
  begin
    VName := VItem.Caption;
    VPattern := VItem.SubItems[0];
    VDefault := StrToBool(VItem.SubItems[1]);
    if TfrBrookActEdit.Execute('Edit', VName, VPattern, VDefault) then
    begin
      VItem.Caption := VName;
      VItem.SubItems[0] := VPattern;
      VItem.SubItems[1] := BoolToStr(VDefault, True);
    end;
  end;
end;

procedure TfrBrookNewProject.btDeleteActClick(Sender: TObject);
var
  I: Integer;
begin
  I := lvActions.ItemIndex;
  if MessageDlg('Delete', Format('Delete action "%s"?',
    [lvActions.Items[I].Caption]), mtConfirmation, mbYesNo, 0) = mrYes then
    lvActions.Items.Delete(I);
end;

procedure TfrBrookNewProject.btPriorClick(Sender: TObject);
begin
  pcWizard.PageIndex := pcWizard.PageIndex - 1;
end;

procedure TfrBrookNewProject.edAppNameContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin
  Handled := True;
end;

procedure TfrBrookNewProject.edAppNameKeyPress(Sender: TObject; var Key: char);
begin
  OnlyAlphaNumeric(Key);
end;

procedure TfrBrookNewProject.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_NEXT: btNext.Click;
    VK_PRIOR: btPrior.Click;
  end;
  if pcWizard.ActivePage = tsActions then
    case Key of
      VK_INSERT: btAddActClick(Sender);
      VK_F2: btEditActClick(Sender);
      VK_DELETE: btDeleteActClick(Sender);
      VK_F1: btPatternsHelpClick(Sender);
    end;
end;

procedure TfrBrookNewProject.FormShow(Sender: TObject);
begin
  pcWizardChange(Sender);
end;

procedure TfrBrookNewProject.lvActionsDblClick(Sender: TObject);
begin
  btEditAct.Click;
end;

procedure TfrBrookNewProject.lvActionsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  btEditAct.Enabled := Selected;
  btDeleteAct.Enabled := Selected;
end;

procedure TfrBrookNewProject.pcWizardChange(Sender: TObject);
var
  I, C: Integer;
begin
  C := Pred(pcWizard.PageCount);
  I := pcWizard.PageIndex;
  btNext.Visible := I < C;
  btPrior.Visible := I < C;
  btNext.Enabled := I < C;
  btPrior.Enabled := I > 0;
  btOK.Visible := I = C;
  case I of
    0: ActiveControl := edAppName;
    1: ActiveControl := btAddAct;
    2:
      begin
        lbInfo1.Hide;
        ActiveControl := btOK;
      end;
  end;
end;

procedure TfrBrookNewProject.pcWizardChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  AllowChange := False;
  case pcWizard.PageIndex of
    0:
      begin
        if Trim(edAppName.Text) = '' then
        begin
          edAppName.SetFocus;
          ShowMessage('Please specify a name for the application.');
          Exit;
        end;
        if not DirectoryExists(edPubHTMLDir.Text) then
        begin
          edPubHTMLDir.SetFocus;
          ShowMessage('Please specify a valid directory.');
          Exit;
        end;
      end;
    1:
      if lvActions.Items.Count = 0 then
      begin
        ShowMessage('Please add at least one action.');
        Exit;
      end;
  end;
  AllowChange := True;
end;

procedure TfrBrookNewProject.tsFinishShow(Sender: TObject);
const
  AppType: array[0..1] of string = ('CGI', 'FastCGI');
begin
  lbInfo3.Caption := Format(lbInfo3.Caption, [AppType[rgAppType.ItemIndex]]);
end;

procedure TfrBrookNewProject.ValidateData(const AName, APattern: string;
  const ADefault: Boolean);
var
  I: Integer;
  VItem: TListItem;
begin
  for I := 0 to Pred(lvActions.Items.Count) do
  begin
    VItem := lvActions.Items[I];
    if CompareText(AName, VItem.Caption) = 0 then
    begin
      if VItem = lvActions.Selected then
        Continue;
      ShowMessage(Format('The action "%s" is already added.',
        [VItem.Caption]));
      Abort;
    end;
    if CompareText(APattern, VItem.SubItems[0]) = 0 then
    begin
      if VItem = lvActions.Selected then
        Continue;
      ShowMessage(Format('There is already an action added with the pattern "%s".',
        [VItem.Caption]));
      Abort;
    end;
    if ADefault and StrToBool(VItem.SubItems[1]) then
    begin
      if VItem = lvActions.Selected then
        Continue;
      ShowMessage(Format('There is already a default action added.',
        [VItem.Caption]));
      Abort;
    end;
  end;
end;

end.


unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, SimpleSFTP, ComCtrls;

type
  TTestSFTPForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    HostEdit: TEdit;
    PortEdit: TEdit;
    Label3: TLabel;
    LoginEdit: TEdit;
    Label4: TLabel;
    PasswordEdit: TEdit;
    Label5: TLabel;
    CurrentDirEdit: TEdit;
    ConnectButton: TButton;
    FileListBox: TListBox;
    DeleteButton: TButton;
    GetFileButton: TButton;
    SendFileButton: TButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    ReloadButton: TButton;
    ProgressBar: TProgressBar;
    AbortButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ConnectButtonClick(Sender: TObject);
    procedure FileListBoxDblClick(Sender: TObject);
    procedure SendFileButtonClick(Sender: TObject);
    procedure GetFileButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure ReloadButtonClick(Sender: TObject);
    procedure AbortButtonClick(Sender: TObject);
  private
    { Private declarations }
    FSFTP:TSimpleSFTP;
    FFileList:TSFTPFileList;
    FConnected:Boolean;
    FAbortFlag:Boolean;
    procedure SetCurrentDir(DirName:string);
    function TransferProgress(UserData:Pointer;Current,Total:Int64):Boolean;
  public
    { Public declarations }
  end;

var
  TestSFTPForm: TTestSFTPForm;

implementation

{$R *.DFM}

procedure TTestSFTPForm.FormCreate(Sender: TObject);
begin
  FSFTP:=TSimpleSFTP.Create;
  FFileList:=TSFTPFileList.Create;
  FConnected:=False;
  FAbortFlag:=False;
end;

procedure TTestSFTPForm.FormDestroy(Sender: TObject);
begin
  FFileList.Free;
  FSFTP.Free;
end;

procedure TTestSFTPForm.ConnectButtonClick(Sender: TObject);
begin
  if FConnected then
  begin
    FSFTP.Disconnect;
    FConnected:=False;
    ConnectButton.Caption:='Connect';
  end
  else
  begin
    FFileList.Clear;
    FileListBox.Clear;
    CurrentDirEdit.Text:='';
    FSFTP.Connect(HostEdit.Text,PortEdit.Text,LoginEdit.Text,PasswordEdit.Text);
    FConnected:=True;
    ConnectButton.Caption:='Disconnect';
    SetCurrentDir('.');
  end;
end;

procedure TTestSFTPForm.SetCurrentDir(DirName:string);
var i:Integer;
begin
  CurrentDirEdit.Text:=FSFTP.SetCurrentDir(DirName);
  FFileList.Clear;
  FileListBox.Clear;
  FSFTP.ListDir(CurrentDirEdit.Text,FFileList);
  for i:=0 to FFileList.Count-1 do FileListBox.Items.Add(FFileList[i].LongName);
end;

procedure TTestSFTPForm.ReloadButtonClick(Sender: TObject);
begin
  SetCurrentDir(CurrentDirEdit.Text);
end;

procedure TTestSFTPForm.FileListBoxDblClick(Sender: TObject);
begin
  if FileListBox.Items.Count=0 then Exit;
  with FFileList[FileListBox.ItemIndex]^ do
    if file_type=SSH_FILEXFER_TYPE_DIRECTORY then
      SetCurrentDir(FileName);
end;

procedure TTestSFTPForm.AbortButtonClick(Sender: TObject);
begin
  FAbortFlag:=True;
end;

function TTestSFTPForm.TransferProgress(UserData:Pointer;Current,Total:Int64):Boolean;
begin
  if Total=0 then ProgressBar.Position:=0
    else ProgressBar.Position:=Round(Current/Total*ProgressBar.Max);
  Application.ProcessMessages;
  Result:=not FAbortFlag;
end;

procedure TTestSFTPForm.SendFileButtonClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    FAbortFlag:=False;
    TransferProgress(nil,0,0);
    FSFTP.PutFile(OpenDialog.FileName,CurrentDirEdit.Text,True,True,False,0,TransferProgress,nil);
    ShowMessage('File transfer completed '+OpenDialog.FileName);
    SetCurrentDir(CurrentDirEdit.Text);
  end;
end;

procedure TTestSFTPForm.GetFileButtonClick(Sender: TObject);
begin
  if (FileListBox.Items.Count=0) or (FileListBox.ItemIndex<0) then Exit;
  with FFileList[FileListBox.ItemIndex]^ do
    if file_type=SSH_FILEXFER_TYPE_DIRECTORY then ShowMessage('Not a file')
    else
    begin
      SaveDialog.FileName:=FileName;
      if SaveDialog.Execute then
      begin
        FAbortFlag:=False;
        TransferProgress(nil,0,0);
        FSFTP.GetFile(CurrentDirEdit.Text,FileName,SaveDialog.FileName,True,True,False,0,
          TransferProgress,nil);
        ShowMessage('File transfer completed '+SaveDialog.FileName);
      end;
    end;
end;

procedure TTestSFTPForm.DeleteButtonClick(Sender: TObject);
begin
  if (FileListBox.Items.Count=0) then Exit;
  with FFileList[FileListBox.ItemIndex]^ do
  begin
    if file_type=SSH_FILEXFER_TYPE_DIRECTORY then
    begin
      if MessageDlg('Delete dir ?',mtConfirmation,[mbOK,mbCancel],0)<>mrOK then Exit;
      FSFTP.DeleteDir(FileName);
    end
    else
    begin
      if MessageDlg('Delete file ?',mtConfirmation,[mbOK,mbCancel],0)<>mrOK then Exit;
      FSFTP.DeleteFile(FileName);
    end;
  end;
  SetCurrentDir(CurrentDirEdit.Text);
end;

end.

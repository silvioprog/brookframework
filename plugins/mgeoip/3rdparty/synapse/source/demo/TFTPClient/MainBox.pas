unit MainBox;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, FTPTSend;

type
  TForm1 = class(TForm)
    Log: TMemo;
    BExit: TButton;
    BAbout: TButton;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    TargetSystemEdit: TEdit;
    TargetPortEdit: TEdit;
    TargetFileEdit: TEdit;
    BGetFile: TButton;
    BPutFile: TButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    procedure BAboutClick(Sender: TObject);
    procedure BExitClick(Sender: TObject);
    procedure BPutFileClick(Sender: TObject);
    procedure BGetFileClick(Sender: TObject);
  private
    { Private-Deklarationen }
    TFTPClient:TTFTPSend;
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.BAboutClick(Sender: TObject);
begin
  // Show a little About-Box
  Application.MessageBox('Synapse Demo Application, (c) 2003 by Christian Brosius','About...',MB_OK);
end;

procedure TForm1.BExitClick(Sender: TObject);
begin
  // Close the TFTP-Client
  Close;
end;

procedure TForm1.BPutFileClick(Sender: TObject);
begin
  if OpenDialog.Execute
    then
      begin
        // Create TFTPClient
        TFTPClient := TTFTPSend.Create;
        Log.Lines.Add('TFTPClient created');

        // Set Target-Parameter
        TFTPClient.TargetHost := TargetSystemEdit.Text;
        Log.Lines.Add('TargetSystem is ' + TFTPClient.TargetHost);
        TFTPClient.TargetPort := TargetPortEdit.Text;
        Log.Lines.Add('TargetPort is ' + TFTPClient.TargetPort);

        // Try sending file
        Log.Lines.Add('Try to send ' + OpenDialog.FileName);
        TFTPClient.Data.LoadFromFile(OpenDialog.FileName);
        if TFTPClient.SendFile(ExtractFileName(OpenDialog.FileName))
          then
            begin
              // Filetransfer successful
              Log.Lines.Add('File successfully sent to TFTPServer');
            end
          else
            begin
              // Filetransfer not successful
              Log.Lines.Add('Error while sending File to TFTPServer');
              Log.Lines.Add('Error #' + IntToStr(TFTPClient.ErrorCode) + ' - ' + TFTPClient.ErrorString);
            end;
        // Free TFTPClient
        TFTPClient.Free;
        Log.Lines.Add('TFTPClient destroyed');
      end;
end;

procedure TForm1.BGetFileClick(Sender: TObject);
begin
  // Create TFTPClient
  TFTPClient := TTFTPSend.Create;
  Log.Lines.Add('TFTPClient created');

  // Set Target-Parameter
  TFTPClient.TargetHost := TargetSystemEdit.Text;
  Log.Lines.Add('TargetSystem is ' + TFTPClient.TargetHost);
  TFTPClient.TargetPort := TargetPortEdit.Text;
  Log.Lines.Add('TargetPort is ' + TFTPClient.TargetPort);

  // Try sending file
  Log.Lines.Add('Try to get "' + TargetFileEdit.Text + '"');
  if TFTPClient.RecvFile(TargetFileEdit.Text)
    then
      begin
        // Filetransfer successful
        Log.Lines.Add('File successfully get from TFTPServer');
        SaveDialog.FileName := TargetFileEdit.Text;
        if SaveDialog.Execute
          then TFTPClient.Data.SaveToFile(SaveDialog.FileName);
      end
    else
      begin
        // Filetransfer not successful
        Log.Lines.Add('Error while getting File from TFTPServer');
        Log.Lines.Add(IntToStr(TFTPClient.ErrorCode) + ' - ' + TFTPClient.ErrorString);
      end;
  // Free TFTPClient
  TFTPClient.Free;
  Log.Lines.Add('TFTPClient destroyed');
end;

end.

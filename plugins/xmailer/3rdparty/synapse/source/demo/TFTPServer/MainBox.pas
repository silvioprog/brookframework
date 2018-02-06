unit MainBox;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TFTPDaemonThread;

type
  TMainForm = class(TForm)
    Log: TMemo;
    BExit: TButton;
    BAbout: TButton;
    Label1: TLabel;
    PathEdit: TEdit;
    procedure BExitClick(Sender: TObject);
    procedure BAboutClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
    TFTPD:TTFTPDaemonThread;
  public
    { Public-Deklarationen }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.BAboutClick(Sender: TObject);
begin
  // Show a little About-Box
  Application.MessageBox('Synapse Demo Application, (c) 2003 by Christian Brosius','About...',MB_OK);
end;

procedure TMainForm.BExitClick(Sender: TObject);
begin
  // Close the TFTP-Server
  Close;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  TFTPD := TTFTPDaemonThread.Create('0.0.0.0','69');
end;

end.

{
   TFTP supports five types of packets, all of which have been mentioned
   above:
      opcode     operation

        1        Read request (RRQ)
        2        Write request (WRQ)
        3        Data (DATA)
        4        Acknowledgment (ACK)
        5        Error (ERROR)


   Error Codes
     Value       Meaning

       0         Not defined, see error message (if any).
       1         File not found.
       2         Access violation.
       3         Disk full or allocation exceeded.
       4         Illegal TFTP operation.
       5         Unknown transfer ID.
       6         File already exists.
       7         No such user.


}

unit TFTPDaemonThread;

interface

uses Classes, SysUtils, FTPTSend;

type
  TTFTPDaemonThread = class(TThread)
  private
    { Private declarations }
    TFTPDaemon:TTFTPSend;
    FIPAdress:String;
    FPort:String;
    FLogMessage:String;
    procedure UpdateLog;
  protected
    procedure Execute; override;
  public
    constructor Create(IPAdress,Port:String);
  end;

implementation

uses MainBox;

constructor TTFTPDaemonThread.Create(IPAdress,Port:String);
begin
  FIPAdress := IPAdress;
  FPort     := Port;
  inherited Create(False);
end;

procedure TTFTPDaemonThread.UpdateLOG;
begin
  MainForm.Log.Lines.Add(FLogMessage);
end;

procedure TTFTPDaemonThread.Execute;
var RequestType:Word;
    FileName:String;
begin
  TFTPDaemon := TTFTPSend.Create;
  FLogMessage := 'ServerThread created on Port ' + FPort;
  Synchronize(UpdateLog);
  TFTPDaemon.TargetHost := FIPAdress;
  TFTPDaemon.TargetPort := FPort;
  try
    while not terminated do
      begin
        if TFTPDaemon.WaitForRequest(RequestType,FileName)
          then
            begin
              // Fill the Log-Memo whith Infos about the request
              case RequestType of
                1:FLogMessage := 'Read-Request from '
                                 + TFTPDaemon.RequestIP + ':' + TFTPDaemon.RequestPort;
                2:FLogMessage := 'Write-Request from '
                                 + TFTPDaemon.RequestIP + ':' + TFTPDaemon.RequestPort;
              end;
              Synchronize(UpdateLog);
              FLogMessage := 'File: ' + Filename;
              Synchronize(UpdateLog);

              // Process the Request
              case RequestType of
                1:begin  // Read request (RRQ)
                    if FileExists(MainForm.PathEdit.Text + FileName)
                      then
                        begin
                          TFTPDaemon.Data.LoadFromFile(MainForm.PathEdit.Text + FileName);
                          if TFTPDaemon.ReplySend
                            then
                              begin
                                FLogMessage := '"' + MainForm.PathEdit.Text + FileName + '" successfully sent.';
                                Synchronize(UpdateLog);
                              end;
                        end
                      else TFTPDaemon.ReplyError(1,'File not Found');
                  end;
                2:begin  // Write request (WRQ)
                    if not FileExists(MainForm.PathEdit.Text + FileName)
                      then
                        begin
                          if TFTPDaemon.ReplyRecv
                            then
                              begin
                                TFTPDaemon.Data.SaveToFile(MainForm.PathEdit.Text + FileName);
                                FLogMessage := 'File sucessfully stored to ' + MainForm.PathEdit.Text + FileName;
                                Synchronize(UpdateLog);
                              end;
                        end
                      else TFTPDaemon.ReplyError(6,'File already exists');
                  end;
              end;
            end;
      end;
  finally
    TFTPDaemon.Free;
  end;
end;

end.

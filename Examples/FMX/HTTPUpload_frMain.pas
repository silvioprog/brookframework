(*    _____   _____    _____   _____   _   __
 *   |  _  \ |  _  \  /  _  \ /  _  \ | | / /
 *   | |_) | | |_) |  | | | | | | | | | |/ /
 *   |  _ <  |  _ <   | | | | | | | | |   (
 *   | |_) | | | \ \  | |_| | | |_| | | |\ \
 *   |_____/ |_|  \_\ \_____/ \_____/ |_| \_\
 *
 *   –– a small library which helps you write quickly REST APIs.
 *
 * Copyright (c) 2012-2018 Silvio Clecio <silvioprog@gmail.com>
 *
 * This file is part of Brook library.
 *
 * Brook library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Brook library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Brook library.  If not, see <http://www.gnu.org/licenses/>.
 *)

unit HTTPUpload_frMain;

interface

uses
  System.SysUtils,
{$IFDEF ANDROID}
  System.IOUtils,
{$ENDIF}
  System.UITypes,
  System.Classes,
  System.Actions,
  FMX.Types,
  FMX.ActnList,
  FMX.Graphics,
  FMX.Controls,
  FMX.StdCtrls,
  FMX.Edit,
  FMX.EditBox,
  FMX.NumberBox,
  FMX.DialogService,
  FMX.Forms,
  FMX.Controls.Presentation,
  BrookUtils,
  BrookHandledClasses,
  BrookHTTPUploads,
  BrookHTTPRequest,
  BrookHTTPResponse,
  BrookHTTPServer,
  Utility;

type
  TfrMain = class(TForm)
    lbPort: TLabel;
    edPort: TNumberBox;
    btStart: TButton;
    btStop: TButton;
    lbLink: TLabel;
    alMain: TActionList;
    acStart: TAction;
    acStop: TAction;
    BrookHTTPServer1: TBrookHTTPServer;
    pnTop: TPanel;
    procedure acStartExecute(Sender: TObject);
    procedure acStopExecute(Sender: TObject);
    procedure edPortChange(Sender: TObject);
    procedure lbLinkMouseEnter(Sender: TObject);
    procedure lbLinkMouseLeave(Sender: TObject);
    procedure lbLinkClick(Sender: TObject);
    procedure BrookHTTPServer1Request(ASender: TObject;
      ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
    procedure BrookHTTPServer1RequestError(ASender: TObject;
      ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse;
      AException: Exception);
    procedure BrookHTTPServer1Error(ASender: TObject; AException: Exception);
    procedure alMainUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
  public
    procedure UpdateLink;
  end;

const
  PAGE_FORM = Concat(
    '<html>',
    '<body>',
    '<form action="" method="post" enctype="multipart/form-data">',
    '<fieldset>',
    '<legend>Choose the files:</legend>',
    'File 1: <input type="file" name="file1"/><br>',
    'File 2: <input type="file" name="file2"/><br>',
    '<input type="submit"/>',
    '</fieldset>',
    '</form>',
    '</body>',
    '</html>'
  );
  PAGE_DONE = Concat(
    '<html>',
    '<head>',
    '<title>Uploads</title>',
    '</head>',
    '<body>',
    '<strong>Uploaded files:</strong><br>',
    '%s',
    '</body>',
    '</html>'
  );
  CONTENT_TYPE = 'text/html; charset=utf-8';

var
  frMain: TfrMain;

implementation

{$R *.fmx}

procedure TfrMain.FormCreate(Sender: TObject);
begin
  if BrookHTTPServer1.UploadsDir.IsEmpty then
    BrookHTTPServer1.UploadsDir :=
{$IFDEF ANDROID}TPath.GetTempPath{$ELSE}BrookTmpDir{$ENDIF};
end;

procedure TfrMain.UpdateLink;
begin
  lbLink.Text := Concat('http://localhost:', edPort.Text);
end;

procedure TfrMain.acStartExecute(Sender: TObject);
begin
  BrookHTTPServer1.Port := edPort.Text.ToInteger;
  BrookHTTPServer1.Open;
  if edPort.Value = 0 then
    edPort.Value := BrookHTTPServer1.Port;
  UpdateLink;
end;

procedure TfrMain.acStopExecute(Sender: TObject);
begin
  BrookHTTPServer1.Close;
end;

procedure TfrMain.edPortChange(Sender: TObject);
begin
  UpdateLink;
end;

procedure TfrMain.lbLinkMouseEnter(Sender: TObject);
begin
  lbLink.Font.Style := lbLink.Font.Style + [TFontStyle.fsUnderline];
end;

procedure TfrMain.lbLinkMouseLeave(Sender: TObject);
begin
  lbLink.Font.Style := lbLink.Font.Style - [TFontStyle.fsUnderline];
end;

procedure TfrMain.lbLinkClick(Sender: TObject);
begin
  Utility.OpenURL(lbLink.Text);
end;

procedure TfrMain.alMainUpdate(Action: TBasicAction; var Handled: Boolean);
begin
  acStart.Enabled := not BrookHTTPServer1.Active;
  acStop.Enabled := not acStart.Enabled;
  edPort.Enabled := acStart.Enabled;
  lbLink.Enabled := not acStart.Enabled;
end;

procedure TfrMain.BrookHTTPServer1Request(ASender: TObject;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
var
  VUpload: TBrookHTTPUpload;
  VFile, VList, VError: string;
begin
  if ARequest.Uploading then
  begin
    VList := '<ol>';
    for VUpload in ARequest.Uploads do
      if VUpload.Save(False, VError) then
        VList := Concat(VList, '<li><a href="?file=', VUpload.Name, '">',
          VUpload.Name, '</a></li>')
      else
        VList := Concat(VList, '<li><font color="red">', VUpload.Name,
          ' - failed - ', VError, '</font></li>');
    VList := Concat(VList, '</ol>');
    AResponse.Send(PAGE_DONE, [VList], CONTENT_TYPE, 200);
  end
  else
  begin
    if ARequest.Params.TryValue('file', VFile) then
      AResponse.SendFile(Concat(BrookHTTPServer1.UploadsDir, PathDelim, VFile))
    else
      AResponse.Send(PAGE_FORM, CONTENT_TYPE, 200);
  end;
end;

procedure TfrMain.BrookHTTPServer1RequestError(ASender: TObject;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse;
  AException: Exception);
begin
  AResponse.Send(
    '<html><head><title>Error</title></head><body><font color="red">%s</font></body></html>',
    [AException.Message], 'text/html; charset=utf-8', 500);
end;

procedure TfrMain.BrookHTTPServer1Error(ASender: TObject;
  AException: Exception);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      TDialogService.MessageDialog(AException.Message, TMsgDlgType.mtError,
        [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0, nil);
    end);
end;

end.

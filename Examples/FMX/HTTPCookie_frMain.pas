(*   _                     _
 *  | |__  _ __ ___   ___ | | __
 *  | '_ \| '__/ _ \ / _ \| |/ /
 *  | |_) | | | (_) | (_) |   <
 *  |_.__/|_|  \___/ \___/|_|\_\
 *
 *  –– an ideal Pascal microframework to develop cross-platform HTTP servers.
 *
 * Copyright (c) 2012-2018 Silvio Clecio <silvioprog@gmail.com>
 *
 * This file is part of Brook library.
 *
 * Brook framework is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Brook framework is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Brook framework.  If not, see <http://www.gnu.org/licenses/>.
 *)

unit HTTPCookie_frMain;

interface

uses
  System.SysUtils,
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
  BrookHandledClasses,
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
  public
    procedure UpdateLink;
  end;

const
  EMPTY_FAVICON = '<link rel="icon" href="data:,">';
  CONTENT_TYPE = 'text/html; charset=utf-8';
  INITIAL_PAGE = Concat('<html><head>', EMPTY_FAVICON, '<title>Cookies</title></head><body>Use F5 to refresh this page ...</body></html>');
  COUNT_PAGE = Concat('<html><head>', EMPTY_FAVICON, '<title>Cookies</title></head><body>Refresh number: %d</body></html>');
  COOKIE_NAME = 'refresh_count';

var
  frMain: TfrMain;

implementation

{$R *.fmx}

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
  OpenURL(lbLink.Text);
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
  VCount: Integer;
begin
  if ARequest.Cookies.IsEmpty then
  begin
    AResponse.Send(INITIAL_PAGE, CONTENT_TYPE, 200);
    AResponse.SetCookie(COOKIE_NAME, '1');
  end
  else
  begin
    VCount := ARequest.Cookies.Get(COOKIE_NAME).ToInteger;
    AResponse.Send(COUNT_PAGE, [VCount], CONTENT_TYPE, 200);
    AResponse.SetCookie(COOKIE_NAME, Succ(VCount).ToString);
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

(*   _                     _
 *  | |__  _ __ ___   ___ | | __
 *  | '_ \| '__/ _ \ / _ \| |/ /
 *  | |_) | | | (_) | (_) |   <
 *  |_.__/|_|  \___/ \___/|_|\_\
 *
 *  –– microframework which helps to develop web Pascal applications.
 *
 * Copyright (c) 2012-2018 Silvio Clecio <silvioprog@gmail.com>
 *
 * This file is part of Brook framework.
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

unit HTTPRouter_frMain;

{$MODE DELPHI}
{$PUSH}{$WARN 5024 OFF}

interface

uses
  SysUtils,
  Classes,
  StdCtrls,
  ActnList,
  Graphics,
  Spin,
  Dialogs,
  Forms,
  LCLIntf,
{$IFDEF VER3_0_0}
  FPC300Fixes,
{$ENDIF}
  BrookLibraryLoader,
  BrookHTTPRequest,
  BrookHTTPResponse,
  BrookHTTPRouter,
  BrookHTTPServer;

type
  TfrMain = class(TForm)
    acStart: TAction;
    acStop: TAction;
    alMain: TActionList;
    BrookHTTPRouter1: TBrookHTTPRouter;
    BrookHTTPServer1: TBrookHTTPServer;
    BrookLibraryLoader1: TBrookLibraryLoader;
    btStart: TButton;
    btStop: TButton;
    edPort: TSpinEdit;
    lbLink: TLabel;
    lbPort: TLabel;
    procedure acStartExecute(Sender: TObject);
    procedure acStopExecute(Sender: TObject);
    procedure BrookHTTPRouter1NotFound(ASender: TObject; const ARoute: string;
      ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
    procedure BrookHTTPRouter1Routes0Request(ASender: TObject;
      ARoute: TBrookHTTPRoute; ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse);
    procedure BrookHTTPRouter1Routes1Request(ASender: TObject;
      ARoute: TBrookHTTPRoute; ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse);
    procedure BrookHTTPRouter1Routes2Request(ASender: TObject;
      ARoute: TBrookHTTPRoute; ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse);
    procedure BrookHTTPServer1Error(ASender: TObject; AException: Exception);
    procedure BrookHTTPServer1Request(ASender: TObject;
      ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
    procedure BrookHTTPServer1RequestError(ASender: TObject;
      ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse;
      AException: Exception);
    procedure BrookHTTPServer1Start(Sender: TObject);
    procedure BrookHTTPServer1Stop(Sender: TObject);
    procedure edPortChange(Sender: TObject);
    procedure lbLinkClick(Sender: TObject);
    procedure lbLinkMouseEnter(Sender: TObject);
    procedure lbLinkMouseLeave(Sender: TObject);
  protected
    procedure DoError(AData: PtrInt);
  public
    procedure UpdateControls; inline;
  end;

var
  frMain: TfrMain;

implementation

{$R *.lfm}

procedure TfrMain.DoError(AData: PtrInt);
var
  S: PString absolute AData;
begin
  try
    MessageDlg(S^, mtError, [mbOK], 0);
  finally
    DisposeStr(S);
  end;
end;

procedure TfrMain.UpdateControls;
begin
  if BrookHTTPServer1.Active then
    edPort.Value := BrookHTTPServer1.Port
  else
    BrookHTTPServer1.Port := edPort.Value;
  lbLink.Caption := Concat('http://localhost:', edPort.Value.ToString);
  acStart.Enabled := not BrookHTTPServer1.Active;
  acStop.Enabled := not acStart.Enabled;
  edPort.Enabled := acStart.Enabled;
  lbLink.Enabled := not acStart.Enabled;
end;

procedure TfrMain.acStartExecute(Sender: TObject);
begin
  BrookLibraryLoader1.Open;
  BrookHTTPRouter1.Open;
  BrookHTTPServer1.Open;
end;

procedure TfrMain.acStopExecute(Sender: TObject);
begin
  BrookHTTPServer1.Close;
end;

procedure TfrMain.lbLinkMouseEnter(Sender: TObject);
begin
  lbLink.Font.Style := lbLink.Font.Style + [fsUnderline];
end;

procedure TfrMain.lbLinkMouseLeave(Sender: TObject);
begin
  lbLink.Font.Style := lbLink.Font.Style - [fsUnderline];
end;

procedure TfrMain.lbLinkClick(Sender: TObject);
begin
  OpenURL(lbLink.Caption);
end;

procedure TfrMain.BrookHTTPServer1Request(ASender: TObject;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  BrookHTTPRouter1.Route(ASender, ARequest, AResponse);
end;

procedure TfrMain.BrookHTTPServer1RequestError(ASender: TObject;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse;
  AException: Exception);
begin
  AResponse.Send(
    '<html><head><title>Error</title></head><body><font color="red">%s</font></body></html>',
    [AException.Message], 'text/html; charset=utf-8', 500);
end;

procedure TfrMain.BrookHTTPRouter1NotFound(ASender: TObject;
  const ARoute: string; ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse);
begin
  AResponse.Send(
    '<html><head><title>Not found</title></head><body>Page not found: %s</body></html>',
    [ARequest.Path], 'text/html; charset=utf-8', 404);
end;

procedure TfrMain.BrookHTTPRouter1Routes0Request(ASender: TObject;
  ARoute: TBrookHTTPRoute; ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse);
begin
  AResponse.Send(
    '<html><head><title>Home page</title></head><body>Home page</body></html>',
    'text/html; charset=utf-8', 200);
end;

procedure TfrMain.BrookHTTPRouter1Routes1Request(ASender: TObject;
  ARoute: TBrookHTTPRoute; ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse);
begin
  AResponse.Send(
    '<html><head><title>Downloads</title></head><body>Downloaded file: %s</body></html>',
    [ARoute.Variables['file']], 'text/html; charset=utf-8', 200);
end;

procedure TfrMain.BrookHTTPRouter1Routes2Request(ASender: TObject;
  ARoute: TBrookHTTPRoute; ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse);
begin
  AResponse.Send(
    '<html><head><title>Page</title></head><body>Page number: %d</body></html>',
    [ARoute.Segments[0].ToInteger], 'text/html; charset=utf-8', 200);
end;

procedure TfrMain.BrookHTTPServer1Start(Sender: TObject);
begin
  UpdateControls;
end;

procedure TfrMain.BrookHTTPServer1Stop(Sender: TObject);
begin
  UpdateControls;
end;

procedure TfrMain.edPortChange(Sender: TObject);
begin
  UpdateControls;
end;

{$PUSH}{$WARN 4055 OFF}
procedure TfrMain.BrookHTTPServer1Error(ASender: TObject;
  AException: Exception);
begin
  Application.QueueAsyncCall(DoError, PtrInt(NewStr(AException.Message)));
end;
{$POP}

{$POP}

end.

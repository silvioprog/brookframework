(*
  Brook framework, FCL CGI Broker

  Copyright (C) 2014 Silvio Clecio

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit BrookFCLCGIBroker;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF BROOK_LOG}
  EventLog,
{$ENDIF}
  BrookClasses, BrookApplication, BrookMessages, BrookConsts, BrookHttpConsts,
  BrookRouter, BrookUtils, BrookHttpDefsBroker, HttpDefs, CustWeb, CustCGI,
  Classes, SysUtils;

type
  TBrookCGIApplication = class;

  { TBrookApplication }

  TBrookApplication = class(TBrookInterfacedObject, IBrookApplication)
  private
    FApp: TBrookCGIApplication;
    function GetTerminated: Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Instance: TObject;
    procedure Run;
    procedure Terminate;
    property Terminated: Boolean read GetTerminated;
  end;

  { TBrookCGIApplication }

  TBrookCGIApplication = class(TCustomCGIApplication)
  protected
    function InitializeWebHandler: TWebHandler; override;
{$IFDEF BROOK_LOG}
    procedure DoRun; override;
  public
    constructor Create(AOwner: TComponent); override;
{$ENDIF}
  end;

  { TBrookCGIRequest }

  TBrookCGIRequest = class(TCGIRequest)
  protected
    procedure DeleteTempUploadedFiles; override;
    function GetTempUploadFileName(
      const {%H-}AName, AFileName: string; {%H-}ASize: Int64): string; override;
    function RequestUploadDir: string; override;
    procedure InitRequestVars; override;
    procedure HandleUnknownEncoding(
      const AContentType: string; AStream: TStream); override;
  end;

  { TBrookCGIResponse }

  TBrookCGIResponse = class(TCGIResponse)
  protected
    procedure CollectHeaders(AHeaders: TStrings); override;
  end;

  { TBrookCGIHandler }

  TBrookCGIHandler = class(TCGIHandler)
  protected
    function CreateRequest: TCGIRequest; override;
    function CreateResponse(AOutput: TStream): TCGIResponse; override;
  public
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
    procedure ShowRequestException(R: TResponse; E: Exception); override;
  end;

implementation

{ TBrookApplication }

function TBrookApplication.GetTerminated: Boolean;
begin
  Result := FApp.Terminated;
end;

constructor TBrookApplication.Create;
begin
  FApp := TBrookCGIApplication.Create(nil);
  FApp.Initialize;
end;

destructor TBrookApplication.Destroy;
begin
  FApp.Free;
  inherited Destroy;
end;

function TBrookApplication.Instance: TObject;
begin
  Result := FApp;
end;

procedure TBrookApplication.Run;
begin
  FApp.Run;
end;

procedure TBrookApplication.Terminate;
begin
  FApp.Terminate;
end;

{ TBrookCGIApplication }

{$IFDEF BROOK_LOG}
constructor TBrookCGIApplication.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  EventLog.Active := False;
  EventLog.RaiseExceptionOnError := False;
  EventLog.LogType := ltFile;
  EventLog.AppendContent := True;
end;

procedure TBrookCGIApplication.DoRun;
begin
  EventLog.FileName := BrookSettings.LogFile;
  inherited DoRun;
end;
{$ENDIF}

function TBrookCGIApplication.InitializeWebHandler: TWebHandler;
begin
  Result := TBrookCGIHandler.Create(Self);
end;

{ TBrookCGIRequest }

procedure TBrookCGIRequest.DeleteTempUploadedFiles;
begin
  if BrookSettings.DeleteUploadedFiles then
    inherited;
end;

function TBrookCGIRequest.GetTempUploadFileName(
  const AName, AFileName: string; ASize: Int64): string;
begin
  if BrookSettings.KeepUploadedNames then
    Result := RequestUploadDir + AFileName
  else
    Result := inherited GetTempUploadFileName(AName, AFileName, ASize);
end;

function TBrookCGIRequest.RequestUploadDir: string;
begin
  Result := BrookSettings.DirectoryForUploads;
  if Result = '' then
    Result := GetTempDir;
  Result := IncludeTrailingPathDelimiter(Result);
end;

procedure TBrookCGIRequest.InitRequestVars;
var
  VMethod: string;
begin
{$IFDEF BROOK_DEBUG}
  try
{$ENDIF}
    VMethod := Method;
    if VMethod = ES then
      raise Exception.Create(SBrookNoRequestMethodError);
    case VMethod of
      BROOK_HTTP_REQUEST_METHOD_DELETE, BROOK_HTTP_REQUEST_METHOD_PUT,
        BROOK_HTTP_REQUEST_METHOD_PATCH:
        begin
          InitPostVars;
          if HandleGetOnPost then
            InitGetVars;
        end;
    else
      inherited;
    end;
{$IFDEF BROOK_DEBUG}
  except
    on E: Exception do
    begin
      WriteLn('Content-Type: text/plain');
      WriteLn;
      WriteLn('Catastrophic error: ', E.Message);
      raise;
    end;
  end;
{$ENDIF}
end;

procedure TBrookCGIRequest.HandleUnknownEncoding(const AContentType: string;
  AStream: TStream);
begin
  if not BrookHandleUnknownEncoding(Self, AContentType, AStream) then
    inherited HandleUnknownEncoding(AContentType, AStream);
end;

{ TBrookCGIResponse }

procedure TBrookCGIResponse.CollectHeaders(AHeaders: TStrings);
begin
  AHeaders.Add(BROOK_HTTP_HEADER_X_POWERED_BY + HS +
    'Brook framework and FCL-Web.');
  inherited CollectHeaders(AHeaders);
end;

{ TBrookCGIHandler }

function TBrookCGIHandler.CreateRequest: TCGIRequest;
begin
  Result := TBrookCGIRequest.CreateCGI(Self);
  if ApplicationURL = ES then
    ApplicationURL := TBrookRouter.RootUrl;
end;

function TBrookCGIHandler.CreateResponse(AOutput: TStream): TCGIResponse;
begin
  Result := TBrookCGIResponse.CreateCGI(Self, AOutput);
end;

procedure TBrookCGIHandler.HandleRequest(ARequest: TRequest; AResponse: TResponse);
{$IFDEF BROOK_LOG}
var
  VLog: string;
  VLogger: TEventLog;
{$ENDIF}
begin
  AResponse.ContentType := BrookFormatContentType;
  try
{$IFDEF BROOK_LOG}
    VLogger := (BrookApp.Instance as TCustomWebApplication).EventLog;
    VLogger.Active := BrookSettings.LogActive;
    if VLogger.Active then
    begin
      if ARequest.PathInfo <> ES then
        VLog := '<REQUEST.PATH_INFO>:' + LineEnding + ARequest.PathInfo +
          LineEnding;
      if ARequest.CookieFields.Count > 0 then
        VLog += '<REQUEST.COOKIES>:' + LineEnding + ARequest.CookieFields.Text;
      if ARequest.ContentFields.Count > 0 then
        VLog += '<REQUEST.FIELDS>:' + LineEnding + ARequest.ContentFields.Text;
      if ARequest.QueryFields.Count > 0 then
        VLog += '<REQUEST.PARAMS>:' + LineEnding + ARequest.QueryFields.Text;
    end;
{$ENDIF}
    TBrookRouter.Service.Route(ARequest, AResponse);
    TBrookCGIRequest(ARequest).DeleteTempUploadedFiles;
{$IFDEF BROOK_LOG}
    if BrookSettings.LogActive and VLogger.Active then
    begin
      if AResponse.Contents.Count > 0 then
        VLog += '<RESPONSE.CONTENT>:' + LineEnding + AResponse.Contents.Text;
      VLogger.Info('[BROOK]:' + LineEnding + VLog);
    end;
{$ENDIF}
  except
    on E: Exception do
{$IFDEF BROOK_LOG}
    begin
      if BrookSettings.LogActive and VLogger.Active then
      begin
        VLog :=
          '<ERROR>:' + LineEnding + E.Message + LineEnding +
          '<STACK>:' + LineEnding + BrookDumpStack(LineEnding) + LineEnding +
          '<STACK_TRACE>:' + LineEnding + BrookDumpStackTrace(LineEnding) +
            LineEnding  + VLog;
        VLogger.Error('[BROOK]:' + LineEnding + VLog);
      end;
{$ENDIF}
      ShowRequestException(AResponse, E);
{$IFDEF BROOK_LOG}
    end;
{$ENDIF}
  end;
end;

procedure TBrookCGIHandler.ShowRequestException(R: TResponse; E: Exception);
begin
  BrookShowRequestException(Self, R, E);
end;

initialization
  BrookRegisterApp(TBrookApplication.Create);

end.

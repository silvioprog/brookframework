(*
  Brook FCL CGI Broker unit.

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

unit BrookFCLCGIBroker;

{$mode objfpc}{$H+}

interface

uses
  BrookClasses, BrookApplication, BrookMessages, BrookConsts, BrookHTTPConsts,
  BrookRouter, BrookUtils, BrookHTTPDefsBroker,HTTPDefs, CustWeb, CustCGI,
  Classes, SysUtils;

type
  TBrookCGIApplication = class;

  { TBrookApplication }

  TBrookApplication = class(TBrookInterfacedObject, IBrookApplication)
  private
    FApp: TBrookCGIApplication;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Instance: TObject;
    procedure Run;
  end;

  { TBrookCGIApplication }

  TBrookCGIApplication = class(TCustomCGIApplication)
  protected
    function InitializeWebHandler: TWebHandler; override;
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

{ TBrookCGIApplication }

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
  VMethod: ShortString;
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
      WriteLn('Catastrophic error:');
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
begin
  AResponse.ContentType := BrookFormatContentType;
  if BrookSettings.Language <> BROOK_DEFAULT_LANGUAGE then
    TBrookMessages.Service.SetLanguage(BrookSettings.Language);
  try
    TBrookRouter.Service.Route(ARequest, AResponse);
    TBrookCGIRequest(ARequest).DeleteTempUploadedFiles;
  except
    on E: Exception do
      ShowRequestException(AResponse, E);
  end;
end;

procedure TBrookCGIHandler.ShowRequestException(R: TResponse; E: Exception);
begin
  BrookShowRequestException(Self, R, E);
end;

initialization
  BrookRegisterApp(TBrookApplication.Create);

end.

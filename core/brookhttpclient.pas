(*
  Brook HTTP Client unit.

  Copyright (C) 2013 Silvio Clecio.

  http://silvioprog.github.io/brookframework

  All contributors:
  Plase see the file CONTRIBUTORS.txt, included in this
  distribution.

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit BrookHTTPClient;

{$i brook.inc}

interface

uses
  BrookClasses, BrookException, BrookMessages, BrookConsts, BrookHTTPConsts,
  Classes, SysUtils, FGL;

type
  { Handles exceptions for @link(TBrookHTTPDef). }
  EBrookHTTPDef = class(EBrook);

  { Handles exceptions for @link(TBrookHTTPDefs). }
  EBrookHTTPDefs = class(EBrook);

  { Handles exceptions for @link(EBrookHTTPClient). }
  EBrookHTTPClient = class(EBrook);

  { Is a metaclass for @link(TBrookHTTPDef) class. }
  TBrookHTTPDefClass = class of TBrookHTTPDef;

  { Is a metaclass for @link(TBrookHTTPDefs) class. }
  TBrookHTTPDefsClass = class of TBrookHTTPDefs;

  { Is a metaclass for @link(TBrookHTTPClient) class. }
  TBrookHTTPClientClass = class of TBrookHTTPClient;

  { Information returned after a request. }
  TBrookHTTPResult = record
    StatusCode: Integer;
    ReasonPhrase, Header, Content: string;
  end;

  { Offers general abstract features for HTTP handling. }
  TBrookHTTPDef = class(TBrookObject)
  protected
    procedure SetContentType(AValue: string); virtual; abstract;
    function GetContentType: string; virtual; abstract;
    function GetClient: TObject; virtual; abstract;
    function GetContents: TStrings; virtual; abstract;
    function GetCookies: TStrings; virtual; abstract;
    function GetDocument: TStream; virtual; abstract;
    function GetHeaders: TStrings; virtual; abstract;
    function GetReasonPhrase: string; virtual; abstract;
    function GetStatusCode: Integer; virtual; abstract;
    function GetMethod: string; virtual; abstract;
    function GetUrl: string; virtual; abstract;
    procedure SetMethod(AValue: string); virtual; abstract;
    procedure SetUrl(AValue: string); virtual; abstract;
  public
    { Creates an instance of a @link(TBrookHTTPDef) class. }
    constructor Create; virtual; abstract;
    { Register the broker class. }
    class procedure Register;
    { Unregister the broker class. }
    class procedure Unregister;
    { Get the broker library name, for example: FCLWeb, Synapse, LNet, Indy etc. }
    class function GetLibrary: string; virtual; abstract;
    { Adds header, replacing an existing one if it exists. }
    procedure AddHeader(const AName, AValue: string); virtual; abstract;
    { Sends request to server. }
    function Request: Boolean; virtual; abstract;
    { Sends request by a GET HTTP request method. }
    class function Get(const AUrl: string;
      AResponse: TStream): Boolean; virtual; abstract;
    { Sends request by a POST HTTP request method. }
    class function Post(const AUrl: string;
      AResponse: TStream): Boolean; virtual; abstract;
    { Sends request by a PUT HTTP request method. }
    class function Put(const AUrl: string;
      AResponse: TStream): Boolean; virtual; abstract;
    { Sends request by a DELETE HTTP request method. }
    class function Delete(const AUrl: string;
      AResponse: TStream): Boolean; virtual; abstract;
    { Sends request by a OPTIONS HTTP request method. }
    class function Options(const AUrl: string;
      AResponse: TStream): Boolean; virtual; abstract;
    { Sends request by a HEAD HTTP request method. }
    class function Head(const AUrl: string;
      AHeaders: TStrings): Boolean; virtual; abstract;
    { Sends request by a POST HTTP request method, passing a form-data as
      parameter. }
    class function PostForm(const AUrl: string;
      AFormData, AResponse: TStream): Boolean; virtual; abstract;
    { Sends request by a POST HTTP request method, passing a form-data as
      parameter. }
    class function PostForm(const AUrl, AFormData: string;
      AResponse: TStream): Boolean; virtual; abstract;
    { Sends request by a POST HTTP request method, passing a file as
      parameter. }
    class function PostFile(const AUrl, AFieldName, AFileName: string;
      AFile, AResponse: TStream): Boolean; virtual; abstract;
    { Sends request by a POST HTTP request method, passing a file as
      parameter. }
    class function PostFile(const AUrl, AFieldName, AFileName: string;
      AResponse: TStream): Boolean; virtual; abstract;
    { Content type of sending data. }
    property ContentType: string read GetContentType write SetContentType;
    { Strings received from the request. }
    property Contents: TStrings read GetContents;
    { Set cookies to be sent and/or received from the server. }
    property Cookies: TStrings read GetCookies;
    { Document received from the request. }
    property Document: TStream read GetDocument;
    { Request headers. }
    property Headers: TStrings read GetHeaders;
    { Result code after successful request. }
    property StatusCode: Integer read GetStatusCode;
    { Result text after successful request. }
    property ReasonPhrase: string read GetReasonPhrase;
    { Method for requests. }
    property Method: string read GetMethod write SetMethod;
    { URL that request is driven to. }
    property Url: string read GetUrl write SetUrl;
    { Is the instance of the HTTP client broker. }
    property Client: TObject read GetClient;
  end;

  { Registers HTTP definitions. }
  TBrookHTTPDefs = class(specialize TFPGList<TBrookHTTPDefClass>)
  private
    class var _List: TBrookHTTPDefs;
  public
    { Registers the service provided by this class. }
    class function Service: TBrookHTTPDefs;
    { Finds a database item by its library name. }
    function Find(const ALibrary: string): TBrookHTTPDefClass;
    { Returns a database item by its library name. }
    function ItemByLibrary(const ALibrary: string): TBrookHTTPDefClass;
  end;

  { Client to perform HTTP requests. }
  TBrookHTTPClient = class(TBrookObject)
  private
    class var _Library: string;
  public
    { Creates an instance of a @link(TBrookHTTPClient) class. }
    constructor Create(const ALibrary: string);
    { Specifies the library to be used by this class. }
    class procedure SetLibrary(const ALibrary: string);
    { Prepares an instance of @code(TBrookHTTPDef). }
    class procedure Prepare(out AHttp: TBrookHTTPDef);
    { Sends request by a GET HTTP request method. }
    class function Get(const AUrl: string; AResponse: TStream): Boolean;
    { Sends request by a POST HTTP request method. }
    class function Post(const AUrl: string; AResponse: TStream): Boolean;
    { Sends request by a PUT HTTP request method. }
    class function Put(const AUrl: string; AResponse: TStream): Boolean;
    { Sends request by a DELETE HTTP request method. }
    class function Delete(const AUrl: string; AResponse: TStream): Boolean;
    { Sends request by a OPTIONS HTTP request method. }
    class function Options(const AUrl: string; AResponse: TStream): Boolean;
    { Sends request by a HEAD HTTP request method. }
    class function Head(const AUrl: string; AHeaders: TStrings): Boolean;
    { Sends request by a POST HTTP request method, passing a form-data as
      parameter. }
    class function PostForm(const AUrl: string; AFormData,
      AResponse: TStream): Boolean;
    { Sends request by a POST HTTP request method, passing a form-data as
      parameter. }
    class function PostForm(const AUrl, AFormData: string;
      AResponse: TStream): Boolean;
    { Sends request by a POST HTTP request method, passing a file as
      parameter. }
    class function PostFile(const AUrl, AFieldName, AFileName: string;
      AFile, AResponse: TStream): Boolean;
    { Sends request by a POST HTTP request method, passing a file as
      parameter. }
    class function PostFile(const AUrl, AFieldName, AFileName: string;
      AResponse: TStream): Boolean;
    { Performs the request, passing a @code(TBrookHTTPDef) as parameter. }
    function Request(AHttp: TBrookHTTPDef): TBrookHTTPResult;
    { Performs the request, passing the method and URL as parameter. }
    function Request(const AMethod, AUrl: string): TBrookHTTPResult;
    { Performs the request, passing URL as parameter. }
    function Request(const AUrl: string): TBrookHTTPResult;
  end;

implementation

{ TBrookHTTPDef }

class procedure TBrookHTTPDef.Register;
begin
  TBrookHTTPDefs.Service.Add(Self);
  BROOK_HTTP_CLIENT_DEFAULT_LIBRARY := GetLibrary;
end;

class procedure TBrookHTTPDef.Unregister;
begin
  TBrookHTTPDefs.Service.Remove(Self);
end;

{ TBrookHTTPDefs }

class function TBrookHTTPDefs.Service: TBrookHTTPDefs;
begin
  if not Assigned(TBrookHTTPDefs._List) then
    TBrookHTTPDefs._List := TBrookHTTPDefs.Create;
  Result := TBrookHTTPDefs._List;
end;

function TBrookHTTPDefs.Find(const ALibrary: string): TBrookHTTPDefClass;
begin
  for Result in Self do
    if SameText(Result.GetLibrary, ALibrary) then
      Exit;
  Result := nil;
end;

function TBrookHTTPDefs.ItemByLibrary(
  const ALibrary: string): TBrookHTTPDefClass;
begin
  if ALibrary = ES then
    raise EBrookHTTPDefs.Create(Self, SBrookEmptyLibraryNameError);
  Result := Find(ALibrary);
  if not Assigned(Result) then
    raise EBrookHTTPDefs.CreateFmt(Self, SBrookItemNotFoundError, [ALibrary]);
end;

{ TBrookHTTPClient }

constructor TBrookHTTPClient.Create(const ALibrary: string);
begin
  _Library := ALibrary;
end;

class procedure TBrookHTTPClient.SetLibrary(const ALibrary: string);
begin
  _Library := ALibrary;
end;

class procedure TBrookHTTPClient.Prepare(out AHttp: TBrookHTTPDef);
begin
  AHttp := TBrookHTTPDefs.Service.ItemByLibrary(_Library).Create;
end;

class function TBrookHTTPClient.Get(const AUrl: string;
  AResponse: TStream): Boolean;
var
  VHttp: TBrookHTTPDef = nil;
begin
  Prepare(VHttp);
  try
    Result := VHttp.Get(AUrl, AResponse);
  finally
    VHttp.Free;
  end;
end;

class function TBrookHTTPClient.Post(const AUrl: string;
  AResponse: TStream): Boolean;
var
  VHttp: TBrookHTTPDef = nil;
begin
  Prepare(VHttp);
  try
    Result := VHttp.Post(AUrl, AResponse);
  finally
    VHttp.Free;
  end;
end;

class function TBrookHTTPClient.Put(const AUrl: string;
  AResponse: TStream): Boolean;
var
  VHttp: TBrookHTTPDef = nil;
begin
  Prepare(VHttp);
  try
    Result := VHttp.Put(AUrl, AResponse);
  finally
    VHttp.Free;
  end;
end;

class function TBrookHTTPClient.Delete(const AUrl: string;
  AResponse: TStream): Boolean;
var
  VHttp: TBrookHTTPDef = nil;
begin
  Prepare(VHttp);
  try
    Result := VHttp.Delete(AUrl, AResponse);
  finally
    VHttp.Free;
  end;
end;

class function TBrookHTTPClient.Options(const AUrl: string;
  AResponse: TStream): Boolean;
var
  VHttp: TBrookHTTPDef = nil;
begin
  Prepare(VHttp);
  try
    Result := VHttp.Options(AUrl, AResponse);
  finally
    VHttp.Free;
  end;
end;

class function TBrookHTTPClient.Head(const AUrl: string;
  AHeaders: TStrings): Boolean;
var
  VHttp: TBrookHTTPDef = nil;
begin
  Prepare(VHttp);
  try
    Result := VHttp.Head(AUrl, AHeaders);
  finally
    VHttp.Free;
  end;
end;

class function TBrookHTTPClient.PostForm(const AUrl: string; AFormData,
  AResponse: TStream): Boolean;
var
  VHttp: TBrookHTTPDef = nil;
begin
  Prepare(VHttp);
  try
    Result := VHttp.PostForm(AUrl, AFormData, AResponse);
  finally
    VHttp.Free;
  end;
end;

class function TBrookHTTPClient.PostForm(const AUrl, AFormData: string;
  AResponse: TStream): Boolean;
var
  VHttp: TBrookHTTPDef = nil;
begin
  Prepare(VHttp);
  try
    Result := VHttp.PostForm(AUrl, AFormData, AResponse);
  finally
    VHttp.Free;
  end;
end;

class function TBrookHTTPClient.PostFile(const AUrl, AFieldName,
  AFileName: string; AFile, AResponse: TStream): Boolean;
var
  VHttp: TBrookHTTPDef = nil;
begin
  Prepare(VHttp);
  try
    Result := VHttp.PostFile(AUrl, AFieldName, AFileName, AFile, AResponse);
  finally
    VHttp.Free;
  end;
end;

class function TBrookHTTPClient.PostFile(const AUrl, AFieldName,
  AFileName: string; AResponse: TStream): Boolean;
var
  VHttp: TBrookHTTPDef = nil;
begin
  Prepare(VHttp);
  try
    Result := VHttp.PostFile(AUrl, AFieldName, AFileName, AResponse);
  finally
    VHttp.Free;
  end;
end;

function TBrookHTTPClient.Request(AHttp: TBrookHTTPDef): TBrookHTTPResult;
begin
  AHttp.Request;
  Result.Content := AHttp.Contents.Text;
  Result.Header := AHttp.Headers.Text;
  Result.StatusCode := AHttp.StatusCode;
  Result.ReasonPhrase := AHttp.ReasonPhrase;
end;

function TBrookHTTPClient.Request(const AMethod, AUrl: string): TBrookHTTPResult;
var
  VHttp: TBrookHTTPDef = nil;
begin
  Prepare(VHttp);
  try
    VHttp.Method := AMethod;
    VHttp.Url := AUrl;
    Result := Request(VHttp);
  finally
    VHttp.Free;
  end;
end;

function TBrookHTTPClient.Request(const AUrl: string): TBrookHTTPResult;
begin
  Result := Request(BROOK_HTTP_REQUEST_METHOD_GET, AUrl);
end;

finalization
  FreeAndNil(TBrookHTTPDefs._List);

end.

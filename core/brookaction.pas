(*
  Brook framework, Action Classes

  Copyright (C) 2014 Silvio Clecio

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit BrookAction;

{$i brook.inc}

interface

uses
  BrookClasses, BrookHttpDefs, BrookException, BrookMessages, BrookUtils,
  BrookHTTPUtils, BrookHTTPConsts, Classes, SysUtils;

type
  { Handles exceptions for @link(TBrookAction). }
  EBrookAction = class(EBrook);

  { Is a metaclass for @link(TBrookAction) class. }
  TBrookActionClass = class of TBrookAction;

  { Provides features to handle HTTP requests and responses. }
  TBrookAction = class(TBrookObject)
  private
    FFields: TStrings;
    FFiles: TBrookUploadedFiles;
    FParams: TStrings;
    FTheRequest: TBrookRequest;
    FTheResponse: TBrookResponse;
    FValues: TStrings;
    function GetMethod: string;
  protected
    property TheRequest: TBrookRequest read FTheRequest;
    property TheResponse: TBrookResponse read FTheResponse;
  public
    { Creates an instance of a @link(TBrookAction) class. }
    constructor Create; overload; virtual;
    { Creates an instance of a @link(TBrookAction) class passing params to
      request/response. }
    constructor Create(ARequest: TBrookRequest;
      AResponse: TBrookResponse); overload; virtual;
    { Frees an instance of @link(TBrookAction) class. }
    destructor Destroy; override;
    { Registers an action.

      @param(APattern Is an expression defining which URLs is used to call
       an action. It is possible to use variables inside URLs:

       @definitionList(
        @itemLabel(@bold(:name) -- Represents a variable that spans single URL
         component between slashes.)
        @item(Examples:

         @code(TMyAction.Register('/foo/:myvar');)

         Value of a variable @code("myvar") can be read from the property
         @link(Values), e.g.:

         @code(Write(Values.Values['myvar']);)

         Any number of variables can be combined:

         @code(TMyAction.Register('/foo/:cat/:id');)
        )
        @itemLabel(@bold(*name) -- Represents a variable that spans one or more
         levels between slashes in the current URL.)
        @item(Examples:

         @code(TMyAction.Register('/home/*path');)

         Any of the following URLs will match:

         http://localhost/cgi-bin/cgi1/home/file @br
         http://localhost/cgi-bin/cgi1/home/dir/file @br
         http://localhost/cgi-bin/cgi1/home/dir/subdir/file etc.

         Variable @code(Values.Values['path']) will receive @code('file'),
         @code('dir/file') or @code('dir/subdir/file') correspondingly.

         You can also add static text after variable part:

         @code(TMyAction.Register('/home/*path/download');)

         http://localhost/cgi-bin/cgi1/home/dir/file/download -- This will match, @br
         http://localhost/cgi-bin/cgi1/home/dir/file/info -- but not this, because ending is different.

         Multi-level variable can be combined with any number of single-level
         variables in any order:

         @code(TMyAction.Register('/home/user/:uid/file/*fpath/version/:vid/info');)

         @bold(@italic(NOTE:)) Only one multi-level variable can be specified per URL.
        )
        @itemLabel(@bold(url/) -- Adds a slash to the end of the URL if does not exist.)
        @item(Example:

         @code(TMyAction.Register('/foo/');)

         An action can be accessed as
         http://localhost/cgi-bin/cgi1/foo or http://localhost/cgi-bin/cgi1/foo/.
         When called as http://localhost/cgi-bin/cgi1/foo, it will be automatically
         redirected to http://localhost/cgi-bin/cgi1/foo/.
         If the pathinfo is different from @code(/foo) a 404 page is returned;
        )
       )
        @bold(@italic(NOTE:)) Two actions can't be registered with the same
        pattern except when they are called by means of different HTTP methods.
      )

      @param(ADefault A action registered as @italic(Default) will be called
        automatically if the URL does not match with @italic(Pattern) of any
        registered actions. It is not allowed to register more than one action
        as default. A typical example of use is:

        @code(TMyAction.Register('*', True);)) }
    class procedure Register(const APattern: string;
      const ADefault: Boolean = False); overload;
    { Registers an action specifying the HTTP request method.

      @param(AMethod Informs the HTTP request method being valid the following
       options: @code(rmAll, rmGet, rmHead, rmOptions, rmPost, rmPut) or
       @code(rmDelete). The only way to register two actions with the same
       pattern is differentiating the value of this parameter.
       If at least one action has this parameter changed, the route mapping is
       enabled in @link(TBrookSettings.Mapped).
       A typical example of use is:

       @longCode(
procedure TMyAction1.Get;
begin
  Write('GET');
end;

procedure TMyAction1.Put;
begin
  Write('PUT');
end;

procedure TMyAction2.Post;
begin
  Write('POST');
end;

initialization
  TMyAction1.Register('/foo1', rmGet);
  TMyAction1.Register('/foo1', rmPut);
  TMyAction2.Register('/foo1', rmPost);)) }
    class procedure Register(const APattern: string;
      const AMethod: TBrookRequestMethod;
      const ADefault: Boolean = False); overload;
    { Returns the path of action. Exemple:

      @code(/cgi-bin/cgi1/myaction). }
    class function GetPath: string;
    { Calls the method @link(TBrookAction.Request). }
    procedure DoRequest(ARequest: TBrookRequest;
      AResponse: TBrookResponse); virtual;
    { Is triggered by a request of any HTTP method. }
    procedure Request(ARequest: TBrookRequest;
      {%H-}AResponse: TBrookResponse); virtual;
    { Get an object with the fields coming from a
        @code(x-www-form-urlencoded) form. }
    procedure GetFields(AObject: TObject);
    { Get an object with the params coming from a @code(QUERY_STRING). }
    procedure GetParams(AObject: TObject);
    { Get an object with the variables coming from an URL. }
    procedure GetValues(AObject: TObject);
    { Creates an URL for action. }
    function UrlFor(AActionClass: TBrookActionClass): string; overload;
    { Creates an URL for an action informing an array of parameters. Exemple:

      @longCode(
      procedure TMyAction.Get;
      begin
        // When calling with http://localhost/cgi-bin/cgi1/foo/myvalue
        // the output will be /cgi-bin/cgi1/foo/myvalue
        Write(UrlFor(TMyAction, ['myvalue']));
      end;

      initialization
        TMyAction.Register('/foo/:myvar');) }
    function UrlFor(AActionClass: TBrookActionClass;
      const AParams: array of string): string; overload;
    { Creates an URL for an action passing an array of parameters however
      informing the class name as string. }
    function UrlFor(AClassName: string;
      const AParams: array of string): string; overload;
    { Creates an URL for an action informing the class name as string. }
    function UrlFor(AClassName: string): string; overload;
    { Is triggered by a GET HTTP request method. }
    procedure Get; virtual;
    { Is triggered by a POST HTTP request method. }
    procedure Post; virtual;
    { Is triggered by a PUT HTTP request method. }
    procedure Put; virtual;
    { Is triggered by a DELETE HTTP request method. }
    procedure Delete; virtual;
    { Is triggered by a HEAD HTTP request method. }
    procedure Head; virtual;
    { Is triggered by an OPTIONS HTTP request method. }
    procedure Options; virtual;
    { Redirects to an URL. }
    procedure Redirect(const AUrl: string); overload;
    { Redirects to an URL informing the (302, 307) status code. }
    procedure Redirect(const AUrl: string; const AStatusCode: Word); overload;
    { Redirects to an URL informing the root URL. }
    procedure Redirect(const AUrl: string; const AUseRootUrl: Boolean); overload;
    { Redirects to an URL informing the (302, 307) status code and the
      ScriptName. }
    procedure Redirect(const AUrl: string; const AUseRootUrl: Boolean;
      const AStatusCode: Word); overload;
    { Raises a message for action exceptions. }
    procedure Error(const AMsg: string); overload;
    { Raises a formated message for action exceptions. }
    procedure Error(const AMsg: string; const AArgs: array of const); overload;
    { Stops the action showing an exception message. }
    procedure Stop(const AMsg: string); overload;
    { Stops the action showing a formatted exception message. }
    procedure Stop(const AMsg: string; const AArgs: array of const); overload;
    { Writes the content of a file. }
    procedure Render(const AFileName: TFileName); overload; virtual;
    { Writes the content of a file passing parameters to the output. }
    procedure Render(const AFileName: TFileName;
      const AArgs: array of const); overload; virtual;
    { Clears all written content with @code(Write(), WriteLn(), Render()) etc. }
    procedure Clear;
    { Checks if a name exists in fields. }
    function Exists(const AName: string): Boolean;
    { Writes a string. }
    procedure Write(const AString: string); overload;
    { Writes a boolean. }
    procedure Write(const ABoolean: Boolean); overload;
    { Writes an integer. }
    procedure Write(const AInteger: Integer); overload;
    { Writes a float. }
    procedure Write(const AFloat: Double); overload;
    { Writes an object. }
    procedure Write(AObject: TObject); overload;
    { Writes a content of stream. }
    procedure Write(AStream: TStream); overload;
    { Writes a formatted string. }
    procedure Write(const AFmt: string; const AArgs: array of const); overload;
    { The list of files coming from a request called by the POST method. }
    property Files: TBrookUploadedFiles read FFiles;
    { The list of variables coming from a request called by the POST method. }
    property Fields: TStrings read FFields;
    { The list of variables coming from a request called by the GET method. }
    property Params: TStrings read FParams;
    { The list of variables received from a parametrized URL. }
    property Values: TStrings read FValues;
    { Returns the HTTP request method. }
    property Method: string read GetMethod;
  end;

  { Provides features to handle HTTP requests and responses mapping URIs to
    object. }
  generic TBrookGAction<T> = class(TBrookAction)
  private
    FEntity: T;
  protected
    procedure FillEntity; virtual;
    function CreateEntity: T; virtual;
    procedure FreeEntity; virtual;
  public
    { Creates an instance of a @link(TBrookGAction) class. }
    constructor Create; overload; override;
    { Frees an instance of @link(TBrookGAction) class. }
    destructor Destroy; override;
    { Is triggered by a request of any HTTP method. }
    procedure Request(ARequest: TBrookRequest;
      {%H-}AResponse: TBrookResponse); override;
    { Maps URI to object. }
    property Entity: T read FEntity write FEntity;
  end;

implementation

uses
  BrookRouter;

{ TBrookAction }

constructor TBrookAction.Create;
begin
  inherited Create;
  FValues := TStringList.Create;
end;

constructor TBrookAction.Create(ARequest: TBrookRequest;
  AResponse: TBrookResponse);
begin
  Create;
  FTheRequest := ARequest;
  FTheResponse := AResponse;
  FFields := FTheRequest.ContentFields;
  FParams := FTheRequest.QueryFields;
  FFiles := FTheRequest.Files;
end;

destructor TBrookAction.Destroy;
begin
  FValues.Free;
  inherited Destroy;
end;

function TBrookAction.GetMethod: string;
begin
  Result := FTheRequest.Method;
end;

procedure TBrookAction.DoRequest(ARequest: TBrookRequest;
  AResponse: TBrookResponse);
begin
  Request(ARequest, AResponse);
end;

procedure TBrookAction.GetFields(AObject: TObject);
begin
  BrookSafeStringsToObject(AObject, FTheRequest.ContentFields);
end;

procedure TBrookAction.GetParams(AObject: TObject);
begin
  BrookSafeStringsToObject(AObject, FTheRequest.QueryFields);
end;

procedure TBrookAction.GetValues(AObject: TObject);
begin
  BrookSafeStringsToObject(AObject, FValues);
end;

class procedure TBrookAction.Register(const APattern: string;
  const ADefault: Boolean);
begin
  Register(APattern, rmAll, ADefault);
end;

class procedure TBrookAction.Register(const APattern: string;
  const AMethod: TBrookRequestMethod; const ADefault: Boolean);
begin
  if Self = TBrookAction then
    raise EBrookAction.Create(Self, SBrookRegiterTBrookActionError);
  if AMethod <> rmAll then
    BrookSettings.Mapped := True;
  TBrookRouter.Service.Routes.Add(Self, LowerCase(APattern), AMethod, ADefault);
end;

class function TBrookAction.GetPath: string;
begin
  Result := BrookIncludeTrailingUrlDelimiter(TBrookRouter.RootUrl) +
    LowerCase(Copy(ClassName, 2, MaxInt));
end;

function TBrookAction.UrlFor(AActionClass: TBrookActionClass;
  const AParams: array of string): string;
begin
  Result := TBrookRouter.Service.UrlFor(AActionClass, AParams);
end;

function TBrookAction.UrlFor(AActionClass: TBrookActionClass): string;
begin
  Result := UrlFor(AActionClass, []);
end;

function TBrookAction.UrlFor(AClassName: string;
  const AParams: array of string): string;
begin
  Result := TBrookRouter.Service.UrlFor(AClassName, AParams);
end;

function TBrookAction.UrlFor(AClassName: string): string;
begin
  Result := UrlFor(AClassName, []);
end;

procedure TBrookAction.Request(ARequest: TBrookRequest;
  AResponse: TBrookResponse);
begin
  case ARequest.Method of
    BROOK_HTTP_REQUEST_METHOD_GET: Get;
    BROOK_HTTP_REQUEST_METHOD_POST: Post;
    BROOK_HTTP_REQUEST_METHOD_PUT: Put;
    BROOK_HTTP_REQUEST_METHOD_DELETE: Delete;
    BROOK_HTTP_REQUEST_METHOD_HEAD: Head;
    BROOK_HTTP_REQUEST_METHOD_OPTIONS: Options;
  end;
end;

procedure TBrookAction.Get;
begin
  TBrookRouter.MethodNotAllowed(FTheResponse);
end;

procedure TBrookAction.Post;
begin
  TBrookRouter.MethodNotAllowed(FTheResponse);
end;

procedure TBrookAction.Put;
begin
  TBrookRouter.MethodNotAllowed(FTheResponse);
end;

procedure TBrookAction.Delete;
begin
  TBrookRouter.MethodNotAllowed(FTheResponse);
end;

procedure TBrookAction.Head;
begin
  TBrookRouter.MethodNotAllowed(FTheResponse);
end;

procedure TBrookAction.Options;
begin
  TBrookRouter.MethodNotAllowed(FTheResponse);
end;

procedure TBrookAction.Redirect(const AUrl: string);
begin
  FTheResponse.SendRedirect(AUrl);
end;

procedure TBrookAction.Redirect(const AUrl: string; const AStatusCode: Word);
begin
  FTheResponse.Code := AStatusCode;
  FTheResponse.CodeText := BrookStatusCodeToReasonPhrase(AStatusCode);
  FTheResponse.SetCustomHeader('Location', AUrl);
end;

procedure TBrookAction.Redirect(const AUrl: string; const AUseRootUrl: Boolean);
begin
  if AUseRootUrl then
    FTheResponse.SendRedirect(TBrookRouter.RootUrl + AUrl)
  else
    FTheResponse.SendRedirect(AUrl);
end;

procedure TBrookAction.Redirect(const AUrl: string;
  const AUseRootUrl: Boolean; const AStatusCode: Word);
begin
  FTheResponse.Code := AStatusCode;
  FTheResponse.CodeText := BrookStatusCodeToReasonPhrase(AStatusCode);
  if AUseRootUrl then
    FTheResponse.SetCustomHeader('Location',
      TBrookRouter.RootUrl(FTheRequest) + AUrl)
  else
    FTheResponse.SetCustomHeader('Location', AUrl);
end;

procedure TBrookAction.Error(const AMsg: string);
begin
  raise EBrookAction.Create(Self, AMsg);
end;

procedure TBrookAction.Error(const AMsg: string; const AArgs: array of const);
begin
  raise EBrookAction.CreateFmt(Self, AMsg, AArgs);
end;

procedure TBrookAction.Stop(const AMsg: string);
begin
  raise EBrookAction.Create(AMsg);
end;

procedure TBrookAction.Stop(const AMsg: string; const AArgs: array of const);
begin
  raise EBrookAction.CreateFmt(AMsg, AArgs);
end;

procedure TBrookAction.Render(const AFileName: TFileName);
begin
  FTheResponse.Contents.LoadFromFile(AFileName);
end;

procedure TBrookAction.Render(const AFileName: TFileName;
  const AArgs: array of const);
begin
  FTheResponse.Contents.LoadFromFile(AFileName);
  FTheResponse.Contents.Text := Format(FTheResponse.Contents.Text, AArgs);
end;

procedure TBrookAction.Clear;
begin
  FTheResponse.Contents.Clear;
end;

function TBrookAction.Exists(const AName: string): Boolean;
begin
  Result := FFields.IndexOfName(AName) > -1;
end;

procedure TBrookAction.Write(const AString: string);
begin
  FTheResponse.Contents.Add(AString);
end;

procedure TBrookAction.Write(const ABoolean: Boolean);
begin
  Write(BoolToStr(ABoolean));
end;

procedure TBrookAction.Write(const AInteger: Integer);
begin
  Write(IntToStr(AInteger));
end;

procedure TBrookAction.Write(const AFloat: Double);
begin
  Write(FloatToStr(AFloat));
end;

procedure TBrookAction.Write(AObject: TObject);
begin
  BrookObjectToStrings(AObject, FTheResponse.Contents);
end;

procedure TBrookAction.Write(AStream: TStream);
begin
  FTheResponse.Contents.LoadFromStream(AStream);
end;

procedure TBrookAction.Write(const AFmt: string; const AArgs: array of const);
begin
  Write(Format(AFmt, AArgs));
end;

{ TBrookGAction }

constructor TBrookGAction.Create;
begin
  inherited Create;
  FEntity := CreateEntity;
end;

destructor TBrookGAction.Destroy;
begin
  FreeEntity;
  inherited Destroy;
end;

function TBrookGAction.CreateEntity: T;
begin
  Result := T.Create;
end;

procedure TBrookGAction.FreeEntity;
begin
  FreeAndNil(FEntity);
end;

procedure TBrookGAction.Request(ARequest: TBrookRequest;
  AResponse: TBrookResponse);
begin
  case ARequest.Method of
    BROOK_HTTP_REQUEST_METHOD_GET: Get;
    BROOK_HTTP_REQUEST_METHOD_POST:
      begin
        FillEntity;
        Post;
      end;
    BROOK_HTTP_REQUEST_METHOD_PUT:
      begin
        FillEntity;
        Put;
      end;
    BROOK_HTTP_REQUEST_METHOD_DELETE:
      begin
        FillEntity;
        Delete;
      end;
    BROOK_HTTP_REQUEST_METHOD_HEAD: Head;
    BROOK_HTTP_REQUEST_METHOD_OPTIONS: Options;
  end;
end;

procedure TBrookGAction.FillEntity;
begin
  GetFields(FEntity);
end;

end.

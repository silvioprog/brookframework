(*
  Brook Action unit.

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

unit BrookAction;

{$i brook.inc}

interface

uses
  BrookClasses, BrookException, BrookMessages, BrookUtils, BrookHTTPUtils,
  BrookConsts, BrookHTTPConsts, HTTPDefs, FPJSON, Classes, SysUtils, Variants;

type
  { Handles exceptions for @link(TBrookAction). }
  EBrookAction = class(EBrook);

  { Is a metaclass for @link(TBrookAction) class. }
  TBrookActionClass = class of TBrookAction;

  { Provides features to handle with HTTP requests and responses. }
  TBrookAction = class(TBrookObject)
  private
    FContentStream: TStream;
    FFields: TJSONObject;
    FParams: TJSONObject;
    FValues: TJSONObject;
    FRequest: TRequest;
    FResponse: TResponse;
    function GetMethod: string;
  protected
    function CreateFields: TJSONObject; virtual;
    function CreateParams: TJSONObject; virtual;
    function CreateValues: TJSONObject; virtual;
    procedure FreeFields; virtual;
    procedure FreeParams; virtual;
    procedure FreeValues; virtual;
    function GetFiles: TUploadedFiles; virtual;
    function GetRequest: TRequest; virtual;
    function GetResponse: TResponse; virtual;
    procedure DoBeforeRequest({%H-}ARequest: TRequest;
      {%H-}AResponse: TResponse); virtual;
    procedure DoAfterRequest({%H-}ARequest: TRequest;
      {%H-}AResponse: TResponse); virtual;
    property ContentStream: TStream read FContentStream write FContentStream;
  public
    { Creates an instance of a @link(TBrookAction) class. }
    constructor Create; overload; virtual;
    { Creates an instance of a @link(TBrookAction) class passing params to
      request/response. }
    constructor Create(ARequest: TRequest; AResponse: TResponse); overload; virtual;
    { Frees an instance of @link(TBrookAction) class. }
    destructor Destroy; override;
    { Fills the @link(Fields) with data coming from a request called by means
      of POST method. }
    procedure FillFields(ARequest: TRequest); virtual;
    { Fills the @link(params) with data coming from a request called by means
      of GET method. }
    procedure FillParams(ARequest: TRequest); virtual;
    { Fills the @link(values) with variables passed through the URL. }
    procedure FillValues(ANames, AValues: TBrookArrayOfString); virtual;
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

         @code(Write(Values['myvar'].AsString);)

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

         Variable @code(Values['path']) will receive @code('file'),
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
    procedure DoRequest(ARequest: TRequest;
      AResponse: TResponse); overload; virtual;
    { Calls the method @link(TBrookAction.Request). }
    procedure DoRequest(ARequest: TRequest; AResponse: TResponse;
      var AHandled: Boolean); overload; virtual;
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
    { Creates an URL for an action passing a JSON data. }
    function UrlFor(AActionClass: TBrookActionClass;
      const AParams: TJSONData): string; overload;
    { Creates an URL for an action passing a JSON data however informing the
      class name as string. }
    function UrlFor(AClassName: string;
      const AParams: TJSONData): string; overload;
    { Is triggered by a request of any HTTP method. }
    procedure Request(ARequest: TRequest;{%H-}AResponse: TResponse); virtual;
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
    { Is triggered by a OPTIONS HTTP request method. }
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
    { Stops the action showing a exception message. }
    procedure Stop(const AMsg: string); overload;
    { Stops the action showing a formatted exception message. }
    procedure Stop(const AMsg: string; const AArgs: array of const); overload;
    { Writes the content of a file. }
    procedure Render(const AFileName: TFileName); overload;
    { Writes the content of a file passing parameters to the output. }
    procedure Render(const AFileName: TFileName;
      const AArgs: array of const); overload;
    { Clears all written content with @code(Write(), WriteLn(), Render()) etc. }
    procedure Clear;
    { Writes a string. }
    procedure Write(const AString: string); overload;
    { Writes a boolean. }
    procedure Write(const ABoolean: Boolean); overload;
    { Writes a boolean formating output. }
    procedure Write(const ABoolean: Boolean;
      const ATrueStr, AFalseStr: string); overload;
    { Writes an integer. }
    procedure Write(const AInteger: Integer); overload;
    { Writes a float. }
    procedure Write(const AFloat: Double); overload;
    { Writes a float formatting output. }
    procedure Write(const AFloat: Double;
      const AFormatSettings: TFormatSettings); overload;
    { Writes a content of stream. }
    procedure Write(AStream: TStream); overload;
    { Writes a formatted string. }
    procedure Write(const AFmt: string; const AArgs: array of const); overload;
    { Writes a formatted string. }
    procedure Write(const AFmt: string; const AArgs: array of const;
      const AFormatSettings: TFormatSettings); overload;
    { Writes the content of a @code(JSONObject). }
    procedure Write(AJSON: TJSONObject); overload;
    { Writes the content of a @code(TJSONArray). }
    procedure Write(AJSON: TJSONArray); overload;
    { Writes the content of a @code(TStrings). }
    procedure Write(S: TStrings); overload;
    { Writes value of any type. Exemple:

      @code(Write([1, 3.14, False, 'ABC'])). }
    procedure Write(const AArgs: array of const); overload;
    { Writes a variant. }
    procedure Write(const AValue: Variant); overload;
    { Writes a string adding a suffix to the end. Used by @code(Write) and
      @code(WriteLn), avoiding code duplication. }
    procedure Write(const AArgs: array of const; const ASuffix: string);
    { Writes a string adding the @code(BR) HTML tag to the end. }
    procedure WriteLn(const AString: string = ES); overload;
    { Writes a boolean adding the @code(BR) HTML tag to the end. }
    procedure WriteLn(const ABoolean: Boolean); overload;
    { Writes a boolean formating output and adding the @code(BR) HTML tag to
      the end. }
    procedure WriteLn(const ABoolean: Boolean;
      const ATrueStr, AFalseStr: string); overload;
    { Writes an integer adding the @code(BR) HTML tag to the end. }
    procedure WriteLn(const AInteger: Integer); overload;
    { Writes a float adding the @code(BR) HTML tag to the end. }
    procedure WriteLn(const AFloat: Double); overload;
    { Writes a float formatting output and adding the @code(BR) HTML tag to
      the end. }
    procedure WriteLn(const AFloat: Double;
      const AFormatSettings: TFormatSettings); overload;
    { Writes a formatted string adding the @code(BR) HTML tag to the end. }
    procedure WriteLn(const AFmt: string;
      const AArgs: array of const); overload;
    { Writes a formatted string adding the @code(BR) HTML tag to the end. }
    procedure WriteLn(const AFmt: string; const AArgs: array of const;
      const AFormatSettings: TFormatSettings); overload;
    { Writes value of any type adding the @code(BR) HTML tag to the end. }
    procedure WriteLn(const AArgs: array of const); overload;
    { Writes a variant adding the @code(BR) HTML tag to the end. }
    procedure WriteLn(const AValue: Variant); overload;
    { Writes the content of a @code(JSONObject) adding the @code(BR) HTML tag
      to the end. }
    procedure WriteLn(AJSON: TJSONObject); overload;
    { Writes the content of a @code(JSONArray) adding the @code(BR) HTML tag
      to the end. }
    procedure WriteLn(AJSON: TJSONArray); overload;
    { Writes the content of a @code(TStrings) adding, for each item, the
      @code(BR) HTML tag to the end. }
    procedure WriteLn(S: TStrings); overload;
    { The list of files coming from a request called by the POST method. }
    property Files: TUploadedFiles read GetFiles;
    { The list of variables coming from a request called by the POST method. }
    property Fields: TJSONObject read FFields;
    { The list of variables coming from a request called by the GET method. }
    property Params: TJSONObject read FParams;
    { The list of variables received from parametrized URLs. }
    property Values: TJSONObject read FValues;
    { Returns the HTTP request method. }
    property Method: string read GetMethod;
  end;

implementation

uses
  BrookRouter;

constructor TBrookAction.Create;
begin
  inherited Create;
  FFields := CreateFields;
  FParams := CreateParams;
  FValues := CreateValues;
end;

constructor TBrookAction.Create(ARequest: TRequest; AResponse: TResponse);
begin
  FRequest := ARequest;
  FResponse := AResponse;
  Create;
end;

destructor TBrookAction.Destroy;
begin
  FreeFields;
  FreeParams;
  FreeValues;
  inherited Destroy;
end;

function TBrookAction.GetFiles: TUploadedFiles;
begin
  Result := GetRequest.Files;
end;

function TBrookAction.GetMethod: string;
begin
  Result := FRequest.Method;
end;

function TBrookAction.CreateFields: TJSONObject;
begin
  Result := TJSONObject.Create;
end;

function TBrookAction.CreateParams: TJSONObject;
begin
  Result := TJSONObject.Create;
end;

function TBrookAction.CreateValues: TJSONObject;
begin
  Result := TJSONObject.Create;
end;

procedure TBrookAction.FreeFields;
begin
  FreeAndNil(FFields);
end;

procedure TBrookAction.FreeParams;
begin
  FreeAndNil(FParams);
end;

procedure TBrookAction.FreeValues;
begin
  FreeAndNil(FValues);
end;

function TBrookAction.GetRequest: TRequest;
begin
  Result := FRequest;
end;

function TBrookAction.GetResponse: TResponse;
begin
  Result := FResponse;
end;

procedure TBrookAction.DoBeforeRequest(ARequest: TRequest; AResponse: TResponse);
begin
end;

procedure TBrookAction.DoAfterRequest(ARequest: TRequest; AResponse: TResponse);
begin
end;

procedure TBrookAction.DoRequest(ARequest: TRequest; AResponse: TResponse);
begin
  DoBeforeRequest(ARequest, AResponse);
  Request(ARequest, AResponse);
  DoAfterRequest(ARequest, AResponse);
end;

procedure TBrookAction.DoRequest(ARequest: TRequest; AResponse: TResponse;
  var AHandled: Boolean);
begin
  DoBeforeRequest(ARequest, AResponse);
  if not AHandled then
    Request(ARequest, AResponse);
  DoAfterRequest(ARequest, AResponse);
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
  Result := IncludeHTTPPathDelimiter(TBrookRouter.RootUrl) +
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

function TBrookAction.UrlFor(AActionClass: TBrookActionClass;
  const AParams: TJSONData): string;
begin
  Result := TBrookRouter.Service.UrlFor(AActionClass, AParams);
end;

function TBrookAction.UrlFor(AClassName: string;
  const AParams: TJSONData): string;
begin
  Result := TBrookRouter.Service.UrlFor(AClassName, AParams);
end;

procedure TBrookAction.Request(ARequest: TRequest; AResponse: TResponse);
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
  TBrookRouter.MethodNotAllowed(FResponse);
end;

procedure TBrookAction.Post;
begin
  TBrookRouter.MethodNotAllowed(FResponse);
end;

procedure TBrookAction.Put;
begin
  TBrookRouter.MethodNotAllowed(FResponse);
end;

procedure TBrookAction.Delete;
begin
  TBrookRouter.MethodNotAllowed(FResponse);
end;

procedure TBrookAction.Head;
begin
  TBrookRouter.MethodNotAllowed(FResponse);
end;

procedure TBrookAction.Options;
begin
  TBrookRouter.MethodNotAllowed(FResponse);
end;

procedure TBrookAction.FillFields(ARequest: TRequest);
var
  I: Integer;
  S, N: TJSONStringType;
begin
  for I := 0 to Pred(ARequest.ContentFields.Count) do
  begin
    S := ARequest.ContentFields.ValueFromIndex[I];
    N := ARequest.ContentFields.Names[I];
    if S = NU then
      FFields.Add(N)
    else
      FFields.Add(N, S);
  end;
end;

procedure TBrookAction.FillParams(ARequest: TRequest);
var
  I: Integer;
  S, N: TJSONStringType;
begin
  for I := 0 to Pred(ARequest.QueryFields.Count) do
  begin
    S := ARequest.QueryFields.ValueFromIndex[I];
    N := ARequest.QueryFields.Names[I];
    if S = NU then
      FParams.Add(N)
    else
      FParams.Add(N, S);
  end;
end;

procedure TBrookAction.FillValues(ANames, AValues: TBrookArrayOfString);
var
  I: Integer;
  S, N: TJSONStringType;
begin
  for I := 0 to High(ANames) do
  begin
    S := AValues[I];
    N := ANames[I];
    if S = NU then
      FValues.Add(N)
    else
      FValues.Add(N, S);
  end;
end;

procedure TBrookAction.Redirect(const AUrl: string);
begin
  FResponse.SendRedirect(AUrl);
end;

procedure TBrookAction.Redirect(const AUrl: string; const AStatusCode: Word);
begin
  FResponse.Code := AStatusCode;
  FResponse.CodeText := BrookStatusCodeToReasonPhrase(AStatusCode);
  FResponse.SetCustomHeader(fieldLocation, AUrl);
end;

procedure TBrookAction.Redirect(const AUrl: string; const AUseRootUrl: Boolean);
begin
  if AUseRootUrl then
    FResponse.SendRedirect(TBrookRouter.RootUrl + AUrl)
  else
    FResponse.SendRedirect(AUrl);
end;

procedure TBrookAction.Redirect(const AUrl: string;
  const AUseRootUrl: Boolean; const AStatusCode: Word);
begin
  FResponse.Code := AStatusCode;
  FResponse.CodeText := BrookStatusCodeToReasonPhrase(AStatusCode);
  if AUseRootUrl then
    FResponse.SetCustomHeader(fieldLocation,
      TBrookRouter.RootUrl(FRequest) + AUrl)
  else
    FResponse.SetCustomHeader(fieldLocation, AUrl);
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
  FResponse.Contents.LoadFromFile(AFileName);
end;

procedure TBrookAction.Render(const AFileName: TFileName;
  const AArgs: array of const);
begin
  FResponse.Contents.LoadFromFile(AFileName);
  FResponse.Contents.Text := Format(FResponse.Contents.Text, AArgs);
end;

procedure TBrookAction.Clear;
begin
  FResponse.Contents.Clear;
end;

procedure TBrookAction.Write(const AString: string);
begin
  if Assigned(FContentStream) then
    FContentStream.Write(Pointer(AString)^, Length(AString))
  else
    FResponse.Contents.Add(AString);
end;

procedure TBrookAction.Write(const ABoolean: Boolean);
begin
  Write(BoolToStr(ABoolean));
end;

procedure TBrookAction.Write(const ABoolean: Boolean; const ATrueStr,
  AFalseStr: string);
begin
  Write(BoolToStr(ABoolean, ATrueStr, AFalseStr));
end;

procedure TBrookAction.Write(const AInteger: Integer);
begin
  Write(IntToStr(AInteger));
end;

procedure TBrookAction.Write(const AFloat: Double);
begin
  Write(FloatToStr(AFloat));
end;

procedure TBrookAction.Write(const AFloat: Double;
  const AFormatSettings: TFormatSettings);
begin
  Write(FloatToStr(AFloat, AFormatSettings));
end;

procedure TBrookAction.Write(AStream: TStream);
begin
  FResponse.Contents.LoadFromStream(AStream);
end;

procedure TBrookAction.Write(const AFmt: string; const AArgs: array of const);
begin
  Write(Format(AFmt, AArgs));
end;

procedure TBrookAction.Write(const AFmt: string; const AArgs: array of const;
  const AFormatSettings: TFormatSettings);
begin
  Write(Format(AFmt, AArgs, AFormatSettings));
end;

procedure TBrookAction.Write(AJSON: TJSONObject);
var
  I: Integer;
begin
  for I := 0 to Pred(AJSON.Count) do
    Write(AJSON.Items[I].AsString);
end;

procedure TBrookAction.Write(AJSON: TJSONArray);
var
  I: Integer;
begin
  for I := 0 to Pred(AJSON.Count) do
    Write(AJSON[I].AsString);
end;

procedure TBrookAction.Write(S: TStrings);
var
  X: string;
begin
  for X in S do
    Write(X);
end;

procedure TBrookAction.Write(const AValue: Variant);
begin
  Write(VarToStr(AValue));
end;

procedure TBrookAction.Write(const AArgs: array of const; const ASuffix: string);
var
  I: Integer;
begin
  for I := 0 to High(AArgs) do
    with AArgs[I] do
      case VType of
        vtInteger: Write(IntToStr(VInteger) + ASuffix);
        vtInt64: Write(IntToStr(VInt64^) + ASuffix);
        vtQWord: Write(IntToStr(VQWord^) + ASuffix);
        vtBoolean: Write(BoolToStr(VBoolean) + ASuffix);
        vtExtended: Write(FloatToStr(VExtended^) + ASuffix);
        vtCurrency: Write(FloatToStr(VCurrency^) + ASuffix);
        vtString: Write(VString^);
        vtAnsiString: Write(AnsiString(VAnsiString) + ASuffix);
        vtChar: Write(VChar + ASuffix);
        vtPChar: Write(VPChar + ASuffix);
        vtPWideChar: Write(AnsiString(VPWideChar) + ASuffix);
        vtWideChar: Write(AnsiString(VWideChar) + ASuffix);
        vtWidestring: Write(AnsiString(WideString(VWideString)) + ASuffix);
        vtObject:
           if VObject is TJSONArray then
             Write(TJSONObject(VObject).AsJSON + ASuffix)
           else
             if VObject is TJSONObject then
               Write(TJSONObject(VObject).AsJSON + ASuffix);
      else
        Write('?unknown variant?' + ASuffix);
      end;
end;

procedure TBrookAction.Write(const AArgs: array of const);
begin
  Write(AArgs, ES);
end;

procedure TBrookAction.WriteLn(const AString: string);
begin
  Write(AString + BR);
end;

procedure TBrookAction.WriteLn(const ABoolean: Boolean);
begin
  Write(BoolToStr(ABoolean) + BR);
end;

procedure TBrookAction.WriteLn(const ABoolean: Boolean; const ATrueStr,
  AFalseStr: string);
begin
  Write(BoolToStr(ABoolean, ATrueStr, AFalseStr) + BR);
end;

procedure TBrookAction.WriteLn(const AInteger: Integer);
begin
  Write(IntToStr(AInteger) + BR);
end;

procedure TBrookAction.WriteLn(const AFloat: Double);
begin
  Write(FloatToStr(AFloat) + BR);
end;

procedure TBrookAction.WriteLn(const AFloat: Double;
  const AFormatSettings: TFormatSettings);
begin
  Write(FloatToStr(AFloat, AFormatSettings) + BR);
end;

procedure TBrookAction.WriteLn(const AFmt: string; const AArgs: array of const);
begin
  Write(Format(AFmt, AArgs) + BR);
end;

procedure TBrookAction.WriteLn(const AFmt: string; const AArgs: array of const;
  const AFormatSettings: TFormatSettings);
begin
  Write(Format(AFmt, AArgs, AFormatSettings) + BR);
end;

procedure TBrookAction.WriteLn(const AArgs: array of const);
begin
  Write(AArgs, BR);
end;

procedure TBrookAction.WriteLn(const AValue: Variant);
begin
  Write(VarToStr(AValue) + BR);
end;

procedure TBrookAction.WriteLn(AJSON: TJSONObject);
begin
  Write(AJSON);
  Write(BR);
end;

procedure TBrookAction.WriteLn(AJSON: TJSONArray);
begin
  Write(AJSON);
  Write(BR);
end;

procedure TBrookAction.WriteLn(S: TStrings);
var
  VVal: string;
begin
  for VVal in S do
    Write(VVal + BR);
end;

end.

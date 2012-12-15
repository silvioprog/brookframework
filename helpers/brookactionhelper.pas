(*
  Brook Action Helper unit.

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

unit BrookActionHelper;

{$i brook.inc}

interface

uses
  BrookAction, BrookConsts, SysUtils, HTTPDefs, FPJSON;

type
  { Adds features to @code(TBrookAction) class. }
  TBrookActionHelper = class helper for TBrookAction
  public
    { Shows a dialog with a information message. }
    procedure Alert(const S: string);
    { Shows a dialog with a confirmation message. }
    function Confirm(const S: string): Boolean;
    { Shows a dialog with a question message. }
    function Prompt(const S, ADefaultValue: string): string;
    { Creates a cookie. }
    procedure SetCookie(const AName, AValue: string;
      const AExpires: TDateTime = NullDate; const APath: string = ES;
      const ADomain: string = ES; const ASecure: Boolean = False;
      const AHttpOnly: Boolean = False);
    { Get a cookie value. }
    function GetCookie(const AName: string): string;
    { Deletes a cookie. }
    procedure DeleteCookie(const AName: string; const APath: string = ES;
      const ADomain: string = ES);
    { Creates a link HTML tag. }
    function Link(ACaption: string; const AUrl: string = ES;
      const AClass: string = ES): string;
    { Creates a link HTML tag for an action passing an array of parameters
      however informing the class name as string }
    function LinkTo(const ACaption: string; AActionClass: TBrookActionClass;
      const AParams: array of string; const AClass: string = ES): string;
    { Creates an link HTML tag for an action passing a JSON data. }
    function LinkTo(const ACaption: string; AActionClass: TBrookActionClass;
      const AParams: TJSONData; const AClass: string = ES): string;
    { Creates an link HTML tag for an action. }
    function LinkTo(const ACaption: string;
      AActionClass: TBrookActionClass; const AClass: string = ES): string;
    { Creates an link HTML tag for an action passing an array of parameters
      however informing the class name as string }
    function LinkTo(const ACaption, AClassName: string;
      const AParams: array of string; const AClass: string = ES): string;
    { Creates an link HTML tag for an action passing a JSON data however
      informing the class name as string. }
    function LinkTo(const ACaption, AClassName: string;
      const AParams: TJSONData; const AClass: string = ES): string;
    { Creates an link HTML tag for an action informing the class name as
      string. }
    function LinkTo(const ACaption, AClassName, AClass: string): string;
    { Creates a button HTML tag for an action passing an array of parameters. }
    function ButtonTo(const ACaption: string; AActionClass: TBrookActionClass;
      const AParams: array of string;
      const AValue: ShortString = 'delete'): string;
    { Creates an button HTML tag for an action passing a JSON data. }
    function ButtonTo(const ACaption: string; AActionClass: TBrookActionClass;
      const AParams: TJSONData; const AValue: ShortString = 'delete'): string;
    { Creates an button HTML tag for an action. }
    function ButtonTo(const ACaption: string; AActionClass: TBrookActionClass;
      const AValue: ShortString = 'delete'): string;
    { Creates a button HTML tag for an action passing an array of parameters
      however informing the class name as string }
    function ButtonTo(const ACaption, AClassName: string;
      const AParams: array of string;
      const AValue: ShortString = 'delete'): string;
    { Creates an button HTML tag for an action passing a JSON data however
      informing the class name as string. }
    function ButtonTo(const ACaption, AClassName: string;
      const AParams: TJSONData; const AValue: ShortString = 'delete'): string;
    { Creates an button HTML tag for an action informing the class name as
      string. }
    function ButtonTo(const ACaption, AClassName: string;
      const AValue: ShortString = 'delete'): string;
  end;

implementation

const
  _BROOK_JS =
    '	function setCookie(name, value, expires) {' + LF +
    '		var curCookie = name + "=" + escape(value) +' + LF +
    '		((expires) ? "; Expires=" + expires.toGMTString() : "");' + LF +
    '		document.cookie = curCookie;' + LF +
    '	}' + LF +
    '	function getCookie(name) {' + LF +
    '		var dc = document.cookie;' + LF +
    '		var prefix = name + "=";' + LF +
    '		var begin = dc.indexOf("; " + prefix);' + LF +
    '		if (begin == -1) {' + LF +
    '				begin = dc.indexOf(prefix);' + LF +
    '				if (begin != 0) return null;' + LF +
    '		} else' + LF +
    '		begin += 2;' + LF +
    '		var end = document.cookie.indexOf(";", begin);' + LF +
    '		if (end == -1)' + LF +
    '		end = dc.length;' + LF +
    '		return unescape(dc.substring(begin + prefix.length, end));' + LF +
    '	}' + LF +
    '	function deleteCookie(name) {' + LF +
    '		if (getCookie(name)) {' + LF +
    '				document.cookie = name + "=" +' + LF +
    '				"; Expires=Thu, 01-Jan-70 00:00:01 GMT";' + LF +
    '		}' + LF +
    '	}';
  _BROOK_FORM =
    '<form class="button_to_form" method="post" action="%s" >' +
    '<input name="_method" value="%s" type="hidden" />' +
    '<input value="%s" type="submit" class="btn" />' +
    '</form>';

procedure TBrookActionHelper.Alert(const S: string);
begin
  GetResponse.Contents.Add('<script type="text/javascript">window.alert("' +
    StringToJSONString(S) + '")</script>');
end;

function TBrookActionHelper.Confirm(const S: string): Boolean;
begin
  GetResponse.Contents.Add(
    '<script type="text/javascript">' + LF +
    _BROOK_JS + LF +
    '	var name = "brookactionhelper_confirm";' + LF +
    '	if (!getCookie(name) && window.confirm("' + StringToJSONString(S) + '")) {' + LF +
    '		setCookie(name, "yes");' + LF +
    '		window.location.reload();' + LF +
    '	} else {' + LF +
    '		deleteCookie(name);' + LF +
    '	}' + LF +
    '</script>');
  Result := GetCookie('brookactionhelper_confirm') <> ES;
end;

function TBrookActionHelper.Prompt(const S, ADefaultValue: string): string;
begin
  GetResponse.Contents.Add(
    '<script type="text/javascript">' + LF +
    _BROOK_JS + LF +
    '	var name = "brookactionhelper_prompt";' + LF +
    '	var value = "' + StringToJSONString(ADefaultValue) + '";' + LF +
    '	if (!getCookie(name)) {' + LF +
    '		value = window.prompt("' + StringToJSONString(S) + '", value);' + LF +
    '		if (value) {' + LF +
    '			setCookie(name, value);' + LF +
    '			window.location.reload();' + LF +
    '		}' + LF +
    '	} else {' + LF +
    '		deleteCookie(name);' + LF +
    '	}' + LF +
    '</script>');
  Result := GetCookie('brookactionhelper_prompt');
end;

procedure TBrookActionHelper.SetCookie(const AName, AValue: string;
  const AExpires: TDateTime; const APath: string; const ADomain: string;
  const ASecure: Boolean; const AHttpOnly: Boolean);
var
  VCookie: TCookie;
begin
  VCookie := GetResponse.Cookies.Add;
  VCookie.Name := AName;
  VCookie.Value := AValue;
  if AExpires <> NullDate then
    VCookie.Expires := AExpires;
  VCookie.Path := APath;
  VCookie.Domain := ADomain;
  VCookie.Secure := ASecure;
  VCookie.HttpOnly := AHTTPOnly;
end;

function TBrookActionHelper.GetCookie(const AName: string): string;
begin
  Result := GetRequest.CookieFields.Values[AName];
end;

procedure TBrookActionHelper.DeleteCookie(const AName: string;
  const APath: string; const ADomain: string);
var
  VCookie: TCookie;
begin
  VCookie := GetResponse.Cookies.Add;
  VCookie.Name := AName;
  VCookie.Path := APath;
  VCookie.Domain := ADomain;
  VCookie.Expire;
end;

function TBrookActionHelper.Link(ACaption: string; const AUrl: string;
  const AClass: string): string;
var
  VClass: string;
begin
  if ACaption = ES then
    ACaption := AUrl;
  if AClass <> ES then
    VClass := 'class="'+AClass+'" ';
  Result := '<a href="' + AUrl + '" '+VClass+'>' + ACaption + '</a>';
end;

function TBrookActionHelper.LinkTo(const ACaption: string;
  AActionClass: TBrookActionClass; const AParams: array of string;
  const AClass: string): string;
begin
  Result := Link(ACaption, UrlFor(AActionClass, AParams), AClass);
end;

function TBrookActionHelper.LinkTo(const ACaption: string;
  AActionClass: TBrookActionClass; const AParams: TJSONData;
  const AClass: string): string;
begin
  Result := Link(ACaption, UrlFor(AActionClass, AParams), AClass);
end;

function TBrookActionHelper.LinkTo(const ACaption: string;
  AActionClass: TBrookActionClass; const AClass: string): string;
begin
  Result := Link(ACaption, UrlFor(AActionClass), AClass);
end;

function TBrookActionHelper.LinkTo(const ACaption, AClassName: string;
  const AParams: array of string; const AClass: string): string;
begin
  Result := Link(ACaption, UrlFor(AClassName, AParams), AClass);
end;

function TBrookActionHelper.LinkTo(const ACaption, AClassName: string;
  const AParams: TJSONData; const AClass: string): string;
begin
  Result := Link(ACaption, UrlFor(AClassName, AParams), AClass);
end;

function TBrookActionHelper.LinkTo(
  const ACaption, AClassName, AClass: string): string;
begin
  Result := Link(ACaption, UrlFor(AClassName), AClass);
end;

function TBrookActionHelper.ButtonTo(const ACaption: string;
  AActionClass: TBrookActionClass; const AParams: array of string;
  const AValue: ShortString): string;
begin
  Result := Format(_BROOK_FORM,
    [UrlFor(AActionClass, AParams), AValue, ACaption]);
end;

function TBrookActionHelper.ButtonTo(const ACaption: string;
  AActionClass: TBrookActionClass; const AParams: TJSONData;
  const AValue: ShortString): string;
begin
  Result := Format(_BROOK_FORM,
    [UrlFor(AActionClass, AParams), AValue, ACaption]);
end;

function TBrookActionHelper.ButtonTo(const ACaption: string;
  AActionClass: TBrookActionClass; const AValue: ShortString): string;
begin
  Result := Format(_BROOK_FORM, [UrlFor(AActionClass), AValue, ACaption]);
end;

function TBrookActionHelper.ButtonTo(const ACaption, AClassName: string;
  const AParams: array of string; const AValue: ShortString): string;
begin
  Result := Format(_BROOK_FORM,
    [UrlFor(AClassName, AParams), AValue, ACaption]);
end;

function TBrookActionHelper.ButtonTo(const ACaption, AClassName: string;
  const AParams: TJSONData; const AValue: ShortString): string;
begin
  Result := Format(_BROOK_FORM,
    [UrlFor(AClassName, AParams), AValue, ACaption]);
end;

function TBrookActionHelper.ButtonTo(const ACaption, AClassName: string;
  const AValue: ShortString): string;
begin
  Result := Format(_BROOK_FORM, [UrlFor(AClassName), AValue, ACaption]);
end;

end.

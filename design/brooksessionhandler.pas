(*
  Brook framework, Session Handler Class

  Copyright (C) 2014 Silvio Clecio

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit BrookSessionHandler;

{$i brook.inc}

interface

uses
  BrookSession;

type
  { Handles the session features. }
  TBrookSessionHandler = class(TBrookSession)
  published
    { Set the session cookie name. }
    property CookieName;
    { Set the session cookie domain. }
    property CookieDomain;
    { Set the session cookie path. }
    property CookiePath;
    { Set the session cookie secure. }
    property CookieSecure;
    { Set the session cookie expiration. }
    property CookieExpires;
    { The session fields. }
    property Fields;
    { The ignored fields by the session. }
    property IgnoredFields;
    { Set the name of session directory. }
    property Directory;
    { Get or set the session ID. }
    property SID;
    { The session file name. }
    property FileName;
    { The session file prefix. }
    property FilePrefix;
    { The remaining seconds for the session finish. }
    property TimeOut;
    { Informs if the session cookie is accessible only by HTTP requests,
      if @code(True), the JavaScript access is not allowed. }
    property HttpOnly;
  end;

implementation

end.

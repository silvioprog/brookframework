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
    property CookieName;
    property CookieDomain;
    property CookiePath;
    property CookieSecure;
    property CookieExpires;
    property Fields;
    property IgnoredFields;
    property Directory;
    property FilePrefix;
    property TimeOut;
    property HttpOnly;
  end;

implementation

end.

(*
  Brook Consts unit.

  Copyright (C) 2012 Silvio Clecio.

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

unit BrookConsts;

{$i brook.inc}

interface

const
  { Empty string. }
  ES = '';
  { BR HTML tag. }
  BR = '<br />';
  { Param separator. }
  HS = ': ';
  { Line ending. }
  LE = LineEnding;
  { NULL }
  NU = #0;
  { TAB }
  HT = #9;
  { LF }
  LF = #10;
  { CR }
  CR = #13;
  { CRLF }
  CRLF = CR + LF;
  { Null date. }
  NullDate = 0;
  { Space. }
  SP = #32;
  { " }
  DQ = #34;
  { # }
  PO = #35;
  { % }
  PT = #37;
  { & }
  AM = #38;
  { ' }
  AP = #39;
  { ( }
  PS = #40;
  { ) }
  PE = #41;
  { * }
  AK = #42;
  { , }
  CS = #44;
  { . }
  DT = #46;
  { / }
  US = #47;
  { : }
  CO = #58;
  { ; }
  SC = #59;
  { < }
  LT = #60;
  { = }
  EQ = #61;
  { > }
  GT = #62;
  { ? }
  QU = #63;
  { @ }
  AT = #64;
  { \ }
  DS = #92;

  BROOK_GUID = '{D2BDD8EF-78C0-47CD-95C3-664CDFFDAA9E}';
  BROOK_APP_GUID = '{669B03B7-AA2D-4B64-AAFC-4FBD4A41267E}';
  BROOK_ERROR_MASK = '%s: %s';
  BROOK_GMT_FRMT = '%s, %.2d %s %d %.2d:%.2d:%.2d GMT';
  BROOK_UUID_MASK = '%.8x%.4x%.4x%.2x%.2x%.2x%.2x%.2x%.2x%.2x%.2x';
  BROOK_UUID_SEP_MASK = '%.8x-%.4x-%.4x-%.2x%.2x-%.2x%.2x%.2x%.2x%.2x%.2x';
  BROOK_OVERRIDDEN_METHOD = '_METHOD';
  BROOK_DEFAULT_LANGUAGE = 'en-US';
  BROOK_SESS_ID = 'BRKSESSID';
  BROOK_SESS_PREFIX = 'brook_sess_';
  BROOK_SESS_DEFAULT_TIMEOUT = 3 * 60; // 3 minutes

implementation

end.

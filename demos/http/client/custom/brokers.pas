unit Brokers;

{$mode objfpc}{$H+}

interface

uses
  BrookFCLCGIBroker, BrookUtils, BrookHTTPConsts;

implementation

initialization
  BrookSettings.ContentType := BROOK_HTTP_CONTENT_TYPE_TEXT_PLAIN;

end.

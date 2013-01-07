unit Brokers;

{$mode objfpc}{$H+}

interface

uses
  BrookFCLCGIBroker, BrookUtils;

implementation

initialization
  BrookSettings.Page500 :=
    '<html><head><title>Internal server error</title></head><body>' +
    '<h1>500 - Internal server error</h1>' +
    '<p style="color: red;" >Error: @error</p></body></html>';

end.

unit Brokers;

{$mode objfpc}{$H+}

interface

uses
  BrookFCLHttpAppBroker, BrookUtils;

implementation

initialization
  BrookSettings.Port := 8080;

end.

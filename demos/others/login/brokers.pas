unit Brokers;

{$mode objfpc}{$H+}

interface

uses
  BrookFCLCGIBroker, BrookUtils;

implementation

initialization
  BrookSettings.Page404 := '404.html';
  BrookSettings.Page500 := '500.html';

end.

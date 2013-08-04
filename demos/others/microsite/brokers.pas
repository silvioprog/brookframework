unit Brokers;

{$mode objfpc}{$H+}

interface

uses
  BrookFCLCGIBroker, BrookUtils;

implementation

initialization
  BrookSettings.Page404File := '404.html';
  BrookSettings.Page500File := '500.html';

end.

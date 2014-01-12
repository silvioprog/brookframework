unit Brokers;

{$mode objfpc}{$H+}

interface

uses
  BrookFCLCGIBroker, BrookConstraints, BrookRouter;

implementation

initialization
  TBrookConstraints.Service.BindExecution(@TBrookRouter.Service.OnExecuteAction);

end.

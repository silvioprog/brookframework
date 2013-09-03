(*
  Brook Application unit.

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

unit BrookApplication;

{$i brook.inc}

interface

uses
  BrookConsts, BrookClasses, BrookException, BrookMessages;

type
  { Handles exceptions for application. }
  EBrookApplication = class(EBrook);

  { Interface of the application. }
  IBrookApplication = interface(IBrookInterface)[BROOK_APP_GUID]
    { Returns the instance of broker application. }
    function Instance: TObject;
    { Initializes and runs the application. }
    procedure Run;
    { Terminates the application. }
    procedure Terminate;
  end;

{ Returns the application instance. }
function BrookApp: IBrookApplication;
{ Register the application. }
procedure BrookRegisterApp(AApp: IBrookApplication);
{ Unregister the application. }
procedure BrookUnregisterApp;

implementation

var
  _BrookAppService: IBrookApplication = nil;

function BrookApp: IBrookApplication;
begin
  if not Assigned(_BrookAppService) then
    raise EBrookApplication.Create('BrookApp',
      SBrookNoApplicationRegisteredError);
  Result := _BrookAppService;
end;

procedure BrookRegisterApp(AApp: IBrookApplication);
begin
  if Assigned(_BrookAppService) then
    raise EBrookApplication.Create('BrookRegisterApp',
      SBrookApplicationAlreadyRegisteredError);
  _BrookAppService := AApp;
end;

procedure BrookUnregisterApp;
begin
  _BrookAppService := nil;
end;

end.

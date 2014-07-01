(*
  Brook framework, Messages Unit

  Copyright (C) 2014 Silvio Clecio

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit BrookMessages;

{$i brook.inc}

interface

var
  SBrookActiveLanguage: string = 'en-US';
  { Error msgs }
  { }
  SBrookInvalidRequestMethodError: string = 'Invalid request method: %s';
  SBrookItemNotFoundError: string = 'Item "%s" not found.';
  SBrookFileNotFoundError: string = 'File not found: %s';
  SBrookNoRequestMethodError: string = 'No REQUEST_METHOD passed from server.';
  SBrookNoApplicationRegisteredError: string = 'No application registered.';
  SBrookApplicationAlreadyRegisteredError: string = 'The application is already registered.';
  SBrookRegiterTBrookActionError: string = 'Unable to register the type TBrookAction directly.';
  SBrookActionAlreadyRegisteredError: string = 'The action "%s" is already registered.';
  SBrookDefaultActionAlreadyRegisteredError: string = 'There is already a default action registered.';
  SBrookPatternAlreadyRegisteredError: string = 'There is already an action registered with the pattern "%s".';
  SBrookRouterServiceAlreadyRegisteredError: string = 'The router service is already registered.';
  SBrookNoRouterServiceRegisteredError: string = 'No router service registered.';
  SBrookNoRouteRegisteredError: string = 'No route registered.';
  SBrookCfgFileNotFoundError: string = 'The config file was not found: "%s"';
  SBrookNotNilError: string = '"%s" must not be nil.';
  SBrookEmptyLibraryNameError: string = 'The library name must not be empty.';
  SBrookMethodNotAllowedError: string = 'HTTP method not allowed for the requested resource.';
  SBrookConstraintAlreadyRegisteredError: string = 'The constraint "%s" is already registered.';
  SBrookConstraintsServiceAlreadyRegisteredError: string = 'The constraints service is already registered.';
  SBrookNoConstraintsServiceRegisteredError: string = 'No constraints service registered.';
  SBrookNoLoggerRegisteredError: string = 'No logger registered.';
  SBrookLoggerAlreadyRegisteredError: string = 'The logger is already registered.';

implementation

end.

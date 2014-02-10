(*
  Brook Messages unit.

  Copyright (C) 2013 Silvio Clecio.

  http://silvioprog.github.io/brookframework

  All contributors:
  Plase see the file CONTRIBUTORS.txt, included in this
  distribution.

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
  SBrookNilJSONParamError: string = '"AJSON" must not be nil.';
  SBrookNilParamError: string = '"%s" must not be nil.';
  SBrookDataBaseServiceAlreadyRegisteredError: string = 'The database service is already registered.';
  SBrookNoDataBaseServiceRegisteredError: string = 'No database service registered.';
  SBrookNoQueryBrokerClassRegisteredError: string = 'No query broker class registered.';
  SBrookNoQueryInstantiatedError: string = 'No query instantiated.';
  SBrookEmptyTableNameError: string = 'The table name must not be empty.';
  SBrookEmptyLibraryNameError: string = 'The library name must not be empty.';
  SBrookMethodNotAllowedError: string = 'HTTP method not allowed for the requested resource.';
  SBrookIncompatibleTypesError: string = 'Incompatible types: got "%s" expected "%s".';
  SBrookConstraintAlreadyRegisteredError: string = 'The constraint "%s" is already registered.';
  SBrookConstraintsServiceAlreadyRegisteredError: string = 'The constraints service is already registered.';
  SBrookNoConstraintsServiceRegisteredError: string = 'No constraints service registered.';

implementation

end.

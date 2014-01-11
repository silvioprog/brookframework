(*
  Brook Messages (pt-BR) unit.

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

unit BrookMessages_ptBR;

{$i brook.inc}

interface

uses
  BrookMessages;

type
  TBrookMessage_ptBR = class(TBrookMessage)
  public
    class procedure Translate; override;
    class function CountryCode: ShortString; override;
    class function CountryName: string; override;
  end;

implementation

resourcestring
{$i brookmessages_ptbr.inc}

class procedure TBrookMessage_ptBR.Translate;
begin
  SBrookInvalidRequestMethodError := SBrookInvalidRequestMethodError_rst;
  SBrookItemNotFoundError := SBrookItemNotFoundError_rst;
  SBrookFileNotFoundError := SBrookFileNotFoundError_rst;
  SBrookNoRequestMethodError := SBrookNoRequestMethodError_rst;
  SBrookNoApplicationRegisteredError := SBrookNoApplicationRegisteredError_rst;
  SBrookApplicationAlreadyRegisteredError :=
    SBrookApplicationAlreadyRegisteredError_rst;
  SBrookRegiterTBrookActionError := SBrookRegiterTBrookActionError_rst;
  SBrookActionAlreadyRegisteredError := SBrookActionAlreadyRegisteredError_rst;
  SBrookDefaultActionAlreadyRegisteredError :=
    SBrookDefaultActionAlreadyRegisteredError_rst;
  SBrookPatternAlreadyRegisteredError := SBrookPatternAlreadyRegisteredError_rst;
  SBrookRouterServiceAlreadyRegisteredError := SBrookRouterServiceAlreadyRegisteredError_rst;
  SBrookNoRouterServiceRegisteredError := SBrookNoRouterServiceRegisteredError_rst;
  SBrookMessagesServiceAlreadyRegisteredError :=
    SBrookMessagesServiceAlreadyRegisteredError_rst;
  SBrookNoMessagesServiceRegisteredError := SBrookNoMessagesServiceRegisteredError_rst;
  SBrookNoRouteRegisteredError := SBrookNoRouteRegisteredError_rst;
  SBrookCfgFileNotFoundError := SBrookCfgFileNotFoundError_rst;
  SBrookNilJSONParamError := SBrookNilJSONParamError_rst;
  SBrookNilParamError := SBrookNilParamError_rst;
  SBrookDataBaseServiceAlreadyRegisteredError :=
    SBrookDataBaseServiceAlreadyRegisteredError_rst;
  SBrookNoDataBaseServiceRegisteredError := SBrookNoDataBaseServiceRegisteredError_rst;
  SBrookNoQueryBrokerClassRegisteredError :=
    SBrookNoQueryBrokerClassRegisteredError_rst;
  SBrookNoQueryInstantiatedError := SBrookNoQueryInstantiatedError_rst;
  SBrookEmptyTableNameError := SBrookEmptyTableNameError_rst;
  SBrookEmptyLibraryNameError := SBrookEmptyLibraryNameError_rst;
  SBrookEmptyCountryCodeError := SBrookEmptyCountryCodeError_rst;
  SBrookMethodNotAllowedError := SBrookMethodNotAllowedError_rst;
  SBrookIncompatibleTypesError := SBrookIncompatibleTypesError_rst;
  SBrookConstraintAlreadyRegisteredError := SBrookConstraintAlreadyRegisteredError_rst;
  SBrookConstraintsServiceAlreadyRegisteredError := SBrookConstraintsServiceAlreadyRegisteredError_rst;
  SBrookNoConstraintsServiceRegisteredError := SBrookNoConstraintsServiceRegisteredError_rst;
end;

class function TBrookMessage_ptBR.CountryCode: ShortString;
begin
  Result := 'pt-BR';
end;

class function TBrookMessage_ptBR.CountryName: string;
begin
  Result := 'Brazil';
end;

initialization
  TBrookMessage_ptBR.Register;

end.

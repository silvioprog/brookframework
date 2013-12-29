(*
  Brook Messages (en-US) unit.

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

unit BrookMessages_enUS;

{$i brook.inc}

interface

uses
  BrookMessages;

type
  TBrookMessage_enUS = class(TBrookMessage)
  public
    class procedure Translate; override;
    class function CountryCode: ShortString; override;
    class function CountryName: string; override;
  end;

implementation

resourcestring
{$i brookmessages_enus.inc}

class procedure TBrookMessage_enUS.Translate;
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
  SBrookRouterAlreadyRegisteredError := SBrookRouterAlreadyRegisteredError_rst;
  SBrookNoRouterRegisteredError := SBrookNoRouterRegisteredError_rst;
  SBrookMessagesAlreadyRegisteredError :=
    SBrookMessagesAlreadyRegisteredError_rst;
  SBrookNoMessagesRegisteredError := SBrookNoMessagesRegisteredError_rst;
  SBrookNoRouteRegisteredError := SBrookNoRouteRegisteredError_rst;
  SBrookCfgFileNotFoundError := SBrookCfgFileNotFoundError_rst;
  SBrookNilJSONParamError := SBrookNilJSONParamError_rst;
  SBrookNilParamError := SBrookNilParamError_rst;
  SBrookDataBaseAlreadyRegisteredError :=
    SBrookDataBaseAlreadyRegisteredError_rst;
  SBrookNoDataBaseRegisteredError := SBrookNoDataBaseRegisteredError_rst;
  SBrookNoQueryBrokerClassRegisteredError :=
    SBrookNoQueryBrokerClassRegisteredError_rst;
  SBrookNoQueryInstantiatedError := SBrookNoQueryInstantiatedError_rst;
  SBrookEmptyTableNameError := SBrookEmptyTableNameError_rst;
  SBrookEmptyLibraryNameError := SBrookEmptyLibraryNameError_rst;
  SBrookEmptyCountryCodeError := SBrookEmptyCountryCodeError_rst;
  SBrookMethodNotAllowedError := SBrookMethodNotAllowedError_rst;
  SBrookIncompatibleTypesError := SBrookIncompatibleTypesError_rst;
end;

class function TBrookMessage_enUS.CountryCode: ShortString;
begin
  Result := 'en-US';
end;

class function TBrookMessage_enUS.CountryName: string;
begin
  Result := 'United States';
end;

initialization
  TBrookMessage_enUS.Register;

end.

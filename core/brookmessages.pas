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

uses
  BrookClasses, BrookConsts, BrookException, Classes, SysUtils;

type
  { Handles exceptions for @link(TBrookMessage). }
  EBrookMessage = class(EBrook);

  { Handles exceptions for @link(TBrookMessages). }
  EBrookMessages = class(EBrook);

  { Is a metaclass for @link(TBrookMessage) class. }
  TBrookMessageClass = class of TBrookMessage;

  { Is a metaclass for @link(TBrookMessages) class. }
  TBrookMessagesClass = class of TBrookMessages;

  { Handles strings for translation of messages. }
  TBrookMessage = class(TBrookObject)
  public
    { Register the broker class. }
    class procedure Register;
    { Translates the framework messages. }
    class procedure Translate; virtual; abstract;
    { Returns the country code, exemple: en-US. }
    class function CountryCode: ShortString; virtual; abstract;
    { Returns the country name, exemple: United States. }
    class function CountryName: string; virtual; abstract;
  end;

  { Registers the available language in the framework. }
  TBrookMessages = class(TBrookObject)
  private
    FList: TFPList;
    function GetItems(const AIndex: Integer): TBrookMessageClass;
    procedure SetItems(const AIndex: Integer; AValue: TBrookMessageClass);
  public
    { Creates an instance of a @link(TBrookMessages) class. }
    constructor Create;
    { Frees an instance of @link(TBrookMessages) class. }
    destructor Destroy; override;
    { Registers the service provided by this class. }
    class procedure RegisterService;
    { Unregisters the service  provided by this class. }
    class procedure UnregisterService;
    { Return a instance of this class. }
    class function Service: TBrookMessages;
    { Returns the number of registered translations. }
    function Count: Integer;
    { Translates the framework messages acoording to the country code. }
    procedure SetLanguage(const ACountryCode: ShortString);
    { Finds a translation item by its country code. }
    function Find(const ACountryCode: ShortString): TBrookMessageClass;
    { Returns a translation item by its country code. }
    function ItemByCountryCode(
      const ACountryCode: ShortString): TBrookMessageClass;
    { Adds a translation item. }
    procedure Add(AClass: TBrookMessageClass);
    { Removes a translation item. }
    procedure Remove(AClass: TBrookMessageClass);
    { The list of translation items. }
    property Items[const AIndex: Integer]: TBrookMessageClass read GetItems
      write SetItems;
  end;

var
  { Error msgs }
  { }
  SBrookInvalidRequestMethodError: string = ES;
  SBrookItemNotFoundError: string = ES;
  SBrookFileNotFoundError: string = ES;
  SBrookNoRequestMethodError: string = ES;
  SBrookNoApplicationRegisteredError: string = ES;
  SBrookApplicationAlreadyRegisteredError: string = ES;
  SBrookRegiterTBrookActionError: string = ES;
  SBrookActionAlreadyRegisteredError: string = ES;
  SBrookDefaultActionAlreadyRegisteredError: string = ES;
  SBrookPatternAlreadyRegisteredError: string = ES;
  SBrookRouterServiceAlreadyRegisteredError: string = ES;
  SBrookNoRouterServiceRegisteredError: string = ES;
  SBrookMessagesServiceAlreadyRegisteredError: string = ES;
  SBrookNoMessagesServiceRegisteredError: string = ES;
  SBrookNoRouteRegisteredError: string = ES;
  SBrookCfgFileNotFoundError: string = ES;
  SBrookNilJSONParamError: string = ES;
  SBrookNilParamError: string = ES;
  SBrookDataBaseServiceAlreadyRegisteredError: string = ES;
  SBrookNoDataBaseServiceRegisteredError: string = ES;
  SBrookNoQueryBrokerClassRegisteredError: string = ES;
  SBrookNoQueryInstantiatedError: string = ES;
  SBrookEmptyTableNameError: string = ES;
  SBrookEmptyLibraryNameError: string = ES;
  SBrookEmptyCountryCodeError: string = ES;
  SBrookMethodNotAllowedError: string = ES;
  SBrookIncompatibleTypesError: string = ES;
  SBrookConstraintAlreadyRegisteredError: string = ES;
  SBrookConstraintsServiceAlreadyRegisteredError: string = ES;
  SBrookNoConstraintsServiceRegisteredError: string = ES;

{ Translates the framework messages acoording to the default language. }
procedure BrookSetDefaultLanguage;

implementation

resourcestring
{$i brookmessages.inc}

var
  _BrookMessagesService: TBrookMessages = nil;
  _BrookMessagesServiceClass: TBrookMessagesClass = nil;

procedure BrookSetDefaultLanguage;
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
  SBrookNoQueryInstantiatedError := SBrookNoQueryInstantiatedError_rst;
  SBrookNoDataBaseServiceRegisteredError := SBrookNoDataBaseServiceRegisteredError_rst;
  SBrookNoQueryBrokerClassRegisteredError :=
    SBrookNoQueryBrokerClassRegisteredError_rst;
  SBrookEmptyTableNameError := SBrookEmptyTableNameError_rst;
  SBrookEmptyLibraryNameError := SBrookEmptyLibraryNameError_rst;
  SBrookEmptyCountryCodeError := SBrookEmptyCountryCodeError_rst;
  SBrookMethodNotAllowedError := SBrookMethodNotAllowedError_rst;
  SBrookIncompatibleTypesError := SBrookIncompatibleTypesError_rst;
  SBrookConstraintAlreadyRegisteredError := SBrookConstraintAlreadyRegisteredError_rst;
  SBrookConstraintsServiceAlreadyRegisteredError := SBrookConstraintsServiceAlreadyRegisteredError_rst;
  SBrookNoConstraintsServiceRegisteredError := SBrookNoConstraintsServiceRegisteredError_rst;
end;

{ TBrookMessage }

class procedure TBrookMessage.Register;
begin
  TBrookMessages.Service.Add(Self);
end;

{ TBrookMessages }

constructor TBrookMessages.Create;
begin
  FList := TFPList.Create;
end;

destructor TBrookMessages.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

class procedure TBrookMessages.RegisterService;
begin
  if Assigned(_BrookMessagesServiceClass) then
    raise EBrookMessages.Create(Self, SBrookMessagesServiceAlreadyRegisteredError);
  _BrookMessagesServiceClass := Self;
end;

class procedure TBrookMessages.UnregisterService;
begin
  FreeAndNil(_BrookMessagesService);
  _BrookMessagesServiceClass := nil;
end;

class function TBrookMessages.Service: TBrookMessages;
begin
  if not Assigned(_BrookMessagesService) then
  begin
    if not Assigned(_BrookMessagesServiceClass) then
      raise EBrookMessages.Create(Self, SBrookNoMessagesServiceRegisteredError);
    _BrookMessagesService := _BrookMessagesServiceClass.Create;
  end;
  Result := _BrookMessagesService;
end;

function TBrookMessages.GetItems(const AIndex: Integer): TBrookMessageClass;
begin
  Result := TBrookMessageClass(FList.Items[AIndex]);
end;

procedure TBrookMessages.SetItems(const AIndex: Integer;
  AValue: TBrookMessageClass);
begin
  FList.Items[AIndex] := AValue;
end;

function TBrookMessages.Count: Integer;
begin
  Result := FList.Count;
end;

procedure TBrookMessages.Add(AClass: TBrookMessageClass);
begin
  FList.Add(AClass);
end;

procedure TBrookMessages.Remove(AClass: TBrookMessageClass);
begin
  FList.Remove(AClass);
end;

procedure TBrookMessages.SetLanguage(const ACountryCode: ShortString);
begin
  ItemByCountryCode(ACountryCode).Translate;
end;

function TBrookMessages.Find(
  const ACountryCode: ShortString): TBrookMessageClass;
var
  I: Integer;
begin
  for I := 0 to Pred(FList.Count) do
  begin
    Result := Items[I];
    if SameText(Result.CountryCode, ACountryCode) then
      Exit;
  end;
  Result := nil;
end;

function TBrookMessages.ItemByCountryCode(
  const ACountryCode: ShortString): TBrookMessageClass;
begin
  if ACountryCode = ES then
    raise EBrookMessages.Create(Self, SBrookEmptyCountryCodeError);
  Result := Find(ACountryCode);
  if not Assigned(Result) then
    raise EBrookMessages.CreateFmt(Self, SBrookItemNotFoundError, [ACountryCode]);
end;

initialization
  TBrookMessages.RegisterService;
  BrookSetDefaultLanguage;

finalization
  TBrookMessages.UnregisterService;

end.

(*
  Brook framework, Router Handler Classe

  Copyright (C) 2014 Silvio Clecio

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit BrookRouterHandler;

{$i brook.inc}

interface

uses
  BrookClasses, BrookRouter, BrookAction, BrookHttpDefs, BrookUtils, Classes;

type
  { Handles the router events. }
  TBrookRouterHandler = class(TBrookComponent)
  private
    FOldAfterExecuteAction: TBrookExecuteActionEvent;
    FOldAfterMatchPattern: TBrookMatchPatternEvent;
    FOldAfterRoute: TBrookRouteEvent;
    FOldBeforeExecuteAction: TBrookExecuteActionEvent;
    FOldBeforeMatchPattern: TBrookMatchPatternEvent;
    FOldBeforeRoute: TBrookRouteEvent;
    FAfterExecuteAction: TBrookExecuteActionEvent;
    FAfterMatchPattern: TBrookMatchPatternEvent;
    FAfterRoute: TBrookRouteEvent;
    FBeforeExecuteAction: TBrookExecuteActionEvent;
    FBeforeMatchPattern: TBrookMatchPatternEvent;
    FBeforeRoute: TBrookRouteEvent;
  protected
    procedure DoAfterExecuteAction(ASender: TObject; AAction: TBrookAction;
      ARequest: TBrookRequest; AResponse: TBrookResponse; ARoute: TBrookRoute;
      var AHandled: Boolean); virtual;
    function DoAfterMatchPattern(ASender: TObject; APattern,
      APathInfo: string; out ARedirect: Boolean; out ANames,
      AValues: TBrookArrayOfString; var AHandled: Boolean): Boolean; virtual;
    procedure DoAfterRoute(ASender: TObject; ARequest: TBrookRequest;
      AResponse: TBrookResponse; var AHandled: Boolean); virtual;
    procedure DoBeforeExecuteAction(ASender: TObject; AAction: TBrookAction;
      ARequest: TBrookRequest; AResponse: TBrookResponse; ARoute: TBrookRoute;
      var AHandled: Boolean); virtual;
    function DoBeforeMatchPattern(ASender: TObject; APattern, APathInfo: string;
      out ARedirect: Boolean; out ANames, AValues: TBrookArrayOfString;
      var AHandled: Boolean): Boolean; virtual;
    procedure DoBeforeRoute(ASender: TObject; ARequest: TBrookRequest;
      AResponse: TBrookResponse; var AHandled: Boolean); virtual;
  public
    { Creates an instance of a @link(TBrookRouterHandler) class. }
    constructor Create(AOwner: TComponent); override;
    { Frees an instance of @link(TBrookRouterHandler) class. }
    destructor Destroy; override;
    { Binds the router events in this class allowing external handling. }
    procedure BindEvents; virtual;
    { Unbinds the router events from this class. }
    procedure UnbindEvents; virtual;
  published
    { Is triggered after the router executes a action. }
    property AfterExecuteAction: TBrookExecuteActionEvent
      read FAfterExecuteAction write FAfterExecuteAction;
    { Is triggered after the router matches a pattern. }
    property AfterMatchPattern: TBrookMatchPatternEvent
      read FAfterMatchPattern write FAfterMatchPattern;
    { Is triggered after the router is routing. }
    property AfterRoute: TBrookRouteEvent read FAfterRoute
      write FAfterRoute;
    { Is triggered before the router executes a action. }
    property BeforeExecuteAction: TBrookExecuteActionEvent
      read FBeforeExecuteAction write FBeforeExecuteAction;
    { Is triggered before the router matches a pattern. }
    property BeforeMatchPattern: TBrookMatchPatternEvent
      read FBeforeMatchPattern write FBeforeMatchPattern;
    { Is triggered before the router is routing. }
    property BeforeRoute: TBrookRouteEvent read FBeforeRoute
      write FBeforeRoute;
  end;

implementation

{ TBrookRouterHandler }

constructor TBrookRouterHandler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BindEvents;
end;

destructor TBrookRouterHandler.Destroy;
begin
  UnbindEvents;
  inherited Destroy;
end;

procedure TBrookRouterHandler.BindEvents;
var
  VService: TBrookRouter;
begin
  VService := TBrookRouter.Service;
  if Assigned(VService.AfterExecuteAction) then
    FOldAfterExecuteAction := VService.AfterExecuteAction;
  VService.AfterExecuteAction := @DoAfterExecuteAction;
  if Assigned(VService.AfterMatchPattern) then
    FOldAfterMatchPattern := VService.AfterMatchPattern;
  VService.AfterMatchPattern := @DoAfterMatchPattern;
  if Assigned(VService.AfterRoute) then
    FOldAfterRoute := VService.AfterRoute;
  VService.AfterRoute := @DoAfterRoute;
  if Assigned(VService.BeforeExecuteAction) then
    FOldBeforeExecuteAction := VService.BeforeExecuteAction;
  VService.BeforeExecuteAction := @DoBeforeExecuteAction;
  if Assigned(VService.BeforeMatchPattern) then
    FOldBeforeMatchPattern := VService.BeforeMatchPattern;
  VService.BeforeMatchPattern := @DoBeforeMatchPattern;
  if Assigned(VService.BeforeRoute) then
    FOldBeforeRoute := VService.BeforeRoute;
  VService.BeforeRoute := @DoBeforeRoute;
end;

procedure TBrookRouterHandler.UnbindEvents;
var
  VService: TBrookRouter;
begin
  VService := TBrookRouter.Service;
  if Assigned(FOldAfterExecuteAction) then
  begin
    VService.AfterExecuteAction := FOldAfterExecuteAction;
    FOldAfterExecuteAction := nil;
  end;
  if Assigned(FOldAfterMatchPattern) then
  begin
    VService.AfterMatchPattern := FOldAfterMatchPattern;
    FOldAfterMatchPattern := nil;
  end;
  if Assigned(FOldAfterRoute) then
  begin
    VService.AfterRoute := FOldAfterRoute;
    FOldAfterRoute := nil;
  end;
  if Assigned(FOldBeforeExecuteAction) then
  begin
    VService.BeforeExecuteAction := FOldBeforeExecuteAction;
    FOldBeforeExecuteAction := nil;
  end;
  if Assigned(FOldBeforeMatchPattern) then
  begin
    VService.BeforeMatchPattern := FOldBeforeMatchPattern;
    FOldBeforeMatchPattern := nil;
  end;
  if Assigned(FOldBeforeRoute) then
  begin
    VService.BeforeRoute := FOldBeforeRoute;
    FOldBeforeRoute := nil;
  end;
end;

procedure TBrookRouterHandler.DoAfterExecuteAction(ASender: TObject;
  AAction: TBrookAction; ARequest: TBrookRequest; AResponse: TBrookResponse;
  ARoute: TBrookRoute; var AHandled: Boolean);
begin
  try
    if Assigned(FAfterExecuteAction) then
      FAfterExecuteAction(ASender, AAction, ARequest, AResponse, ARoute,
        AHandled);
  finally
    if Assigned(FOldAfterExecuteAction) then
      FOldAfterExecuteAction(ASender, AAction, ARequest, AResponse, ARoute,
        AHandled);
  end;
end;

function TBrookRouterHandler.DoAfterMatchPattern(ASender: TObject; APattern,
  APathInfo: string; out ARedirect: Boolean; out ANames,
  AValues: TBrookArrayOfString; var AHandled: Boolean): Boolean;
begin
  Result := Assigned(FAfterMatchPattern);
  try
    if Result then
      Result := FAfterMatchPattern(ASender, APattern, APathInfo, ARedirect,
        ANames, AValues, AHandled);
  finally
    if Assigned(FOldAfterMatchPattern) then
      FOldAfterMatchPattern(ASender, APattern, APathInfo, ARedirect, ANames,
        AValues, AHandled);
  end;
end;

procedure TBrookRouterHandler.DoAfterRoute(ASender: TObject;
  ARequest: TBrookRequest; AResponse: TBrookResponse; var AHandled: Boolean);
begin
  try
    if Assigned(FAfterRoute) then
      FAfterRoute(ASender, ARequest, AResponse, AHandled);
  finally
    if Assigned(FOldAfterRoute) then
      FOldAfterRoute(ASender, ARequest, AResponse, AHandled);
  end;
end;

procedure TBrookRouterHandler.DoBeforeExecuteAction(ASender: TObject;
  AAction: TBrookAction; ARequest: TBrookRequest; AResponse: TBrookResponse;
  ARoute: TBrookRoute; var AHandled: Boolean);
begin
  try
    if Assigned(FBeforeExecuteAction) then
      FBeforeExecuteAction(ASender, AAction, ARequest, AResponse, ARoute,
        AHandled);
  finally
    if Assigned(FOldBeforeExecuteAction) then
      FOldBeforeExecuteAction(ASender, AAction, ARequest, AResponse, ARoute,
        AHandled);
  end;
end;

function TBrookRouterHandler.DoBeforeMatchPattern(ASender: TObject; APattern,
  APathInfo: string; out ARedirect: Boolean; out ANames,
  AValues: TBrookArrayOfString; var AHandled: Boolean): Boolean;
begin
  Result := Assigned(FBeforeMatchPattern);
  try
    if Result then
      Result := FBeforeMatchPattern(ASender, APattern, APathInfo, ARedirect,
        ANames, AValues, AHandled);
  finally
    if Assigned(FOldBeforeMatchPattern) then
      FOldBeforeMatchPattern(ASender, APattern, APathInfo, ARedirect, ANames,
        AValues, AHandled);
  end;
end;

procedure TBrookRouterHandler.DoBeforeRoute(ASender: TObject;
  ARequest: TBrookRequest; AResponse: TBrookResponse; var AHandled: Boolean);
begin
  try
    if Assigned(FBeforeRoute) then
      FBeforeRoute(ASender, ARequest, AResponse, AHandled);
  finally
    if Assigned(FOldBeforeRoute) then
      FOldBeforeRoute(ASender, ARequest, AResponse, AHandled);
  end;
end;

end.

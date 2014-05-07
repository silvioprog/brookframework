(*
  Brook framework, Middleware Classes

  Copyright (C) 2014 Silvio Clecio

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit BrookMiddleware;

{$i brook.inc}

interface

uses
  BrookClasses, BrookAction, BrookRouter, BrookException, HTTPDefs;

type
  { Handles exceptions for @link(TBrookMiddleware). }
  EBrookMiddleware = class(EBrook);

  { Is a metaclass for @link(TBrookMiddleware) class. }
  TBrookMiddlewareClass = class of TBrookMiddleware;

  { Intermediates two classes through a @code(TBrookExecuteActionEvent) event. }
  TBrookMiddleware = class(TBrookObject)
  private
    FOldExecute: TBrookExecuteActionEvent;
  protected
    procedure DoExecute(ASender: TObject; AAction: TBrookAction;
      ARequest: TRequest; AResponse: TResponse; ARoute: TBrookRoute;
      var AHandled: Boolean); virtual;
  public
    { Creates an instance of a @link(TBrookMiddleware) class. }
    constructor Create(ABoundEvent: PBrookExecuteActionEvent); virtual;
    { Is triggered when the @code(TBrookExecuteActionEvent) event bound in this
      class is executed. }
    procedure Execute({%H-}ASender: TObject;{%H-}AAction: TBrookAction;
      {%H-}ARoute: TBrookRoute); virtual;
    { Bindes a @code(TBrookExecuteActionEvent) event to this class keeping the
      implementation of a previously declared event. }
    procedure BindExecution(AEvent: PBrookExecuteActionEvent);
  end;

implementation

{ TBrookCustomMiddleware }

constructor TBrookMiddleware.Create(ABoundEvent: PBrookExecuteActionEvent);
begin
  inherited Create;
  BindExecution(ABoundEvent);
end;

procedure TBrookMiddleware.DoExecute(ASender: TObject;
  AAction: TBrookAction; ARequest: TRequest; AResponse: TResponse;
  ARoute: TBrookRoute; var AHandled: Boolean);
begin
  Execute(ASender, AAction, ARoute);
  if Assigned(FOldExecute) then
    FOldExecute(ASender, AAction, ARequest, AResponse, ARoute, AHandled);
end;

procedure TBrookMiddleware.Execute(ASender: TObject; AAction: TBrookAction;
  ARoute: TBrookRoute);
begin
end;

procedure TBrookMiddleware.BindExecution(AEvent: PBrookExecuteActionEvent);
begin
  FOldExecute := AEvent^;
  AEvent^ := @DoExecute;
end;

end.


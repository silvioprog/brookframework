unit ConfigClient;

{$mode objfpc}{$H+}

interface

uses
  Forms,
  SysUtils,
  BrookConfigurator,
  Classes;

type

  { TConfigClient }

  TConfigClient = class(TObject)
   private
    Fappname: string;
    Fhost: string;
    Fport: integer;
    FCfg : TBrookConfigurator;
    function Configuration : String;
    function getappname: string;
    function gethost: string;
    function getport: integer;
    procedure Setappname(AValue: string);
    procedure Sethost(AValue: string);
    procedure Setport(AValue: integer);
   public
    constructor create;
    destructor destroy; override;
   published
    property host : string read gethost write Sethost;
    property port : integer read getport write Setport;
    property appname : string read getappname write Setappname;
  end;

implementation

{ TConfigClient }

function TConfigClient.Configuration: String;
var
  FApp : String;
begin
  FApp       := ExtractFileName( ChangeFileExt( Application.ExeName, '.cfg') );
  result     := format('%s%s',[ ExtractFilePath( Application.ExeName ), FApp]);
end;

function TConfigClient.getappname: string;
begin
  if (fappname = '') then
  fappname := 'addressbook';
  result   := fappname;
end;

function TConfigClient.gethost: string;
begin
   if (fhost = '') then
   fhost := '127.0.0.1';
   result   := fhost;
end;

function TConfigClient.getport: integer;
begin
   if (fport = 0) then
   fport := 80;
   result   := fport;
end;

procedure TConfigClient.Setappname(AValue: string);
begin
  if Fappname = AValue then Exit;
  Fappname := AValue;
end;

procedure TConfigClient.Sethost(AValue: string);
begin
  if Fhost = AValue then Exit;
  Fhost := AValue;
end;

procedure TConfigClient.Setport(AValue: integer);
begin
  if Fport = AValue then Exit;
  Fport := AValue;
end;

constructor TConfigClient.create;
begin
  inherited;
  FCfg := TBrookConfigurator.Create;
  FCfg.Configuration  := Configuration;
  FCfg.Load;
  FCfg.Target := Self;
  FCfg.Configure;
end;

destructor TConfigClient.destroy;
begin
  FCfg.Free;
  inherited destroy;
end;

end.


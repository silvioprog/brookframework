unit testbrookapplication;

{$mode objfpc}{$H+}

interface

uses
  BrookApplication, BrookClasses, fpcunit, testregistry;

type

  { TApp }

  TApp = class
  private
    FTest: Boolean;
  public
    property Test: Boolean read FTest write FTest;
  end;

  { TBrokerApp }

  TBrokerApp = class(TBrookInterfacedObject, IBrookApplication)
  private
    FApp: TApp;
  public
    constructor Create;
    destructor Destroy; override;
    function GetTerminated: Boolean;
    procedure Terminate;
    function Instance: TObject;
    procedure Run;
  end;

  { TTestBrookApplication }

  TTestBrookApplication = class(TTestCase)
  published
    procedure TestInstance;
    procedure TestRun;
  end;

implementation

{ TBrokerApp }

constructor TBrokerApp.Create;
begin
  FApp := TApp.Create;
end;

destructor TBrokerApp.Destroy;
begin
  FApp.Free;
  inherited Destroy;
end;

function TBrokerApp.GetTerminated: Boolean;
begin
  Result := False;
end;

procedure TBrokerApp.Terminate;
begin
end;

function TBrokerApp.Instance: TObject;
begin
  Result := FApp;
end;

procedure TBrokerApp.Run;
begin
  FApp.Test := True;
end;

{ TTestBrookApplication }

procedure TTestBrookApplication.TestInstance;
begin
  AssertTrue('Invalid instance', BrookApp.Instance.ClassName = 'TApp');
end;

procedure TTestBrookApplication.TestRun;
var
  ap: TObject;
begin
  BrookApp.Run;
  ap := BrookApp.Instance;
  AssertTrue('No running', (ap is TApp) and TApp(ap).Test);
end;

initialization
  BrookRegisterApp(TBrokerApp.Create);
  RegisterTest(TTestBrookApplication);

end.


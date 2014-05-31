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
    FTerminated: Boolean;
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
    procedure TestTerminate;
    procedure GetTerminated;
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
  Result := FTerminated;
end;

procedure TBrokerApp.Terminate;
begin
  FTerminated := True;
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

procedure TTestBrookApplication.TestTerminate;
begin
  BrookApp.Terminate;
end;

procedure TTestBrookApplication.GetTerminated;
begin
  AssertTrue(BrookApp.Terminated);
end;

initialization
  BrookRegisterApp(TBrokerApp.Create);
  RegisterTest(TTestBrookApplication);

end.


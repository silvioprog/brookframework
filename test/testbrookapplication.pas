unit testbrookapplication;

{$mode objfpc}{$H+}

interface

uses
  BrookApplication, BrookClasses, fpcunit, testregistry;

type
  TApp = class
  private
    FTest: Boolean;
  public
    property Test: Boolean read FTest write FTest;
  end;

  TBrokerApp = class(TBrookInterfacedObject, IBrookApplication)
  private
    FApp: TApp;
  public
    constructor Create;
    destructor Destroy; override;
    function Instance: TObject;
    procedure Run;
  end;

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
  VApp: TObject;
begin
  BrookApp.Run;
  VApp := BrookApp.Instance;
  AssertTrue('No running', (VApp is TApp) and TApp(VApp).Test);
end;

initialization
  BrookRegisterApp(TBrokerApp.Create);
  RegisterTest(TTestBrookApplication);

end.


unit PingThread;

interface

uses Classes, PingSend, IPUtils;

type
     PPingResult = ^TPingResult;
     TPingResult = Record
                     IPAdress:String;
                     Exists:Boolean;
                   end;


type
  TPingThread = class(TThread)
  private
    { Private declarations }
  protected
    procedure Execute; override;
  public
    PingResult:TPingResult;
    Ready:Boolean;
    constructor Create(Ping:TPingResult);
  end;

implementation

{ TPingThread }

constructor TPingThread.Create(Ping:TPingResult);
begin
  PingResult.IPAdress := Ping.IPAdress;
  inherited Create(False);
end;

procedure TPingThread.Execute;
var Ping:TPingSend;
begin
  Ready := false;
  Ping  := TPingSend.Create;
  Ping.Timeout := 2000;
  PingResult.Exists := Ping.Ping(PingResult.IPAdress);
  Ping.Free;
  Ready := true;
end;

end.

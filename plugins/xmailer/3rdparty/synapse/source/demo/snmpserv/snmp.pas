unit snmp;

interface

uses
  Classes, blcksock, winsock, Synautil, SysUtils, SNMPSend, asn1util;

type
  TUDPSnmpDaemon = class(TThread)
  private
    Sock:TUDPBlockSocket;
    snmprec: TSNMPRec;
  public
    Constructor Create;
    Destructor Destroy; override;
    procedure Execute; override;
    procedure ProcessSnmpRequest(PDU: integer; var OID, Value: string;
      var valuetype: integer);
  end;


implementation

{ TUDPSnmpDaemon }

Constructor TUDPSnmpDaemon.Create;
begin
  sock:=TUDPBlockSocket.create;
  snmprec := TSNMPRec.create;
  FreeOnTerminate:=true;
  inherited create(false);
end;

Destructor TUDPSnmpDaemon.Destroy;
begin
  Sock.free;
  snmprec.free;
  inherited Destroy;
end;

procedure TUDPSnmpDaemon.Execute;
var
  Buf: string;
  n: integer;
  mib: TSNMPMib;
  oid, value: string;
  valuetype: integer;
begin
  with sock do
    begin
      bind('0.0.0.0','161');
      if sock.LastError<>0 then
        exit;
      repeat
        if terminated then break;
      	buf := sock.RecvPacket(1000);
      	if sock.lasterror = 0 then
    	  begin
          snmprec.Clear;
          snmprec.DecodeBuf(buf);
          for n := 0 to snmprec.MIBCount - 1 do
          begin
            mib := snmprec.MIBByIndex(n);
            if mib <> nil then
            begin
              oid := mib.OID;
              value := mib.Value;
              valuetype := mib.valuetype;
              ProcessSnmpRequest(snmprec.PDUType, oid, value, valuetype);
              mib.OID := oid;
              mib.Value := value;
              mib.valuetype := valuetype;
            end;
          end;
          snmprec.PDUType := PDUGetResponse;
          snmprec.ErrorStatus := 0;
          Buf := snmprec.EncodeBuf;
          sock.SendString(Buf);
        end;
      until false;
    end;
end;

procedure TUDPSnmpDaemon.ProcessSnmpRequest(PDU: integer; var OID, Value: string;
  var valuetype: integer);
begin
  if PDU = PDUGetRequest then
  begin
    if OID = '1.3.6.1.2.1.1.1.0' then
    begin
      Value := 'Synapse SNMP agent demo';
      Valuetype := ASN1_OCTSTR;
    end;
  end;
end;

end.

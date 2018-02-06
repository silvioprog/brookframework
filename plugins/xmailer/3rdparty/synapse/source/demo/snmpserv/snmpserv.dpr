program snmpserv;

uses
  Forms,
  main in 'main.pas' {Form1},
  snmp in 'snmp.pas',
  SNMPSend in 'SNMPSend.pas',
  ASN1Util in 'ASN1Util.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

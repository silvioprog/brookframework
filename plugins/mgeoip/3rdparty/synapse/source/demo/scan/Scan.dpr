program Scan;

{$APPTYPE CONSOLE}

uses SysUtils, IPUtils, PingThread;

var i,j:Cardinal;
    Ping:Array of TPingResult;
    PingCount,Cardinal1,Cardinal2:Cardinal;
    Puffer:String;
    ThreadArray:Array of TPingThread;
    ThreadsComplete:Boolean;
begin
  WriteLn;
  WriteLn('Scan v1.0');
  WriteLn('Synapse Demo Application');
  WriteLn('(c)2003 by Christian Brosius');
  WriteLn;
  if (ParamCount = 2)and // Parse Commandline
     (IsIPAdress(ParamStr(1)))and
     (IsIPAdress(ParamStr(2)))
    then
      begin
        Cardinal1 := IPToCardinal(StrToIP(ParamStr(1)));
        Cardinal2 := IPToCardinal(StrToIP(ParamStr(2)));
        // Count of Adresses to ping
        PingCount := (Cardinal2 - Cardinal1) + 1;

        // Show Adresscount to User
        Write('Pinging ' + IntToStr(PingCount) + ' Adresses');
        // Initialize dyn. Arrays
        SetLength(Ping,PingCount);
        SetLength(ThreadArray,PingCount);
        j := 0;
        for i := Cardinal1 to Cardinal2 do
          begin
            Ping[j].IPAdress  := IPToStr(CardinalToIP(i));
            Ping[j].Exists    := false;
            Inc(j);
          end;

        // Create one Thread for each Ping
        for i := 0 to PingCount-1 do
          begin
            ThreadArray[i] := TPingThread.Create(Ping[i]);
          end;

        Write(' ');

        // Wait till all threads are executed
        repeat
          ThreadsComplete := true;
          Write('.');
          Sleep(1000);
          for i := 0 to PingCount-1 do
            begin
              if not ThreadArray[i].Ready
                then
                  begin
                    ThreadsComplete := false;
                    break;
                  end;
            end;
        until ThreadsComplete;

        WriteLn;
        WriteLn;

        // Show Results to User
        for i := 0 to PingCount-1 do
          begin
            if ThreadArray[i].PingResult.Exists
              then
                begin
                  Puffer := IntToStr(i+1) + '  ' + ThreadArray[i].PingResult.IPAdress;
                  WriteLn(Puffer);
                end;
          end;

        // Free Threads
        for i := 0 to PingCount-1 do
          begin
            ThreadArray[i].Free;
          end;
      end
    else
      begin
        WriteLn('Syntax:  Scan StartIP StopIP');
        WriteLn;
        WriteLn('Description:');
        WriteLn('  With Scan you can do a very fast scan of Adresses on your Network-Segment.');
        WriteLn;
        WriteLn('Example:  scan 192.168.50.1 192.168.50.254');
      end;
end.

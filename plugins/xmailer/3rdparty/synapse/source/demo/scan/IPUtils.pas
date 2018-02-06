unit IPUtils;

interface

uses SysUtils;

type TIPAdresse = record
                    Oct1,
                    Oct2,
                    Oct3,
                    Oct4:Byte;
                   end;

function StrToIP(const Value:String):TIPAdresse;
function IPToStr(const Adresse:TIPAdresse):String;
function IPToCardinal(const Adresse:TIPAdresse):Cardinal;
function CardinalToIP(const Value:Cardinal):TIPAdresse;
function IsIPAdress(const Value:String):Boolean;

implementation

// IPAdresse in Cardinal umwandeln
function IPToCardinal(const Adresse:TIPAdresse):Cardinal;
begin
  Result :=  (Adresse.Oct1*16777216)
            +(Adresse.Oct2*65536)
            +(Adresse.Oct3*256)
            +(Adresse.Oct4);
end;

// Cardinal in IP-Adresse umwandeln
function CardinalToIP(const Value:Cardinal):TIPAdresse;
begin
  Result.Oct1 := Value div 16777216;
  Result.Oct2 := Value div 65536;
  Result.Oct3 := Value div 256;
  Result.Oct4 := Value mod 256;
end;

// IP-Adresse in String umwandeln
function IPToStr(const Adresse:TIPAdresse):String;
begin
  Result := IntToStr(Adresse.Oct1) + '.' +
            IntToStr(Adresse.Oct2) + '.' +
            IntToStr(Adresse.Oct3) + '.' +
            IntToStr(Adresse.Oct4);
end;

function StrToIP(const Value:String):TIPAdresse;
var n,x: Integer;
    Posi:Array[1..4]of Integer;
    Oktet:Array[1..4]of String;
begin
  x := 0;
  // es dürfen nur Zahlen und Punkte vorhanden sein
  for n := 1 to Length(Value) do
    begin
      // Zähle die Punkte
      if Value[n] = '.'
        then
          begin
            Inc(x);
            Posi[x] := n;
          end
        else Oktet[x+1] := Oktet[x+1] + Value[n];
    end;
  Result.Oct1 := StrToInt(Oktet[1]);
  Result.Oct2 := StrToInt(Oktet[2]);
  Result.Oct3 := StrToInt(Oktet[3]);
  Result.Oct4 := StrToInt(Oktet[4]);
end;

function IsIPAdress(const Value:String):Boolean;
var n,x,i: Integer;
    Posi:Array[1..4]of Integer;
    Oktet:Array[1..4]of String;
begin
  Result := true;
  x := 0;

  // es dürfen nur Zahlen und Punkte vorhanden sein
  for n := 1 to Length(Value) do
    if not (Value[n] in ['0'..'9','.'])
      then
        begin
          // ungültiges Zeichen -> keine IP-Adresse
          Result := false;
          break;
        end
      else
        begin
          // Zähle die Punkte
          if Value[n] = '.'
            then
              begin
                Inc(x);
                Posi[x] := n;
              end
            else
              begin
                 Oktet[x+1] := Oktet[x+1] + Value[n];
              end;
        end;

  for i := 1 to 4 do
    if (StrToInt(Oktet[i])>255)then Result := false;

  // es müssen genau 3 Punkte vorhanden sein
  if x <> 3
    then
      begin
        // Anzahl der Punkte <> 3 -> keine IP-Adresse
        Result := false;
      end;
end;

end.

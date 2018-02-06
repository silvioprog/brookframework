{$MODE DELPHI}

Program testmime;

uses
  mimepart, classes;

type
  Tc = class(TObject)
  public
    class procedure ph(const Sender: TMimePart);
  end;
  
class procedure Tc.ph(const Sender: TMimePart);
begin
  Sender.DecodePart;
  Sender.EncodePart;
end;

var
  l: tstringlist;
  m:tmimepart;
begin
  l := TStringList.create;
  m := tmimepart.create;
  try
    m.OnWalkPart:=tc.ph;
    m.Lines.LoadFromFile(paramstr(1));
    m.DecomposeParts;
    m.WalkPart;
    m.ComposeParts;
    m.Lines.SaveToFile(paramstr(1) + '.repack');
  finally
    m.free;
    l.free;
  end;
end.


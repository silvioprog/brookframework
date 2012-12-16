(*
  Brook New Plugin unit.

  Copyright (C) 2012 Silvio Clecio.

  http://brookframework.org

  All contributors:
  Plase see the file CONTRIBUTORS.txt, included in this
  distribution.

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit frmBrookNewPlugin;

{$i brook.inc}

interface

uses
  Forms, ExtCtrls, Buttons, Controls;

type
  TfrBrookNewPlugin = class(TForm)
    btOK: TBitBtn;
    btCancel: TBitBtn;
    pnCliente: TPanel;
    pnBottom: TPanel;
    rgType: TRadioGroup;
  public
    class function Execute: Integer;
  end;

implementation

{$R *.lfm}

class function TfrBrookNewPlugin.Execute: Integer;
begin
  with Self.Create(nil) do
    try
      Result := -1;
      ShowModal;
      if ModalResult <> mrOK then
        Exit;
      Result := rgType.ItemIndex;
    finally
      Free;
    end;
end;

end.


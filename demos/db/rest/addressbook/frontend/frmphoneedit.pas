unit frmPhoneEdit;

{$mode objfpc}{$H+}

interface

uses
  frmCustomEdit, ExtCtrls, Buttons, StdCtrls;

type
  TfrPhoneEdit = class(TfrCustomEdit)
    input_number: TEdit;
    lbNumber: TLabel;
  end;

implementation

{$R *.lfm}

end.


object Form1: TForm1
  Left = 535
  Top = 430
  Width = 314
  Height = 251
  Caption = 'Simple TFTP-Client'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    306
    224)
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 8
    Top = 146
    Width = 65
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'TargetSystem'
  end
  object Label3: TLabel
    Left = 199
    Top = 146
    Width = 50
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'TargetPort'
  end
  object Label4: TLabel
    Left = 8
    Top = 170
    Width = 76
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'FileName to Get'
  end
  object Log: TMemo
    Left = 8
    Top = 8
    Width = 289
    Height = 129
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object BExit: TButton
    Left = 222
    Top = 194
    Width = 55
    Height = 23
    Anchors = [akBottom]
    Caption = 'E&xit'
    TabOrder = 1
    OnClick = BExitClick
  end
  object BAbout: TButton
    Left = 27
    Top = 194
    Width = 55
    Height = 23
    Anchors = [akBottom]
    Caption = '&About'
    TabOrder = 2
    OnClick = BAboutClick
  end
  object TargetSystemEdit: TEdit
    Left = 88
    Top = 143
    Width = 104
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 3
    Text = '127.0.0.1'
  end
  object TargetPortEdit: TEdit
    Left = 255
    Top = 143
    Width = 41
    Height = 21
    Anchors = [akRight, akBottom]
    TabOrder = 4
    Text = '69'
  end
  object TargetFileEdit: TEdit
    Left = 88
    Top = 167
    Width = 208
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 5
  end
  object BGetFile: TButton
    Left = 94
    Top = 194
    Width = 55
    Height = 23
    Anchors = [akBottom]
    Caption = '&Get File'
    TabOrder = 6
    OnClick = BGetFileClick
  end
  object BPutFile: TButton
    Left = 158
    Top = 194
    Width = 55
    Height = 23
    Anchors = [akBottom]
    Caption = '&Put File'
    TabOrder = 7
    OnClick = BPutFileClick
  end
  object OpenDialog: TOpenDialog
    FilterIndex = 0
    Title = 'Select File to put ...'
    Left = 16
    Top = 16
  end
  object SaveDialog: TSaveDialog
    Title = 'Save File ...'
    Left = 48
    Top = 16
  end
end

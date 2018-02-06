object MainForm: TMainForm
  Left = 276
  Top = 417
  Width = 503
  Height = 270
  Caption = 'Simple TFTP-Server'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    495
    238)
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 10
    Top = 199
    Width = 30
    Height = 16
    Anchors = [akLeft, akBottom]
    Caption = 'Path:'
  end
  object Log: TMemo
    Left = 8
    Top = 0
    Width = 481
    Height = 186
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object BExit: TButton
    Left = 483
    Top = 261
    Width = 86
    Height = 31
    Anchors = [akBottom]
    Caption = 'E&xit'
    TabOrder = 1
    OnClick = BExitClick
  end
  object BAbout: TButton
    Left = 37
    Top = 261
    Width = 86
    Height = 31
    Anchors = [akBottom]
    Caption = '&About'
    TabOrder = 2
    OnClick = BAboutClick
  end
  object PathEdit: TEdit
    Left = 72
    Top = 196
    Width = 413
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 3
    Text = 'C:\'
  end
end

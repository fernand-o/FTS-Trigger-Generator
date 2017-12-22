object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Full Text Search Trigger Generator'
  ClientHeight = 637
  ClientWidth = 986
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 59
    Width = 31
    Height = 13
    Caption = 'Fields:'
  end
  object MemoFields: TMemo
    Left = 8
    Top = 80
    Width = 313
    Height = 549
    Lines.Strings = (
      'MemoFields')
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object EditTable: TLabeledEdit
    Left = 8
    Top = 32
    Width = 177
    Height = 21
    EditLabel.Width = 30
    EditLabel.Height = 13
    EditLabel.Caption = 'Table:'
    TabOrder = 1
  end
  object Button1: TButton
    Left = 327
    Top = 240
    Width = 74
    Height = 25
    Caption = 'Generate'
    TabOrder = 2
    OnClick = Button1Click
  end
  object MemoResults: TMemo
    Left = 423
    Top = 0
    Width = 563
    Height = 637
    Align = alRight
    TabOrder = 3
  end
end

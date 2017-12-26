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
      '    descricao text, #A'
      '    quantidade integer NOT NULL,'
      '    numero_nf character varying(6), #A'
      '    recibo_operadora character varying(15),'
      '    documento character varying(20), #A'
      '    tipo_acomodacao character varying(20),'
      '    regime character varying(20),'
      '    categoria_quarto character varying(20),'
      '    tipo_cabine character varying(30),'
      '    numero_cabine character varying(10),    '
      '    nome_navio character varying(30),'
      '    destino_navio character varying(30),'
      '    categoria_veiculo character varying(20),'
      '    local_retirada character varying(30),'
      '    local_devolucao character varying(30),'
      '    nome_pacote character varying(50),'
      '    transporte character varying(20),'
      '    tipo_seguro character varying(15),'
      '    categoria_cabine character varying(3),'
      '    observacoes memo,'
      '    numero_externo character varying(20), #A'
      '    diarias integer')
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
    Text = 'vendas_produtos'
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
  object Button2: TButton
    Left = 327
    Top = 288
    Width = 75
    Height = 25
    Caption = 'ToDelphiString'
    TabOrder = 4
    OnClick = Button2Click
  end
end

object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Verifying OpenCV dependencies'
  ClientHeight = 581
  ClientWidth = 486
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnActivate = FormActivate
  DesignSize = (
    486
    581)
  TextHeight = 13
  object btn1: TButton
    Left = 8
    Top = 24
    Width = 189
    Height = 25
    Caption = 'Verifying OpenCV dependencies'
    TabOrder = 0
    OnClick = btn1Click
  end
  object pb1: TProgressBar
    Left = 8
    Top = 64
    Width = 470
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    ExplicitWidth = 627
  end
  object redt1: TRichEdit
    Left = 0
    Top = 84
    Width = 486
    Height = 497
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      'redt1')
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 2
    ExplicitTop = 83
    ExplicitHeight = 487
  end
end

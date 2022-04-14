object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'VideoCapture'
  ClientHeight = 412
  ClientWidth = 1091
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object lbl1: TLabel
    Left = 372
    Top = 348
    Width = 305
    Height = 49
    AutoSize = False
    Caption = 'lbl1'
    WordWrap = True
  end
  object lbl2: TLabel
    Left = 732
    Top = 348
    Width = 305
    Height = 49
    AutoSize = False
    Caption = 'lbl1'
    WordWrap = True
  end
  object cvw1: TCVView
    Left = 8
    Top = 32
    Width = 305
    Height = 305
    Source = cvcptrsrcWebCam
  end
  object cvw2: TCVView
    Left = 372
    Top = 32
    Width = 305
    Height = 305
    Source = cvcptrsrcFile
  end
  object cvw3: TCVView
    Left = 732
    Top = 32
    Width = 305
    Height = 305
    Source = cvcptrsrcStream
  end
  object chk1: TCheckBox
    Left = 8
    Top = 8
    Width = 305
    Height = 17
    Caption = 'Web camera'
    TabOrder = 3
    OnClick = chk1Click
  end
  object chk2: TCheckBox
    Left = 372
    Top = 8
    Width = 305
    Height = 17
    Caption = 'File'
    TabOrder = 4
    OnClick = chk2Click
  end
  object chk3: TCheckBox
    Left = 732
    Top = 8
    Width = 305
    Height = 17
    Caption = 'IP Stream'
    TabOrder = 5
    OnClick = chk3Click
  end
  object cvcptrsrcWebCam: TCVCaptureSource
    SourceTypeClassName = 'TCVWebCameraSource'
    Left = 24
    Top = 52
  end
  object cvcptrsrcFile: TCVCaptureSource
    SourceTypeClassName = 'TCVFileSource'
    SourceType.Delay = 40
    SourceType.Loop = True
    Left = 384
    Top = 56
  end
  object cvcptrsrcStream: TCVCaptureSource
    SourceTypeClassName = 'TCVFileSource'
    SourceType.Delay = 40
    SourceType.FileName = 'rtsp://wowzaec2demo.streamlock.net/vod/mp4:BigBuckBunny_115k.mp4'
    Left = 744
    Top = 56
  end
end

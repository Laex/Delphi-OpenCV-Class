object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'VideoCapture'
  ClientHeight = 370
  ClientWidth = 1091
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object lbl1: TLabel
    Left = 8
    Top = 8
    Width = 66
    Height = 15
    Caption = 'Web camera'
  end
  object lbl2: TLabel
    Left = 372
    Top = 8
    Width = 18
    Height = 15
    Caption = 'File'
  end
  object lbl3: TLabel
    Left = 732
    Top = 8
    Width = 49
    Height = 15
    Caption = 'IP stream'
  end
  object cvwWebCam: TCVView
    Left = 8
    Top = 32
    Width = 345
    Height = 325
    Source = cvcptrsrcWebCam
  end
  object cvwFile: TCVView
    Left = 372
    Top = 32
    Width = 345
    Height = 325
    Source = cvcptrsrcFile
  end
  object cvwStream: TCVView
    Left = 732
    Top = 32
    Width = 345
    Height = 325
    Source = cvcptrsrcStream
  end
  object cvcptrsrcWebCam: TCVCaptureSource
    Enabled = True
    SourceTypeClassName = 'TCVWebCameraSource'
    Left = 24
    Top = 52
  end
  object cvcptrsrcFile: TCVCaptureSource
    Enabled = False
    SourceTypeClassName = 'TCVFileSource'
    SourceType.Delay = 40
    Left = 384
    Top = 56
  end
  object cvcptrsrcStream: TCVCaptureSource
    Enabled = True
    SourceTypeClassName = 'TCVFileSource'
    SourceType.Delay = 40
    SourceType.FileName = 
      'http://210.148.114.53/-wvhttp-01-/GetOneShot?image_size=640x480&' +
      'frame_count=1000000000'
    Left = 744
    Top = 56
  end
end

unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, CVClass,
  CVResource, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    cvwWebCam: TCVView;
    cvwFile: TCVView;
    cvcptrsrcWebCam: TCVCaptureSource;
    cvcptrsrcFile: TCVCaptureSource;
    cvwStream: TCVView;
    cvcptrsrcStream: TCVCaptureSource;
    lbl1: TLabel;
    lbl2: TLabel;
    lbl3: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  With (cvcptrsrcFile.SourceType as TCVFileSource) do
  begin
    FileName := OpenCVData + 'Megamind.avi';
    Loop     := True;
    Enabled  := True;
  end;
end;

end.

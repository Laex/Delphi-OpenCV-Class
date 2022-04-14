unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, CVClass,
  cv.resource, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    cvcptrsrcWebCam: TCVCaptureSource;
    cvcptrsrcFile: TCVCaptureSource;
    cvcptrsrcStream: TCVCaptureSource;
    cvw1: TCVView;
    cvw2: TCVView;
    cvw3: TCVView;
    chk1: TCheckBox;
    chk2: TCheckBox;
    chk3: TCheckBox;
    lbl1: TLabel;
    lbl2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure chk1Click(Sender: TObject);
    procedure chk2Click(Sender: TObject);
    procedure chk3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.chk1Click(Sender: TObject);
begin
  cvcptrsrcWebCam.Enabled := chk1.Checked;
end;

procedure TForm1.chk2Click(Sender: TObject);
begin
  cvcptrsrcFile.Enabled := chk2.Checked;
end;

procedure TForm1.chk3Click(Sender: TObject);
begin
  cvcptrsrcStream.Enabled := chk3.Checked;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  With (cvcptrsrcFile.SourceType as TCVFileSource) do
  begin
    FileName := OpenCVData + 'Megamind.avi';
    lbl1.Caption := FileName;
  end;

  With (cvcptrsrcStream.SourceType as TCVFileSource) do
    lbl2.Caption := FileName;

  chk1.Checked := cvcptrsrcWebCam.Enabled;
  chk2.Checked := cvcptrsrcFile.Enabled;
  chk3.Checked := cvcptrsrcStream.Enabled;

end;

end.

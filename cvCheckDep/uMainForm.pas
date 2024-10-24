unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TMainForm = class(TForm)
    btn1: TButton;
    pb1: TProgressBar;
    redt1: TRichEdit;
    procedure btn1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    procedure Verifying_OpenCV_Dependencies;
    function CheckLoadDLL(const DLLFileName: String; Var ModulePath: String; Var ErrorCode: Cardinal; var ErrorString: string): Boolean;
    procedure OutText(const Text: String; const TextColor: TColor = clBlack; const isBold: Boolean = False);
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  cv.opencv;

const
  msRuntime_Suffix = '140';
  Debug_Suffix     = 'd';
  Lib_Ext          = '.dll';

  // Release and Debug
  msRuntime: array of string = [   //
    'concrt' + msRuntime_Suffix,   //
    'msvcp' + msRuntime_Suffix,    //
    'ucrtbase',                    //
    'vcruntime' + msRuntime_Suffix //
    ];

  // Release
  cvCore: array of string = [                      //
    'opencv_videoio_ffmpeg' + cvdllversion + '_64' //
    ];

  // Release and Debug
  cvRuntime: array of string = [                  //
    'opencv_videoio_msmf' + cvdllversion + '_64', //
    'opencv_world' + cvdllversion                 //
    ];

  // Release and Debug
  cvDelphiOpenCV: array of string = [ //
    'opencv_delphi' + cvdllversion    //
    ];

procedure TMainForm.FormActivate(Sender: TObject);
begin
  Verifying_OpenCV_Dependencies;
end;

procedure TMainForm.btn1Click(Sender: TObject);
begin
  Verifying_OpenCV_Dependencies;
end;

function TMainForm.CheckLoadDLL(const DLLFileName: String; Var ModulePath: String; Var ErrorCode: Cardinal; var ErrorString: string): Boolean;
Var
  DLL: HMODULE;
  buffer: array [0 .. MAX_PATH] of char;
begin
  DLL := LoadLibraryEx(PChar(DLLFileName), 0, LOAD_WITH_ALTERED_SEARCH_PATH);
  if DLL = 0 then
  begin
    ErrorCode   := GetLastError;
    ErrorString := SysErrorMessage(ErrorCode);
    Result      := False;
  end
  else
  begin
    FillChar(buffer, SizeOf(buffer), 0);
    if (GetModuleFileName(DLL, @buffer[0], Length(buffer)) > 0) then
      ModulePath := buffer;
    FreeLibrary(DLL);
    Result := True;
  end;
end;

procedure TMainForm.OutText(const Text: String; const TextColor: TColor; const isBold: Boolean);
begin
  redt1.SelAttributes.Bold  := isBold;
  redt1.SelAttributes.Color := TextColor;
  redt1.Lines.Add(Text);
end;

procedure TMainForm.Verifying_OpenCV_Dependencies;

  procedure RunTest(const Text: String; const libs: array of string; const Debug_Exists: Boolean = True);
  Var
    i: Integer;
    LibNames: TArray<string>;
    LibName: string;
    ModulePath: String;
    ErrorCode: Cardinal;
    ErrorString: string;
  begin
    OutText('------- ' + Text + ' -------', clBlack, True);
    for i := 0 to High(libs) do
    begin
      LibNames := [libs[i] + Lib_Ext];
      if Debug_Exists then
        LibNames := LibNames + [libs[i] + Debug_Suffix + Lib_Ext];
      for LibName in LibNames do
      begin
        if CheckLoadDLL(LibName, ModulePath, ErrorCode, ErrorString) then
        begin
          OutText(LibName + ' - ok', clGreen);
          OutText('    Path: ' + ModulePath, clGray);
        end
        else
        begin
          OutText('Verifying ' + LibName + ' - failed', clRed);
          OutText('   Error code: ' + ErrorCode.ToString + ' - ' + ErrorString, clGray);
        end;
        pb1.Position := pb1.Position + 1;
        Application.ProcessMessages;
      end;
    end;
  end;

Var
  buffer: array [0 .. 8192 - 1] of char;
begin
  redt1.Lines.Clear;
  pb1.Position := 0;
  btn1.Enabled := False;
  pb1.Max      := 2 * Length(msRuntime) + Length(cvCore) + 2 * Length(cvRuntime) + 2 * Length(cvDelphiOpenCV);
  Application.ProcessMessages;
  try
    FillChar(buffer, SizeOf(buffer), 0);
    if (GetDllDirectory(Length(buffer), @buffer[0]) <> 0) and (GetLastError() <> NO_ERROR) then
    begin
      // mmo1.Lines.Add('Search paths:');
      OutText('Search paths:', clBlack, True);
      // mmo1.Lines.Add(buffer);
      OutText(buffer);
    end;
    RunTest('Verifying OpenCV core', cvCore, False);
    RunTest('Verifying OpenCV runtime', cvRuntime);
    RunTest('Verifying Delphi OpenCV wrapper', cvDelphiOpenCV);
    RunTest('Verifying Microsoft Runtime', msRuntime);
  finally
    pb1.Position := 0;
    btn1.Enabled := True;
  end;
end;

end.

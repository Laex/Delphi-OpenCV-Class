program qrcode;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  CVResource,
  opencv_world,
  TypInfo;

Var
  keys: String =                                   //
    '{h help ? |        | print help messages }' + //
    '{i in     |        | input image path (also switches to image detection mode) }' + //
    '{detect   | false  | detect QR code only (skip decoding) }' + //
    '{m multi  |        | use detect for multiple qr-codes }' + //
    '{o out    | qr_code.png | path to result file }' + //
    '{save_detections | false  | save all QR detections (video mode only) }' + //
    '{save_all | false  | save all processed frames  (video mode only) }';

  g_modeMultiQR: bool = false;
  g_detectOnly: bool = false;

  g_out_file_name, g_out_file_ext: String;
  g_save_idx: int = 0;

  g_saveDetections: bool = false;
  g_saveAll: bool = false;

function getQRModeString(): string;
begin
  Result := 'QR';
  if g_modeMultiQR then
    Result := Result + ' multi';
  if g_detectOnly then
    Result := Result + ' detector';
end;

procedure drawQRCodeContour(const color_image: TMat; const corners: StdVectorPoint);
begin
  if (not corners.empty()) then
  begin
    Var
      show_radius: double;
    if (color_image.rows > color_image.cols) then
      show_radius := (2.813 * color_image.rows) / color_image.cols
    else
      show_radius := (2.813 * color_image.cols) / color_image.rows;
    VAr
      contour_radius: double := show_radius * 0.4;

    Var
      contours: StdVectorVectorPoint;

    contours.push_back(corners);
    drawContours(color_image, contours, 0, Scalar(211, 0, 148), cvRound(contour_radius));

    var
      rng: TRNG := 1000;
    for Var i := 0 to 4 - 1 do
    begin
      Var
        color: TScalar := Scalar(rng.uniform(0, 255), rng.uniform(0, 255), rng.uniform(0, 255));
      circle(color_image, corners[i], cvRound(show_radius), color, -1);
    end;
  end;
end;

begin
  try
    Var
      cmd_parser: TCommandLineParser;
    cmd_parser.CommandLineParser(keys);
    cmd_parser.about('This program detects the QR-codes from camera or images using the OpenCV library.');
    if (cmd_parser.has('help')) then
    begin
      cmd_parser.printMessage();
      Halt(0);
    end;

    Var
      in_file_name: String := cmd_parser.get<cvstdstring>('in'); // path to input image

    if (cmd_parser.has('out')) then
    begin
      g_out_file_name := cmd_parser.get<cvstdstring>('out'); // path to output image
      g_out_file_ext := ExtractFileExt(g_out_file_name);
      g_out_file_name := ChangeFileExt(g_out_file_name, '');
      if Length(g_out_file_ext) = 0 then
        g_out_file_ext := '.png';
    end;

    if (not cmd_parser.check()) then
    begin
      cmd_parser.printErrors();
      Halt(1);
    end;

    g_modeMultiQR := cmd_parser.has('multi') and cmd_parser.get<bool>('multi');
    g_detectOnly := cmd_parser.has('detect') and cmd_parser.get<bool>('detect');
    //
    g_saveDetections := cmd_parser.has('save_detections') and cmd_parser.get<bool>('save_detections');
    g_saveAll := cmd_parser.has('save_all') and cmd_parser.get<bool>('save_all');

    Var
      return_code: int := 0;
    if (in_file_name.IsEmpty) then
      return_code := liveQRCodeDetect()
    else
      return_code := imageQRCodeDetect(in_file_name);

    Halt(return_code);

  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      Readln;
    end;
  end;

end.

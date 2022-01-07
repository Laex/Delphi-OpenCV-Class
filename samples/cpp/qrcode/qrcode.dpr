program qrcode;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  System.Diagnostics,
  cpp.utils,
  cv.resource,
  cv.opencv;

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
  g_detectOnly: bool  = false;

  g_out_file_name, g_out_file_ext: String;
  g_save_idx: int = 0;

  g_saveDetections: bool = false;
  g_saveAll: bool        = false;

function getQRModeString(): string;
begin
  Result := 'QR';
  if g_modeMultiQR then
    Result := Result + ' multi';
  if g_detectOnly then
    Result := Result + ' detector';
end;

procedure drawQRCodeContour(const color_image: TMat; const corners: Vector<TPoint>);
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
      contours: Vector<Vector<TPoint>>;

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

procedure runQR(const qrcode: TQRCodeDetector; const input: TMat; const corners: Vector<TPoint>; const decode_info: Vector<CppString>
  // +global: bool g_modeMultiQR, bool g_detectOnly
  );
begin
  if (not g_modeMultiQR) then
  begin
    if (not g_detectOnly) then
    begin
      Var
        decode_info1: CppString := qrcode.detectAndDecode(input, corners);
      decode_info.push_back(decode_info1);
    end
    else
    begin
      Var
        detection_result: bool := qrcode.detect(input, corners);
        // CV_UNUSED(detection_result);
    end
  end
  else
  begin
    if (not g_detectOnly) then
    begin
      Var
        result_detection: bool := qrcode.detectAndDecodeMulti(input, decode_info, corners);
        // CV_UNUSED(result_detection);
    end
    else
    begin
      Var
        result_detection: bool := qrcode.detectMulti(input, corners);
        // CV_UNUSED(result_detection);
    end;
  end;
end;

procedure drawFPS(const color_image: TMat; fps: double);
begin
  var
    convert: String := format('%.2f', [fps]) + ' FPS (' + getQRModeString() + ')';
  putText(color_image, convert, Point(25, 25), FONT_HERSHEY_DUPLEX, 1, Scalar(0, 0, 255), 2);
end;

procedure drawQRCodeResults(const frame: TMat; const corners: Vector<TPoint>; const decode_info: Vector<CppString>; const fps: double);
begin
  if (not corners.empty()) then
  begin
    (*
      Var
      i: size_t := 0;
      While i < corners.size() do
      begin
      var
      qr_idx: size_t := i div 4;
      Var
      qrcode_contour: Vector<TPoint> := Vector<TPoint>.Create(corners.&begin() + i, corners.&begin() + i + 4);

      drawQRCodeContour(frame, qrcode_contour);

      Writeln(' QR[', qr_idx, ']@', TMat(qrcode_contour).reshape(2, 1), ': ');

      if (decode_info.size() > qr_idx) then
      begin
      if (not decode_info[qr_idx].empty()) then
      Writeln(' ''' + decode_info[qr_idx] + ''' ')
      else
      Writeln(' can ''t decode QR code');
      end
      else
      Writeln(' decode information is not available(disabled)');

      i := i + 4;
      end;
    *)
  end
  else
    Writeln('QR code is not detected');

  drawFPS(frame, fps);
end;

function processQRCodeDetection(const qrcode: TQRCodeDetector; const input: TMat; Var r: TMat; const corners: Vector<TPoint>): double;
begin
  if (input.channels() = 1) then
    cvtColor(input, r, COLOR_GRAY2BGR)
  else
    input.copyTo(r);

  Writeln('Run ', getQRModeString(), ' on image: ', input.size.p[0], ',', input.size.p[1], ' (', input.&type, ')');

  Var
    timer: TStopwatch;

  Var
    decode_info: Vector<CppString>;

  timer.start();
  runQR(qrcode, input, corners, decode_info);
  timer.stop();

  Var
    fps: double := 1 / timer.Elapsed.Seconds;

  drawQRCodeResults(r, corners, decode_info, fps);

  Result := fps;
end;

function liveQRCodeDetect(): int;
begin
  Var
    cap: TVideoCapture;

  cap.open(0);

  if (not cap.isOpened()) then
  begin
    Writeln('Cannot open a camera');
    Exit(2);
  end;

  Writeln('Press ''m'' to switch between detectAndDecode and detectAndDecodeMulti');
  Writeln('Press ''d'' to switch between decoder and detector');
  Writeln('Press '' '' (space) to save result into images');
  Writeln('Press ''ESC'' to exit');
  Var
    qrcode: TQRCodeDetector;

  While True do
  begin
    Var
      frame: TMat;
    cap.read(frame);
    if (frame.empty()) then
    begin
      Writeln('End of video stream');
      break;
    end;

    var
      forceSave: bool := g_saveAll;

    Var
      r: TMat;
      try var corners: Vector<TPoint>;
    Var
      fps: double := processQRCodeDetection(qrcode, frame, r, corners);

    Writeln('FPS: ', fps);
    forceSave := forceSave or (g_saveDetections and (not corners.empty));
    // forceSave |= fps < 1.0;

  except
    On E: Exception do
    begin
      Writeln('ERROR exception: ', E.Message);
      forceSave := True;
    end;
  end;

  if (not r.empty()) then
    imshow('QR code', r);

  Var
    code: int := waitKey(1);
  if (code < 0) and (not forceSave) then
    continue; // timeout
  Var
    c: char := char(code);
  if (c = ' ') or forceSave then
  begin
    Var
      fsuffix: string := format('-%05d', [g_save_idx]);
    Inc(g_save_idx);

    Var
      fname_input: string := g_out_file_name + fsuffix + '_input.png';
    Writeln('Saving QR code detection input: ', fname_input, ' ...');
    imwrite(fname_input, frame);

    var
      fname: string := g_out_file_name + fsuffix + g_out_file_ext;
    Writeln('Saving QR code detection result: ', fname, ' ...');
    imwrite(fname, r);

    Writeln('Saved');
  end;
  if (c = 'm') then
  begin
    g_modeMultiQR := not g_modeMultiQR;
    Write('Switching QR code mode ==> ');
    if g_modeMultiQR then
      Writeln('detectAndDecodeMulti')
    else
      Writeln('detectAndDecode');
  end;
  if (c = 'd') then
  begin
    g_detectOnly := not g_detectOnly;
    Write('Switching QR decoder mode ==> ');
    if g_detectOnly then
      Writeln('detect')
    else
      Writeln('decode');
  end;
  if (c = #27) then
  begin
    Writeln('ESC is pressed. Exiting...');
    break;
  end;
end;
Writeln('Exit.');

Result := 0;
end;

begin
  try
    Var
      cmd_parser: TCommandLineParser;
    cmd_parser := keys;
    cmd_parser.about('This program detects the QR-codes from camera or images using the OpenCV library.');
    if (cmd_parser.has('help')) then
    begin
      cmd_parser.printMessage();
      Halt(0);
    end;

    Var
      in_file_name: String := cmd_parser.get<string>('in'); // path to input image

    if (cmd_parser.has('out')) then
    begin
      g_out_file_name := cmd_parser.get<string>('out'); // path to output image
      g_out_file_ext := ExtractFileExt(g_out_file_name);
      g_out_file_name := ChangeFileExt(g_out_file_name, '');
      if Length(g_out_file_ext) = 0 then
        g_out_file_ext := '.png';
    end;

    // if (not cmd_parser.check()) then
    // begin
    // cmd_parser.printErrors();
    // Halt(1);
    // end;

    g_modeMultiQR := cmd_parser.has('multi') and cmd_parser.get<bool>('multi');
    g_detectOnly := cmd_parser.has('detect') and cmd_parser.get<bool>('detect');
    //
    g_saveDetections := cmd_parser.has('save_detections') and cmd_parser.get<bool>('save_detections');
    g_saveAll := cmd_parser.has('save_all') and cmd_parser.get<bool>('save_all');

    Var
      return_code: int := 0;
    if (in_file_name.IsEmpty) then
      return_code := liveQRCodeDetect()
      // else
      // return_code := imageQRCodeDetect(in_file_name)
        ;

    Halt(return_code);

  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      Readln;
    end;
  end;

end.

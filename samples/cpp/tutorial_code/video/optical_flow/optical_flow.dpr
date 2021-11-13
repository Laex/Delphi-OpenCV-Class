program optical_flow;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$R *.res}

uses
  System.SysUtils,
  CVResource,
  opencv_world;

const
  EXIT_FAILURE = 1;

Var
  pp1, pp2: TPoint2f;
  // prevPts: TInputArray;

begin
  try
    (*
      const
      about = //
      'This sample demonstrates Lucas-Kanade Optical Flow calculation.'#13#10 + //
      'The example file can be downloaded from:'#13#10 + //
      '  https://www.bogotobogo.com/python/OpenCV_Python/images/mean_shift_tracking/slow_traffic_small.mp4';
      const
      keys =                                            //
      '{ h help |      | print this help message }' + //
      '{ @image | vtest.avi | path to image file }';
      var
      parser: TCommandLineParser;
      parser.CommandLineParser(keys);
      parser.about(about);
      if (parser.has('help')) then
      begin
      parser.printMessage();
      Halt(0);
      end;
      Var
      filename: cppstring := parser.get<string>('@image');
      if (not parser.check()) then
      begin
      parser.printErrors();
      Halt(0);
      end;
    *)
    Var
      capture: TVideoCapture;

    capture.open(OpenCVData + 'vtest.avi');

    if (not capture.isOpened()) then
    begin
      // error in opening the video input
      Writeln('Unable to open file!');
      Halt(0);
    end;

    // Create some random colors
    Var
      colors: Vector<TScalar>;
    Var
      rng: TRNG;
    for Var i: int := 0 to 100 - 1 do
    begin
      Var
        r: int := rng.uniform(0, 256);
      Var
        g: int := rng.uniform(0, 256);
      Var
        b: int := rng.uniform(0, 256);
      colors.push_back(Scalar(r, g, b));
    end;

    Var
      old_frame, old_gray: TMat;
    Var
      p0, p1: Vector<TPoint2f>;

      // Take first frame and find corners in it
    capture.read(old_frame);
    cvtColor(old_frame, old_gray, COLOR_BGR2GRAY);
    goodFeaturesToTrack(old_gray, p0, 100, 0.3, 7, TMat.Mat, 7, false, 0.04);

    // Create a mask image for drawing purposes
    Var
      mask: TMat := TMat.zeros(old_frame.size, old_frame.&type);

      // Var
      // j: Integer := 0;

    while (true) do
    begin
      Var
        frame, frame_gray: TMat;

      capture.read(frame);
      if (frame.empty()) then
        break;
      cvtColor(frame, frame_gray, COLOR_BGR2GRAY);

      // calculate optical flow
      Var
        status: Vector<uchar>;
      Var
        err: Vector<float>;
      Var
      criteria := TTermCriteria.TermCriteria(TTermCriteria.COUNT + TTermCriteria.EPS, 10, 0.03);

      // Var
      // prevImg: TInputArray := old_gray;
      // Var
      // nextImg: TInputArray := frame_gray;
      // Var
      // prevPts: TInputArray := p0;
      // Var
      // nextPts: TInputOutputArray := p1;
      // Var
      // _status: TOutputArray := status;
      // Var
      // _err: TOutputArray := err;

      {
        winSize: UInt64;
        maxLevel: Int;
        criteria: TTermCriteria;
        flags: Int = 0;
        minEigThreshold: double = 1E-4
      }
      // if j = 1 then
      // Writeln;
      // Writeln(j);
      // Inc(j);
      calcOpticalFlowPyrLK(old_gray, frame_gray, p0, p1, status, err, size(15, 15), 2, criteria);

      Var
        good_new: Vector<TPoint2f>;

      for Var i: int := 0 to p0.size() - 1 do
      begin
        // Select good points
        if (status[i] = 1) then
        begin
          good_new.push_back(p1[i]);
          // draw the tracks

          pp1 := p0[i];
          pp2 := p1[i];

          line(mask, p1[i], p0[i], colors[i], 2);
          circle(frame, p1[i], 5, colors[i], -1);
        end;
      end;
      Var
        img: TMat;

      add(frame, mask, img);

      imshow('Frame', img);

      Var
        keyboard: int := waitKey(30);
      if (keyboard = int('q')) or (keyboard = 27) then
        break;

      // Now update the previous frame and previous points
      old_gray := frame_gray.clone();
      p0 := good_new;
    end;

  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      Readln;
    end;
  end;

end.

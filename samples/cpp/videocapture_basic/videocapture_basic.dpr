program videocapture_basic;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  CVResource,
  opencv_world;

begin
  try
    Var
      frame: TMat;
      // --- INITIALIZE VIDEOCAPTURE
    Var
      cap: TVideoCapture;
      // open the default camera using default API
      // cap.open(0);
      // OR advance usage: select any API backend
    Var
      deviceID: int := 0; // 0 = open default camera
    Var
      apiID: VideoCaptureAPIs := CAP_ANY; // 0 = autodetect default API
      // open selected camera using selected API
    cap.open(deviceID, apiID);
    // check if we succeeded
    if (not cap.isOpened()) then
    begin
      Writeln('ERROR! Unable to open camera');
      Halt(1);
    end;

    // --- GRAB AND WRITE LOOP
    Writeln('Start grabbing');
    Writeln('Press any key to terminate');
    while True do
    begin
      // wait for a new frame from camera and store it into 'frame'
      cap.read(frame);
      // check if we succeeded
      if (frame.empty()) then
      begin
        Writeln('ERROR! blank frame grabbed');
        break;
      end;
      // show live and wait for a key with timeout long enough to show images
      imshow('Live', frame);
      if (waitKey(5) >= 0) then
        break;
    end;
    // the camera will be deinitialized automatically in VideoCapture destructor

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.

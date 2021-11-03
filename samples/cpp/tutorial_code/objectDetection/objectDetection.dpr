program objectDetection;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  CVResource,
  opencv_delphi,
  opencv_world;

Var
  (* * Global variables *)
  face_cascade: TCascadeClassifier;
  eyes_cascade: TCascadeClassifier;

  (* * @function detectAndDisplay *)
procedure detectAndDisplay(frame: TMat);
Var
  frame_gray: TMat;
  // n: Int64;
begin
  cvtColor(frame, frame_gray, COLOR_BGR2GRAY);
  equalizeHist(frame_gray, frame_gray);

  // -- Detect faces
  Var
    faces: StdVectorRect;
  face_cascade.detectMultiScale(frame_gray, faces);

  // n := faces.size;

  for var i: Integer := 0 to faces.size() - 1 do
  begin
    Var
      center: TPoint := Point(faces[i].x + faces[i].width div 2, faces[i].y + faces[i].height div 2);
    ellipse(frame, center, size(faces[i].width div 2, faces[i].height div 2), 0, 0, 360, Scalar(255, 0, 255), 4);

    // Mat faceROI = frame_gray(faces[i]);
    Var
      faceROI: TMat := frame_gray.Mat(faces[0]);

      // -- In each face, detect eyes
    Var
      eyes: StdVectorRect;
    eyes_cascade.detectMultiScale(faceROI, eyes);

    for Var j: Integer := 0 to eyes.size() - 1 do
    begin
      var
        eye_center: TPoint := Point(faces[i].x + eyes[j].x + eyes[j].width div 2, faces[i].y + eyes[j].y + eyes[j].height div 2);
      Var
        radius: Integer := cvRound((eyes[j].width + eyes[j].height) * 0.25);

      circle(frame, eye_center, radius, Scalar(255, 0, 0), 4);
    end;
  end;

  // -- Show what you got
  imshow('Capture - Face detection', frame);
end;

begin
  try
    // CommandLineParser parser(argc, argv,
    // "{help h||}"
    // "{face_cascade|data/haarcascades/haarcascade_frontalface_alt.xml|Path to face cascade.}"
    // "{eyes_cascade|data/haarcascades/haarcascade_eye_tree_eyeglasses.xml|Path to eyes cascade.}"
    // "{camera|0|Camera device number.}");

    Writeln('This program demonstrates using the cv::CascadeClassifier class to detect objects (Face + eyes) in a video stream.');
    Writeln('You can use Haar or LBP features.');
    Writeln;

    // String face_cascade_name = samples::findFile( parser.get<String>("face_cascade"));
    Var
      face_cascade_name: CvStdString := OprnCVHaar + 'haarcascade_frontalface_alt.xml';

      // String eyes_cascade_name = samples::findFile( parser.get<String>("eyes_cascade"));
    Var
      eyes_cascade_name: CvStdString := OprnCVHaar + 'haarcascade_eye_tree_eyeglasses.xml';

      // -- 1. Load the cascades
    if (not face_cascade.load(face_cascade_name)) then
    begin
      Writeln('--(!)Error loading face cascade');
      Halt(1);
    end;
    if (not eyes_cascade.load(eyes_cascade_name)) then
    begin
      Writeln('--(!)Error loading eyes cascade');
      Halt(1);
    end;

    Var
      camera_device: Integer := 0;
    var
      capture: TVideoCapture;
      // -- 2. Read the video stream
    capture.open(camera_device);
    if (not capture.isOpened()) then
    begin
      Writeln('--(!)Error opening video capture');
      Halt(1);
    end;

    Var
      frame: TMat;
    while (capture.read(frame)) do
    begin
      if (frame.empty()) then
      begin
        Writeln('--(!) No captured frame -- Break!');
        break;
      end;

      // -- 3. Apply the classifier to the frame
      detectAndDisplay(frame);

      if (waitKey(10) = 27) then
        break; // escape
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.

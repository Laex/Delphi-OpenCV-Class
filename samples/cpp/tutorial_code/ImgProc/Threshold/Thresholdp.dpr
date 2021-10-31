program Thresholdp;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  CVResource in 'CVResource.pas',
  opencv_delphi in 'opencv_delphi.pas',
  opencv_world in 'opencv_world.pas';

const
  EXIT_FAILURE = 1;

Var
  /// Global Variables
  threshold_value: integer = 0;
  threshold_type: integer = 3;
  max_value: integer = 255;
  max_type: integer = 4;
  max_binary_value: integer = 255;

  src, src_gray, dst: TMat;
  window_name: string = 'Threshold Demo';

  trackbar_type: string = 'Type: \n 0: Binary \n 1: Binary Inverted \n 2: Truncate \n 3: To Zero \n 4: To Zero Inverted';
  trackbar_value: string = 'Value';

  // ![Threshold_Demo]
  (* *
    * @function Threshold_Demo
  *)
procedure Threshold_Demo(a: integer; p: pointer);
begin
  (* 0: Binary
    1: Binary Inverted
    2: Threshold Truncated
    3: Threshold to Zero
    4: Threshold to Zero Inverted
  *)
  threshold(src_gray, dst, threshold_value, max_binary_value, threshold_type);
  imshow(window_name, dst);
end;
// ![Threshold_Demo]

begin
  try

    // ! [load]
    Var
      imageName: CppString := CVResourcePath + 'stuff.jpg'; // by default

    if (ParamCount > 0) then
      imageName := ParamStr(1);

    src := imread(imageName, IMREAD_COLOR); // Load an image

    if (src.empty()) then
    begin
      Writeln('Cannot read the image: ' + imageName);
      Halt(1);
    end;

    cvtColor(src, src_gray, COLOR_BGR2GRAY); // Convert the image to Gray
    // ! [load]

    // ! [window]
    namedWindow(window_name, WINDOW_AUTOSIZE); // Create a window to display results
    // ! [window]

    // ! [trackbar]
    createTrackbar(trackbar_type, window_name, @threshold_type, max_type, Threshold_Demo); // Create a Trackbar to choose type of Threshold

    createTrackbar(trackbar_value, window_name, @threshold_value, max_value, Threshold_Demo); // Create a Trackbar to choose Threshold value
    // ! [trackbar]

    Threshold_Demo(0, nil); // Call the function to initialize

    /// Wait until the user finishes the program
    waitKey();

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.

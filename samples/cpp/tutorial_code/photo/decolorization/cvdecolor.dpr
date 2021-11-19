program cvdecolor;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  CVResource,
  opencv_world;

begin
  try
    // CommandLineParser parser( argc, argv, '{@input | HappyFish.jpg | input image}' );
    Var
      src: TMat;
    if ParamCount = 0 then
      src := imread(OpenCVData + 'HappyFish.jpg', IMREAD_COLOR)
    else
      src := imread(ParamStr(1), IMREAD_COLOR);
    if (src.empty()) then
    begin
      Writeln('Could not open or find the image!');
      Writeln('Usage: ', ParamStr(0), ' <Input image>');
      Halt(1);
    end;

    Var
      gray, color_boost: TMat;

    decolor(src, gray, color_boost);
    imshow('Source Image', src);
    imshow('grayscale', gray);
    imshow('color_boost', color_boost);
    waitKey(0);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.

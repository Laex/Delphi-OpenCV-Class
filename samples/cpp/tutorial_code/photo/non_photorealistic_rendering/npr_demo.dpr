program npr_demo;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  CVResource,
  opencv_world;

begin
  try
    var
      num, &type: int;
      // CommandLineParser parser(argc, argv, '{@input | lena.jpg | input image}');

    Var
      src: TMat;

    if ParamCount = 0 then
      src := imread(OpenCVData + 'lena.jpg', IMREAD_COLOR)
    else
      src := imread(ParamStr(1), IMREAD_COLOR);

    if (src.empty()) then
    begin
      Writeln('Could not open or find the image!');
      Writeln('Usage: ', ParamStr(0), ' <Input image>');
      Halt(1);
    end;

    Writeln;
    Writeln(' Edge Preserve Filter');
    Writeln('----------------------');

    Writeln('Options: ');
    Writeln;

    Writeln('1) Edge Preserve Smoothing');
    Writeln('   -> Using Normalized convolution Filter');
    Writeln('   -> Using Recursive Filter');
    Writeln('2) Detail Enhancement');
    Writeln('3) Pencil sketch/Color Pencil Drawing');
    Writeln('4) Stylization');
    Writeln;

    Writeln('Press number 1-4 to choose from above techniques: ');

    Readln(num);

    Var
      img: TMat;

    if (num = 1) then
    begin
      Writeln;
      Writeln('Press 1 for Normalized Convolution Filter and 2 for Recursive Filter: ');

      Readln(&type);

      edgePreservingFilter(src, img, &type);
      imshow('Edge Preserve Smoothing', img);

    end
    else if (num = 2) then
    begin
      detailEnhance(src, img);
      imshow('Detail Enhanced', img);
    end
    else if (num = 3) then
    begin
      Var
        img1: TMat;
      pencilSketch(src, img1, img, 10, 0.1, 0.03);
      imshow('Pencil Sketch', img1);
      imshow('Color Pencil Sketch', img);
    end
    else if (num = 4) then
    begin
      stylization(src, img);
      imshow('Stylization', img);
    end;
    waitKey(0);

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.

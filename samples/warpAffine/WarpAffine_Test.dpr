program WarpAffine_Test;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  cv.opencv,
  cv.resource;

const
  intput_file = OpenCVData + 'lena.jpg';

var
  srcTri: TAffineTransformPoints;
  dstTri: TAffineTransformPoints;

begin
  try
    if not FileExists(intput_file) then
    begin
      Writeln('Could not open or find the image <' + intput_file + '>');
      Halt;
    end;
    var
      src: TMat := imread(intput_file);

    srcTri[0] := Point2f(0, 0);
    srcTri[1] := Point2f(src.cols - 1, 0);
    srcTri[2] := Point2f(0, src.rows - 1);

    dstTri[0] := Point2f(0, src.rows * 0.33);
    dstTri[1] := Point2f(src.cols * 0.85, src.rows * 0.25);
    dstTri[2] := Point2f(src.cols * 0.15, src.rows * 0.7);
    var
      warp_mat: TMat := getAffineTransform(srcTri, dstTri);
    var
      warp_dst: TMat := TMat.zeros(src.rows, src.cols, src.&type());
    warpAffine(src, warp_dst, warp_mat, warp_dst.size);
    var
      center: TPoint := Point(warp_dst.cols div 2, warp_dst.rows div 2);
    var
      angle: double := -50.0;
    var
      scale: double := 0.6;
    var
      center2f: TPoint2f;
    center2f.x := center.x;
    center2f.y := center.y;
    var
      rot_mat: TMat := getRotationMatrix2D(center2f, angle, scale);
    var
      warp_rotate_dst: TMat;
    warpAffine(warp_dst, warp_rotate_dst, rot_mat, warp_dst.size);
    imshow('Source image', src);
    imshow('Warp', warp_dst);
    imshow('Warp + Rotate', warp_rotate_dst);
    waitKey();
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.

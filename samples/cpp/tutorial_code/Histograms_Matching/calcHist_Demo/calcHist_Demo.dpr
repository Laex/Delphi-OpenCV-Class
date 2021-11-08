program calcHist_Demo;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  CVResource,
  opencv_world;

const
  EXIT_FAILURE = 1;

Var
  // ! [Set the ranges ( for B,G,R) )]
  range: array [0 .. 1] of float = ( // the upper boundary is exclusive
    0,
    256
  );
  histRange: array [0 .. 0] of pfloat = ( // ! [Set the ranges ( for B,G,R) )]
    @range
  );

begin
  try
    Var
      src: TMat;
      // ! [Load image]
    if ParamCount > 0 then
      src := imread(ParamStr(1), IMREAD_COLOR)
    else
      src := imread(OpenCVData + 'lena.jpg', IMREAD_COLOR);
    if (src.empty()) then
      Halt(EXIT_FAILURE);

    // ! [Separate the image in 3 places ( B, G and R )]
    Var
      bgr_planes: TStdVector<TMat>; // vector<Mat> bgr_planes;

    split(src, bgr_planes); // ! [Separate the image in 3 places ( B, G and R )]

    // ! [Establish the number of bins]
    Var
      histSize: int := 256;
      // ! [Establish the number of bins]

      // ! [Set the ranges ( for B,G,R) )]
      // Moved up!

    var
      uniform: bool := true; // ! [Set histogram param]
    Var
      accumulate: bool := false; // ! [Set histogram param]

      // ! [Compute the histograms]
    Var
      b_hist: TMat;
    Var
      g_hist: TMat;
    Var
      r_hist: TMat;
    calcHist(@bgr_planes[0], 1, nil, TMat.Mat(), b_hist, 1, @histSize, @histRange[0], uniform, accumulate);
    calcHist(@bgr_planes[1], 1, nil, TMat.Mat(), g_hist, 1, @histSize, @histRange[0], uniform, accumulate);
    calcHist(@bgr_planes[2], 1, nil, TMat.Mat(), r_hist, 1, @histSize, @histRange[0], uniform, accumulate);
    // ! [Compute the histograms]

    // ! [Draw the histograms for B, G and R]
    Var
      hist_w: int := 512;
    Var
      hist_h: int := 400;
    var
      bin_w: int := cvRound(hist_w / histSize);

    Var
      histImage: TMat := TMat.Mat(hist_h, hist_w, CV_8UC3, Scalar(0, 0, 0));
      // ! [Draw the histograms for B, G and R]

      // ! [Normalize the result to ( 0, histImage.rows )]
    normalize(b_hist, b_hist, 0, histImage.rows, NORM_MINMAX, -1, TMat.Mat());
    normalize(g_hist, g_hist, 0, histImage.rows, NORM_MINMAX, -1, TMat.Mat());
    normalize(r_hist, r_hist, 0, histImage.rows, NORM_MINMAX, -1, TMat.Mat());
    // ! [Normalize the result to ( 0, histImage.rows )]

    // ! [Draw for each channel]
    for Var i: int := 1 to histSize - 1 do
    begin
      line(histImage, Point(bin_w * (i - 1), hist_h - cvRound(b_hist.at<float>(i - 1))), Point(bin_w * (i), hist_h - cvRound(b_hist.at<float>(i))), Scalar(255, 0, 0), 2, LineTypes(8), 0);
      line(histImage, Point(bin_w * (i - 1), hist_h - cvRound(g_hist.at<float>(i - 1))), Point(bin_w * (i), hist_h - cvRound(g_hist.at<float>(i))), Scalar(0, 255, 0), 2, LineTypes(8), 0);
      line(histImage, Point(bin_w * (i - 1), hist_h - cvRound(r_hist.at<float>(i - 1))), Point(bin_w * (i), hist_h - cvRound(r_hist.at<float>(i))), Scalar(0, 0, 255), 2, LineTypes(8), 0);
    end;
    // ! [Draw for each channel]

    // ! [Display]
    imshow('Source image', src);
    imshow('calcHist Demo', histImage);
    waitKey();
    // ! [Display]
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      Readln;
    end;
  end;

end.

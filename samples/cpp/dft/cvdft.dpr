(*
  This file is part of Delphi-OpenCV-Class project.
  https://github.com/Laex/Delphi-OpenCV-Class

  It is subject to the license terms in the LICENSE file found in the top-level directory
  of this distribution and at https://www.apache.org/licenses/LICENSE-2.0.txt

  Copyright 2021, Laentir Valetov, laex@bk.ru

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
*)
program cvdft;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  WinApi.Windows,
  System.SysUtils,
  cpp.utils,
  cv.resource,
  cv.opencv;

procedure help;
begin
  printf( //
    '\nThis program demonstrated the use of the discrete Fourier transform (dft)\n' + //
    'The dft of an image is taken and it''s power spectrum is displayed.\n' + //
    'Usage:\n ' + argv[0] + ' [image_name -- default lena.jpg]\n');
end;

Type
  pMat1f = ^TMat1f;

begin
  try
    help;
    Var
      filename: string := OpenCVData + 'lena.jpg';
    Var
      img: TMat := imread(filename, IMREAD_GRAYSCALE);
    if (img.empty()) then
    begin
      printf('Cannot read image file: ' + filename + '\n');
      Halt(1);
    end;

    Var
      m: int := getOptimalDFTSize(img.rows);
    Var
      N: int := getOptimalDFTSize(img.cols);
    Var
      padded: TMat;
    copyMakeBorder(img, padded, 0, m - img.rows, 0, N - img.cols, BORDER_CONSTANT, TScalar.all(0));

    Var
      planes: TArray<TMat> := [TMat.Mat(), TMat.zeros(padded.size, CV_32F)];

    var
      planes0: pMat1f := @planes[0];

    planes0^ := TMat1f.Mat(padded);

    Var
      complexImg: TMat;

    merge(planes, complexImg);

    dft(complexImg, complexImg);

    // compute log(1 + sqrt(Re(DFT(img))**2 + Im(DFT(img))**2))
    split(complexImg, planes);
    magnitude(planes[0], planes[1], planes[0]);
    Var
      mag: TMat := planes[0];
    mag := mag + TScalar.all(1);
    log(mag, mag);

    // crop the spectrum, if it has an odd number of rows or columns
    mag := mag.Mat(Rect(0, 0, mag.cols and -2, mag.rows and -2));

    Var
      cx: int := mag.cols div 2;
    Var
      cy: int := mag.rows div 2;

      // rearrange the quadrants of Fourier image
      // so that the origin is at the image center
    Var
      tmp: TMat;
    Var
      q0: TMat := TMat.Mat(mag, Rect(0, 0, cx, cy));
    Var
      q1: TMat := TMat.Mat(mag, Rect(cx, 0, cx, cy));
    Var
      q2: TMat := TMat.Mat(mag, Rect(0, cy, cx, cy));
    Var
      q3: TMat := TMat.Mat(mag, Rect(cx, cy, cx, cy));

    q0.copyTo(tmp);
    q3.copyTo(q0);
    tmp.copyTo(q3);

    q1.copyTo(tmp);
    q2.copyTo(q1);
    tmp.copyTo(q2);

    normalize(mag, mag, 0, 1, NORM_MINMAX);

    imshow('spectrum magnitude', mag);
    waitKey();
  except
    on E: Exception do
    begin
      WriteLn(E.ClassName, ': ', E.Message);
      Readln;
    end;
  end;

end.

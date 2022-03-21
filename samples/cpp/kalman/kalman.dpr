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
program kalman;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  WinApi.Windows,
  System.SysUtils,
  cpp.utils,
  cv.resource,
  cv.opencv;

function calcPoint(const center: TPoint2f; R, angle: double): TPoint;
begin
  Result := center + Point2f(cos(angle), -sin(angle)) * R;
end;

procedure help();
begin
  printf('\nExample of c calls to OpenCV''s Kalman filter.\n' + //
    '   Tracking of rotating point.\n' + //
    '   Point moves in a circle and is characterized by a 1D state.\n' + //
    '   state_k+1 = state_k + speed + process_noise N(0, 1e-5)\n' + //
    '   The speed is constant.\n' + //
    '   Both state and measurements vectors are 1D (a point angle),\n' + //
    '   Measurement is the real state + gaussian noise N(0, 1e-1).\n' + //
    '   The real and the measured points are connected with red line segment,\n' + //
    '   the real and the estimated points are connected with yellow line segment,\n' + //
    '   the real and the corrected estimated points are connected with green line segment.\n' + //
    '   (if Kalman filter works correctly,\n' + //
    '    the yellow segment should be shorter than the red one and\n' + //
    '    the green segment should be shorter than the yellow one).' + //
    '\n' + //
    '   Pressing any key (except ESC) will reset the tracking.\n' + //
    '   Pressing ESC will stop the program.\n' //
    );
end;

Var
      KF: TKalmanFilter;

begin
  try
    help();
    Var
      img: TMat := TMat.Mat(500, 500, CV_8UC3);
//    Var
      KF{: TKalmanFilter} := TKalmanFilter.KalmanFilter(2, 1, 0);
    Var
      state: TMat := TMat.Mat(2, 1, CV_32F); (* (phi, delta_phi) *)
    Var
      processNoise: TMat := TMat.Mat(2, 1, CV_32F);
    Var
      measurement: TMat := TMat.zeros(1, 1, CV_32F);
    Var
      code: char := char(-1);

    While True do
    begin
      img.assign(TScalar.all(0));

      state.st<float>(0, 0.0);           // state.at<float>(0) := 0.0;
      state.st<float>(1, 2 * CV_PI / 6); // state.at<float>(1) := 2 * CV_PI / 6;
      KF.transitionMatrix := TMat_<float>.Mat(2, 2) shl [1, 1, 0, 1];

//      for Var i:=0 to 1 do
//      for Var j:=0 to 1 do
//      WriteLn(
//      KF.transitionMatrix.at<float>(i,j));

      setIdentity(KF.measurementMatrix);
      setIdentity(KF.processNoiseCov, TScalar.all(1E-5));
      setIdentity(KF.measurementNoiseCov, TScalar.all(1E-1));
      setIdentity(KF.errorCovPost, TScalar.all(1));

      randn(KF.statePost, TScalar.all(0), TScalar.all(0.1));

      While True do
      begin
        Var
          center: TPoint2f := Point2f(img.cols * 0.5, img.rows * 0.5);
        Var
          R: float := img.cols / 3;
        Var
          stateAngle: double := state.at<float>(0);
        Var
          statePt: TPoint := calcPoint(center, R, stateAngle);

        Var
          prediction: TMat := KF.predict();
        Var
          predictAngle: double := prediction.at<float>(0);
        Var
          predictPt: TPoint := calcPoint(center, R, predictAngle);

          // generate measurement
        randn(measurement, TScalar.all(0), TScalar.all(KF.measurementNoiseCov.at<float>(0)));
        measurement := measurement + KF.measurementMatrix * state;

        Var
          measAngle: double := measurement.at<float>(0);
        Var
          measPt: TPoint := calcPoint(center, R, measAngle);

          // correct the state estimates based on measurements
          // updates statePost & errorCovPost
        KF.correct(measurement);
        Var
          improvedAngle: double := KF.statePost.at<float>(0);
        Var
          improvedPt: TPoint := calcPoint(center, R, improvedAngle);

          // plot points
        img := img * 0.2;
        drawMarker(img, measPt, Scalar(0, 0, 255), MARKER_SQUARE, 5, 2);
        drawMarker(img, predictPt, Scalar(0, 255, 255), MARKER_SQUARE, 5, 2);
        drawMarker(img, improvedPt, Scalar(0, 255, 0), MARKER_SQUARE, 5, 2);
        drawMarker(img, statePt, Scalar(255, 255, 255), MARKER_STAR, 10, 1);
        // forecast one step
        Var
          test: TMat := TMat.Mat(KF.transitionMatrix * KF.statePost);
        drawMarker(img, calcPoint(center, R, TMat.Mat(KF.transitionMatrix * KF.statePost).at<float>(0)), Scalar(255, 255, 0), MARKER_SQUARE, 12, 1);

        line(img, statePt, measPt, Scalar(0, 0, 255), 1, LINE_AA, 0);
        line(img, statePt, predictPt, Scalar(0, 255, 255), 1, LINE_AA, 0);
        line(img, statePt, improvedPt, Scalar(0, 255, 0), 1, LINE_AA, 0);

        randn(processNoise, Scalar(0), TScalar.all(sqrt(KF.processNoiseCov.at<float>(0, 0))));

        state := KF.transitionMatrix * state + processNoise;

        imshow('Kalman', img);
        code := char(waitKey(1000));

        if (code <> #0) then
          break;
      end;
      if (code = #27) or (code = 'q') or (code = 'Q') then
        break;
    end;

  except
    on e: Exception do
    begin
      WriteLn(e.ClassName, ': ', e.Message);
      Readln;
    end;
  end;

end.

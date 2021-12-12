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
program asift;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  WinApi.Windows,
  System.SysUtils,
  CVResource,
  cpputils,
  opencv_world;

procedure help;
begin
  cout //
    + 'This is a sample usage of AffineFeature detector/extractor.\n' //
    + 'And this is a ObjectPascal version of samples/python/asift.py\n' //
    + 'Usage: ' + argv[0] + '\n' //
    + '     [ --feature <sift|orb|brisk> ]         # Feature to use.\n' //
    + '     [ --flann ]                            # use Flann-based matcher instead of bruteforce.\n' //
    + '     [ --maxlines <number(50 as default)> ] # The maximum number of lines in visualizing the matching result.\n' //
    + '     [ --image1 <image1(aero1.jpg as default)> ]\n' //
    + '     [ --image2 <image2(aero3.jpg as default)> ] # Path to images to compare.' //
    + endl;
end;

function timer(): double;
begin
  Result := getTickCount() / getTickFrequency();
end;

begin
  try
    help;
    (*
      vector<String> fileName;
      cv::CommandLineParser parser(argc, argv,
      '{help h ||}'
      '{feature|brisk|}'
      '{flann||}'
      '{maxlines|50|}'
      '{image1|aero1.jpg|}{image2|aero3.jpg|}');
      if (parser.has('help'))
      {
      help(argv);
      return 0;
      }
    *)
    // default
    Var
      feature: string := 'brisk';
    Var
      useFlann: bool := false;
    Var
      maxlines: int := 50;
    Var
      fileName: TArray<String> := //
        [                         //
        OpenCVData + 'aero1.jpg', //
        OpenCVData + 'aero3.jpg'  //
        ];

    Var
      img1: TMat := imread(fileName[0], IMREAD_GRAYSCALE);
    Var
      img2: TMat := imread(fileName[1], IMREAD_GRAYSCALE);
    if (img1.empty()) then
    begin
      cerr + 'Image ' + fileName[0] + ' is empty or cannot be found' + endl;
      Halt(1);
    end;

    if (img2.empty()) then
    begin
      cerr + 'Image ' + fileName[1] + ' is empty or cannot be found' + endl;
      Halt(1);
    end;

    Var
      backend: TPtr<TFeature2D>;
    Var
      matcher: TPtr<TDescriptorMatcher>;

    if (feature = 'sift') then
    begin
      backend := TSIFT.create();
      if (useFlann) then
        matcher := TDescriptorMatcher.create('FlannBased')
      else
        matcher := TDescriptorMatcher.create('BruteForce');
    end
    else if (feature = 'orb') then
    begin
      backend := TORB.create();
      if (useFlann) then
        matcher := makePtr<FlannBasedMatcher>(makePtr<Tflann.LshIndexParams>(6, 12, 1))
      else
        matcher := TDescriptorMatcher.create('BruteForce-Hamming');
    end
    else if (feature = 'brisk') then
    begin
      backend := TBRISK.create();
      if (useFlann) then
        matcher := makePtr<FlannBasedMatcher>(makePtr<Tflann.LshIndexParams>(6, 12, 1));
    else
      matcher := TDescriptorMatcher.create('BruteForce-Hamming');
  end
else
begin
  cerr + feature + ' is not supported. See --help' + endl;
  Halt(1);
end;

cout + 'extracting with ' + feature + '...' + endl;

Var
  ext: TPtr<AffineFeature> := TAffineFeature.create(backend);

Var
  kp1, kp2: vector<TKeyPoint>;

Var
  desc1, desc2: TMat;

ext.v.detectAndCompute(img1, TMat.Mat(), kp1, desc1);
ext.v.detectAndCompute(img2, TMat.Mat(), kp2, desc2);
cout + 'img1 - ' + kp1.size() + ' features, ' //
  + 'img2 - ' + kp2.size() + ' features'      //
  + endl;

cout + 'matching with ' + iif(useFlann, 'flann', 'bruteforce') + '...' + endl;

Var
  start: double := timer();

  // match and draw
Var
  rawMatches: vector<vector<TDMatch>>;

Var
  p1, p2: vector<TPoint2f>;

Var
  distances: vector<float>;
matcher.v.knnMatch(desc1, desc2, rawMatches, 2);
// filter_matches
for Var i := 0 to rawMatches.size() - 1 do
begin
  Var
    m: vector<TDMatch> := rawMatches[i];
  if (m.size() = 2) and (m[0].distance < m[1].distance * 0.75) then
  begin
    p1.push_back(kp1[m[0].queryIdx].pt);
    p2.push_back(kp2[m[0].trainIdx].pt);
    distances.push_back(m[0].distance);
  end;
end;

Var
  status: vector<uchar>;

Var
  pointPairs: vector<pair<Point2f, Point2f>>;

Var
  H: TMat := findHomography(p1, p2, status, RANSAC);

Var
  inliers: int := 0;
for i := 0 to status.size() - 1 do
begin
  if (status[i]) then
  begin
    pointPairs.push_back(make_pair(p1[i], p2[i]));
    distances[inliers] := distances[i];
    // CV_Assert(inliers <= (int)i);
    inc(inliers);
  end;
end;
distances.resize(inliers);

cout + 'execution time: ' + fixed + setprecision(2) + (timer() - start) * 1000 + ' ms' + endl;
cout + inliers + ' / ' + status.size() + ' inliers/matched' + endl;

cout + 'visualizing...' + endl;

Var
  indices: vector<int> := inliers;
sortIdx(distances, indices, SORT_EVERY_ROW + SORT_ASCENDING);

// explore_match
Var
  h1: int := img1.size().height;

Var
  w1: int := img1.size().width;

Var
  h2: int := img2.size().height;

Var
  w2: int := img2.size().width;

Var
  vis: TMat := TMat.zeros(max(h1, h2), w1 + w2, CV_8U);
img1.copyTo(Mat(vis, Rect(0, 0, w1, h1)));
img2.copyTo(Mat(vis, Rect(w1, 0, w2, h2)));
cvtColor(vis, vis, COLOR_GRAY2BGR);

Var
  corners: vector<TPoint2f> := 4;

corners[0] := Point2f(0, 0);
corners[1] := Point2f((float)w1, 0);
corners[2] := Point2f((float)w1, (float)h1);
corners[3] := Point2f(0, (float)h1);

Var
  icorners: vector<TPoint2i>;

perspectiveTransform(corners, corners, H);
transform(corners, corners, Matx23f(1, 0, (float)w1, 0, 1, 0));
Mat(corners).convertTo(icorners, CV_32S);
polylines(vis, icorners, true, Scalar(255, 255, 255));

for i := 0 to min(inliers, maxlines) - 1 do
begin
  Var
    idx: int := indices[i];
  Var
    pi1: TPoint2f := pointPairs[idx].first;
  Var
    pi2: TPoint2f := pointPairs[idx].second;
  circle(vis, pi1, 2, Scalar(0, 255, 0), -1);
  circle(vis, pi2 + Point2f(w1, 0), 2, Scalar(0, 255, 0), -1);
  line(vis, pi1, pi2 + Point2f(w1, 0), Scalar(0, 255, 0));
end;
if (inliers > maxlines) then
  cout + 'only ' + maxlines + ' inliers are visualized' + endl;
imshow('affine find_obj', vis);

// Mat vis2 = Mat::zeros(max(h1, h2), w1+w2, CV_8U);
// Mat warp1;
// warpPerspective(img1, warp1, H, Size(w1, h1));
// warp1.copyTo(Mat(vis2, Rect(0, 0, w1, h1)));
// img2.copyTo(Mat(vis2, Rect(w1, 0, w2, h2)));
// imshow('warped', vis2);

waitKey();
cout + 'done' + endl;
except
  on E: Exception do
  begin
    WriteLn(E.ClassName, ': ', E.Message);
    Readln;
  end;
end;

end.

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
program lkdemo;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  cpp.utils,
  cv.resource,
  cv.opencv;

procedure help();
begin
    // print a welcome message, and the OpenCV version
  cout + '\nThis is a demo of Lukas-Kanade optical flow lkdemo(),\n' //
    + 'Using OpenCV version ' + CV_VERSION + endl;
  cout + '\nIt uses camera by default, but you can provide a path to video as an argument.\n'; //
  cout + '\nHot keys: \n' //
    + '\tESC - quit the program\n'             //
    + '\tr - auto-initialize tracking\n'       //
    + '\tc - delete all the points\n'          //
    + '\tn - switch the `night` mode on/off\n' //
    + 'To add/remove a feature point click it\n' + endl;
end;

Var
  point: TPoint2f;
  addRemovePt: bool = false;

procedure onMouse(event: int; x, y, flags: int; param: pointer);
begin
  if (event = int(EVENT_LBUTTONDOWN)) then
  begin
    point := Point2f(x, y);
    addRemovePt := true;
  end;
end;

begin
  try
    help;

    var
      cap: TVideoCapture;
    Var
      termcrit: TTermCriteria := TTermCriteria.TermCriteria(TTermCriteria.COUNT or TTermCriteria.EPS, 20, 0.03);
    Var
      subPixWinSize: TSize := TSize.Size(10, 10);
    var
      winSize: TSize := TSize.Size(31, 31);

    Var
      MAX_COUNT: int := 500;
    Var
      needToInit: bool := false;
    Var
      nightMode: bool := false;

        // cv::CommandLineParser parser(argc, argv, "begin@input|0|end;");
        // string input = parser.get<string>("@input");
        //
        // if( input.size() == 1 && isdigit(input[0]) )
        // cap.open(input[0] - '0');
        // else
        // cap.open(input);
    if (ParamCount = 0) or (isIntNumber(ParamStr(1))) then
    begin
      if (ParamCount = 0) then
        cap.open(0)
      else
        cap.open(ParamStr(1).ToInteger);
    end
    else if ParamCount > 0 then
      cap.open(ParamStr(1));
    if (not cap.isOpened()) then
    begin
      cout + ' Could not initialize capturing...\n';
      Halt(0);
    end;

    namedWindow(' LK Demo ', 1);
    setMouseCallback(' LK Demo ', onMouse);

    Var
      gray, prevGray, image, frame: TMat;
    Var
      points: TArray<vector<TPoint2f>>;
    SetLength(points, 2);

    while true do
    begin
      cap > frame;
      if (frame.empty()) then
        break;

      frame.copyTo(image);
      cvtColor(image, gray, COLOR_BGR2GRAY);

      if (nightMode) then
        image.assign(TScalar.all(0));

      if (needToInit) then
      begin
          // automatic initialization
        goodFeaturesToTrack(gray, points[1], MAX_COUNT, 0.01, 10, TMat.Mat(), 3, 3, false, 0.04);
        cornerSubPix(gray, points[1], subPixWinSize, Size(-1, -1), termcrit);
        addRemovePt := false;
      end
      else if (not points[0].empty()) then
      begin
        var
          status: vector<uchar>;
        Var
          err: vector<float>;

        if (prevGray.empty()) then
          gray.copyTo(prevGray);

        calcOpticalFlowPyrLK(prevGray, gray, points[0], points[1], status, err, winSize, 3, termcrit, 0, 0.001);

        var
          i, k: size_t;

        k := 0;
        for i := 0 to points[1].Size() - 1 do
        begin
          if (addRemovePt) then
          begin
            if (norm(point - points[1][i]) <= 5) then
            begin
              addRemovePt := false;
              continue;
            end;
          end;

          if (status[i] = 0) then
            continue;

          points[1][k] := points[1][i];
          k := k + 1;

          circle(image, points[1][i], 3, Scalar(0, 255, 0), -1, 8);

        end;
        points[1].resize(k);
      end;

      if (addRemovePt and (points[1].Size() < MAX_COUNT)) then
      begin
        var
          tmp: vector<TPoint2f>;

        tmp.push_back(point);

        cornerSubPix(gray, tmp, winSize, Size(-1, -1), termcrit);

        points[1].push_back(tmp[0]);
        addRemovePt := false;
      end;

      needToInit := false;
      imshow(' LK Demo ', image);

      Var
        c: int := waitKey(10);
      if (c = 27) then
        break;
      case Chr(c) of
        'r':
          needToInit := true;

        'c':
          begin
            points[0].clear();
            points[1].clear();
          end;
        'n':
          nightMode := not nightMode;

      end;

      TSwap.swap<TPoint2f>(points[1], points[0]);
      swap(prevGray, gray);
    end;

  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      Readln;
    end;
  end;

end.

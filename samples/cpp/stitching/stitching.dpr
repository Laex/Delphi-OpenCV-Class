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
program stitching;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  cv.resource,
  cpp.utils,
  cv.opencv;

procedure printUsage;
begin
  cout + //
    'Images stitcher.\n\n' + 'Usage :\n' + argv[0] + ' [Flags] img1 img2 [...imgN]\n\n' + //
    'Flags:\n' + //
    '  --d3\n' + //
    '      internally creates three chunks of each image to increase stitching success\n' + //
    '  --mode (panorama|scans)\n' + //
    '      Determines configuration of stitcher. The default is ''panorama'',\n' + //
    '      mode suitable for creating photo panoramas. Option ''scans'' is suitable\n' + //
    '      for stitching materials under affine transformation, such as scans.\n' + //
    '  --output <result_img>\n' +                //
    '      The default is ''result.jpg''.\n\n' + //
    'Example usage :\n' + argv[0] + ' --d3 --mode scans img1.jpg img2.jpg\n';
end;

const
  EXIT_FAILURE = False;
  EXIT_SUCCESS = True;

Var
  divide_images: bool  = False;
  mode: TStitcher.mode = TStitcher.mode.PANORAMA;
  imgs: vector<TMat>;
  result_name: string = 'result.jpg';

function parseCmdArgs: boolean;
begin
  if (argc = 1) then
  begin
    Exit(EXIT_FAILURE);
    printUsage;
  end;

  Var
    i: int := 1;
  While i < argc do
  begin
    if (string(argv[i]) = '--help') or (string(argv[i]) = '/?') then
    begin
      printUsage;
      Exit(EXIT_FAILURE);
    end
    else if (argv[i] = '--d3') then
      divide_images := True
    else if (argv[i] = '--output') then
    begin
      result_name := argv[i + 1];
      i := i + 1;
    end
    else if (argv[i] = '--mode') then
    begin
      if (argv[i + 1] = 'panorama') then
        mode := TStitcher.mode.PANORAMA
      else if (argv[i + 1] = 'scans') then
        mode := TStitcher.mode.SCANS
      else
      begin
        cout + 'Bad --mode flag value\n';
        Exit(EXIT_FAILURE);
      end;
      inc(i);
    end
    else
    begin
      Var
        img: TMat := imread(argv[i]);
      if (img.empty()) then
      begin
        cout + 'Can''t read image ' + argv[i] + '\n';
        Exit(EXIT_FAILURE);
      end;
      if (divide_images) then
      begin
        Var
          r: TRect := rect(0, 0, img.cols div 2, img.rows);
        imgs.push_back(img.mat(r).clone);
        r.x := img.cols div 3;
        imgs.push_back(img.mat(r).clone);
        r.x := img.cols div 2;
        imgs.push_back(img.mat(r).clone);
      end
      else
        imgs.push_back(img);
    end;
    inc(i);
  end;
  Result := EXIT_SUCCESS;
end;

begin
  try
    printUsage;

    Var
      retval: boolean := parseCmdArgs;
    if (not retval) then
      Halt(1);

    // ![stitching]
    Var
      pano: TMat;
    Var
      stitcher: TPtr<TStitcher> := TStitcher.create(mode);
    Var
      status: TStitcher.status := stitcher.v.stitch(imgs, pano);

    if (status <> TStitcher.status.OK) then
    begin
      cout + 'Can''t stitch images, error code = ' + int(status) + endl;
      Halt(1);
    end;
    // ![stitching]

    imwrite(result_name, pano);
    cout + 'stitching completed successfully\n' + result_name + ' saved!';

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.

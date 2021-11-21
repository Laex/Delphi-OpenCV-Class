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
program videocapture_image_sequence;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  CVResource,
  opencv_world;

procedure help();
begin
  WriteLn('This sample shows you how to read a sequence of images using the VideoCapture interface.');
  WriteLn('Usage: ', ExtractFileName(ParamStr(0)), ' <image_mask> (example mask: example_%02d.jpg)');
  WriteLn('Image mask defines the name variation for the input images that have to be read as a sequence.');
  WriteLn('Using the mask example_%02d.jpg will read in images labeled as ''example_00.jpg'' ''example_01.jpg'' etc.');
end;

begin
  try

    help();
    // cv::CommandLineParser parser(argc, argv, '{@image| ../data/left%02d.jpg |}');
    // first_file = parser.get<string>('@image');
    Var
      first_file: String := OpenCVData + 'left%02d.jpg';

    if (first_file.Length = 0) then
      Halt(1);

    Var
      sequence: TVideoCapture := first_file;

    if (not sequence.isOpened()) then
    begin
      WriteLn('Failed to open the image sequence!');
      Halt(1);
    end;

    Var
      image: TMat;
    namedWindow('Image sequence | press ESC to close', WINDOW_AUTOSIZE);

    while True do
    begin
      // Read in image from sequence
      sequence > image;

      // If no image was retrieved -> end of sequence
      if (image.empty()) then
      begin
        WriteLn('End of Sequence');
        break;
      end;

      imshow('Image sequence | press ESC to close', image);

      if (waitKey(500) = 27) then
        break;
    end;

  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.

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
program video_write;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$R *.res}

uses
  System.SysUtils,
  CVResource,
  opencv_world;

begin
  try
    Writeln( //
      '------------------------------------------------------------------------------'#13#10 + //
      'This program shows how to write video files.'#13#10 + //
      'You can extract the R or G or B color channel of the input video.'#13#10 + //
      'Usage:'#13#10 + //
      'video_write <input_video_name> [ R | G | B] [Y | N]'#13#10 + //
      '------------------------------------------------------------------------------'#13#10);

    Var
      source: string; // the source file name
    Var
      askOutputType: bool; // If false it will use the inputs codec type
    Var
      NAME: String; // Form the new name with container

    Var
      R: Char; // Channel char

    if ParamCount <> 3 then
    begin
      Writeln('Not enough parameters. Try default params');
      source        := OpenCVData + 'Megamind.avi';
      askOutputType := False;
      NAME          := ChangeFileExt(ExtractFileName(source), 'R.avi');
      R             := 'R';
    end
    else
    begin
      source        := ParamStr(1);          // the source file name
      askOutputType := ParamStr(3)[1] = 'Y'; // If false it will use the inputs codec type
      R             := ParamStr(2)[1];
      NAME          := ChangeFileExt(source, R + '.avi'); // Form the new name with container
    end;

    Var
      inputVideo: TVideoCapture := source; // Open input
    if (not inputVideo.isOpened()) then
    begin
      Writeln('Could not open the input video: ', source);
      Halt(1);
    end;

    Var
      ex: int := Trunc(inputVideo.get(CAP_PROP_FOURCC)); // Get Codec Type- Int form

      // Transform from int to char via Bitwise operators
    var
      EXT: TArray<AnsiChar> := [//
      AnsiChar(ex and $FF),//
      AnsiChar((ex and $FF00) shr 8),//
      AnsiChar((ex and $FF0000) shr 16),//
      AnsiChar((ex and $FF000000) shr 24),//
      AnsiChar(0)];

    Var
      S: TSize := Size(Trunc(inputVideo.get(CAP_PROP_FRAME_WIDTH)), // Acquire input size
        Trunc(inputVideo.get(CAP_PROP_FRAME_HEIGHT)));

    Var
      outputVideo: TVideoWriter; // Open the output

    if (askOutputType) then
      outputVideo.open(NAME, -1, inputVideo.get(CAP_PROP_FPS), S, True)
    else
      outputVideo.open(NAME, ex, inputVideo.get(CAP_PROP_FPS), S, True);

    if (not outputVideo.isOpened()) then
    begin
      Writeln('Could not open the output video for write: ', Name);
      Halt(1);
    end;

    Writeln('Input frame resolution: Width=', S.width, '  Height=', S.height, //
      ' of nr#: ', inputVideo.get(CAP_PROP_FRAME_COUNT):1:4);
    Write('Input codec type: ');
    for Var i := 0 to High(EXT) do
      Write(EXT[i]);
    Writeln;
    Var
      channel: int := 2; // Select the channel to save

      case R of //
        'R': channel := 2;
      'G': channel   := 1;
      'B': channel   := 0;
  end;

  Var
    src, res: TMat;
  Var
    spl: vector<TMat>;

  While True do // Show the image captured in the window and repeat
  begin
    inputVideo > src; // read
    if (src.empty()) then
      break; // check if at end

    split(src, spl); // process - extract only the correct channel
    for Var i: int := 0 to 2 do
      if (i <> channel) then
        spl[i] := TMat.zeros(S, spl[0].&type);
    merge(spl, res);

    // outputVideo.write(res); //save or
    outputVideo < res;
  end;

  Writeln('Finished writing');

except
  on E: Exception do
  begin
    Writeln(E.ClassName, ': ', E.Message);
    Readln;
  end;
end;

end.

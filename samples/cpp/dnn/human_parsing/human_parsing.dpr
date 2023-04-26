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
program human_parsing;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  cpp.utils,
  cv.resource,
  cv.opencv,
  CmdLineParser;

function parse_human(const image: TMat; const model: string; backend: int = int(backend.DNN_BACKEND_DEFAULT); target: int = int(target.DNN_TARGET_CPU)): TMat;
begin
  Var
    flipped: TMat;
  flip(image, flipped, 1);
  Var
    batch: vector<TMat>;
  batch.push_back(image);
  batch.push_back(flipped);
  Var
    blob: TMat := blobFromImages(batch, 1.0, Size(), Scalar(104.00698793, 116.66876762, 122.67891434));

  Var
    net: TNet := readNet(model);

  net.setPreferableBackend(backend);
  net.setPreferableTarget(target);
  net.setInput(blob);
  Var
    _out: TMat := net.forward();
    // expected output: [2, 20, 384, 384], (2 lists(orig, flipped) of 20 body part heatmaps 384x384)

    // LIP classes:
    // 0 Background, 1 Hat, 2 Hair, 3 Glove, 4 Sunglasses, 5 UpperClothes, 6 Dress, 7 Coat, 8 Socks, 9 Pants
    // 10 Jumpsuits, 11 Scarf, 12 Skirt, 13 Face, 14 LeftArm, 15 RightArm, 16 LeftLeg, 17 RightLeg, 18 LeftShoe. 19 RightShoe
  Var
    colors: TArray<TVec3b> := [ //
      Vec3b(0, 0, 0), Vec3b(128, 0, 0), Vec3b(255, 0, 0), Vec3b(0, 85, 0), Vec3b(170, 0, 51), Vec3b(255, 85, 0), Vec3b(0, 0, 85), Vec3b(0, 119, 221), Vec3b(85, 85, 0), Vec3b(0, 85, 85), Vec3b(85, 51, 0), Vec3b(52, 86, 128), Vec3b(0, 128, 0), Vec3b(0, 0, 255), Vec3b(51, 170, 221), Vec3b(0, 255, 255), Vec3b(85, 255, 170), Vec3b(170, 255, 85), Vec3b(255, 255, 0), Vec3b(255, 170, 0)];

  Var
    segm: TMat := TMat.Mat(image.Size, CV_8UC3, Scalar(0, 0, 0));
  Var
    maxval: TMat := TMat.Mat(image.Size, CV_32F, Scalar(0));

    // iterate over body part heatmaps (LIP classes)
  for Var i: int := 0 to _out.Size[1] - 1 do
  begin
    // resize heatmaps to original image size
    // "head" is  the original image result, "tail" the flipped copy
    Var
      head: TMat;
    Var
      h: TMat := TMat.Mat(_out.Size[2], _out.Size[3], CV_32F, _out.pt<float>(0, i));
    resize(h, head, image.Size);

    // we have to swap the last 3 pairs in the "tail" list
    var
      tail_order: TArray<int> := [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 15, 14, 17, 16, 19, 18];
    Var
      tail: TMat;
    Var
      t: TMat := TMat.Mat(_out.Size[2], _out.Size[3], CV_32F, _out.pt<float>(1, tail_order[i]));
    resize(t, tail, image.Size);
    flip(tail, tail, 1);

    // mix original and flipped result
    Var
      avg: TMat := (head + tail) * 0.5;

      // write color if prob value > maxval
    Var
      cmask: TMat;
    compare(avg, maxval, cmask, CMP_GT);
    segm.setTo(colors[i], cmask);

    // keep largest values for next iteration
    max(avg, maxval, maxval);
  end;
  cvtColor(segm, segm, COLOR_RGB2BGR);
  Result := segm;
end;

begin
  try
    Var
      parser: TCommandLineParser := TCommandLineParser.Create( //
        '{help    h |                 | show help screen / args}' + //
        '{image   i |                 | person image to process }' + //
        '{model   m |lip_jppnet_384.pb| network model}' + //
        '{backend b | 0               | Choose one of computation backends: ' + //
        { ----------- } '0: automatically (by default), ' + //
        { ----------- } '1: Halide language (http://halide-lang.org/), ' + //
        { ----------- } '2: Intel''s Deep Learning Inference Engine (https://software.intel.com/openvino-toolkit), ' + //
        { ----------- } '3: OpenCV implementation, ' + //
        { ----------- } '4: VKCOM, ' +                 //
        { ----------- } '5: CUDA }' +                  //
        '{target  t | 0               | Choose one of target computation devices: ' + //
        { ----------- } '0: CPU target (by default), ' + //
        { ----------- } '1: OpenCL, ' +                  //
        { ----------- } '2: OpenCL fp16 (half-float precision), ' + //
        { ----------- } '3: VPU, ' +    //
        { ----------- } '4: Vulkan, ' + //
        { ----------- } '6: CUDA, ' +   //
        { ----------- } '7: CUDA fp16 (half-float preprocess) }');

    if (argc = 1) or parser.has('help') then
    begin
      parser.printMessage();
      Halt(0);
    end;

    if not parser.check then
    begin
      // parser.printError();
      Halt(0);
    end;

    Var
      model: string := parser.get<string>('model');
    Assert(FileExists(model), '"model" file must exist');
    Var
      image: string := parser.get<string>('image');
    Assert(FileExists(image), '"image" file must exist');
    Var
      backend: int := parser.get<int>('backend');
    Var
      target: int := parser.get<int>('target');
    Var
      input: TMat := imread(image);
    Var
      segm: TMat := parse_human(input, model, backend, target);

    imshow('human parsing', segm);
    waitKey();

  except
    on E: Exception do
    begin
      WriteLn(E.ClassName, ': ', E.Message);
      Readln;
    end;
  end;

end.

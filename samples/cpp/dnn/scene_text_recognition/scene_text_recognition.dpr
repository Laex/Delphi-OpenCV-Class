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
program scene_text_recognition;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  cpp.utils,
  cv.resource,
  cv.opencv,
  CmdLineParser;

Var
  keys: string = //
    '{ help  h                          | | Print help message. }' + //
    '{ inputImage i                     | | Path to an input image. Skip this argument to capture frames from a camera. }' + //
    '{ modelPath mp                     | crnn_cs_CN.onnx | Path to a binary .onnx file contains trained CRNN text recognition model. ' + //
  { --------- } 'Download links are provided in doc/tutorials/dnn/dnn_text_spotting/dnn_text_spotting.markdown}' + //
    '{ RGBInput rgb                     |0| 0: imread with flags=IMREAD_GRAYSCALE; 1: imread with flags=IMREAD_COLOR. }' + //
    '{ evaluate e                       |false| false: predict with input images; true: evaluate on benchmarks. }' + //
    '{ evalDataPath edp                 | | Path to benchmarks for evaluation. ' + //
  { --------- } 'Download links are provided in doc/tutorials/dnn/dnn_text_spotting/dnn_text_spotting.markdown}' + //
    '{ vocabularyPath vp                | alphabet_36.txt | Path to recognition vocabulary. ' + //
  { --------- } 'Download links are provided in doc/tutorials/dnn/dnn_text_spotting/dnn_text_spotting.markdown}';

  // Convert the predictions to lower case, and remove other characters.
  // Only for Evaluation
function convertForEval(input: String): String;
begin
  Result := default (String);
  for Var i := 1 to input.length do
  begin
    Var
      ch: char := input[i];
    if (Ord(ch) >= 97) and (Ord(ch) <= 122) then
      Result := Result + ch
    else if (Ord(ch) >= 65) and (Ord(ch) <= 90) then
      Result := Result + Chr(Ord(ch) + 32)
    else
      continue;
  end;
end;

begin
  try
    // Parse arguments
    Var
      parser: CommandLineParser := keys;
    parser.about('Use this script to run the PyTorch implementation of ' + //
      'An End-to-End Trainable Neural Network for Image-based SequenceRecognition and Its Application to Scene Text Recognition ' + //
      '(https://arxiv.org/abs/1507.05717)');
    if (argc = 1) or parser.has('help') then
    begin
      parser.printMessage();
      Halt(0);
    end;

    Var
      modelPath: String := parser.get<String>('modelPath');
    Var
      vocPath: String := parser.get<String>('vocabularyPath');
    var
      imreadRGB: int := parser.get<int>('RGBInput');

    if (not parser.check()) then
    begin
      parser.printErrors();
      Halt(1);
    end;

    // Load the network
    Assert(not modelPath.isempty);
    Var
      recognizer: TTextRecognitionModel := modelPath;
      // Load vocabulary
    Assert(not vocPath.isempty);
    Assert(FileExists(vocPath));
    Var
      vocFile: TStringList := TStringList.Create;
    Var
      vocabulary: vector<CppString>;
    try
      vocFile.LoadFromFile(vocPath);
      Assert(vocFile.Count > 0);
      for Var vocLine: String in vocFile do
        vocabulary.push_back(vocLine);
    finally
      vocFile.Free;
    end;

    recognizer.setVocabulary(vocabulary);
    recognizer.setDecodeType('CTC-greedy');

    // Set parameters
    Var
      scale: double := 1.0 / 127.5;
    Var
      mean: TScalar := Scalar(127.5, 127.5, 127.5);
    Var
      inputSize: TSize := size(100, 32);
    recognizer.setInputParams(scale, inputSize, mean);

    if (parser.get<bool>('evaluate')) then
    begin
      // For evaluation
      Var
        evalDataPath: String := parser.get<String>('evalDataPath');
      Assert(not evalDataPath.isempty);
      Var
        gtPath: String := evalDataPath + '/test_gts.txt';
      Assert(FileExists(gtPath));
      Var
        evalGts: TStringList := TStringList.Create;
      try
        evalGts.LoadFromFile(gtPath);
        Assert(evalGts.Count > 0);
        Var
          cntRight: int := 0;
        Var
          cntAll: int := 0;
        Var
          timer: TTickMeter;
        timer.reset();

        for Var gtLine: String in evalGts do
        begin
          Var
            splitLoc: size_t := gtLine.IndexOf(' ');

          Var
            imgPath: String := evalDataPath + '/' + gtLine.Substring(1, splitLoc);
          Var
            gt: String := gtLine.Substring(splitLoc + 1);

            // Inference
          Var
            frame: TMat := imread(findFile(imgPath), imreadRGB);
          Assert(not frame.empty);
          timer.start();
          Var
            recognitionResult: string := recognizer.recognize(frame);
          timer.stop();

          if (gt = convertForEval(recognitionResult)) then
            Inc(cntRight);
          Inc(cntAll);
        end;
        cout + 'Accuracy(%): ' + cntRight / cntAll + endl;

        cout + 'Average Inference Time(ms): ' + timer.getTimeMilli() / cntAll + endl;
      finally
        evalGts.Free;
      end;
    end
    else
    begin
      // Create a window
      const
        winName: string = 'Input Cropped Image';

        // Open an image file
      Assert(parser.has('inputImage'));
      Var
        frame: TMat := imread(findFile(parser.get<String>('inputImage')), ImreadModes(imreadRGB));
      Assert(not frame.empty);

      // Recognition
      Var
        recognitionResult: string := recognizer.recognize(frame);

      imshow(winName, frame);

      cout + 'Predition: "' + recognitionResult + '"' + endl;
      waitKey();
    end

  except
    on E: Exception do
    begin
      WriteLn(E.ClassName, ': ', E.Message);
      Readln;
    end;
  end;

end.

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
program neural_network;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  WinApi.Windows,
  System.SysUtils,
  cpp.utils,
  cv.resource,
  cv.opencv;

procedure PrintMat(const A: TMat);
Var
  i, j: Integer;
begin
  for i := 0 to A.rows - 1 do
  begin
    for j := 0 to A.cols - 1 do
      write(A.at<float>(i, j):10:8);
    WriteLn;
  end;
end;

begin
  try
    // create random training data
    Var
      data: TMat_<float> := TMat_<float>.Mat(100, 100);
    randn(data, TMat.zeros(1, 1, data.&type), TMat.ones(1, 1, data.&type));

    // half of the samples for each class
    Var
      responses: TMat_<float> := TMat_<float>.Mat(data.rows, 2);
    for Var i: int := 0 to data.rows - 1 do
    begin
      if (i < data.rows div 2) then
      begin
        responses.pt(i, 0)^ := 1;
        responses.pt(i, 1)^ := 0;
      end
      else
      begin
        responses.pt(i, 0)^ := 0;
        responses.pt(i, 1)^ := 1;
      end;
    end;

    (*
      //example code for just a single response (regression)
      Mat_<float> responses(data.rows, 1);
      for (int i=0; i<responses.rows; ++i)
      responses(i, 0) = i < responses.rows / 2 ? 0 : 1;
    *)

    // create the neural network
    Var
      layerSizes: TMat_<int> := TMat_<int>.Mat(1, 3);
    layerSizes.pt(0, 0)^ := data.cols;
    layerSizes.pt(0, 1)^ := 20;
    layerSizes.pt(0, 2)^ := responses.cols;

    Var
      network: TPtr<TANN_MLP> := TANN_MLP.create();

    network.v.setLayerSizes(layerSizes);
    network.v.setActivationFunction(TANN_MLP.ActivationFunctions.SIGMOID_SYM, 0.1, 0.1);
    network.v.setTrainMethod(TANN_MLP.TrainingMethods.BACKPROP, 0.1, 0.1);
    Var
      trainData: TPtr<TTrainData> := TTrainData.create(data, ROW_SAMPLE, responses);

    network.v.train(trainData);
    if (network.v.isTrained()) then
    begin
      printf('Predict one-vector:\n');
      Var
        result: TMat;
      network.v.predict(TMat.ones(1, data.cols, data.&type()), result);
      PrintMat(result);

      printf('Predict training data:\n');
      for Var i: int := 0 to data.rows - 1 do
      begin
        network.v.predict(data.row(i), result);
        PrintMat(result);
      end;
    end;

  except
    on E: Exception do
    begin
      WriteLn(E.ClassName, ': ', E.Message);
      Readln;
    end;
  end;

end.

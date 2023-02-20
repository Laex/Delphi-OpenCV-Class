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

{$IFDEF OPENCV_FEATURES_2D_HPP}
{ TSIFT }

class function TSIFT.create(nfeatures: int = 0; nOctaveLayers: int = 3; contrastThreshold: double = 0.04; //
  edgeThreshold: double = 10; sigma: double = 1.6): TPtr<TFeature2D>;
begin
  class_proc_CV_SIFT_create(Result, nfeatures, nOctaveLayers, contrastThreshold, edgeThreshold, sigma);
end;

{ TDescriptorMatcher }

class function TDescriptorMatcher.create(const descriptorMatcherType: String): TPtr<TDescriptorMatcher>;
begin
  class_proc_CV_DESCRIPTORMATCHER_create(Result, descriptorMatcherType);
end;

{ TORB }

class function TORB.create(nfeatures: int = 500; scaleFactor: float = 1.2; nlevels: int = 8; edgeThreshold: int = 31; firstLevel: int = 0;
  WTA_K: int = 2; scoreType: scoreTypes = HARRIS_SCORE; patchSize: int = 31; fastThreshold: int = 20): TPtr<TFeature2D>;
begin
  class_proc_CV_ORB_create(Result,nfeatures, scaleFactor, nlevels, edgeThreshold, firstLevel, WTA_K, int(scoreType), patchSize, fastThreshold);
end;

{$ENDIF}

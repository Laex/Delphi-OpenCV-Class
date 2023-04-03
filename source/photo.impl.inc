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

{$IF defined(OPENCV_PHOTO_HPP) and (not defined(OPENCV_PHOTO_HPP_IMPL))}
{$DEFINE OPENCV_PHOTO_HPP_IMPL}

{$IF defined(OPENCV_IMGPROC_HPP) and (not defined(OPENCV_IMGPROC_HPP_IMPL))}{$I 'imgproc.impl.inc'}{$ifend}

procedure inpaint(const src: TInputArray; const inpaintMask: TInputArray; const dst: TOutputArray; inpaintRadius: double; flags: int);
begin
{$IF not defined(PACKAGE)}
  proc_inpaint(src, inpaintMask, dst, inpaintRadius, flags);
{$IFEND}
end;

procedure fastNlMeansDenoising(const src: TInputArray; const dst: TOutputArray; const h: vector<float>; templateWindowSize: int = 7; searchWindowSize: int = 21; normType: int = NORM_L2);
begin
{$IF not defined(PACKAGE)}
  proc_fastNlMeansDenoising_1(src, dst, h, templateWindowSize, searchWindowSize, normType);
{$IFEND}
end;

procedure fastNlMeansDenoisingColored(const src: TInputArray; const dst: TOutputArray; h: float = 3; hColor: float = 3; templateWindowSize: int = 7; searchWindowSize: int = 21);
begin
{$IF not defined(PACKAGE)}
  proc_fastNlMeansDenoisingColored_1(src, dst, h, hColor, templateWindowSize, searchWindowSize);
{$IFEND}
end;

procedure fastNlMeansDenoisingMulti(const srcImgs: TInputArrayOfArrays; const dst: TOutputArray; imgToDenoiseIndex: int; temporalWindowSize: int; h: float = 3; templateWindowSize: int = 7; searchWindowSize: int = 21);
begin
{$IF not defined(PACKAGE)}
  proc_fastNlMeansDenoisingMulti_1(srcImgs, dst, imgToDenoiseIndex, temporalWindowSize, h, templateWindowSize, searchWindowSize);
{$IFEND}
end;

procedure fastNlMeansDenoisingMulti(const srcImgs: TInputArrayOfArrays; const dst: TOutputArray; imgToDenoiseIndex: int; temporalWindowSize: int; const h: vector<float>; templateWindowSize: int = 7; searchWindowSize: int = 21; normType: int = NORM_L2);
begin
{$IF not defined(PACKAGE)}
  proc_fastNlMeansDenoisingMulti(srcImgs, dst, imgToDenoiseIndex, temporalWindowSize, h, templateWindowSize, searchWindowSize, normType);
{$IFEND}
end;

procedure fastNlMeansDenoisingColoredMulti(const srcImgs: TInputArrayOfArrays; const dst: TOutputArray; imgToDenoiseIndex: int; temporalWindowSize: int; h: float = 3; hColor: float = 3; templateWindowSize: int = 7; searchWindowSize: int = 21);
begin
{$IF not defined(PACKAGE)}
  proc_fastNlMeansDenoisingColoredMulti(srcImgs, dst, imgToDenoiseIndex, temporalWindowSize, h, hColor, templateWindowSize, searchWindowSize);
{$IFEND}
end;

procedure denoise_TVL1(const observations: vector<TMat>; Var res: TMat; lambda: double = 1.0; niters: int = 30);
begin
{$IF not defined(PACKAGE)}
  proc_denoise_TVL1(observations, res, lambda, niters);
{$IFEND}
end;

procedure decolor(const src: TInputArray; const grayscale: TOutputArray; const color_boost: TOutputArray);
begin
{$IF not defined(PACKAGE)}
  proc_decolor(src, grayscale, color_boost);
{$IFEND}
end;

procedure seamlessClone(const src: TInputArray; const dst: TInputArray; const mask: TInputArray; const p: TPoint; const blend: TOutputArray; flags: int);
begin
{$IF not defined(PACKAGE)}
  proc_seamlessClone(src, dst, mask, p, blend, flags);
{$IFEND}
end;

procedure colorChange(const src: TInputArray; const mask: TInputArray; const dst: TOutputArray; red_mul: float = 1.0; green_mul: float = 1.0; blue_mul: float = 1.0);
begin
{$IF not defined(PACKAGE)}
  proc_colorChange(src, mask, dst, red_mul, green_mul, blue_mul);
{$IFEND}
end;

procedure illuminationChange(const src: TInputArray; const mask: TInputArray; const dst: TOutputArray; alpha: float = 0.2; beta: float = 0.4);
begin
{$IF not defined(PACKAGE)}
  proc_illuminationChange(src, mask, dst, alpha, beta);
{$IFEND}
end;

procedure textureFlattening(const src: TInputArray; const mask: TInputArray; const dst: TOutputArray; low_threshold: float = 30; high_threshold: float = 45; kernel_size: int = 3);
begin
{$IF not defined(PACKAGE)}
  proc_textureFlattening(src, mask, dst, low_threshold, high_threshold, kernel_size);
{$IFEND}
end;

procedure edgePreservingFilter(const src: TInputArray; const dst: TOutputArray; flags: int = 1; sigma_s: float = 60; sigma_r: float = 0.4);
begin
{$IF not defined(PACKAGE)}
  proc_edgePreservingFilter(src, dst, flags, sigma_s, sigma_r);
{$IFEND}
end;

procedure detailEnhance(const src: TInputArray; const dst: TOutputArray; sigma_s: float = 10; sigma_r: float = 0.15);
begin
{$IF not defined(PACKAGE)}
  proc_detailEnhance(src, dst, sigma_s, sigma_r);
{$IFEND}
end;

procedure pencilSketch(const src: TInputArray; const dst1: TOutputArray; const dst2: TOutputArray; sigma_s: float = 60; sigma_r: float = 0.07; shade_factor: float = 0.02);
begin
{$IF not defined(PACKAGE)}
  proc_pencilSketch(src, dst1, dst2, sigma_s, sigma_r, shade_factor);
{$IFEND}
end;

procedure stylization(const src: TInputArray; const dst: TOutputArray; sigma_s: float = 60; sigma_r: float = 0.45);
begin
{$IF not defined(PACKAGE)}
  proc_stylization(src, dst, sigma_s, sigma_r);
{$IFEND}
end;

{$IFEND}

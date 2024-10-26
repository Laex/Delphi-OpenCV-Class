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

unit cv.utils;

{$I opt.inc}

interface

uses
  WinApi.Windows,
  System.Types,
  VCL.Graphics,
  cv.opencv;

function ipDraw(const dc: HDC; const img: TMat; const rect: System.Types.TRect; const Stretch: Boolean = True): Boolean;
function BmpToMat(const Bitmap: TBitmap; out M: TMat): Boolean;
function GetDIBits(const Bitmap: TBitmap; out Bits: Pointer; out BitsSize, _Width: DWord): Boolean; overload;
function GetDIBits(const Bitmap: TBitmap; out Bits: Pointer; out _Width: DWord): Boolean; overload;
function MatToBmp(const M: TMat; out Bitmap: TBitmap; const PixelFormat: TPixelFormat = pfDevice): Boolean;

implementation

const
  DefaultVGA256Palette: array [0 .. 255] of TColorRef = //
    ($000000, $800000, $008000, $808000, $000080, $800080, $008080, $C0C0C0, $808080, $FF0000, $00FF00, $FFFF00, $0000FF, $FF00FF, $00FFFF, $FFFFFF,
    $000000, $00005F, $000087, $0000AF, $0000D7, $0000FF, $005F00, $005F5F, $005F87, $005FAF, $005FD7, $005FFF, $008700, $00875F, $008787, $0087AF,
    $0087D7, $0087FF, $00AF00, $00AF5F, $00AF87, $00AFAF, $00AFD7, $00AFFF, $00D700, $00D75F, $00D787, $00D7AF, $00D7D7, $00D7FF, $00FF00, $00FF5F,
    $00FF87, $00FFAF, $00FFD7, $00FFFF, $5F0000, $5F005F, $5F0087, $5F00AF, $5F00D7, $5F00FF, $5F5F00, $5F5F5F, $5F5F87, $5F5FAF, $5F5FD7, $5F5FFF,
    $5F8700, $5F875F, $5F8787, $5F87AF, $5F87D7, $5F87FF, $5FAF00, $5FAF5F, $5FAF87, $5FAFAF, $5FAFD7, $5FAFFF, $5FD700, $5FD75F, $5FD787, $5FD7AF,
    $5FD7D7, $5FD7FF, $5FFF00, $5FFF5F, $5FFF87, $5FFFAF, $5FFFD7, $5FFFFF, $870000, $87005F, $870087, $8700AF, $8700D7, $8700FF, $875F00, $875F5F,
    $875F87, $875FAF, $875FD7, $875FFF, $878700, $87875F, $878787, $8787AF, $8787D7, $8787FF, $87AF00, $87AF5F, $87AF87, $87AFAF, $87AFD7, $87AFFF,
    $87D700, $87D75F, $87D787, $87D7AF, $87D7D7, $87D7FF, $87FF00, $87FF5F, $87FF87, $87FFAF, $87FFD7, $87FFFF, $AF0000, $AF005F, $AF0087, $AF00AF,
    $AF00D7, $AF00FF, $AF5F00, $AF5F5F, $AF5F87, $AF5FAF, $AF5FD7, $AF5FFF, $AF8700, $AF875F, $AF8787, $AF87AF, $AF87D7, $AF87FF, $AFAF00, $AFAF5F,
    $AFAF87, $AFAFAF, $AFAFD7, $AFAFFF, $AFD700, $AFD75F, $AFD787, $AFD7AF, $AFD7D7, $AFD7FF, $AFFF00, $AFFF5F, $AFFF87, $AFFFAF, $AFFFD7, $AFFFFF,
    $D70000, $D7005F, $D70087, $D700AF, $D700D7, $D700FF, $D75F00, $D75F5F, $D75F87, $D75FAF, $D75FD7, $D75FFF, $D78700, $D7875F, $D78787, $D787AF,
    $D787D7, $D787FF, $D7AF00, $D7AF5F, $D7AF87, $D7AFAF, $D7AFD7, $D7AFFF, $D7D700, $D7D75F, $D7D787, $D7D7AF, $D7D7D7, $D7D7FF, $D7FF00, $D7FF5F,
    $D7FF87, $D7FFAF, $D7FFD7, $D7FFFF, $FF0000, $FF005F, $FF0087, $FF00AF, $FF00D7, $FF00FF, $FF5F00, $FF5F5F, $FF5F87, $FF5FAF, $FF5FD7, $FF5FFF,
    $FF8700, $FF875F, $FF8787, $FF87AF, $FF87D7, $FF87FF, $FFAF00, $FFAF5F, $FFAF87, $FFAFAF, $FFAFD7, $FFAFFF, $FFD700, $FFD75F, $FFD787, $FFD7AF,
    $FFD7D7, $FFD7FF, $FFFF00, $FFFF5F, $FFFF87, $FFFFAF, $FFFFD7, $FFFFFF, $080808, $121212, $1C1C1C, $262626, $303030, $3A3A3A, $444444, $4E4E4E,
    $585858, $626262, $6C6C6C, $767676, $808080, $8A8A8A, $949494, $9E9E9E, $A8A8A8, $B2B2B2, $BCBCBC, $C6C6C6, $D0D0D0, $DADADA, $E4E4E4, $EEEEEE);

var
  Default256Palette: array [0 .. 255] of TRGBQuad;

function ipDraw(const dc: HDC; const img: TMat; const rect: System.Types.TRect; const Stretch: Boolean = True): Boolean;
type
  pBitmapInfoHeader = ^TBitmapInfoHeader;
  pBitmapInfo       = ^TBitmapInfo;
var
  BitmapInfo: pBitmapInfo;
/  DIBHdr: pBitmapInfoHeader;
  wimg: TMat;
begin
  if img.empty then
    Exit(false);

  if (img.cols mod 4) <> 0 then
    resize(img, wimg, size((img.cols div 4) * 4, img.rows))
  else
    wimg := img;

  if wimg.channels = 1 then
  begin
    BitmapInfo := AllocMem(SizeOf(TBitmapInfoHeader) + SizeOf(TRGBQuad) * 256);
    Move(Default256Palette, BitmapInfo^.bmiColors[0], SizeOf(Default256Palette));
  end
  else
  begin
    BitmapInfo                          := AllocMem(SizeOf(TBitmapInfoHeader) + SizeOf(TRGBQuad));
    BitmapInfo^.bmiHeader.biCompression := BI_RGB; // Имеет значение для img.channels>1
  end;

  try
    DIBHdr             := @BitmapInfo^.bmiHeader;
    DIBHdr^.biSize     := SizeOf(TBitmapInfoHeader);
    DIBHdr^.biWidth    := wimg.cols;
    DIBHdr^.biHeight   := -wimg.rows;
    DIBHdr^.biPlanes   := 1;
    DIBHdr^.biBitCount := 8 * wimg.channels;

    if Stretch then
    begin
      SetStretchBltMode(dc, COLORONCOLOR);
      SetMapMode(dc, MM_TEXT);
      // Stretch the image to fit the rectangle
      var
        iResult: Integer := StretchDIBits( //
          dc, rect.Left, rect.Top, rect.Width, rect.Height, 0, 0, wimg.cols, wimg.rows, wimg.Data, BitmapInfo^, DIB_RGB_COLORS, SRCCOPY);
      Result := (iResult > 0); // and (iResult <> GDI_ERROR);
    end
    else
    begin
      // Draw without scaling
      var
        iResult: Integer := SetDIBitsToDevice( //
          dc, rect.Left, rect.Top, wimg.cols, wimg.rows, 0, 0, 0, wimg.rows, wimg.Data, BitmapInfo^, DIB_RGB_COLORS);
      Result := (iResult > 0); // and (iResult <> GDI_ERROR);
    end;
  finally
    FreeMem(BitmapInfo);
  end;
end;

function GetDIBits(const Bitmap: TBitmap; out Bits: Pointer; out _Width: DWord): Boolean;
var
  BitsSize: DWord;
begin
  Result := GetDIBits(Bitmap, Bits, BitsSize, _Width);
end;

function GetDIBits(const Bitmap: TBitmap; out Bits: Pointer; out BitsSize, _Width: DWord): Boolean;
var
  BitmapInfo: pBitmapInfo;
  InfoSize: DWord;
begin
  if Bitmap.empty then
    Exit(false);
  BitmapInfo := nil;
  Result     := True;
  try
    GetDIBSizes(Bitmap.Handle, InfoSize, BitsSize);
    BitmapInfo := AllocMem(InfoSize);
    if BitmapInfo = nil then
      Exit(false);
    Bits := AllocMem(BitsSize);
    if Bits = nil then
      Exit(false);
    if not GetDIB(Bitmap.Handle, Bitmap.Palette, BitmapInfo^, Bits^) then
      Exit(false);

    if (BitmapInfo.bmiHeader.biBitCount <> 32) and ((Bitmap.Width mod 4) <> 0) then
      _Width := ((Bitmap.Width div 4) + 1) * 4
    else
      _Width := Bitmap.Width;

  finally
    if BitmapInfo <> nil then
      FreeMem(BitmapInfo, InfoSize);
    if (not Result) and (Bits <> nil) then
      FreeMem(Bits);
  end;
end;

function BmpToMat(const Bitmap: TBitmap; out M: TMat): Boolean;
var
  BitmapInfo: pBitmapInfo;
  InfoSize: DWord;
  Bits: Pointer;
  BitsSize: DWord;
  img_type, new_width: Integer;
begin

  if Bitmap.empty then
    Exit(false);

  BitmapInfo := nil;
  InfoSize   := 0;
  Bits       := nil;
  BitsSize   := 0;
  Result     := True;
  try
    GetDIBSizes(Bitmap.Handle, InfoSize, BitsSize);
    BitmapInfo := AllocMem(InfoSize);
    if BitmapInfo = nil then
      Exit(false);
    Bits := AllocMem(BitsSize);
    if Bits = nil then
      Exit(false);
    if not GetDIB(Bitmap.Handle, Bitmap.Palette, BitmapInfo^, Bits^) then
      Exit(false);
    case Bitmap.PixelFormat of
      pf8bit:
        img_type := CV_8UC1;
      pf16bit:
        img_type := CV_8UC2;
      pf24bit:
        img_type := CV_8UC3;
      pf32bit:
        img_type := CV_8UC4;
    else
      if BitmapInfo.bmiHeader.biBitCount = 32 then
        img_type := CV_8UC4
      else
        Exit(false);
    end;

    if (BitmapInfo.bmiHeader.biBitCount <> 32) and ((Bitmap.Width mod 4) <> 0) then
      new_width := ((Bitmap.Width div 4) + 1) * 4
    else
      new_width := Bitmap.Width;

    M := TMat.Mat(Bitmap.Height, new_width, img_type, Bits, TMat.AUTO_STEP);
    flip(M, M, 0);

  finally
    if BitmapInfo <> nil then
      FreeMem(BitmapInfo, InfoSize);
    if (not Result) and (Bits <> nil) then
      FreeMem(Bits);
  end;
end;

function MatToBmp(const M: TMat; out Bitmap: TBitmap; const PixelFormat: TPixelFormat): Boolean;
begin
  Bitmap             := TBitmap.Create(M.cols, M.rows);
  Bitmap.PixelFormat := PixelFormat;
  ipDraw(Bitmap.Canvas.Handle, M, System.Types.rect(0, 0, M.cols, M.rows), false);
  Result := True;
end;

initialization

{ TODO : Convert to constant array }
for var i := 0 to 255 do
  with Default256Palette[i] do
  begin
    rgbRed      := i;
    rgbGreen    := i;
    rgbBlue     := i;
    rgbReserved := 0;
  end;

end.

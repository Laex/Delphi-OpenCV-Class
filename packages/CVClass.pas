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

unit CVClass;

{$I opt.inc}

interface

uses
  WinApi.Windows,
  WinApi.Messages,
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.SyncObjs,
  VCL.Controls,
  VCL.Graphics,
  VCL.Themes,
  VCL.Direct2D,
  D2D1,
  cpp.utils,
  cv.opencv;

type

  TMat = cv.opencv.TMat;

  ICVDataReceiver = interface
    ['{7EBE0282-0731-45EB-8A1D-1097C2CBC680}']
    procedure TakeMat(const AMat: TMat);
    procedure SetSource(const Value: TObject);
  end;

  ICVDataSource = interface
    ['{03150528-1FB4-4677-9194-D63E38D0B67E}']
    procedure AddReceiver(const CVReceiver: ICVDataReceiver);
    procedure RemoveReceiver(const CVReceiver: ICVDataReceiver);

    function getEnabled: Boolean;
    procedure setEnabled(const Value: Boolean);

    function getObjectName: string;
    // function getMat: TMat;
    // function GetName: string;
    // function getHeight: Integer;
    // function getWidth: Integer;
    // function GetFPS: double;

    property Enabled: Boolean read getEnabled write setEnabled;
    // property Mat: TMat read getMat;
    // property Name: String read GetName;
    // property Width: Integer Read getWidth;
    // property Height: Integer Read getHeight;
    // property FPS: double read GetFPS;
  end;

  TCVReceiverList         = TThreadList<ICVDataReceiver>;
  TOnCVNotify             = procedure(Sender: TObject; const AMat: TMat) of object;
  TOnCVNotifyVar          = procedure(Sender: TObject; var AMat: TMat) of object;
  TOnCVAfterPaint         = TOnCVNotify;
  TOnBeforeNotifyReceiver = TOnCVNotifyVar;

  TCVDataSource = class(TComponent, ICVDataSource)
  protected
    FCVReceivers: TCVReceiverList;
    FOnBeforeNotifyReceiver: TOnBeforeNotifyReceiver;
    FOnCVNotify: TOnCVNotifyVar;
    procedure NotifyReceiver(const AMat: TMat); virtual;

    function getEnabled: Boolean; virtual; abstract;
    procedure setEnabled(const Value: Boolean); virtual; abstract;
    function getObjectName: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddReceiver(const CVReceiver: ICVDataReceiver); virtual;
    procedure RemoveReceiver(const CVReceiver: ICVDataReceiver); virtual;
    function getMat: TMat; virtual; abstract;
  published
    property Enabled: Boolean read getEnabled write setEnabled default False;
    property OnBeforeNotifyReceiver: TOnBeforeNotifyReceiver read FOnBeforeNotifyReceiver write FOnBeforeNotifyReceiver;
    property OnCVNotify: TOnCVNotifyVar read FOnCVNotify write FOnCVNotify;
  end;

  TCVDataReceiver = class(TComponent, ICVDataReceiver)
  private
    [weak]
    FCVSource: ICVDataSource;
  protected
    procedure SetSource(const Value: TObject); virtual;
    procedure SetCVSource(const Value: ICVDataSource); virtual;
  public
    procedure TakeMat(const AMat: TMat); virtual; abstract;
    destructor Destroy; override;
    function isSourceEnabled: Boolean; virtual;
  published
    property Source: ICVDataSource read FCVSource write SetCVSource;
  end;

  TCVDataProxy = class(TCVDataSource, ICVDataReceiver)
  private
    [weak]
    FCVSource: ICVDataSource;
  protected
    procedure SetSource(const Value: TObject); virtual;
    procedure SetCVSource(const Value: ICVDataSource); virtual;
  public
    procedure TakeMat(const AMat: TMat); virtual; abstract;
    destructor Destroy; override;
  published
    property Source: ICVDataSource read FCVSource write SetCVSource;
  end;

  TCVViewPaintEngine = (peGDI, peD2D);

  [ComponentPlatformsAttribute(pidWin64)]
  TCVView = class(TCustomControl, ICVDataReceiver)
  private
    FMat: pMat; // Stored last received Mat
    [weak]
    FCVSource: ICVDataSource;
    FStretch: Boolean;
    FOnAfterPaint: TOnCVAfterPaint;
    FOnBeforePaint: TOnCVNotify;
    FCenter: Boolean;
    FProportional: Boolean;
    FPaintEngine: TCVViewPaintEngine;
    FD2DCanvas: TDirect2DCanvas;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure TakeMat(const AMat: TMat);
    procedure SetSource(const Value: TObject);
    function getMat: TMat;
    procedure setMat(const Value: TMat);
    function PaintRect: System.Types.TRect;
    procedure SetCVSource(const Value: ICVDataSource);
    procedure PaintDisignInfo;
    procedure PaintInRunTime;
    procedure SetPaintEngine(const Value: TCVViewPaintEngine);
    function D2DDraw(const dc: HDC; const img: TMat; const rect: System.Types.TRect): Boolean;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function isSourceEnabled: Boolean;
    function MatIsEmpty: Boolean;
    procedure DrawMat(const AMat: TMat);
  published
    property Source: ICVDataSource read FCVSource write SetCVSource;
    property Mat: TMat read getMat write setMat;
    property Proportional: Boolean read FProportional write FProportional default False;
    property Stretch: Boolean read FStretch write FStretch default True;
    property Center: Boolean read FCenter write FCenter default False;
    property PaintEngine: TCVViewPaintEngine read FPaintEngine write SetPaintEngine;
    property Align;
    property OnAfterPaint: TOnCVAfterPaint read FOnAfterPaint write FOnAfterPaint;
    property OnBeforePaint: TOnCVNotify read FOnBeforePaint write FOnBeforePaint;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelUp;
    property OnMouseWheelDown;
    property Enabled;
  end;

  TOCVLock                   = TLightweightMREW;
  TPersistentAccessProtected = class(TPersistent);

  TCVCaptureThread = class(TThread)
  private type
    TSourceType = (stStream, stFile);
  private
    FSourceType: TSourceType;
    FFileName: string;
    FVideoAPIs: TVideoCaptureAPIs;
    FCameraIndex: Integer;
  private
    FCapture: TVideoCapture;
    FOnNotifyData: TOnCVNotify;
    FOnNoData: TNotifyEvent;
    FThreadDelay: Cardinal;
  protected
    procedure TerminatedSet; override;
    procedure Execute; override;
  public
    constructor Create(const AFileName: string; const AThreadDelay: Cardinal = 1000 div 25; const VideoAPIs: TVideoCaptureAPIs = CAP_ANY); overload;
    constructor Create(const ACameraIndex: Integer; const AThreadDelay: Cardinal = 1000 div 25;
      const VideoAPIs: TVideoCaptureAPIs = CAP_ANY); overload;
    procedure SetResolution(const Width, Height: Double);
    property OnNoData: TNotifyEvent read FOnNoData write FOnNoData;
    property OnNotifyData: TOnCVNotify read FOnNotifyData write FOnNotifyData;
    property ThreadDelay: Cardinal read FThreadDelay write FThreadDelay;
  end;

  TCVCustomResolution = class(TPersistent)
  private
    FWidth, FHeight: Cardinal;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
  published
    property Width: Cardinal read FWidth write FWidth default 800;
    property Height: Cardinal read FHeight write FHeight default 600;
  end;

  {
    Является родителем для источников внутри TCVCaptureSource
 }
  TCVVideoCaptureAPIs = ( //
    ANY,                  // !< Auto detect == 0
    VFW,                  // !< Video For Windows (obsolete, removed)
    V4L,                  // !< V4L/V4L2 capturing support
    V4L2,                 // !< Same as CAP_V4L
    FIREWIRE,             // !< IEEE 1394 drivers
    FIREWARE,             // !< Same value as CAP_FIREWIRE
    IEEE1394,             // !< Same value as CAP_FIREWIRE
    DC1394,               // !< Same value as CAP_FIREWIRE
    CMU1394,              // !< Same value as CAP_FIREWIRE
    QT,                   // !< QuickTime (obsolete, removed)
    UNICAP,               // !< Unicap drivers (obsolete, removed)
    DSHOW,                // !< DirectShow (via videoInput)
    PVAPI,                // !< PvAPI, Prosilica GigE SDK
    OPENNI,               // !< OpenNI (for Kinect)
    OPENNI_ASUS,          // !< OpenNI (for Asus Xtion)
    ANDROID,              // !< Android - not used
    XIAPI,                // !< XIMEA Camera API
    AVFOUNDATION,
    // !< AVFoundation framework for iOS (OS X Lion will have the same API)
    GIGANETIX,     // !< Smartek Giganetix GigEVisionSDK
    MSMF,          // !< Microsoft Media Foundation (via videoInput)
    WINRT,         // !< Microsoft Windows Runtime using Media Foundation
    INTELPERC,     // !< RealSense (former Intel Perceptual Computing SDK)
    REALSENSE,     // !< Synonym for CAP_INTELPERC
    OPENNI2,       // !< OpenNI2 (for Kinect)
    OPENNI2_ASUS,  // !< OpenNI2 (for Asus Xtion and Occipital Structure sensors)
    OPENNI2_ASTRA, // !< OpenNI2 (for Orbbec Astra)
    GPHOTO2,       // !< gPhoto2 connection
    GSTREAMER,     // !< GStreamer
    FFMPEG,        // !< Open and record video file or stream using the FFMPEG library
    IMAGES,        // !< OpenCV Image Sequence (e.g. img_%02d.jpg)
    ARAVIS,        // !< Aravis SDK
    OPENCV_MJPEG,  // !< Built-in OpenCV MotionJPEG codec
    INTEL_MFX,     // !< Intel MediaSDK
    XINE,          // !< XINE engine (Linux)
    UEYE           // !< uEye Camera API
    );

  TCVCustomSource = class(TComponent)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  private
    FOwner: TPersistent;
    FNotifyChange: TNotifyEvent;
    FThreadDelay: Cardinal;
    FCaptureAPIs: TCVVideoCaptureAPIs;
    procedure setThreadDelay(const Value: Cardinal);
    procedure setCaptureAPIs(const Value: TCVVideoCaptureAPIs);
  protected
    function GetOwner: TPersistent; override;
    procedure DoNotifyChange;
    property OnNotifyChange: TNotifyEvent read FNotifyChange write FNotifyChange;
  public
    constructor Create(AOwner: TPersistent); reintroduce; virtual;
    function GetNamePath: string; override;
    property name;
  published
    property Delay: Cardinal read FThreadDelay write setThreadDelay default 0;
    property CaptureAPIs: TCVVideoCaptureAPIs read FCaptureAPIs write setCaptureAPIs default ANY;
  end;

  TCVWebCameraResolution = (r160x120, r176x144, r320x240, r352x288, r424x240, r640x360, r640x480, r800x448, r800x600, r960x544, r1280x720, rCustom);

  TCVWebCameraResolutionValue = record
    W, H: Double;
  end;

const
  CVWebCameraResolutionValue: array [TCVWebCameraResolution] of TCVWebCameraResolutionValue = //
    ( //
    (W: 160; H: 120),  //
    (W: 176; H: 144),  //
    (W: 320; H: 240),  //
    (W: 352; H: 288),  //
    (W: 424; H: 240),  //
    (W: 640; H: 360),  //
    (W: 640; H: 480),  //
    (W: 800; H: 448),  //
    (W: 800; H: 600),  //
    (W: 960; H: 544),  //
    (W: 1280; H: 720), //
    (W: 0; H: 0)       //
    );

type

  [ComponentPlatformsAttribute(pidWin64)]
  TCVWebCameraSource = class(TCVCustomSource)
  private
    FResolution: TCVWebCameraResolution;
    FCameraIndex: Integer;
    FCustomResolution: TCVCustomResolution;
    procedure setCameraIndex(const Value: Integer);
    procedure SetResolution(const Value: TCVWebCameraResolution);
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
  published
    property CameraIndex: Integer read FCameraIndex write setCameraIndex default 0;
    property Resolution: TCVWebCameraResolution read FResolution write SetResolution default r800x600;
    property CustomResolution: TCVCustomResolution read FCustomResolution write FCustomResolution;
  end;

  [ComponentPlatformsAttribute(pidWin64)]
  TCVFileSource = class(TCVCustomSource)
  private
    FFileName: TFileName;
    FLoop: Boolean;
    procedure SetFileName(const Value: TFileName);
  public
    constructor Create(AOwner: TPersistent); override;
  published
    property FileName: TFileName read FFileName write SetFileName;
    property Loop: Boolean read FLoop write FLoop default False;
  end;

  TCVSourceTypeClass = class of TCVCustomSource;

  ICVEditorPropertiesContainer = interface
    ['{418F88DD-E35D-4425-BF24-E753E83D35D6}']
    function GetProperties: TCVCustomSource;
    function GetPropertiesClass: TCVSourceTypeClass;
    procedure SetPropertiesClass(Value: TCVSourceTypeClass);
  end;

  [ComponentPlatformsAttribute(pidWin64)]
  TCVCaptureSource = class(TCVDataSource, ICVEditorPropertiesContainer)
  protected
    FSourceThread: TCVCaptureThread;

    FOperation: TCVCustomSource;
    FOperationClass: TCVSourceTypeClass;

    FEnabled: Boolean;

    // Работа со встроенным свойством
    function GetPropertiesClassName: string;
    procedure SetProperties(const Value: TCVCustomSource);
    procedure SetPropertiesClassName(const Value: string);
    function GetProperties: TCVCustomSource;
    function GetPropertiesClass: TCVSourceTypeClass;
    procedure SetPropertiesClass(Value: TCVSourceTypeClass);
    // Создение/Уничтожение/Пересоздание встроенного свойства
    procedure CreateProperties;
    procedure DestroyProperties;
    procedure RecreateProperties;

    // Вызывается после загрузки компонента
    // Если Enabled=true - запуск потока
    procedure Loaded; override;

    // Зпуск и остановка потока
    procedure setEnabled(const Value: Boolean); override;
    function getEnabled: Boolean; override;

    // Пристыкуется к потоку для получения данных
    procedure OnNotifyDataCaptureThread(Sender: TObject; const AMat: TMat);
    procedure OnNoDataCaptureThread(Sender: TObject);
    procedure OnTerminateCaptureThread(Sender: TObject);

    // События от изменения параметров встроенного свойства
    procedure OnNotifyChange(Sender: TObject);

    // Для внутреннего свойства
    property SourceTypeClass: TCVSourceTypeClass read GetPropertiesClass write SetPropertiesClass;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Создает и запускает поток
    procedure StartCapture;
    // Останавливает и уничтожает поток
    procedure StopCapture;
  published
    property SourceTypeClassName: string read GetPropertiesClassName write SetPropertiesClassName;
    property SourceType: TCVCustomSource read GetProperties write SetProperties;
    property Enabled stored True;
  end;

  [ComponentPlatformsAttribute(pidWin64)]
  TCVVideoWriter = class(TCVDataProxy)
  private
    FWriter: pVideoWriter;
    FFileName: TFileName;
    FEnabled: Boolean;
    FFourCC: AnsiString;
    FFPS: Cardinal;
    FisColored: Boolean;
    FResolution: TCVCustomResolution;
    FSameResolution: Boolean;
    procedure SetFileName(const Value: TFileName);
    procedure CloseWriter;
    procedure OpenWriter(const S: TSize);
    procedure SetFourCC(const Value: AnsiString);
    procedure setFPS(const Value: Cardinal);
    procedure setisColored(const Value: Boolean);
    procedure setSameResolution(const Value: Boolean);
  protected
    procedure setEnabled(const Value: Boolean); override;
    function Writer: pVideoWriter;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure TakeMat(const AMat: TMat); override;
  published
    property Enabled: Boolean read FEnabled write setEnabled default False;
    property OutputFileName: TFileName read FFileName write SetFileName;
    property FourCC: AnsiString read FFourCC write SetFourCC;
    property FPS: Cardinal read FFPS write setFPS default 24;
    property isColored: Boolean read FisColored write setisColored default True;
    property SameResolution: Boolean read FSameResolution write setSameResolution default True;
    property Resolution: TCVCustomResolution read FResolution write FResolution;
  end;

  [ComponentPlatformsAttribute(pidWin64)]
  TRegisteredCaptureSource = class(TStringList)
  public
    function FindByClassName(const ClassName: string): TCVSourceTypeClass;
    function FindByName(const Name: string): TCVSourceTypeClass;
    function GetNameByClass(const IOClass: TClass): string;
    procedure RegisterIOClass(const IOClass: TClass; const ClassName: string);
  end;

function GetRegisteredCaptureSource: TRegisteredCaptureSource;
function CV_FOURCC(const c1, c2, c3, c4: AnsiChar): Integer; overload;
{$IFDEF USE_INLINE}inline;
{$ENDIF}
function CV_FOURCC(const c: AnsiString): Integer; overload;
{$IFDEF USE_INLINE}inline;
{$ENDIF}

implementation

uses
  System.UITypes,
  WinApi.DxgiFormat,
  cv.utils;

var
  _RegisteredCaptureSource: TRegisteredCaptureSource = nil;

function GetRegisteredCaptureSource: TRegisteredCaptureSource;
begin
  if not Assigned(_RegisteredCaptureSource) then
    _RegisteredCaptureSource := TRegisteredCaptureSource.Create;
  Result                     := _RegisteredCaptureSource;
end;

{ TCVDataSource }

procedure TCVDataSource.AddReceiver(const CVReceiver: ICVDataReceiver);
begin
  FCVReceivers.Add(CVReceiver);
end;

constructor TCVDataSource.Create(AOwner: TComponent);
begin
  inherited;
  FCVReceivers := TCVReceiverList.Create;
end;

destructor TCVDataSource.Destroy;
begin
  FCVReceivers.Free;
  inherited;
end;

function TCVDataSource.getObjectName: string;
begin
  Result := name;
end;

procedure TCVDataSource.NotifyReceiver(const AMat: TMat);
var
  R: ICVDataReceiver;
  LockList: TList<ICVDataReceiver>;
  M: TMat;
begin
  M := AMat.Clone;
  if Assigned(FOnBeforeNotifyReceiver) then
    FOnBeforeNotifyReceiver(Self, M);
  LockList := FCVReceivers.LockList;
  try
    for R in LockList do
      R.TakeMat(M);
  finally
    FCVReceivers.UnlockList;
  end;
  if Assigned(FOnCVNotify) then
    FOnCVNotify(Self, M);
end;

procedure TCVDataSource.RemoveReceiver(const CVReceiver: ICVDataReceiver);
begin
  FCVReceivers.Remove(CVReceiver);
end;

{ TCVDataReceiver }

destructor TCVDataReceiver.Destroy;
begin
  if Assigned(FCVSource) then
    FCVSource.RemoveReceiver(Self);
  inherited;
end;

function TCVDataReceiver.isSourceEnabled: Boolean;
begin
  Result := Assigned(FCVSource) and FCVSource.Enabled;
end;

procedure TCVDataReceiver.SetCVSource(const Value: ICVDataSource);
begin
  if (FCVSource <> Value) then
  begin
    if Assigned(FCVSource) then
      FCVSource.RemoveReceiver(Self);
    FCVSource := Value;
    if Assigned(FCVSource) then
      FCVSource.AddReceiver(Self);
  end;
end;

procedure TCVDataReceiver.SetSource(const Value: TObject);
begin
  if (Value <> Self) then
    Source := Value as TCVDataSource;
end;

{ TCVDataProxy }

destructor TCVDataProxy.Destroy;
begin
  if Assigned(FCVSource) then
    FCVSource.RemoveReceiver(Self);
  inherited;
end;

procedure TCVDataProxy.SetCVSource(const Value: ICVDataSource);
begin
  if (FCVSource <> Value) then
  begin
    if Assigned(FCVSource) then
      FCVSource.RemoveReceiver(Self);
    FCVSource := Value;
    if Assigned(FCVSource) then
      FCVSource.AddReceiver(Self);
  end;
end;

procedure TCVDataProxy.SetSource(const Value: TObject);
begin
  if (Value <> Self) then
    Source := Value as TCVDataSource;
end;

{ TCVView }

constructor TCVView.Create(AOwner: TComponent);
begin
  inherited;
  FComponentStyle := FComponentStyle - [csInheritable];
  FStretch        := True;
  FProportional   := False;
  FCenter         := False;
end;

destructor TCVView.Destroy;
begin
  Source := nil;
  if Assigned(FMat) then
  begin
    Dispose(FMat);
    FMat := nil;
  end;
  if Assigned(FD2DCanvas) then
    FreeAndNil(FD2DCanvas);
  inherited;
end;

procedure TCVView.DrawMat(const AMat: TMat);
begin
  if Enabled { or MatIsEmpty } then
  begin
    Mat := AMat;
    Invalidate;
  end;
end;

function TCVView.getMat: TMat;
begin
  if Assigned(FMat) then
    Result := FMat^;
end;

function TCVView.isSourceEnabled: Boolean;
begin
  Result := Assigned(Source) and (Source.Enabled);
end;

function TCVView.MatIsEmpty: Boolean;
begin
  Result := (not Assigned(FMat)) or FMat^.empty;
end;

procedure TCVView.PaintDisignInfo { (var Message: TWMPaint) };
begin
  Canvas.Lock;
  try
    Canvas.Font.Color := clWindowText;
    var
      Text: string := name + ': ' + ClassName;
    var
      TextOneHeight: Integer := Canvas.TextHeight(Text) + 5;
    var
      TextHeight: Integer := TextOneHeight * 2 - 5;
    if not Assigned(Source) then
      TextHeight := TextHeight + TextOneHeight;

    var
      x: Integer := (ClientWidth - Canvas.TextWidth(Text)) div 2;
    var
      y: Integer := (ClientHeight - TextHeight) div 2;
    Canvas.TextOut(x, y, Text);

    Text := '(' + ClientWidth.ToString + ',' + ClientHeight.ToString + ')';
    x    := (ClientWidth - Canvas.TextWidth(Text)) div 2;
    y    := y + TextOneHeight;
    Canvas.TextOut(x, y, Text);
    if Assigned(Source) then
    begin
      Canvas.Font.Color := clWindowText;
      Text              := 'Source: ' + Source.getObjectName;
    end
    else
    begin
      Canvas.Font.Color := clRed;
      Text              := 'Source not defined';
    end;
    x := (ClientWidth - Canvas.TextWidth(Text)) div 2;
    y := y + TextOneHeight;
    Canvas.TextOut(x, y, Text);

    Text := 'Paint engine: ';
    if PaintEngine = peGDI then
      Text := Text + 'GDI'
    else
      Text            := Text + 'Direct2D';
    x                 := (ClientWidth - Canvas.TextWidth(Text)) div 2;
    y                 := y + TextOneHeight;
    Canvas.Font.Color := clBlack;
    Canvas.TextOut(x, y, Text);
  finally
    Canvas.Unlock;
  end;
end;

function TCVView.D2DDraw(const dc: HDC; const img: TMat; const rect: System.Types.TRect): Boolean;
begin
  Result := not img.empty;
  if Result then
  begin
    if not Assigned(FD2DCanvas) then
      FD2DCanvas := TDirect2DCanvas.Create(dc, rect);
    FD2DCanvas.BeginDraw;
    try
      var
        B: TBitmap;
      try
        MatToBmp(img, B, TPixelFormat.pf24bit);
        if Self.Stretch then
          FD2DCanvas.StretchDraw(rect, B)
        else
          FD2DCanvas.Draw(0, 0, B);
      finally
        B.Free;
      end;
    finally
      FD2DCanvas.EndDraw;
    end;
  end;
end;

procedure TCVView.PaintInRunTime;
var
  dc: HDC;
  ps: TPaintStruct;
  R: Boolean;
begin
  dc := BeginPaint(Handle, ps);
  try
    if Assigned(OnBeforePaint) then
      OnBeforePaint(Self, FMat^);
    if PaintEngine = peGDI then
      R := ipDraw(dc, FMat^, PaintRect)
    else
      R := D2DDraw(dc, FMat^, PaintRect);
    if R and Assigned(OnAfterPaint) then
      OnAfterPaint(Self, FMat^);
  finally
    EndPaint(Handle, ps);
  end;
end;

function TCVView.PaintRect: System.Types.TRect;
var
  ViewWidth, ViewHeight, CliWidth, CliHeight: Integer;
  AspectRatio: Double;
begin
  if MatIsEmpty then
    Exit(System.Types.rect(0, 0, 0, 0));

  ViewWidth  := FMat.cols;
  ViewHeight := FMat.rows;
  CliWidth   := ClientWidth;
  CliHeight  := ClientHeight;
  if (Proportional and ((ViewWidth > CliWidth) or (ViewHeight > CliHeight))) or Stretch then
  begin
    if Proportional and (ViewWidth > 0) and (ViewHeight > 0) then
    begin
      AspectRatio := ViewWidth / ViewHeight;
      if ViewWidth > ViewHeight then
      begin
        ViewWidth  := CliWidth;
        ViewHeight := Trunc(CliWidth / AspectRatio);
        if ViewHeight > CliHeight then
        begin
          ViewHeight := CliHeight;
          ViewWidth  := Trunc(CliHeight * AspectRatio);
        end;
      end
      else
      begin
        ViewHeight := CliHeight;
        ViewWidth  := Trunc(CliHeight * AspectRatio);
        if ViewWidth > CliWidth then
        begin
          ViewWidth  := CliWidth;
          ViewHeight := Trunc(CliWidth / AspectRatio);
        end;
      end;
    end
    else
    begin
      ViewWidth  := CliWidth;
      ViewHeight := CliHeight;
    end;
  end;

  with Result do
  begin
    Left   := 0;
    Top    := 0;
    Right  := ViewWidth;
    Bottom := ViewHeight;
  end;

  if Center then
    OffsetRect(Result, (CliWidth - ViewWidth) div 2, (CliHeight - ViewHeight) div 2);
end;

procedure TCVView.SetCVSource(const Value: ICVDataSource);
begin
  if FCVSource <> Value then
  begin
    if Assigned(FCVSource) and (not(csDesigning in ComponentState)) then
      FCVSource.RemoveReceiver(Self);
    FCVSource := Value;
    if Assigned(FCVSource) and (not(csDesigning in ComponentState)) then
      FCVSource.AddReceiver(Self);
  end;
end;

procedure TCVView.setMat(const Value: TMat);
begin
  if not Assigned(FMat) then
    New(FMat);
  FMat^ := Value;
end;

procedure TCVView.SetPaintEngine(const Value: TCVViewPaintEngine);
begin
  if FPaintEngine <> Value then
  begin
    if TDirect2DCanvas.Supported or (Value <> peD2D) then
      FPaintEngine := Value;
    if (csDesigning in ComponentState) then
      Invalidate;
  end;
end;

procedure TCVView.SetSource(const Value: TObject);
begin
  Source := Value as TCVDataSource;
end;

procedure TCVView.TakeMat(const AMat: TMat);
begin
  if (ComponentState * [csDestroying, csDesigning]) = [] then
    DrawMat(AMat);
end;

procedure TCVView.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  if (csDesigning in ComponentState) or MatIsEmpty then
    inherited;
end;

procedure TCVView.WMPaint(var Message: TWMPaint);
begin
  if (csDesigning in ComponentState) then
  begin
    inherited;
    PaintDisignInfo;
  end
  else if not MatIsEmpty then
    PaintInRunTime
  else
    inherited;
end;

procedure TCVView.WMSize(var Message: TWMSize);
begin
  if (not(csDesigning in ComponentState)) and Assigned(FD2DCanvas) then
  begin
    var
      pixelSize: D2D1_SIZE_U := D2D1SizeU(Width, Height);
    ID2D1HwndRenderTarget(FD2DCanvas.RenderTarget).Resize(pixelSize);
  end
  else
    inherited;
end;

{ TCVCaptureSource }

var
  CVVideoCaptureAPIs: array [TCVVideoCaptureAPIs] of TVideoCaptureAPIs = //
    ( //
    CAP_ANY,         // !< Auto detect == 0
    CAP_VFW,         // !< Video For Windows obsolete, removed)
    CAP_V4L,         // !< V4L/V4L2 capturing support
    CAP_V4L2,        // !< Same as CAP_V4L
    CAP_FIREWIRE,    // !< IEEE 1394 drivers
    CAP_FIREWARE,    // !< Same value as CAP_FIREWIRE
    CAP_IEEE1394,    // !< Same value as CAP_FIREWIRE
    CAP_DC1394,      // !< Same value as CAP_FIREWIRE
    CAP_CMU1394,     // !< Same value as CAP_FIREWIRE
    CAP_QT,          // !< QuickTime obsolete, removed)
    CAP_UNICAP,      // !< Unicap drivers obsolete, removed)
    CAP_DSHOW,       // !< DirectShow via videoInput)
    CAP_PVAPI,       // !< PvAPI, Prosilica GigE SDK
    CAP_OPENNI,      // !< OpenNI for Kinect)
    CAP_OPENNI_ASUS, // !< OpenNI for Asus Xtion)
    CAP_ANDROID,     // !< Android - not used
    CAP_XIAPI,       // !< XIMEA Camera API
    CAP_AVFOUNDATION,
    // !< AVFoundation framework for iOS OS X Lion will have the same API)
    CAP_GIGANETIX, // !< Smartek Giganetix GigEVisionSDK
    CAP_MSMF,      // !< Microsoft Media Foundation via videoInput)
    CAP_WINRT,     // !< Microsoft Windows Runtime using Media Foundation
    CAP_INTELPERC, // !< RealSense former Intel Perceptual Computing SDK)
    CAP_REALSENSE, // !< Synonym for CAP_INTELPERC
    CAP_OPENNI2,   // !< OpenNI2 for Kinect)
    CAP_OPENNI2_ASUS,
    // !< OpenNI2 for Asus Xtion and Occipital Structure sensors)
    CAP_OPENNI2_ASTRA, // !< OpenNI2 for Orbbec Astra)
    CAP_GPHOTO2,       // !< gPhoto2 connection
    CAP_GSTREAMER,     // !< GStreamer
    CAP_FFMPEG,
    // !< Open and record video file or stream using the FFMPEG library
    CAP_IMAGES,       // !< OpenCV Image Sequence e.g. img_%02d.jpg)
    CAP_ARAVIS,       // !< Aravis SDK
    CAP_OPENCV_MJPEG, // !< Built-in OpenCV MotionJPEG codec
    CAP_INTEL_MFX,    // !< Intel MediaSDK
    CAP_XINE,         // !< XINE engine Linux)
    CAP_UEYE          // !< uEye Camera API
  );

constructor TCVCaptureSource.Create(AOwner: TComponent);
begin
  inherited;
  FEnabled := False;
end;

procedure TCVCaptureSource.setEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    if csDesigning in ComponentState then
      FEnabled := Value
    else
    begin
      if not Value then
        StopCapture;
      if Value then
        StartCapture;
      FEnabled := Value;
    end;
  end;
end;

procedure TCVCaptureSource.CreateProperties;
begin
  if FOperationClass <> nil then
  begin
    FOperation                := FOperationClass.Create(Self);
    FOperation.OnNotifyChange := OnNotifyChange;
  end;
end;

destructor TCVCaptureSource.Destroy;
begin
  StopCapture;
  inherited;
end;

procedure TCVCaptureSource.DestroyProperties;
begin
  StopCapture;
  FreeAndNil(FOperation);
end;

function TCVCaptureSource.getEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TCVCaptureSource.GetProperties: TCVCustomSource;
begin
  if not Assigned(FOperation) then
    FOperation := TCVWebCameraSource.Create(Self);
  Result       := FOperation;
end;

function TCVCaptureSource.GetPropertiesClass: TCVSourceTypeClass;
begin
  Result := TCVSourceTypeClass(SourceType.ClassType);
end;

function TCVCaptureSource.GetPropertiesClassName: string;
begin
  Result := SourceType.ClassName;
end;

procedure TCVCaptureSource.Loaded;
begin
  inherited;
  if not(csDesigning in ComponentState) then
  begin
    if Enabled then
      StartCapture;
  end;
end;

procedure TCVCaptureSource.OnNoDataCaptureThread(Sender: TObject);
begin
  if FOperationClass = TCVFileSource then
  begin
    var
      FileSource: TCVFileSource := FOperation as TCVFileSource;
    if FileSource.Loop then
      FSourceThread.FCapture.&set(CAP_PROP_POS_FRAMES, 0);
  end;
end;

procedure TCVCaptureSource.OnNotifyChange(Sender: TObject);
begin
  StopCapture;
  if Enabled then
    StartCapture;
end;

procedure TCVCaptureSource.OnNotifyDataCaptureThread(Sender: TObject; const AMat: TMat);
begin
  NotifyReceiver(AMat);
end;

procedure TCVCaptureSource.OnTerminateCaptureThread(Sender: TObject);
begin
  FSourceThread := nil;
  FEnabled      := False;
end;

procedure TCVCaptureSource.RecreateProperties;
begin
  DestroyProperties;
  CreateProperties;
end;

procedure TCVCaptureSource.SetProperties(const Value: TCVCustomSource);
begin
  if (FOperation <> nil) and (Value <> nil) then
    FOperation.Assign(Value);
end;

procedure TCVCaptureSource.SetPropertiesClass(Value: TCVSourceTypeClass);
begin
  if FOperationClass <> Value then
  begin
    FOperationClass := Value;
    RecreateProperties;
  end;
end;

procedure TCVCaptureSource.SetPropertiesClassName(const Value: string);
begin
  SourceTypeClass := TCVSourceTypeClass(GetRegisteredCaptureSource.FindByClassName(Value));
end;

procedure TCVCaptureSource.StartCapture;
begin
  if Assigned(FSourceThread) or (csDesigning in ComponentState) then
    Exit;

  // var
  // OldEnabled: Boolean := FEnabled;

  if FOperationClass = TCVWebCameraSource then
  begin
    var
      WebCameraSource: TCVWebCameraSource := FOperation as TCVWebCameraSource;

    FSourceThread := TCVCaptureThread.Create( //
      WebCameraSource.CameraIndex,            //
      WebCameraSource.Delay,                  //
      CVVideoCaptureAPIs[WebCameraSource.CaptureAPIs]);

    if WebCameraSource.Resolution <> rCustom then
      with CVWebCameraResolutionValue[WebCameraSource.Resolution] do
        FSourceThread.SetResolution(W, H)
    else
      FSourceThread.SetResolution(WebCameraSource.CustomResolution.Width, WebCameraSource.CustomResolution.Height);
  end
  else if FOperationClass = TCVFileSource then
  begin
    var
      FileSource: TCVFileSource := FOperation as TCVFileSource;
    FSourceThread               := TCVCaptureThread.Create( //
      FileSource.FileName, //
      FileSource.Delay, //
      CVVideoCaptureAPIs[FileSource.CaptureAPIs]);
  end;

  if Assigned(FSourceThread) then
  begin
    FSourceThread.OnNoData     := OnNoDataCaptureThread;
    FSourceThread.OnNotifyData := OnNotifyDataCaptureThread;
    FSourceThread.OnTerminate  := OnTerminateCaptureThread;
    // if OldEnabled then
    // begin
    FSourceThread.Start;
    FEnabled := True;
    // end;
  end;
end;

procedure TCVCaptureSource.StopCapture;
begin
  if Assigned(FSourceThread) then
  begin
    FSourceThread.OnNoData     := nil;
    FSourceThread.OnNotifyData := nil;
    FSourceThread.OnTerminate  := nil;
    FSourceThread.Terminate;
    FSourceThread := nil;
    FEnabled      := False;
  end;
end;

{ TRegisteredCaptureSource }

function TRegisteredCaptureSource.FindByClassName(const ClassName: string): TCVSourceTypeClass;
var
  i: Integer;
begin
  Result := nil;
  for i  := 0 to Count - 1 do
    if TCVSourceTypeClass(Objects[i]).ClassName = ClassName then
      Exit(TCVSourceTypeClass(Objects[i]));
end;

function TRegisteredCaptureSource.FindByName(const Name: string): TCVSourceTypeClass;
var
  i: Integer;
begin
  i := IndexOf(name);
  if i <> -1 then
    Result := TCVSourceTypeClass(Objects[i])
  else
    Result := nil;
end;

function TRegisteredCaptureSource.GetNameByClass(const IOClass: TClass): string;
var
  i: Integer;
begin
  Result := '';
  for i  := 0 to Count - 1 do
    if Integer(Objects[i]) = Integer(IOClass) then
    begin
      Result := Self[i];
      Break;
    end;
end;

procedure TRegisteredCaptureSource.RegisterIOClass(const IOClass: TClass; const ClassName: string);
begin
  AddObject(ClassName, TObject(IOClass));
  RegisterClass(TPersistentClass(IOClass));
end;

{ TCVFileSource }

constructor TCVFileSource.Create(AOwner: TPersistent);
begin
  inherited;
  FThreadDelay := 1000 div 25;
end;

procedure TCVFileSource.SetFileName(const Value: TFileName);
begin
  if FFileName <> Value then
  begin
    FFileName := Value;
    DoNotifyChange;
  end;
end;

{ TCVCustomSource }

procedure TCVCustomSource.AssignTo(Dest: TPersistent);
begin
  inherited;

end;

constructor TCVCustomSource.Create(AOwner: TPersistent);
begin
  if AOwner is TComponent then
    inherited Create(AOwner as TComponent)
  else
    inherited Create(nil);
  SetSubComponent(True);
  FOwner := AOwner;
end;

procedure TCVCustomSource.DoNotifyChange;
begin
  if Assigned(FNotifyChange) then
    FNotifyChange(Self);
end;

function TCVCustomSource.GetNamePath: string;
var
  S: string;
  lOwner: TPersistent;
begin
  Result := inherited GetNamePath;
  lOwner := GetOwner;
  if
  { } (lOwner <> nil) and
  { } (
    { } (csSubComponent in TComponent(lOwner).ComponentStyle) or
    { } (TPersistentAccessProtected(lOwner).GetOwner <> nil)
    { } ) then
  begin
    S := lOwner.GetNamePath;
    if S <> '' then
      Result := S + '.' + Result;
  end;
end;

function TCVCustomSource.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TCVCustomSource.setCaptureAPIs(const Value: TCVVideoCaptureAPIs);
begin
  if FCaptureAPIs <> Value then
  begin
    FCaptureAPIs := Value;
    DoNotifyChange;
  end;
end;

procedure TCVCustomSource.setThreadDelay(const Value: Cardinal);
begin
  if FThreadDelay <> Value then
  begin
    FThreadDelay := Value;
    DoNotifyChange;
  end;
end;

{ TCVWebCameraSource }

constructor TCVWebCameraSource.Create(AOwner: TPersistent);
begin
  inherited;
  FCustomResolution := TCVCustomResolution.Create;
  FResolution       := r800x600;
  FThreadDelay      := 0;
end;

destructor TCVWebCameraSource.Destroy;
begin
  FCustomResolution.Free;
  inherited;
end;

procedure TCVWebCameraSource.setCameraIndex(const Value: Integer);
begin
  if FCameraIndex <> Value then
  begin
    FCameraIndex := Value;
    DoNotifyChange;
  end;
end;

procedure TCVWebCameraSource.SetResolution(const Value: TCVWebCameraResolution);
begin
  if FResolution <> Value then
  begin
    FResolution := Value;
    DoNotifyChange;
  end;
end;

{ TCVCaptureThread }

constructor TCVCaptureThread.Create(const AFileName: string; const AThreadDelay: Cardinal; const VideoAPIs: TVideoCaptureAPIs);
begin
  inherited Create(True);
  FThreadDelay    := AThreadDelay;
  FSourceType     := stFile;
  FFileName       := AFileName;
  FVideoAPIs      := VideoAPIs;
  FreeOnTerminate := True;
end;

constructor TCVCaptureThread.Create(const ACameraIndex: Integer; const AThreadDelay: Cardinal; const VideoAPIs: TVideoCaptureAPIs);
begin
  inherited Create(True);
  FThreadDelay    := AThreadDelay;
  FSourceType     := stStream;
  FCameraIndex    := ACameraIndex;
  FVideoAPIs      := VideoAPIs;
  FreeOnTerminate := True;
end;

procedure TCVCaptureThread.Execute;
var
  frame: TMat;
begin
  if FSourceType = stFile then
  begin
    if not FCapture.open(FFileName, FVideoAPIs) then
      Exit;
  end
  else if FSourceType = stStream then
  begin
    if not FCapture.open(FCameraIndex, FVideoAPIs) then
      Exit;
  end
  else
    Assert(False);

  if FCapture.isOpened then
    while not Terminated do
      try
        FCapture.Read(frame);
        if not Terminated then
        begin
          if not frame.empty then
          begin
            if Assigned(OnNotifyData) then
            begin
              OnNotifyData(Self, frame);
              if FThreadDelay <> 0 then
                Sleep(FThreadDelay);
            end;
          end
          else if Assigned(OnNoData) then
            OnNoData(Self);
        end;
      except
        Break;
      end;
end;

procedure TCVCaptureThread.SetResolution(const Width, Height: Double);
begin
  if FCapture.isOpened then
  begin
    FCapture.&set(CAP_PROP_FRAME_WIDTH, Width);
    FCapture.&set(CAP_PROP_FRAME_HEIGHT, Height);
  end;
end;

procedure TCVCaptureThread.TerminatedSet;
begin
  inherited;
  FCapture.Release;
end;

{ TCVCustomResolution }

procedure TCVCustomResolution.AssignTo(Dest: TPersistent);
begin
  if Dest is TCVCustomResolution then
    with TCVCustomResolution(Dest) do
    begin
      FWidth  := Self.FWidth;
      FHeight := Self.FHeight;
    end
  else
    inherited AssignTo(Dest);
end;

constructor TCVCustomResolution.Create;
begin
  inherited;
  FWidth  := 800;
  FHeight := 600;
end;

{ TCVVideoWriter }

procedure TCVVideoWriter.CloseWriter;
begin
  if Assigned(FWriter) then
  begin
    Dispose(FWriter);
    FWriter := nil;
  end;
  FEnabled := False;
end;

constructor TCVVideoWriter.Create(AOwner: TComponent);
begin
  inherited;
  FFourCC         := 'XVID';
  FFPS            := 24;
  FisColored      := True;
  FSameResolution := True;
  FResolution     := TCVCustomResolution.Create;
end;

destructor TCVVideoWriter.Destroy;
begin
  CloseWriter;
  FResolution.Free;
  inherited;
end;

procedure TCVVideoWriter.OpenWriter(const S: TSize);
var
  ex: Int;
begin
  if Writer.isOpened then
    CloseWriter;

  if (Length(FFileName) > 0) then
  begin
    if Length(FFourCC) = 4 then
      ex := CV_FOURCC(FFourCC)
    else
      ex     := -1;
    FEnabled := Writer.open(FFileName, ex, Int(FFPS), S, FisColored);
  end;
end;

procedure TCVVideoWriter.setEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    if (csDesigning in ComponentState) or (csLoading in ComponentState) then
      FEnabled := Value
    else
    begin
      CloseWriter;
      FEnabled := Value;
    end;
  end;
end;

procedure TCVVideoWriter.SetFileName(const Value: TFileName);
begin
  if (not FEnabled) or (csDesigning in ComponentState) or (csLoading in ComponentState) then
  begin
    if FFileName <> Value then
      FFileName := Value;
  end;
end;

procedure TCVVideoWriter.SetFourCC(const Value: AnsiString);
var
  V: AnsiString;
begin
  if (not FEnabled) or (csDesigning in ComponentState) or (csLoading in ComponentState) then
  begin
    V := Value;
    if Length(V) > 4 then
      SetLength(V, 4);
    if (Length(V) = 4) and (FFourCC <> V) then
      FFourCC := V;
  end;
end;

procedure TCVVideoWriter.setFPS(const Value: Cardinal);
begin
  if (FFPS <> Value) and ((not FEnabled) or (csDesigning in ComponentState)) or (csLoading in ComponentState) then
    FFPS := Value;
end;

procedure TCVVideoWriter.setisColored(const Value: Boolean);
begin
  if (FisColored <> Value) and ((not FEnabled) or (csDesigning in ComponentState)) or (csLoading in ComponentState) then
    FisColored := Value;
end;

procedure TCVVideoWriter.setSameResolution(const Value: Boolean);
begin
  if (FSameResolution <> Value) and ((not FEnabled) or (csDesigning in ComponentState)) or (csLoading in ComponentState) then
    FSameResolution := Value;
end;

procedure TCVVideoWriter.TakeMat(const AMat: TMat);
begin
  if not(csDesigning in ComponentState) or (csLoading in ComponentState) then
  begin
    if FEnabled then
    begin
      if not Writer.isOpened then
      begin
        if FSameResolution then
          OpenWriter(AMat.size)
        else
          OpenWriter(size(FResolution.Width, FResolution.Height));
      end;
      if FEnabled then
        Writer.write(AMat);
    end;
    NotifyReceiver(AMat);
  end;
end;

function TCVVideoWriter.Writer: pVideoWriter;
begin
  if not Assigned(FWriter) then
    New(FWriter);
  Result := FWriter;
end;

function CV_FOURCC(const c1, c2, c3, c4: AnsiChar): Integer;
begin
  Result := Integer(c1) + (Integer(c2) shl 8) + (Integer(c3) shl 16) + (Integer(c4) shl 24);
end;

function CV_FOURCC(const c: AnsiString): Integer;
begin
  Assert(Length(c) = 4);
  Result := CV_FOURCC(c[1], c[2], c[3], c[4]);
end;

initialization

GetRegisteredCaptureSource.RegisterIOClass(TCVWebCameraSource, 'Web camera');
GetRegisteredCaptureSource.RegisterIOClass(TCVFileSource, 'From file or stream');

finalization

if Assigned(_RegisteredCaptureSource) then
  FreeAndNil(_RegisteredCaptureSource);

end.

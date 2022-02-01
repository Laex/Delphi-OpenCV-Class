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
unit CVRegVCL;

interface

procedure Register;

implementation

uses
  Windows,
  System.SysUtils,
  System.Classes,
  DesignIntf,
  ToolsAPI,
  CVClass;

{$i '../../source/core/version.inc'}

const
  PackageText = 'Delphi binding for OpenCV';

resourcestring
  resPackageName = PackageText+' '+CV_VERSION;
  resAboutDescription = PackageText;
  resAboutURL = 'https://github.com/Laex/Delphi-OpenCV-Class/';
  resAboutCopyright = 'Copyright (c) 2021 Laentir Valetov';
  resLicense = 'Apache-2.0 License';
  //
  ResENoSplashServices = 'Unable to get Borland Splash Services';
  ResENoAboutServices = 'Unable to get Borland About Services';

procedure Register;
begin
  RegisterComponents('OpenCV', [TCVView, TCVCaptureSource,TCVVideoWriter]);
  RegisterClasses([TCVWebCameraSource, TCVFileSource]);
end;

var
  AboutBoxServices: IOTAAboutBoxServices = nil;
  AboutBoxIndex: Integer = 0;

procedure RegisterAboutBox;
var
  ProductImage: HBITMAP;
begin
  Supports(BorlandIDEServices, IOTAAboutBoxServices, AboutBoxServices);
  Assert(Assigned(AboutBoxServices), ResENoAboutServices);
  ProductImage := LoadBitmap(FindResourceHInstance(HInstance), 'DOCVSPLASH');
  AboutBoxIndex := AboutBoxServices.AddPluginInfo(resPackageName, resAboutDescription, ProductImage, False, resLicense);
end;

procedure UnregisterAboutBox;
begin
  if (AboutBoxIndex <> 0) and Assigned(AboutBoxServices) then
  begin
    AboutBoxServices.RemovePluginInfo(AboutBoxIndex);
    AboutBoxIndex := 0;
    AboutBoxServices := nil;
  end;
end;

procedure RegisterSplashScreen;
var
  ProductImage: HBITMAP;
begin
  Assert(Assigned(SplashScreenServices), ResENoSplashServices);
  ProductImage := LoadBitmap(FindResourceHInstance(HInstance), 'DOCVSPLASH');
  SplashScreenServices.AddPluginBitmap(resPackageName, ProductImage, False, resLicense);
end;

initialization

RegisterSplashScreen;
RegisterAboutBox;

finalization

UnregisterAboutBox;

end.

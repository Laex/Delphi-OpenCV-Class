# Delphi binding for OpenCV

<p align="center">    
	<img src="https://tokei.rs/b1/github/Laex/Delphi-OpenCV-Class" />
	<a href="https://img.shields.io/github/repo-size/Laex/Delphi-OpenCV-Class?style=flat&logo=github&logoColor=whitesmoke&label=Repo%20Size">
      <img alt="Repo Size" src="https://img.shields.io/github/repo-size/Laex/Delphi-OpenCV-Class?style=flat&logo=github&logoColor=whitesmoke&label=Repo%20Size" />
    </a>
    <a href="https://github.com/Laex/Delphi-OpenCV-Class/graphs/contributors">
      <img alt="GitHub Contributors" src="https://img.shields.io/github/contributors/Laex/Delphi-OpenCV-Class" /> 
    </a>
    <a href="https://github.com/Laex/Delphi-OpenCV-Class/issues">
      <img alt="Issues" src="https://img.shields.io/github/issues/Laex/Delphi-OpenCV-Class?color=0088ff" />
    </a>
    <a href="https://github.com/Laex/Delphi-OpenCV-Class/pulls">
      <img alt="GitHub pull requests" src="https://img.shields.io/github/issues-pr/Laex/Delphi-OpenCV-Class?color=0088ff" />
    </a>    
    <a href="https://img.shields.io/github/commit-activity/m/Laex/Delphi-OpenCV-Class">
      <img alt="commit activity" src="https://img.shields.io/github/commit-activity/m/Laex/Delphi-OpenCV-Class" />
    </a>    
	<a href="https://img.shields.io/github/license/Laex/Delphi-OpenCV-Class">
      <img alt="LICENSE" src="https://img.shields.io/github/license/Laex/Delphi-OpenCV-Class" />
    </a>
</p>

* Experimental Delphi binding for OpenCV 4.10.0
* Development environment - Delphi 10.4, 11, 12
* x64 platform only
  
###### Contributors:
* Laentir Valetov (email: laex@bk.ru)
### What is completed (approximately)
---
- [ ] **10%** - core. [Core functionality](https://docs.opencv.org/4.x/d0/de1/group__core.html)
- [ ] **50%** - imgproc. [Image Processing](https://docs.opencv.org/4.x/d7/dbd/group__imgproc.html)
- [x] **100%** - imgcodecs. [Image file reading and writing](https://docs.opencv.org/4.x/d4/da8/group__imgcodecs.html)
- [ ] videoio. [Video I/O](https://docs.opencv.org/4.x/dd/de7/group__videoio.html)
- [x] **100%** - highgui. [High-level GUI](https://docs.opencv.org/4.x/d7/dfc/group__highgui.html) 
- [ ] **10%** - video. [Video Analysis](https://docs.opencv.org/4.x/d7/de9/group__video.html)
- [ ] **0%** - calib3d. [Camera Calibration and 3D Reconstruction](https://docs.opencv.org/4.x/d9/d0c/group__calib3d.html)
- [ ] **1%** - features2d. [2D Features Framework](https://docs.opencv.org/4.x/da/d9b/group__features2d.html)
- [ ] **10%** - objdetect. [Object Detection](https://docs.opencv.org/4.x/d5/d54/group__objdetect.html)
- [ ] **1%** - dnn. [Deep Neural Network module](https://docs.opencv.org/4.x/d6/d0f/group__dnn.html)
- [ ] **0%** - ml. [Machine Learning](https://docs.opencv.org/4.x/dd/ded/group__ml.html)
- [ ] **5%** - flann. [Clustering and Search in Multi-Dimensional Spaces](https://docs.opencv.org/4.x/dc/de5/group__flann.html)
- [ ] **10%** - photo. [Computational Photography](https://docs.opencv.org/4.x/d1/d0d/group__photo.html)
- [ ] **30%** - stitching. [Images stitching](https://docs.opencv.org/4.x/d1/d46/group__stitching.html)
- [ ] **0%** - gapi. [Graph API](https://docs.opencv.org/4.x/d0/d1e/gapi.html)
### Usage:
---
1. Download the current state of the project in [zip][1] format or via [git][2]
2. Download [OpenCV 4.10.0][3], run and unpack to the selected directory \(for example in `<opencv>`).

The following DLLs are required to work properly

- **from** `<opencv>\build\x64\vc16\bin\` or **from** `<delphi-opencv>\redist\opencv_delphi4100.zip`
  * opencv_videoio_ffmpeg4100_64.dll - `release`
  * opencv_videoio_msmf4100_64.dll   - `release`
  * opencv_videoio_msmf4100_64d.dll  - `debug`
  * opencv_world4100.dll             - `release`
  * opencv_world4100d.dll            - `debug`

- **from** `redist\opencv_delphi4100.zip`**
  * opencv_delphi4100.dll - `release`
  * opencv_delphi4100d.dll - `debug`

**For the library to work properly, you need**
- place next to the executable file of your program (examples of usage from `<delphi-opencv>\samples` are compiled into `<delphi-opencv>\bin`)<br>
**or**
- to use in any projects for the x64 platform, you need to copy the `dll` files to `C:\Windows\System32\`.

> [!WARNING]
> You may not have the Microsoft runtime libraries installed on your computer, which are required for OpenCV to work. Take advantage of the [advice](https://answers.microsoft.com/en-us/windows/forum/all/vcruntime140dll-and-msvcp140dll-missing-in-windows/caf454d1-49f4-4d2b-b74a-c83fb7c38625) ([source](https://docs.microsoft.com/en-us/cpp/windows/latest-supported-vc-redist)):
> 
> "The lack of these DLLs can be fixed by installing "Microsoft Visual C++ 2015 - 2022 Redistributable". Probably the application that is showing this error depends on it installed. Download and install both the [x86](https://aka.ms/vs/17/release/vc_redist.x86.exe) and [x64](https://aka.ms/vs/17/release/vc_redist.x64.exe) versions"

> [!IMPORTANT]
> You can check the availability of the required libraries (dll) for running projects using the [CheckCVDep.exe][4] utility (`<delphi-opencv>\cvCheckDep`)

3. Set Delphi environment setting

Add the Library path for the modules of the project in Delphi IDE:<br>
`Tools-Options` -> `Language-Delphi-Library-"Windows 64-bit"-Library path` add path `<delphi-opencv>\source\` and `<delphi-opencv>\packages`

### To run demo examples
---
1. Change the path in the `OpenCVRootPath` variable in the `<delphi-opencv>\source\CVResource.pas` module to the path to the unpacked [OpenCV 4.10.0][3] library. This is only needed to run examples from samples.
2. Open `<delphi-opencv>\samples\Samples.groupproj`

### To instal components
---
1. For components, install the packages from `<delphi-opencv>\packages\<Delphi Version>\CVClassGroup.groupproj`
1. Run demo from `<delphi-opencv>\samples\Samples.groupproj`
---------------------------
###### Donate
* <a href="https://liberapay.com/Laex/donate"><img alt="Donate using Liberapay" src="https://liberapay.com/assets/widgets/donate.svg"></a>
* [Boosty](https://boosty.to/laex/donate)
* [Patreon](https://patreon.com/laentir?utm_medium=unknown&utm_source=join_link&utm_campaign=creatorshare_creator&utm_content=copyLink)
* BTC: 3MTXVtRgQnA22EtBxP97Nws6GS8autp38s

[1]: https://github.com/Laex/Delphi-OpenCV-Class/archive/refs/heads/main.zip
[2]: https://github.com/Laex/Delphi-OpenCV-Class.git
[3]: https://opencv.org/releases/
[4]: https://github.com/Laex/Delphi-OpenCV-Class/raw/refs/heads/main/cvCheckDep/CheckCVDep.exe

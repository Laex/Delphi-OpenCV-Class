# Delphi binding for OpenCV
* Experimental Delphi binding for OpenCV 4.10.0
* Development environment - Delphi 10.4, 11, 12
* x64 platform only
##### Contributors:
* Laentir Valetov (email: laex@bk.ru)
## Usage:
1. Download the current state of the project as [zip][1] or as [git][2]
2. Download [OpenCV 4.10.0][3], run and unpack to the selected directory.<br>
The following DLLs are required to work properly<br>
__From `<opencv>\build\x64\vc16\bin\` or from `redist\opencv_delphi4100.zip`__
* opencv_videoio_ffmpeg4100_64.dll - `release`<br>
* opencv_videoio_msmf4100_64.dll   - `release`<br>
* opencv_world4100.dll             - `release`<br>
* opencv_videoio_msmf4100_64d.dll  - `debug`<br>
* opencv_world4100d.dll            - `debug`<br>
**From `redist\opencv_delphi4100.zip`**
* opencv_delphi4100.dll - `release`<br>
* opencv_delphi4100d.dll - `debug`<br>

__For the library to work properly, you need__
- place next to the executable file of your program. To run examples - in `<project>\bin\`<br>
**or**
- copy to `C:\Windows\System32\`

3. Set Delphi environment setting
> Add the Library path for the modules of the project in Delphi IDE:<br>
`Tools-Options` -> `Language-Delphi-Library-"Windows 64-bit"-Library path` add path<br>`<project>\source\` and `<project>\packages`
#### To run demo examples ####
1. Change path `OpenCVRootPath` in module` CVResource.pas` to path to unpacked OpenCV `<opencv>`. This is only needed to run examples from samples.
2. Open `<project>\samples\Samples.groupproj`
#### To instal components ####
1. For components, install the packages from<br>
`<project>\packages\<Delphi Version>\CVClassGroup.groupproj`
2. Run demo from `<project>\samples\Component\`
#### If VCRUNTIME is missing ####
Use the [advice](https://answers.microsoft.com/en-us/windows/forum/all/vcruntime140dll-and-msvcp140dll-missing-in-windows/caf454d1-49f4-4d2b-b74a-c83fb7c38625) ([Source](https://docs.microsoft.com/en-us/cpp/windows/latest-supported-vc-redist))
The lack of these DLLs can be fixed by installing "Microsoft Visual C++ 2015 - 2022 Redistributable"
Probably the application that is showing this error depends on it installed
Download and install both the [x86](https://aka.ms/vs/17/release/vc_redist.x86.exe) and [x64](https://aka.ms/vs/17/release/vc_redist.x64.exe) versions
---------------------------
**Donate**<br>
[Patreon](https://patreon.com/laentir)
BTC: 3MTXVtRgQnA22EtBxP97Nws6GS8autp38s

[1]: https://github.com/Laex/Delphi-OpenCV-Class/archive/refs/heads/main.zip
[2]: https://github.com/Laex/Delphi-OpenCV-Class.git
[3]: https://opencv.org/releases/

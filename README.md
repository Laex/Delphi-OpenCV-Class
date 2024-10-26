# Delphi binding for OpenCV
* Experimental Delphi binding for OpenCV 4.10.0
* Development environment - Delphi 10.4, 11, 12
* x64 platform only
###### Contributors:
* Laentir Valetov (email: laex@bk.ru)
#### Usage:
1. Download the current state of the project as [zip][1] or as [git][2]
2. Download [OpenCV 4.10.0][3], run and unpack to the selected directory.
The following DLLs are required to work properly

**from** `<opencv>\build\x64\vc16\bin\` or from `redist\opencv_delphi4100.zip`
* opencv_videoio_ffmpeg4100_64.dll - `release`
* opencv_videoio_msmf4100_64.dll   - `release`
* opencv_videoio_msmf4100_64d.dll  - `debug`
* opencv_world4100.dll             - `release`
* opencv_world4100d.dll            - `debug`

**from** `redist\opencv_delphi4100.zip`**
* opencv_delphi4100.dll - `release`
* opencv_delphi4100d.dll - `debug`

**For the library to work properly, you need**
- place next to the executable file of your program (examples of usage from `<delphi-opencv>\samples` are compiled into `<delphi-opencv>\bin`)<br>
**or**
- to use in any projects for the x64 platform, you need to copy the `dll` files to `C:\Windows\System32\`.

3. Set Delphi environment setting
> Add the Library path for the modules of the project in Delphi IDE:
`Tools-Options` -> `Language-Delphi-Library-"Windows 64-bit"-Library path` add path `<delphi-opencv>\source\` and `<delphi-opencv>\packages`
#### To run demo examples
1. Change the path in the `OpenCVRootPath` variable in the `<delphi-opencv>\source\CVResource.pas` module to the path to the unpacked [OpenCV 4.10.0][3] library. This is only needed to run examples from samples.
2. Open `<delphi-opencv>\samples\Samples.groupproj`
#### To instal components
1. For components, install the packages from `<delphi-opencv>\packages\<Delphi Version>\CVClassGroup.groupproj`
1. Run demo from `<delphi-opencv>\samples\Samples.groupproj`
> [!WARNING]
> You may not have the Microsoft runtime libraries installed on your computer, which are required for OpenCV to work. Take advantage of the [advice](https://answers.microsoft.com/en-us/windows/forum/all/vcruntime140dll-and-msvcp140dll-missing-in-windows/caf454d1-49f4-4d2b-b74a-c83fb7c38625) ([source](https://docs.microsoft.com/en-us/cpp/windows/latest-supported-vc-redist)). The lack of these DLLs can be fixed by installing "Microsoft Visual C++ 2015 - 2022 Redistributable". Probably the application that is showing this error depends on it installed. Download and install both the [x86](https://aka.ms/vs/17/release/vc_redist.x86.exe) and [x64](https://aka.ms/vs/17/release/vc_redist.x64.exe) versions

> [!IMPORTANT]
> You can check the availability of the required libraries (dll) for running projects using the [CheckCVDep.exe][4] utility (`<delphi-opencv>\cvCheckDep`)

---------------------------
###### Donate
* [Boosty](https://boosty.to/laex/donate)
* [Patreon](https://patreon.com/laentir?utm_medium=unknown&utm_source=join_link&utm_campaign=creatorshare_creator&utm_content=copyLink)
* BTC: 3MTXVtRgQnA22EtBxP97Nws6GS8autp38s

[1]: https://github.com/Laex/Delphi-OpenCV-Class/archive/refs/heads/main.zip
[2]: https://github.com/Laex/Delphi-OpenCV-Class.git
[3]: https://opencv.org/releases/
[4]: https://github.com/Laex/Delphi-OpenCV-Class/raw/refs/heads/main/cvCheckDep/CheckCVDep.exe

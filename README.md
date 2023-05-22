# Delphi binding for OpenCV
* Experimental Delphi binding for OpenCV 4.7.0
* Development environment - Delphi 10.4, 11
* x64 platform only

![](https://tokei.rs/b1/github/Laex/Delphi-OpenCV-Class)
##### Contributors:
+ Laentir Valetov (email: laex@bk.ru)
## Usage:
1. Download the current state of the project as [zip][1] or as [git][2]
2. Download [OpenCV 4.7.0][3], run and unpack to the selected directory.<br>
The following DLLs are required to work properly<br>
__From `<opencv>\build\x64\vc16\bin\`__
* opencv_videoio_ffmpeg470_64.dll - `release`<br>
* opencv_videoio_msmf470_64.dll   - `release`<br>
* opencv_world470.dll             - `release`<br>
* opencv_videoio_msmf470_64d.dll  - `debug`<br>
* opencv_world470d.dll            - `debug`<br>
**From `<project>\bin\`**
* opencv_delphi470.dll - `release`<br>
* opencv_delphi470d.dll - `debug`<br>
                                                                                       
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
---------------------------
**Donate**<br>
[PayPal USD](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=5Z5JQ7C9JCJQN)<br>
[PayPal EUR](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=WQYST8J8PR4K2)<br>
[PayPal RUB](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=XN8D6TJMSXPFL)<br>
[Yandex Money](https://money.yandex.ru/to/410011600173245)<br>
[![Donatecoins](http://donatecoins.org/btc/3MTXVtRgQnA22EtBxP97Nws6GS8autp38s.svg)](http://donatecoins.org/btc/3MTXVtRgQnA22EtBxP97Nws6GS8autp38s)

[1]: https://github.com/Laex/Delphi-OpenCV-Class/archive/refs/heads/main.zip
[2]: https://github.com/Laex/Delphi-OpenCV-Class.git
[3]: https://github.com/opencv/opencv/releases/download/4.7.0/opencv-4.7.0-windows.exe

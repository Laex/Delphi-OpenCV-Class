# Delphi-OpenCV for C++ classes
* Experimental Delphi binding for OpenCV 4.5.4
* Development environment - Delphi 10.4, 11
##### Contributors:
+ Laentir Valetov (email: laex@bk.ru)
## Usage:
1. Download the current state of the project as [zip][1] or as [git][2]
2. Download [OpenCV 4.5.4][3], run and unpack to the selected directory.
Copy dynamic libraries files from `<opencv>\build\x64\vc15\bin\` to `<project>\bin\`
> opencv_videoio_ffmpeg454_64.dll - `release`<br>
> opencv_videoio_msmf454_64.dll   - `release`<br>
> opencv_world454.dll             - `release`<br>
> opencv_videoio_msmf454_64d.dll  - `debug`<br>
> opencv_world454d.dll            - `debug`<br>
3. Set Delphi environment setting
> Add the Library path for the modules of the project in Delphi IDE:<br>
`Tools-Options` -> `Language-Delphi-Library-"Windows 64-bit"-Library path` add path to `<project>\source\`
4. Change path `OpenCVRootPath` in module` CVResource.pas` to path to unpacked OpenCV `<opencv>`. This is only needed to run examples from samples.
5. Open `<project>\samples\Samples.groupproj`
---------------------------
**Donate**<br>
[PayPal USD](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=5Z5JQ7C9JCJQN)<br>
[PayPal EUR](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=WQYST8J8PR4K2)<br>
[PayPal RUB](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=XN8D6TJMSXPFL)<br>
[Yandex Money](https://money.yandex.ru/to/410011600173245)<br>
[![Donatecoins](http://donatecoins.org/btc/3MTXVtRgQnA22EtBxP97Nws6GS8autp38s.svg)](http://donatecoins.org/btc/3MTXVtRgQnA22EtBxP97Nws6GS8autp38s)

[1]: https://github.com/Laex/Delphi-OpenCV-Class/archive/refs/heads/main.zip
[2]: https://github.com/Laex/Delphi-OpenCV-Class.git
[3]: https://github.com/opencv/opencv/releases/download/4.5.4/opencv-4.5.4-vc14_vc15.exe
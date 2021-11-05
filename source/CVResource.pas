unit CVResource;

interface

const
  OpenCVRootPath = 'C:\OpenCV\opencv\';
  OpenCVData     = OpenCVRootPath + 'sources\samples\data\';
  OprnCVHaar     = OpenCVRootPath + 'sources\data\haarcascades\';

implementation

{$IFDEF DEBUG}

Uses
  System.SysUtils;

initialization

Assert(DirectoryExists(OpenCVRootPath), 'Specify the path to OpenCV');
{$ENDIF}

end.

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
program delaunay2;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  WinApi.Windows,
  System.SysUtils,
  cpp.utils,
  cv.resource,
  cv.opencv;

procedure help;
begin
  cout + '\nThis program demonstrates iterative construction of\n' + //
    'delaunay triangulation and voronoi tessellation.\n' + //
    'It draws a random set of points in an image and then delaunay triangulates them.\n' + //
    'Usage: \n';
  cout + argv[0];
  cout + '\n\nThis program builds the triangulation interactively, you may stop this process by\n' + //
    'hitting any key.\n';
end;

procedure draw_subdiv_point(const img: TMat; const fp: TPoint2f; const color: TScalar);
begin
  circle(img, fp, 3, color, Int(FILLED), LINE_8, 0);
end;

procedure draw_subdiv(const img: TMat; const subdiv: TSubdiv2D; const delaunay_color: TScalar);
begin

{$DEFINE one}
{$IFDEF one}
  var
    triangleList: vector<TVec6f>;
  subdiv.getTriangleList(triangleList);
  var
    pt: vector<TPoint> := 3;

  for Var i := 0 to triangleList.size() - 1 do
  begin
    Var
      t: TVec6f := triangleList[i];
    pt[0] := Point(cvRound(t[0]), cvRound(t[1]));
    pt[1] := Point(cvRound(t[2]), cvRound(t[3]));
    pt[2] := Point(cvRound(t[4]), cvRound(t[5]));
    line(img, pt[0], pt[1], delaunay_color, 1, LINE_AA, 0);
    line(img, pt[1], pt[2], delaunay_color, 1, LINE_AA, 0);
    line(img, pt[2], pt[0], delaunay_color, 1, LINE_AA, 0);
  end;
{$ELSE}
  var
    edgeList: vector<TVec4f>;
  subdiv.getEdgeList(edgeList);
  for var i := 0 to edgeList.size() - 1 do
  begin
    var
      e: TVec4f := edgeList[i];
    var
      pt0: TPoint := Point(cvRound(e[0]), cvRound(e[1]));
    var
      pt1: TPoint := Point(cvRound(e[2]), cvRound(e[3]));
    line(img, pt0, pt1, delaunay_color, 1, LINE_AA, 0);
  end;
{$ENDIF}
end;

procedure locate_point(const img: TMat; const subdiv: TSubdiv2D; const fp: TPoint2f; const active_color: TScalar);
begin
  var
    e0: Int := 0;
  var
    vertex: Int := 0;

  subdiv.locate(fp, e0, vertex);

  if (e0 > 0) then
  begin
    var
      e: Int := e0;
      repeat var org, dst: TPoint2f;
    if (subdiv.edgeOrg(e, org) > 0) and (subdiv.edgeDst(e, dst) > 0) then
      line(img, org, dst, active_color, 3, LINE_AA, 0);

    e := subdiv.getEdge(e, TSubdiv2D.NEXT_AROUND_LEFT);
    until e <> e0;
  end;
  draw_subdiv_point(img, fp, active_color);
end;

procedure paint_voronoi(const img: TMat; const subdiv: TSubdiv2D);
begin
  var
    facets: vector<vector<TPoint2f>>;
  var
    centers: vector<TPoint2f>;
  subdiv.getVoronoiFacetList(vector<Int>.vector, facets, centers);

  var
    ifacet: vector<TPoint>;
  var
    ifacets: vector < vector < TPoint >> := 1;

  for Var i := 0 to facets.size() - 1 do
  begin
    ifacet.resize(facets[i].size());
    for var j := 0 to facets[i].size() - 1 do
      ifacet[j] := facets[i][j];

    var
      color: TScalar;
    color[0] := random(256);
    color[1] := random(256);
    color[2] := random(256);
    fillConvexPoly(img, ifacet, color, 8, 0);

    ifacets[0] := ifacet;
    polylines(img, ifacets, true, TScalar.Scalar(), 1, Int(LINE_AA), 0);
    circle(img, centers[i], 3, TScalar.Scalar(), Int(FILLED), LINE_AA, 0);
  end;
end;

begin
  try
    help;

    var
      active_facet_color: TScalar := [0, 0, 255];
    Var
      delaunay_color: TScalar := [255, 255, 255];
    Var
      rect: TRect := [0, 0, 600, 600];

    Var
      subdiv: TSubdiv2D := rect;

    Var
      img: TMat := TMat.Mat(rect.size, CV_8UC3);

    img.Assign(TScalar.all(0));
    var
      win: string := 'Delaunay Demo';
    imshow(win, img);

    for Var i := 0 to 199 do
    begin
      var
        fp: TPoint2f := Point2f(random(rect.width - 10) + 5, random(rect.height - 10) + 5);

      locate_point(img, subdiv, fp, active_facet_color);
      imshow(win, img);

      if (waitKey(100) >= 0) then
        break;

      subdiv.insert(fp);

      img.Assign(TScalar.all(0));
      draw_subdiv(img, subdiv, delaunay_color);
      imshow(win, img);

      if (waitKey(100) >= 0) then
        break;
    end;

    img.Assign(TScalar.all(0));
    paint_voronoi(img, subdiv);
    imshow(win, img);

    waitKey(0);

  except
    on e: Exception do
    begin
      WriteLn(e.ClassName, ': ', e.Message);
      Readln;
    end;
  end;

end.

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

  // 998	??0QuadEdge@Subdiv2D@cv@@QEAA@H@Z	??0QuadEdge@Subdiv2D@cv@@QEAA@H@Z	cv::Subdiv2D::QuadEdge::QuadEdge(int)
  // 999	??0QuadEdge@Subdiv2D@cv@@QEAA@XZ	??0QuadEdge@Subdiv2D@cv@@QEAA@XZ	cv::Subdiv2D::QuadEdge::QuadEdge(void)
  // 1182	??0Subdiv2D@cv@@QEAA@$$QEAV01@@Z	??0Subdiv2D@cv@@QEAA@$$QEAV01@@Z	cv::Subdiv2D::Subdiv2D(class cv::Subdiv2D &&)
  // 1183	??0Subdiv2D@cv@@QEAA@AEBV01@@Z	??0Subdiv2D@cv@@QEAA@AEBV01@@Z	cv::Subdiv2D::Subdiv2D(class cv::Subdiv2D const &)
  {
    1184
    ??0Subdiv2D@cv@@QEAA@V?$Rect_@H@1@@Z
    ??0Subdiv2D@cv@@QEAA@V?$Rect_@H@1@@Z
    cv::Subdiv2D::Subdiv2D(class cv::Rect_<int>)
  }
procedure constructor_Subdiv2D(Var Obj: TSubdiv2D; const rect: TRect);
overload; external opencv_world_dll name '??0Subdiv2D@cv@@QEAA@V?$Rect_@H@1@@Z'{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
  {
    1185
    ??0Subdiv2D@cv@@QEAA@XZ
    ??0Subdiv2D@cv@@QEAA@XZ
    cv::Subdiv2D::Subdiv2D(void)
  }
procedure constructor_Subdiv2D(Var Obj: TSubdiv2D); overload;
external opencv_world_dll name '??0Subdiv2D@cv@@QEAA@XZ'{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
  // 1310	??0Vertex@Subdiv2D@cv@@QEAA@V?$Point_@M@2@_NH@Z	??0Vertex@Subdiv2D@cv@@QEAA@V?$Point_@M@2@_NH@Z	cv::Subdiv2D::Vertex::Vertex(class cv::Point_<float>,bool,int)
  // 1311	??0Vertex@Subdiv2D@cv@@QEAA@XZ	??0Vertex@Subdiv2D@cv@@QEAA@XZ	cv::Subdiv2D::Vertex::Vertex(void)
  {
    1761
    ??1Subdiv2D@cv@@QEAA@XZ
    ??1Subdiv2D@cv@@QEAA@XZ
    cv::Subdiv2D::~Subdiv2D(void)
  }
procedure destructor_Subdiv2D(const Obj: TSubdiv2D); external opencv_world_dll name '??1Subdiv2D@cv@@QEAA@XZ'{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
  // 2421	??4QuadEdge@Subdiv2D@cv@@QEAAAEAU012@$$QEAU012@@Z	??4QuadEdge@Subdiv2D@cv@@QEAAAEAU012@$$QEAU012@@Z	struct cv::Subdiv2D::QuadEdge & cv::Subdiv2D::QuadEdge::operator=(struct cv::Subdiv2D::QuadEdge &&)
  // 2422	??4QuadEdge@Subdiv2D@cv@@QEAAAEAU012@AEBU012@@Z	??4QuadEdge@Subdiv2D@cv@@QEAAAEAU012@AEBU012@@Z	struct cv::Subdiv2D::QuadEdge & cv::Subdiv2D::QuadEdge::operator=(struct cv::Subdiv2D::QuadEdge const &)
  {
    2544
    ??4Subdiv2D@cv@@QEAAAEAV01@$$QEAV01@@Z
    ??4Subdiv2D@cv@@QEAAAEAV01@$$QEAV01@@Z
    class cv::Subdiv2D & cv::Subdiv2D::operator=(class cv::Subdiv2D &&)
  }
procedure operator_assign_Subdiv2D(const Obj: TSubdiv2D; const v: TSubdiv2D); external opencv_world_dll name '??4Subdiv2D@cv@@QEAAAEAV01@$$QEAV01@@Z'{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
  {
    2545
    ??4Subdiv2D@cv@@QEAAAEAV01@AEBV01@@Z
    ??4Subdiv2D@cv@@QEAAAEAV01@AEBV01@@Z
    class cv::Subdiv2D & cv::Subdiv2D::operator=(class cv::Subdiv2D const &)
  }
procedure operator_assign_Subdiv2D_const(const Obj: TSubdiv2D; const v: TSubdiv2D); external opencv_world_dll name '??4Subdiv2D@cv@@QEAAAEAV01@AEBV01@@Z'{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
  // 2615	??4Vertex@Subdiv2D@cv@@QEAAAEAU012@$$QEAU012@@Z	??4Vertex@Subdiv2D@cv@@QEAAAEAU012@$$QEAU012@@Z	struct cv::Subdiv2D::Vertex & cv::Subdiv2D::Vertex::operator=(struct cv::Subdiv2D::Vertex &&)
  // 2616	??4Vertex@Subdiv2D@cv@@QEAAAEAU012@AEBU012@@Z	??4Vertex@Subdiv2D@cv@@QEAAAEAU012@AEBU012@@Z	struct cv::Subdiv2D::Vertex & cv::Subdiv2D::Vertex::operator=(struct cv::Subdiv2D::Vertex const &)
  // 3729	?calcVoronoi@Subdiv2D@cv@@IEAAXXZ	?calcVoronoi@Subdiv2D@cv@@IEAAXXZ	void cv::Subdiv2D::calcVoronoi(void)
  // 3773	?checkSubdiv@Subdiv2D@cv@@IEBAXXZ	?checkSubdiv@Subdiv2D@cv@@IEBAXXZ	void cv::Subdiv2D::checkSubdiv(void)
  // 3902	?clearVoronoi@Subdiv2D@cv@@IEAAXXZ	?clearVoronoi@Subdiv2D@cv@@IEAAXXZ	void cv::Subdiv2D::clearVoronoi(void)
  // 4015	?connectEdges@Subdiv2D@cv@@IEAAHHH@Z	?connectEdges@Subdiv2D@cv@@IEAAHHH@Z	int cv::Subdiv2D::connectEdges(int,int)
  // 4372	?deleteEdge@Subdiv2D@cv@@IEAAXH@Z	?deleteEdge@Subdiv2D@cv@@IEAAXH@Z	void cv::Subdiv2D::deleteEdge(int)
  // 4373	?deletePoint@Subdiv2D@cv@@IEAAXH@Z	?deletePoint@Subdiv2D@cv@@IEAAXH@Z	void cv::Subdiv2D::deletePoint(int)
  {
    4553
    ?edgeDst@Subdiv2D@cv@@QEBAHHPEAV?$Point_@M@2@@Z
    ?edgeDst@Subdiv2D@cv@@QEBAHHPEAV?$Point_@M@2@@Z
    int cv::Subdiv2D::edgeDst(int,class cv::Point_<float> *)
  }
function edgeDst_Subdiv2D(const Obj: TSubdiv2D; edge: int; var dstpt: TPoint2f): int;
  external opencv_world_dll name '?edgeDst@Subdiv2D@cv@@QEBAHHPEAV?$Point_@M@2@@Z'{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
  {
    4554
    ?edgeOrg@Subdiv2D@cv@@QEBAHHPEAV?$Point_@M@2@@Z
    ?edgeOrg@Subdiv2D@cv@@QEBAHHPEAV?$Point_@M@2@@Z
    int cv::Subdiv2D::edgeOrg(int,class cv::Point_<float> *)
  }
function edgeOrg_Subdiv2D(const Obj: TSubdiv2D; edge: int; var orgpt: TPoint2f): int;
  external opencv_world_dll name '?edgeOrg@Subdiv2D@cv@@QEBAHHPEAV?$Point_@M@2@@Z'{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
  // 4764	?findNearest@Subdiv2D@cv@@QEAAHV?$Point_@M@2@PEAV32@@Z	?findNearest@Subdiv2D@cv@@QEAAHV?$Point_@M@2@PEAV32@@Z	int cv::Subdiv2D::findNearest(class cv::Point_<float>,class cv::Point_<float> *)
  {
    4930
    ?getEdge@Subdiv2D@cv@@QEBAHHH@Z
    ?getEdge@Subdiv2D@cv@@QEBAHHH@Z
    int cv::Subdiv2D::getEdge(int,int)
  }
function getEdge_Subdiv2D(const Obj: TSubdiv2D; edge: int; nextEdgeType: int): int; external opencv_world_dll name '?getEdge@Subdiv2D@cv@@QEBAHHH@Z'{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
  // 4931	?getEdgeList@Subdiv2D@cv@@QEBAXAEAV?$vector@V?$Vec@M$03@cv@@V?$allocator@V?$Vec@M$03@cv@@@std@@@std@@@Z	?getEdgeList@Subdiv2D@cv@@QEBAXAEAV?$vector@V?$Vec@M$03@cv@@V?$allocator@V?$Vec@M$03@cv@@@std@@@std@@@Z	void cv::Subdiv2D::getEdgeList(class std::vector<class cv::Vec<float,4>,class std::allocator<class cv::Vec<float,4> > > &)
  // 5005	?getLeadingEdgeList@Subdiv2D@cv@@QEBAXAEAV?$vector@HV?$allocator@H@std@@@std@@@Z	?getLeadingEdgeList@Subdiv2D@cv@@QEBAXAEAV?$vector@HV?$allocator@H@std@@@std@@@Z	void cv::Subdiv2D::getLeadingEdgeList(class std::vector<int,class std::allocator<int> > &)
  {
    5152
    ?getTriangleList@Subdiv2D@cv@@QEBAXAEAV?$vector@V?$Vec@M$05@cv@@V?$allocator@V?$Vec@M$05@cv@@@std@@@std@@@Z
    ?getTriangleList@Subdiv2D@cv@@QEBAXAEAV?$vector@V?$Vec@M$05@cv@@V?$allocator@V?$Vec@M$05@cv@@@std@@@std@@@Z
    void cv::Subdiv2D::getTriangleList(class std::vector<class cv::Vec<float,6>,class std::allocator<class cv::Vec<float,6> > > &)
  }
procedure getTriangleList_Subdiv2D(const Obj: TSubdiv2D; const triangleList: vector<TVec6f>);
  external opencv_world_dll name '?getTriangleList@Subdiv2D@cv@@QEBAXAEAV?$vector@V?$Vec@M$05@cv@@V?$allocator@V?$Vec@M$05@cv@@@std@@@std@@@Z'{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
  // 5173	?getVertex@Subdiv2D@cv@@QEBA?AV?$Point_@M@2@HPEAH@Z	?getVertex@Subdiv2D@cv@@QEBA?AV?$Point_@M@2@HPEAH@Z	class cv::Point_<float> cv::Subdiv2D::getVertex(int,int *)
  {
    5176
    ?getVoronoiFacetList@Subdiv2D@cv@@QEAAXAEBV?$vector@HV?$allocator@H@std@@@std@@AEAV?$vector@V?$vector@V?$Point_@M@cv@@V?$allocator@V?$Point_@M@cv@@@std@@@std@@V?$allocator@V?$vector@V?$Point_@M@cv@@V?$allocator@V?$Point_@M@cv@@@std@@@std@@@2@@4@AEAV?$vector@V?$Point_@M@cv@@V?$allocator@V?$Point_@M@cv@@@std@@@4@@Z
    ?getVoronoiFacetList@Subdiv2D@cv@@QEAAXAEBV?$vector@HV?$allocator@H@std@@@std@@AEAV?$vector@V?$vector@V?$Point_@M@cv@@V?$allocator@V?$Point_@M@cv@@@std@@@std@@V?$allocator@V?$vector@V?$Point_@M@cv@@V?$allocator@V?$Point_@M@cv@@@std@@@std@@@2@@4@AEAV?$vector@V?$Point_@M@cv@@V?$allocator@V?$Point_@M@cv@@@std@@@4@@Z
    void cv::Subdiv2D::getVoronoiFacetList(class std::vector<int,class std::allocator<int> > const &,class std::vector<class std::vector<class cv::Point_<float>,class std::allocator<class cv::Point_<float> > >,class std::allocator<class std::vector<class cv::Point_<float>,class std::allocator<class cv::Point_<float> > > > > &,class std::vector<class cv::Point_<float>,class std::allocator<class cv::Point_<float> > > &)
  }
procedure getVoronoiFacetList_Subdiv2D(const Obj: TSubdiv2D; const idx: vector<int>; const facetList: vector<vector<TPoint2f>>; facetCenters: vector<TPoint2f>);
  external opencv_world_dll
  name '?getVoronoiFacetList@Subdiv2D@cv@@QEAAXAEBV?$vector@HV?$allocator@H@std@@@std@@AEAV?$vector@V?'+//
       '$vector@V?$Point_@M@cv@@V?$allocator@V?$Point_@M@cv@@@std@@@std@@V?$allocator@V?$vector@V?$Point_@'+//
       'M@cv@@V?$allocator@V?$Point_@M@cv@@@std@@@std@@@2@@4@AEAV?$vector@V?$Point_@M@cv@@V?$allocator@V?$Point_@M@cv@@@std@@@4@@Z'
  {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
  // 5298	?initDelaunay@Subdiv2D@cv@@QEAAXV?$Rect_@H@2@@Z	?initDelaunay@Subdiv2D@cv@@QEAAXV?$Rect_@H@2@@Z	void cv::Subdiv2D::initDelaunay(class cv::Rect_<int>)
  {
    5321
    ?insert@Subdiv2D@cv@@QEAAHV?$Point_@M@2@@Z
    ?insert@Subdiv2D@cv@@QEAAHV?$Point_@M@2@@Z
    int cv::Subdiv2D::insert(class cv::Point_<float>)
  }
function insert_Subdiv2D(const Obj: TSubdiv2D; const pt: UInt64): int;
external opencv_world_dll name '?insert@Subdiv2D@cv@@QEAAHV?$Point_@M@2@@Z'{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
  {
    5322
    ?insert@Subdiv2D@cv@@QEAAXAEBV?$vector@V?$Point_@M@cv@@V?$allocator@V?$Point_@M@cv@@@std@@@std@@@Z
    ?insert@Subdiv2D@cv@@QEAAXAEBV?$vector@V?$Point_@M@cv@@V?$allocator@V?$Point_@M@cv@@@std@@@std@@@Z
    void cv::Subdiv2D::insert(class std::vector<class cv::Point_<float>,class std::allocator<class cv::Point_<float> > > const &)
  }
  // 5403	?isRightOf@Subdiv2D@cv@@IEBAHV?$Point_@M@2@H@Z	?isRightOf@Subdiv2D@cv@@IEBAHV?$Point_@M@2@H@Z	int cv::Subdiv2D::isRightOf(class cv::Point_<float>,int)
  // 5422	?isfree@QuadEdge@Subdiv2D@cv@@QEBA_NXZ	?isfree@QuadEdge@Subdiv2D@cv@@QEBA_NXZ	bool cv::Subdiv2D::QuadEdge::isfree(void)
  // 5423	?isfree@Vertex@Subdiv2D@cv@@QEBA_NXZ	?isfree@Vertex@Subdiv2D@cv@@QEBA_NXZ	bool cv::Subdiv2D::Vertex::isfree(void)
  // 5425	?isvirtual@Vertex@Subdiv2D@cv@@QEBA_NXZ	?isvirtual@Vertex@Subdiv2D@cv@@QEBA_NXZ	bool cv::Subdiv2D::Vertex::isvirtual(void)
  {
    5492
    ?locate@Subdiv2D@cv@@QEAAHV?$Point_@M@2@AEAH1@Z
    ?locate@Subdiv2D@cv@@QEAAHV?$Point_@M@2@AEAH1@Z
    int cv::Subdiv2D::locate(class cv::Point_<float>,int &,int &)
  }
function locale_Subdiv2D(const Obj: TSubdiv2D; const pt: UInt64; Var edge, vertex: int): int;
  external opencv_world_dll name '?locate@Subdiv2D@cv@@QEAAHV?$Point_@M@2@AEAH1@Z'{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
  // 5734	?newEdge@Subdiv2D@cv@@IEAAHXZ	?newEdge@Subdiv2D@cv@@IEAAHXZ	int cv::Subdiv2D::newEdge(void)
  // 5736	?newPoint@Subdiv2D@cv@@IEAAHV?$Point_@M@2@_NH@Z	?newPoint@Subdiv2D@cv@@IEAAHV?$Point_@M@2@_NH@Z	int cv::Subdiv2D::newPoint(class cv::Point_<float>,bool,int)
  // 5739	?nextEdge@Subdiv2D@cv@@QEBAHH@Z	?nextEdge@Subdiv2D@cv@@QEBAHH@Z	int cv::Subdiv2D::nextEdge(int)
  // 6168	?rotateEdge@Subdiv2D@cv@@QEBAHHH@Z	?rotateEdge@Subdiv2D@cv@@QEBAHHH@Z	int cv::Subdiv2D::rotateEdge(int,int)
  // 6297	?setEdgePoints@Subdiv2D@cv@@IEAAXHHH@Z	?setEdgePoints@Subdiv2D@cv@@IEAAXHHH@Z	void cv::Subdiv2D::setEdgePoints(int,int,int)
  // 6507	?splice@Subdiv2D@cv@@IEAAXHH@Z	?splice@Subdiv2D@cv@@IEAAXHH@Z	void cv::Subdiv2D::splice(int,int)
  // 6589	?swapEdges@Subdiv2D@cv@@IEAAXH@Z	?swapEdges@Subdiv2D@cv@@IEAAXH@Z	void cv::Subdiv2D::swapEdges(int)
  // 6591	?symEdge@Subdiv2D@cv@@QEBAHH@Z	?symEdge@Subdiv2D@cv@@QEBAHH@Z	int cv::Subdiv2D::symEdge(int)

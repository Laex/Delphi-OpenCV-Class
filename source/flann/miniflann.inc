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

{$if not defined(OPENCV_MINIFLANN_HPP)}
{$define OPENCV_MINIFLANN_HPP}

//! @cond IGNORED

{$IF not defined(OPENCV_CORE_HPP)}{$I 'core.inc'}{$ifend}
{$I 'flann/defines.inc'}

//namespace cv
//{

//namespace flann
//{
Type
 FlannIndexType =( //
    FLANN_INDEX_TYPE_8U = CV_8U,    //
    FLANN_INDEX_TYPE_8S = CV_8S,    //
    FLANN_INDEX_TYPE_16U = CV_16U,  //
    FLANN_INDEX_TYPE_16S = CV_16S,  //
    FLANN_INDEX_TYPE_32S = CV_32S,  //
    FLANN_INDEX_TYPE_32F = CV_32F,  //
    FLANN_INDEX_TYPE_64F = CV_64F,  //
    FLANN_INDEX_TYPE_STRING,        //
    FLANN_INDEX_TYPE_BOOL,          //
    FLANN_INDEX_TYPE_ALGORITHM,     //
    LAST_VALUE_FLANN_INDEX_TYPE = FLANN_INDEX_TYPE_ALGORITHM //
);

//struct CV_EXPORTS IndexParams
//{
//    IndexParams();
//    ~IndexParams();
//
//    String getString(const String& key, const String& defaultVal=String()) const;
//    int getInt(const String& key, int defaultVal=-1) const;
//    double getDouble(const String& key, double defaultVal=-1) const;
//
//    void setString(const String& key, const String& value);
//    void setInt(const String& key, int value);
//    void setDouble(const String& key, double value);
//    void setFloat(const String& key, float value);
//    void setBool(const String& key, bool value);
//    void setAlgorithm(int value);
//
//    // FIXIT: replace by void write(FileStorage& fs) const + read()
//    void getAll(std::vector<String>& names,
//                std::vector<FlannIndexType>& types,
//                std::vector<String>& strValues,
//                std::vector<double>& numValues) const;
//
//    void* params;
//
//private:
//    IndexParams(const IndexParams &); // copy disabled
//    IndexParams& operator=(const IndexParams &); // assign disabled
//};
//
//struct CV_EXPORTS KDTreeIndexParams : public IndexParams
//{
//    KDTreeIndexParams(int trees=4);
//};
//
//struct CV_EXPORTS LinearIndexParams : public IndexParams
//{
//    LinearIndexParams();
//};
//
//struct CV_EXPORTS CompositeIndexParams : public IndexParams
//{
//    CompositeIndexParams(int trees = 4, int branching = 32, int iterations = 11,
//                         cvflann::flann_centers_init_t centers_init = cvflann::FLANN_CENTERS_RANDOM, float cb_index = 0.2f );
//};
//
//struct CV_EXPORTS AutotunedIndexParams : public IndexParams
//{
//    AutotunedIndexParams(float target_precision = 0.8f, float build_weight = 0.01f,
//                         float memory_weight = 0, float sample_fraction = 0.1f);
//};
//
//struct CV_EXPORTS HierarchicalClusteringIndexParams : public IndexParams
//{
//    HierarchicalClusteringIndexParams(int branching = 32,
//                      cvflann::flann_centers_init_t centers_init = cvflann::FLANN_CENTERS_RANDOM, int trees = 4, int leaf_size = 100 );
//};
//
//struct CV_EXPORTS KMeansIndexParams : public IndexParams
//{
//    KMeansIndexParams(int branching = 32, int iterations = 11,
//                      cvflann::flann_centers_init_t centers_init = cvflann::FLANN_CENTERS_RANDOM, float cb_index = 0.2f );
//};
//
//struct CV_EXPORTS LshIndexParams : public IndexParams
//{
//    LshIndexParams(int table_number, int key_size, int multi_probe_level);
//};
//
//struct CV_EXPORTS SavedIndexParams : public IndexParams
//{
//    SavedIndexParams(const String& filename);
//};
//
//struct CV_EXPORTS SearchParams : public IndexParams
//{
//    SearchParams( int checks, float eps, bool sorted, bool explore_all_trees );
//    SearchParams( int checks = 32, float eps = 0, bool sorted = true );
//};
//
//class CV_EXPORTS_W Index
//{
//public:
//    CV_WRAP Index();
//    CV_WRAP Index(InputArray features, const IndexParams& params, cvflann::flann_distance_t distType=cvflann::FLANN_DIST_L2);
//    virtual ~Index();
//
//    CV_WRAP virtual void build(InputArray features, const IndexParams& params, cvflann::flann_distance_t distType=cvflann::FLANN_DIST_L2);
//    CV_WRAP virtual void knnSearch(InputArray query, OutputArray indices,
//                   OutputArray dists, int knn, const SearchParams& params=SearchParams());
//
//    CV_WRAP virtual int radiusSearch(InputArray query, OutputArray indices,
//                             OutputArray dists, double radius, int maxResults,
//                             const SearchParams& params=SearchParams());
//
//    CV_WRAP virtual void save(const String& filename) const;
//    CV_WRAP virtual bool load(InputArray features, const String& filename);
//    CV_WRAP virtual void release();
//    CV_WRAP cvflann::flann_distance_t getDistance() const;
//    CV_WRAP cvflann::flann_algorithm_t getAlgorithm() const;
//
//protected:
//    bool load_(const String& filename);
//
//    cvflann::flann_distance_t distType;
//    cvflann::flann_algorithm_t algo;
//    int featureType;
//    void* index;
//    Mat features_clone;  // index may store features pointer internally for searching, so avoid dangling pointers: https://github.com/opencv/opencv/issues/17553
//};
//
//} } // namespace cv::flann

//! @endcond

{$ifend}

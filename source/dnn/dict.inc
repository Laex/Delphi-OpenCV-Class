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

{$if not defined(OPENCV_DNN_DNN_DICT_HPP)}
{$define OPENCV_DNN_DNN_DICT_HPP}


{$IF not defined(OPENCV_CORE_HPP)}{$I 'core.inc'}{$ifend}
{$if not defined(OPENCV_DNN_DNN_HPP)}{$i 'dnn/dnn.inc'}{$ifend}


//! @addtogroup dnn
//! @{

Type

(** @brief This struct stores the scalar value (or array) of one of the following type: double, cv::String or int64.
 *  @todo Maybe int64 is useless because double type exactly stores at least 2^52 integers.
 *)
DictValue = record
//    DictValue(const DictValue &r);
//    DictValue(bool i)           : type(Param::INT), pi(new AutoBuffer<int64,1>) { (*pi)[0] = i ? 1 : 0; }       //!< Constructs integer scalar
//    DictValue(int64 i = 0)      : type(Param::INT), pi(new AutoBuffer<int64,1>) { (*pi)[0] = i; }       //!< Constructs integer scalar
//    CV_WRAP DictValue(int i)    : type(Param::INT), pi(new AutoBuffer<int64,1>) { (*pi)[0] = i; }       //!< Constructs integer scalar
//    DictValue(unsigned p)       : type(Param::INT), pi(new AutoBuffer<int64,1>) { (*pi)[0] = p; }       //!< Constructs integer scalar
//    CV_WRAP DictValue(double p)         : type(Param::REAL), pd(new AutoBuffer<double,1>) { (*pd)[0] = p; }     //!< Constructs floating point scalar
//    CV_WRAP DictValue(const String &s)  : type(Param::STRING), ps(new AutoBuffer<String,1>) { (*ps)[0] = s; }   //!< Constructs string scalar
//    DictValue(const char *s)            : type(Param::STRING), ps(new AutoBuffer<String,1>) { (*ps)[0] = s; }   //!< @overload

//    template<typename TypeIter>
//    static DictValue arrayInt(TypeIter begin, int size);    //!< Constructs integer array
//    template<typename TypeIter>
//    static DictValue arrayReal(TypeIter begin, int size);   //!< Constructs floating point array
//    template<typename TypeIter>
//    static DictValue arrayString(TypeIter begin, int size); //!< Constructs array of strings

//    template<typename T>
//    T get(int idx = -1) const; //!< Tries to convert array element with specified index to requested type and returns its.

//    int size() const;

//    CV_WRAP bool isInt() const;
//    CV_WRAP bool isString() const;
//    CV_WRAP bool isReal() const;

//    CV_WRAP int getIntValue(int idx = -1) const;
//    CV_WRAP double getRealValue(int idx = -1) const;
//    CV_WRAP String getStringValue(int idx = -1) const;

//    DictValue &operator=(const DictValue &r);

//    friend std::ostream &operator<<(std::ostream &stream, const DictValue &dictv);

//    ~DictValue();

private

//    Param type;

//    union
//    {
//        AutoBuffer<int64, 1> *pi;
//        AutoBuffer<double, 1> *pd;
//        AutoBuffer<String, 1> *ps;
//        void *pv;
//    };

//    DictValue(Param _type, void *_p) : type(_type), pv(_p) {}
//    void release();
end;

(** @brief This class implements name-value dictionary, values are instances of DictValue. *)
Dict = record
//    typedef std::map<String, DictValue> _Dict;
//    _Dict dict;

public

    //! Checks a presence of the @p key in the dictionary.
//    bool has(const String &key) const;

    //! If the @p key in the dictionary then returns pointer to its value, else returns NULL.
//    DictValue *ptr(const String &key);

    (** @overload *)
//    const DictValue *ptr(const String &key) const;

    //! If the @p key in the dictionary then returns its value, else an error will be generated.
//    const DictValue &get(const String &key) const;

    (** @overload *)
//    template <typename T>
//    T get(const String &key) const;

    //! If the @p key in the dictionary then returns its value, else returns @p defaultValue.
//    template <typename T>
//    T get(const String &key, const T &defaultValue) const;

    //! Sets new @p value for the @p key, or adds new key-value pair into the dictionary.
//    template<typename T>
//    const T &set(const String &key, const T &value);

    //! Erase @p key from the dictionary.
//    void erase(const String &key);

//    friend std::ostream &operator<<(std::ostream &stream, const Dict &dict);

//    std::map<String, DictValue>::const_iterator begin() const;

//    std::map<String, DictValue>::const_iterator end() const;
end;

//! @}

{$ifend}

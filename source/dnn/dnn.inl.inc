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

{$ifndef OPENCV_DNN_DNN_INL_HPP}
{$define OPENCV_DNN_DNN_INL_HPP}

{$include dnn/dnn.inc}

(*
//namespace cv {
//namespace dnn {
//CV__DNN_INLINE_NS_BEGIN

template<typename TypeIter>
DictValue DictValue::arrayInt(TypeIter begin, int size)
{
    DictValue res(Param::INT, new AutoBuffer<int64, 1>(size));
    for (int j = 0; j < size; begin++, j++)
        (*res.pi)[j] = *begin;
    return res;
}

template<typename TypeIter>
DictValue DictValue::arrayReal(TypeIter begin, int size)
{
    DictValue res(Param::REAL, new AutoBuffer<double, 1>(size));
    for (int j = 0; j < size; begin++, j++)
        (*res.pd)[j] = *begin;
    return res;
}

template<typename TypeIter>
DictValue DictValue::arrayString(TypeIter begin, int size)
{
    DictValue res(Param::STRING, new AutoBuffer<String, 1>(size));
    for (int j = 0; j < size; begin++, j++)
        (*res.ps)[j] = *begin;
    return res;
}

template<>
inline DictValue DictValue::get<DictValue>(int idx) const
{
    CV_Assert(idx == -1);
    return *this;
}

template<>
inline int64 DictValue::get<int64>(int idx) const
{
    CV_Assert((idx == -1 && size() == 1) || (idx >= 0 && idx < size()));
    idx = (idx == -1) ? 0 : idx;

    if (type == Param::INT)
    {
        return (*pi)[idx];
    }
    else if (type == Param::REAL)
    {
        double doubleValue = (*pd)[idx];

        double fracpart, intpart;
        fracpart = std::modf(doubleValue, &intpart);
        CV_Assert(fracpart == 0.0);

        return (int64)doubleValue;
    }
    else if (type == Param::STRING)
    {
        return std::atoi((*ps)[idx].c_str());
    }
    else
    {
        CV_Assert(isInt() || isReal() || isString());
        return 0;
    }
}

template<>
inline int DictValue::get<int>(int idx) const
{
    return (int)get<int64>(idx);
}

inline int DictValue::getIntValue(int idx) const
{
    return (int)get<int64>(idx);
}

template<>
inline unsigned DictValue::get<unsigned>(int idx) const
{
    return (unsigned)get<int64>(idx);
}

template<>
inline bool DictValue::get<bool>(int idx) const
{
    return (get<int64>(idx) != 0);
}

template<>
inline double DictValue::get<double>(int idx) const
{
    CV_Assert((idx == -1 && size() == 1) || (idx >= 0 && idx < size()));
    idx = (idx == -1) ? 0 : idx;

    if (type == Param::REAL)
    {
        return (*pd)[idx];
    }
    else if (type == Param::INT)
    {
        return (double)(*pi)[idx];
    }
    else if (type == Param::STRING)
    {
        return std::atof((*ps)[idx].c_str());
    }
    else
    {
        CV_Assert(isReal() || isInt() || isString());
        return 0;
    }
}

inline double DictValue::getRealValue(int idx) const
{
    return get<double>(idx);
}

template<>
inline float DictValue::get<float>(int idx) const
{
    return (float)get<double>(idx);
}

template<>
inline String DictValue::get<String>(int idx) const
{
    CV_Assert(isString());
    CV_Assert((idx == -1 && ps->size() == 1) || (idx >= 0 && idx < (int)ps->size()));
    return (*ps)[(idx == -1) ? 0 : idx];
}


inline String DictValue::getStringValue(int idx) const
{
    return get<String>(idx);
}

inline void DictValue::release()
{
    switch (type)
    {
    case Param::INT:
        delete pi;
        break;
    case Param::STRING:
        delete ps;
        break;
    case Param::REAL:
        delete pd;
        break;
    case Param::BOOLEAN:
    case Param::MAT:
    case Param::MAT_VECTOR:
    case Param::ALGORITHM:
    case Param::FLOAT:
    case Param::UNSIGNED_INT:
    case Param::UINT64:
    case Param::UCHAR:
    case Param::SCALAR:
        break; // unhandled
    }
}

inline DictValue::~DictValue()
{
    release();
}

inline DictValue & DictValue::operator=(const DictValue &r)
{
    if (&r == this)
        return *this;

    if (r.type == Param::INT)
    {
        AutoBuffer<int64, 1> *tmp = new AutoBuffer<int64, 1>(*r.pi);
        release();
        pi = tmp;
    }
    else if (r.type == Param::STRING)
    {
        AutoBuffer<String, 1> *tmp = new AutoBuffer<String, 1>(*r.ps);
        release();
        ps = tmp;
    }
    else if (r.type == Param::REAL)
    {
        AutoBuffer<double, 1> *tmp = new AutoBuffer<double, 1>(*r.pd);
        release();
        pd = tmp;
    }

    type = r.type;

    return *this;
}

inline DictValue::DictValue(const DictValue &r)
    : pv(NULL)
{
    type = r.type;

    if (r.type == Param::INT)
        pi = new AutoBuffer<int64, 1>(*r.pi);
    else if (r.type == Param::STRING)
        ps = new AutoBuffer<String, 1>(*r.ps);
    else if (r.type == Param::REAL)
        pd = new AutoBuffer<double, 1>(*r.pd);
}

inline bool DictValue::isString() const
{
    return (type == Param::STRING);
}

inline bool DictValue::isInt() const
{
    return (type == Param::INT);
}

inline bool DictValue::isReal() const
{
    return (type == Param::REAL || type == Param::INT);
}

inline int DictValue::size() const
{
    switch (type)
    {
    case Param::INT:
        return (int)pi->size();
    case Param::STRING:
        return (int)ps->size();
    case Param::REAL:
        return (int)pd->size();
    case Param::BOOLEAN:
    case Param::MAT:
    case Param::MAT_VECTOR:
    case Param::ALGORITHM:
    case Param::FLOAT:
    case Param::UNSIGNED_INT:
    case Param::UINT64:
    case Param::UCHAR:
    case Param::SCALAR:
        break; // unhandled
    }
    CV_Error_(Error::StsInternal, ("Unhandled type (%d)", static_cast<int>(type)));
}

inline std::ostream &operator<<(std::ostream &stream, const DictValue &dictv)
{
    int i;

    if (dictv.isInt())
    {
        for (i = 0; i < dictv.size() - 1; i++)
            stream << dictv.get<int64>(i) << ", ";
        stream << dictv.get<int64>(i);
    }
    else if (dictv.isReal())
    {
        for (i = 0; i < dictv.size() - 1; i++)
            stream << dictv.get<double>(i) << ", ";
        stream << dictv.get<double>(i);
    }
    else if (dictv.isString())
    {
        for (i = 0; i < dictv.size() - 1; i++)
            stream << "\"" << dictv.get<String>(i) << "\", ";
        stream << dictv.get<String>(i);
    }

    return stream;
}

/////////////////////////////////////////////////////////////////

inline bool Dict::has(const String &key) const
{
    return dict.count(key) != 0;
}

inline DictValue *Dict::ptr(const String &key)
{
    _Dict::iterator i = dict.find(key);
    return (i == dict.end()) ? NULL : &i->second;
}

inline const DictValue *Dict::ptr(const String &key) const
{
    _Dict::const_iterator i = dict.find(key);
    return (i == dict.end()) ? NULL : &i->second;
}

inline const DictValue &Dict::get(const String &key) const
{
    _Dict::const_iterator i = dict.find(key);
    if (i == dict.end())
        CV_Error(Error::StsObjectNotFound, "Required argument \"" + key + "\" not found into dictionary");
    return i->second;
}

template <typename T>
inline T Dict::get(const String &key) const
{
    return this->get(key).get<T>();
}

template <typename T>
inline T Dict::get(const String &key, const T &defaultValue) const
{
    _Dict::const_iterator i = dict.find(key);

    if (i != dict.end())
        return i->second.get<T>();
    else
        return defaultValue;
}

template<typename T>
inline const T &Dict::set(const String &key, const T &value)
{
    _Dict::iterator i = dict.find(key);

    if (i != dict.end())
        i->second = DictValue(value);
    else
        dict.insert(std::make_pair(key, DictValue(value)));

    return value;
}

inline void Dict::erase(const String &key)
{
    dict.erase(key);
}

inline std::ostream &operator<<(std::ostream &stream, const Dict &dict)
{
    Dict::_Dict::const_iterator it;
    for (it = dict.dict.begin(); it != dict.dict.end(); it++)
        stream << it->first << " : " << it->second << "\n";

    return stream;
}

inline std::map<String, DictValue>::const_iterator Dict::begin() const
{
    return dict.begin();
}

inline std::map<String, DictValue>::const_iterator Dict::end() const
{
    return dict.end();
}

CV__DNN_INLINE_NS_END
}
}
*)

{$endif OPENCV_DNN_DNN_INL_HPP}

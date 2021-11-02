unit opencv_delphi;

interface

{$I opencv_delphi.inc}

{$REGION 'CvStdString'}

type
  CvStdString = record
  private{$HINTS OFF}
    Dummy: array [0 .. 39] of byte;
{$HINTS ON}
  public
    class operator Initialize(out Dest: CvStdString);
    class operator Finalize(var Dest: CvStdString);

    function length: UInt64; {$IFDEF USE_INLINE}inline; {$ENDIF}
    function size: UInt64; {$IFDEF USE_INLINE}inline; {$ENDIF}
    procedure erase(const _Off: UInt64 = 0); {$IFDEF USE_INLINE}inline; {$ENDIF}
    procedure assign(const p: pAnsiChar); {$IFDEF USE_INLINE}inline; {$ENDIF}
    class operator assign(var Dest: CvStdString; const [ref] Src: CvStdString);
    class operator Implicit(const p: pAnsiChar): CvStdString;
    class operator Implicit(const s: string): CvStdString;
    class operator Implicit(const s: CvStdString): string;
  end;

  CppString = CvStdString;

  pCppString = ^CvStdString;
  pCvStdString = ^CvStdString;

  // ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@$$QEAV01@@Z	std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > &&)
  // ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@$$QEAV01@AEBV?$allocator@D@1@@Z	std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > &&,class std::allocator<char> const &)
  // ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@AEBV01@@Z	std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &)
  // ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@AEBV01@AEBV?$allocator@D@1@@Z	std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &,class std::allocator<char> const &)
  // ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@AEBV01@_K1AEBV?$allocator@D@1@@Z	std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &,unsigned __int64,unsigned __int64,class std::allocator<char> const &)
  // ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@AEBV01@_KAEBV?$allocator@D@1@@Z	std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &,unsigned __int64,class std::allocator<char> const &)
  // ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@AEBV?$allocator@D@1@@Z	std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >(class std::allocator<char> const &)

  // ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@QEBD@Z
  // std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >(char const * const)
procedure Constructor_CppString(Obj: pCppString; pac: pAnsiChar); overload; external opencv_delphi_dll name '??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@QEBD@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@QEBDAEBV?$allocator@D@1@@Z	std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >(char const * const,class std::allocator<char> const &)
// ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@QEBD_K@Z	std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >(char const * const,unsigned __int64)
// ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@QEBD_KAEBV?$allocator@D@1@@Z	std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >(char const * const,unsigned __int64,class std::allocator<char> const &)
// ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@V?$initializer_list@D@1@AEBV?$allocator@D@1@@Z	std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >(class std::initializer_list<char>,class std::allocator<char> const &)

// ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@XZ
// std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >(void)
procedure Constructor_CppString(Obj: pCppString); overload; external opencv_delphi_dll name '??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@XZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@_KD@Z	std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >(unsigned __int64,char)
// ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@_KDAEBV?$allocator@D@1@@Z	std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >(unsigned __int64,char,class std::allocator<char> const &)

// ??1?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@XZ
// std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::~basic_string<char,struct std::char_traits<char>,class std::allocator<char> >(void)
procedure Destructor_CppString(Obj: pCppString); overload; external opencv_delphi_dll name '??1?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@XZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// ??4?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV01@$$QEAV01@@Z	class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::operator=(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > &&)
// ??4?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV01@AEBV01@@Z	class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::operator=(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &)
// ??4?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV01@D@Z	class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::operator=(char)
// ??4?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV01@QEBD@Z	class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::operator=(char const * const)
// ??4?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV01@V?$initializer_list@D@1@@Z	class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::operator=(class std::initializer_list<char>)
// ??A?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAD_K@Z	char & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::operator[](unsigned __int64)
// ??A?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAAEBD_K@Z	char const & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::operator[](unsigned __int64)
// ??Y?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV01@AEBV01@@Z	class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::operator+=(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &)
// ??Y?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV01@D@Z	class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::operator+=(char)
// ??Y?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV01@QEBD@Z	class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::operator+=(char const * const)
// ??Y?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV01@V?$initializer_list@D@1@@Z	class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::operator+=(class std::initializer_list<char>)
// ?_Become_small@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEAAXXZ	void std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::_Become_small(void)
// ?_Calculate_growth@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEBA_K_K@Z	unsigned __int64 std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::_Calculate_growth(unsigned __int64)
// ?_Calculate_growth@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@CA_K_K00@Z	unsigned __int64 std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::_Calculate_growth(unsigned __int64,unsigned __int64,unsigned __int64)
// ?_Construct@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAXQEAD0Urandom_access_iterator_tag@2@@Z	void std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::_Construct(char * const,char * const,struct std::random_access_iterator_tag)
// ?_Construct@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAXQEBD0Urandom_access_iterator_tag@2@@Z	void std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::_Construct(char const * const,char const * const,struct std::random_access_iterator_tag)
// ?_Construct_lv_contents@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEAAXAEBV12@@Z	void std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::_Construct_lv_contents(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &)
// ?_Copy_assign@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEAAXAEBV12@U?$integral_constant@_N$00@2@@Z	void std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::_Copy_assign(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &,struct std::integral_constant<bool,1>)
// ?_Copy_assign@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEAAXAEBV12@U?$integral_constant@_N$0A@@2@@Z	void std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::_Copy_assign(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &,struct std::integral_constant<bool,0>)
// ?_Copy_assign_val_from_small@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEAAXAEBV12@@Z	void std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::_Copy_assign_val_from_small(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &)
// ?_Copy_s@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEAD_K_K1@Z	unsigned __int64 std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::_Copy_s(char * const,unsigned __int64,unsigned __int64,unsigned __int64)
// ?_Eos@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEAAX_K@Z	void std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::_Eos(unsigned __int64)
// ?_Equal@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_NAEBV12@@Z	bool std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::_Equal(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &)
// ?_Equal@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_NQEBD@Z	bool std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::_Equal(char const * const)
// ?_Getal@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEAAAEAV?$allocator@D@2@XZ	class std::allocator<char> & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::_Getal(void)
// ?_Getal@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEBAAEBV?$allocator@D@2@XZ	class std::allocator<char> const & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::_Getal(void)
// ?_Memcpy_val_from@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEAAXAEBV12@@Z	void std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::_Memcpy_val_from(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &)
// ?_Move_assign@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEAAXAEAV12@U?$integral_constant@_N$00@2@@Z	void std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::_Move_assign(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > &,struct std::integral_constant<bool,1>)
// ?_Move_assign@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEAAXAEAV12@U?$integral_constant@_N$0A@@2@@Z	void std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::_Move_assign(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > &,struct std::integral_constant<bool,0>)
// ?_Move_assign@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEAAXAEAV12@U_Equal_allocators@2@@Z	void std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::_Move_assign(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > &,struct std::_Equal_allocators)
// ?_Orphan_all@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAXXZ	void std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::_Orphan_all(void)
// ?_Swap_bx_large_with_small@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAXAEAV?$_String_val@U?$_Simple_types@D@std@@@2@0@Z	void std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::_Swap_bx_large_with_small(class std::_String_val<struct std::_Simple_types<char> > &,class std::_String_val<struct std::_Simple_types<char> > &)
// ?_Swap_data@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAXAEAV12@U?$integral_constant@_N$00@2@@Z	void std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::_Swap_data(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > &,struct std::integral_constant<bool,1>)
// ?_Swap_data@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAXAEAV12@U?$integral_constant@_N$0A@@2@@Z	void std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::_Swap_data(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > &,struct std::integral_constant<bool,0>)
// ?_Swap_proxy_and_iterators@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEAAXAEAV12@@Z	void std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::_Swap_proxy_and_iterators(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > &)
// ?_Take_contents@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEAAXAEAV12@U?$integral_constant@_N$00@2@@Z	void std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::_Take_contents(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > &,struct std::integral_constant<bool,1>)
// ?_Take_contents@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEAAXAEAV12@U?$integral_constant@_N$0A@@2@@Z	void std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::_Take_contents(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > &,struct std::integral_constant<bool,0>)
// ?_Tidy_deallocate@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEAAXXZ	void std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::_Tidy_deallocate(void)
// ?_Tidy_init@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEAAXXZ	void std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::_Tidy_init(void)
// ?_Unchecked_begin@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAPEADXZ	char * std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::_Unchecked_begin(void)
// ?_Unchecked_begin@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAPEBDXZ	char const * std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::_Unchecked_begin(void)
// ?_Unchecked_end@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAPEADXZ	char * std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::_Unchecked_end(void)
// ?_Unchecked_end@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAPEBDXZ	char const * std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::_Unchecked_end(void)
// ?_Xlen@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@CAXXZ	void std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::_Xlen(void)
// ?append@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@AEBV12@@Z	class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::append(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &)
// ?append@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@AEBV12@_K_K@Z	class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::append(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &,unsigned __int64,unsigned __int64)
// ?append@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@QEBD@Z	class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::append(char const * const)
// ?append@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@QEBD_K@Z	class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::append(char const * const,unsigned __int64)
// ?append@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@V?$initializer_list@D@2@@Z	class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::append(class std::initializer_list<char>)
// ?append@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_KD@Z	class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::append(unsigned __int64,char)
// ?assign@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@$$QEAV12@@Z	class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::assign(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > &&)

// ?assign@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@AEBV12@@Z
// class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::assign(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &)
procedure assign_CppString(Obj: pCppString; a: pCppString) { : pCppString }; overload;
  external opencv_delphi_dll name '?assign@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@AEBV12@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// ?assign@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@AEBV12@_K_K@Z	class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::assign(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &,unsigned __int64,unsigned __int64)

// ?assign@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@QEBD@Z
// class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::assign(char const * const)
procedure assign_CppString(Obj: pCppString; pac: pAnsiChar); overload; external opencv_delphi_dll name '?assign@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@QEBD@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// ?assign@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@QEBD_K@Z	class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::assign(char const * const,unsigned __int64)
// ?assign@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@V?$initializer_list@D@2@@Z	class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::assign(class std::initializer_list<char>)
// ?assign@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_KD@Z	class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::assign(unsigned __int64,char)
// ?at@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAD_K@Z	char & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::at(unsigned __int64)
// ?at@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAAEBD_K@Z	char const & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::at(unsigned __int64)
// ?back@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEADXZ	char & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::back(void)
// ?back@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAAEBDXZ	char const & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::back(void)
// ?begin@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA?AV?$_String_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@XZ	class std::_String_iterator<class std::_String_val<struct std::_Simple_types<char> > > std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::begin(void)
// ?begin@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA?AV?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@XZ	class std::_String_const_iterator<class std::_String_val<struct std::_Simple_types<char> > > std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::begin(void)

// ?c_str@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAPEBDXZ
// char const * std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::c_str(void)
function c_str_CppString(Obj: pCppString { ; r: pAnsiChar } ): pAnsiChar; external opencv_delphi_dll name '?c_str@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAPEBDXZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// ?capacity@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KXZ	unsigned __int64 std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::capacity(void)
// ?cbegin@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA?AV?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@XZ	class std::_String_const_iterator<class std::_String_val<struct std::_Simple_types<char> > > std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::cbegin(void)
// ?cend@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA?AV?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@XZ	class std::_String_const_iterator<class std::_String_val<struct std::_Simple_types<char> > > std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::cend(void)
// ?clear@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAXXZ	void std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::clear(void)
// ?compare@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAHAEBV12@@Z	int std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::compare(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &)
// ?compare@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAHQEBD@Z	int std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::compare(char const * const)
// ?compare@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAH_K0AEBV12@00@Z	int std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::compare(unsigned __int64,unsigned __int64,class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &,unsigned __int64,unsigned __int64)
// ?compare@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAH_K0AEBV12@@Z	int std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::compare(unsigned __int64,unsigned __int64,class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &)
// ?compare@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAH_K0QEBD0@Z	int std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::compare(unsigned __int64,unsigned __int64,char const * const,unsigned __int64)
// ?compare@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAH_K0QEBD@Z	int std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::compare(unsigned __int64,unsigned __int64,char const * const)
// ?copy@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEAD_K_K@Z	unsigned __int64 std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::copy(char * const,unsigned __int64,unsigned __int64)
// ?crbegin@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA?AV?$reverse_iterator@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@std@@@2@XZ	class std::reverse_iterator<class std::_String_const_iterator<class std::_String_val<struct std::_Simple_types<char> > > > std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::crbegin(void)
// ?crend@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA?AV?$reverse_iterator@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@std@@@2@XZ	class std::reverse_iterator<class std::_String_const_iterator<class std::_String_val<struct std::_Simple_types<char> > > > std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::crend(void)
// ?data@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAPEBDXZ	char const * std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::data(void)
// ?empty@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_NXZ	bool std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::empty(void)
// ?end@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA?AV?$_String_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@XZ	class std::_String_iterator<class std::_String_val<struct std::_Simple_types<char> > > std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::end(void)
// ?end@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA?AV?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@XZ	class std::_String_const_iterator<class std::_String_val<struct std::_Simple_types<char> > > std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::end(void)
// ?erase@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA?AV?$_String_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@0@Z	class std::_String_iterator<class std::_String_val<struct std::_Simple_types<char> > > std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::erase(class std::_String_const_iterator<class std::_String_val<struct std::_Simple_types<char> > >,class std::_String_const_iterator<class std::_String_val<struct std::_Simple_types<char> > >)
// ?erase@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA?AV?$_String_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@@Z	class std::_String_iterator<class std::_String_val<struct std::_Simple_types<char> > > std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::erase(class std::_String_const_iterator<class std::_String_val<struct std::_Simple_types<char> > >)

// ?erase@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_K@Z
// class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::erase(unsigned __int64)
procedure erase_CppString(Obj: pCppString; _Off: UInt64 = 0); overload; external opencv_delphi_dll name '?erase@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_K@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// ?erase@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_K_K@Z	class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::erase(unsigned __int64,unsigned __int64)
// ?find@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KAEBV12@_K@Z	unsigned __int64 std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::find(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &,unsigned __int64)
// ?find@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KD_K@Z	unsigned __int64 std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::find(char,unsigned __int64)
// ?find@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K1@Z	unsigned __int64 std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::find(char const * const,unsigned __int64,unsigned __int64)
// ?find@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K@Z	unsigned __int64 std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::find(char const * const,unsigned __int64)
// ?find_first_not_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KAEBV12@_K@Z	unsigned __int64 std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::find_first_not_of(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &,unsigned __int64)
// ?find_first_not_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KD_K@Z	unsigned __int64 std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::find_first_not_of(char,unsigned __int64)
// ?find_first_not_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K1@Z	unsigned __int64 std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::find_first_not_of(char const * const,unsigned __int64,unsigned __int64)
// ?find_first_not_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K@Z	unsigned __int64 std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::find_first_not_of(char const * const,unsigned __int64)
// ?find_first_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KAEBV12@_K@Z	unsigned __int64 std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::find_first_of(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &,unsigned __int64)
// ?find_first_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KD_K@Z	unsigned __int64 std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::find_first_of(char,unsigned __int64)
// ?find_first_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K1@Z	unsigned __int64 std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::find_first_of(char const * const,unsigned __int64,unsigned __int64)
// ?find_first_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K@Z	unsigned __int64 std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::find_first_of(char const * const,unsigned __int64)
// ?find_last_not_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KAEBV12@_K@Z	unsigned __int64 std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::find_last_not_of(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &,unsigned __int64)
// ?find_last_not_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KD_K@Z	unsigned __int64 std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::find_last_not_of(char,unsigned __int64)
// ?find_last_not_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K1@Z	unsigned __int64 std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::find_last_not_of(char const * const,unsigned __int64,unsigned __int64)
// ?find_last_not_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K@Z	unsigned __int64 std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::find_last_not_of(char const * const,unsigned __int64)
// ?find_last_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KAEBV12@_K@Z	unsigned __int64 std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::find_last_of(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &,unsigned __int64)
// ?find_last_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KD_K@Z	unsigned __int64 std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::find_last_of(char,unsigned __int64)
// ?find_last_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K1@Z	unsigned __int64 std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::find_last_of(char const * const,unsigned __int64,unsigned __int64)
// ?find_last_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K@Z	unsigned __int64 std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::find_last_of(char const * const,unsigned __int64)
// ?front@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEADXZ	char & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::front(void)
// ?front@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAAEBDXZ	char const & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::front(void)
// ?get_allocator@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA?AV?$allocator@D@2@XZ	class std::allocator<char> std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::get_allocator(void)
// ?insert@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA?AV?$_String_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@D@Z	class std::_String_iterator<class std::_String_val<struct std::_Simple_types<char> > > std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::insert(class std::_String_const_iterator<class std::_String_val<struct std::_Simple_types<char> > >,char)
// ?insert@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA?AV?$_String_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@V?$initializer_list@D@2@@Z	class std::_String_iterator<class std::_String_val<struct std::_Simple_types<char> > > std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::insert(class std::_String_const_iterator<class std::_String_val<struct std::_Simple_types<char> > >,class std::initializer_list<char>)
// ?insert@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA?AV?$_String_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@_KD@Z	class std::_String_iterator<class std::_String_val<struct std::_Simple_types<char> > > std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::insert(class std::_String_const_iterator<class std::_String_val<struct std::_Simple_types<char> > >,unsigned __int64,char)
// ?insert@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_K0D@Z	class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::insert(unsigned __int64,unsigned __int64,char)
// ?insert@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_KAEBV12@0_K@Z	class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::insert(unsigned __int64,class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &,unsigned __int64,unsigned __int64)
// ?insert@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_KAEBV12@@Z	class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::insert(unsigned __int64,class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &)
// ?insert@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_KQEBD0@Z	class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::insert(unsigned __int64,char const * const,unsigned __int64)
// ?insert@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_KQEBD@Z	class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::insert(unsigned __int64,char const * const)

// ?length@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KXZ
// unsigned __int64 std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::length(void)
function length_CppString(Obj: pCppString): UInt64; overload; external opencv_delphi_dll name '?length@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KXZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// ?max_size@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KXZ	unsigned __int64 std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::max_size(void)
// ?pop_back@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAXXZ	void std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::pop_back(void)
// ?push_back@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAXD@Z	void std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::push_back(char)
// ?rbegin@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA?AV?$reverse_iterator@V?$_String_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@std@@@2@XZ	class std::reverse_iterator<class std::_String_iterator<class std::_String_val<struct std::_Simple_types<char> > > > std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::rbegin(void)
// ?rbegin@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA?AV?$reverse_iterator@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@std@@@2@XZ	class std::reverse_iterator<class std::_String_const_iterator<class std::_String_val<struct std::_Simple_types<char> > > > std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::rbegin(void)
// ?rend@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA?AV?$reverse_iterator@V?$_String_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@std@@@2@XZ	class std::reverse_iterator<class std::_String_iterator<class std::_String_val<struct std::_Simple_types<char> > > > std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::rend(void)
// ?rend@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA?AV?$reverse_iterator@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@std@@@2@XZ	class std::reverse_iterator<class std::_String_const_iterator<class std::_String_val<struct std::_Simple_types<char> > > > std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::rend(void)
// ?replace@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@0AEBV12@@Z	class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::replace(class std::_String_const_iterator<class std::_String_val<struct std::_Simple_types<char> > >,class std::_String_const_iterator<class std::_String_val<struct std::_Simple_types<char> > >,class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &)
// ?replace@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@0QEBD@Z	class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::replace(class std::_String_const_iterator<class std::_String_val<struct std::_Simple_types<char> > >,class std::_String_const_iterator<class std::_String_val<struct std::_Simple_types<char> > >,char const * const)
// ?replace@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@0QEBD_K@Z	class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::replace(class std::_String_const_iterator<class std::_String_val<struct std::_Simple_types<char> > >,class std::_String_const_iterator<class std::_String_val<struct std::_Simple_types<char> > >,char const * const,unsigned __int64)
// ?replace@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@0V?$initializer_list@D@2@@Z	class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::replace(class std::_String_const_iterator<class std::_String_val<struct std::_Simple_types<char> > >,class std::_String_const_iterator<class std::_String_val<struct std::_Simple_types<char> > >,class std::initializer_list<char>)
// ?replace@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@0_KD@Z	class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::replace(class std::_String_const_iterator<class std::_String_val<struct std::_Simple_types<char> > >,class std::_String_const_iterator<class std::_String_val<struct std::_Simple_types<char> > >,unsigned __int64,char)
// ?replace@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_K0AEBV12@@Z	class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::replace(unsigned __int64,unsigned __int64,class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &)
// ?replace@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_K0QEBD@Z	class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::replace(unsigned __int64,unsigned __int64,char const * const)
// ?replace@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_K_K0D@Z	class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::replace(unsigned __int64,unsigned __int64,unsigned __int64,char)
// ?replace@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_K_KAEBV12@01@Z	class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::replace(unsigned __int64,unsigned __int64,class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &,unsigned __int64,unsigned __int64)
// ?replace@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_K_KQEBD0@Z	class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > & std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::replace(unsigned __int64,unsigned __int64,char const * const,unsigned __int64)
// ?reserve@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAX_K@Z	void std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::reserve(unsigned __int64)
// ?resize@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAX_KD@Z	void std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::resize(unsigned __int64,char)
// ?rfind@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KAEBV12@_K@Z	unsigned __int64 std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::rfind(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &,unsigned __int64)
// ?rfind@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KD_K@Z	unsigned __int64 std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::rfind(char,unsigned __int64)
// ?rfind@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K1@Z	unsigned __int64 std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::rfind(char const * const,unsigned __int64,unsigned __int64)
// ?rfind@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K@Z	unsigned __int64 std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::rfind(char const * const,unsigned __int64)
// ?shrink_to_fit@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAXXZ	void std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::shrink_to_fit(void)

// ?size@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KXZ
// unsigned __int64 std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::size(void)
function size_CppString(Obj: pCppString): UInt64; overload; external opencv_delphi_dll name '?size@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KXZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?substr@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA?AV12@_K0@Z	class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::substr(unsigned __int64,unsigned __int64)
// ?swap@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAXAEAV12@@Z	void std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >::swap(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > &)
{$ENDREGION}
//
{$REGION 'std::vector<Rect>'}

type
  StdVectorRect = record
  private
  {$HINTS OFF}
    Dummy: array [0 .. 31] of byte;
{$HINTS ON}
  public
    class operator Initialize(out Dest: StdVectorRect);
    class operator Finalize(var Dest: StdVectorRect);

    function size: int64; {$IFDEF USE_INLINE}inline; {$ENDIF}
  end;

  pStdVectorRect = ^StdVectorRect;
  //
  // ??0?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEAA@$$QEAV01@@Z	public: __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >(class std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > > && __ptr64) __ptr64
  // ??0?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEAA@$$QEAV01@AEBV?$allocator@V?$Rect_@H@cv@@@1@@Z	public: __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >(class std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > > && __ptr64,class std::allocator<class cv::Rect_<int> > const & __ptr64) __ptr64
  // ??0?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEAA@AEBV01@@Z	public: __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >(class std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > > const & __ptr64) __ptr64
  // ??0?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEAA@AEBV01@AEBV?$allocator@V?$Rect_@H@cv@@@1@@Z	public: __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >(class std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > > const & __ptr64,class std::allocator<class cv::Rect_<int> > const & __ptr64) __ptr64
  // ??0?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEAA@AEBV?$allocator@V?$Rect_@H@cv@@@1@@Z	public: __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >(class std::allocator<class cv::Rect_<int> > const & __ptr64) __ptr64
  // ??0?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEAA@V?$initializer_list@V?$Rect_@H@cv@@@1@AEBV?$allocator@V?$Rect_@H@cv@@@1@@Z	public: __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >(class std::initializer_list<class cv::Rect_<int> >,class std::allocator<class cv::Rect_<int> > const & __ptr64) __ptr64
  // ??0?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEAA@XZ
  // public: __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >(void) __ptr64
procedure Constructor_StdVectorRect(Obj: pStdVectorRect); external opencv_delphi_dll name '??0?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEAA@XZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ??0?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEAA@_KAEBV?$Rect_@H@cv@@AEBV?$allocator@V?$Rect_@H@cv@@@1@@Z	public: __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >(unsigned __int64,class cv::Rect_<int> const & __ptr64,class std::allocator<class cv::Rect_<int> > const & __ptr64) __ptr64
// ??0?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEAA@_KAEBV?$allocator@V?$Rect_@H@cv@@@1@@Z	public: __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >(unsigned __int64,class std::allocator<class cv::Rect_<int> > const & __ptr64) __ptr64
// ??1?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEAA@XZ
// public: __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::~vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >(void) __ptr64
procedure Destructor_StdVectorRect(Obj: pStdVectorRect); external opencv_delphi_dll name '??1?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEAA@XZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ??4?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEAAAEAV01@$$QEAV01@@Z	public: class std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > > & __ptr64 __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::operator=(class std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > > && __ptr64) __ptr64
// ??4?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEAAAEAV01@AEBV01@@Z	public: class std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > > & __ptr64 __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::operator=(class std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > > const & __ptr64) __ptr64
// ??4?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEAAAEAV01@V?$initializer_list@V?$Rect_@H@cv@@@1@@Z	public: class std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > > & __ptr64 __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::operator=(class std::initializer_list<class cv::Rect_<int> >) __ptr64
// ??A?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEAAAEAV?$Rect_@H@cv@@_K@Z
// public: class cv::Rect_<int> & __ptr64 __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::operator[](unsigned __int64) __ptr64
function operator_get_StdVectorRect(Obj: pStdVectorRect; i: UInt64): pStdVectorRect;
  external opencv_delphi_dll name '??A?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEAAAEAV?$Rect_@H@cv@@_K@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ??A?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEBAAEBV?$Rect_@H@cv@@_K@Z	public: class cv::Rect_<int> const & __ptr64 __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::operator[](unsigned __int64)const __ptr64
// ?_Buy_nonzero@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@AEAAX_K@Z	private: void __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::_Buy_nonzero(unsigned __int64) __ptr64
// ?_Buy_raw@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@AEAAX_K@Z	private: void __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::_Buy_raw(unsigned __int64) __ptr64
// ?_Calculate_growth@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@AEBA_K_K@Z	private: unsigned __int64 __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::_Calculate_growth(unsigned __int64)const __ptr64
// ?_Change_array@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@AEAAXQEAV?$Rect_@H@cv@@_K1@Z	private: void __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::_Change_array(class cv::Rect_<int> * __ptr64 const,unsigned __int64,unsigned __int64) __ptr64
// ?_Clear_and_reserve_geometric@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@AEAAX_K@Z	private: void __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::_Clear_and_reserve_geometric(unsigned __int64) __ptr64
// ?_Copy_assign@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@AEAAXAEBV12@U?$integral_constant@_N$00@2@@Z	private: void __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::_Copy_assign(class std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > > const & __ptr64,struct std::integral_constant<bool,1>) __ptr64
// ?_Copy_assign@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@AEAAXAEBV12@U?$integral_constant@_N$0A@@2@@Z	private: void __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::_Copy_assign(class std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > > const & __ptr64,struct std::integral_constant<bool,0>) __ptr64
// ?_Destroy@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@AEAAXPEAV?$Rect_@H@cv@@0@Z	private: void __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::_Destroy(class cv::Rect_<int> * __ptr64,class cv::Rect_<int> * __ptr64) __ptr64
// ?_Getal@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@AEAAAEAV?$allocator@V?$Rect_@H@cv@@@2@XZ	private: class std::allocator<class cv::Rect_<int> > & __ptr64 __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::_Getal(void) __ptr64
// ?_Getal@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@AEBAAEBV?$allocator@V?$Rect_@H@cv@@@2@XZ	private: class std::allocator<class cv::Rect_<int> > const & __ptr64 __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::_Getal(void)const __ptr64
// ?_Make_iterator@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@AEAA?AV?$_Vector_iterator@V?$_Vector_val@U?$_Simple_types@V?$Rect_@H@cv@@@std@@@std@@@2@QEAV?$Rect_@H@cv@@@Z	private: class std::_Vector_iterator<class std::_Vector_val<struct std::_Simple_types<class cv::Rect_<int> > > > __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::_Make_iterator(class cv::Rect_<int> * __ptr64 const) __ptr64
// ?_Make_iterator_offset@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@AEAA?AV?$_Vector_iterator@V?$_Vector_val@U?$_Simple_types@V?$Rect_@H@cv@@@std@@@std@@@2@_K@Z	private: class std::_Vector_iterator<class std::_Vector_val<struct std::_Simple_types<class cv::Rect_<int> > > > __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::_Make_iterator_offset(unsigned __int64) __ptr64
// ?_Move_assign@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@AEAAXAEAV12@U?$integral_constant@_N$00@2@@Z	private: void __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::_Move_assign(class std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > > & __ptr64,struct std::integral_constant<bool,1>) __ptr64
// ?_Move_assign@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@AEAAXAEAV12@U?$integral_constant@_N$0A@@2@@Z	private: void __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::_Move_assign(class std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > > & __ptr64,struct std::integral_constant<bool,0>) __ptr64
// ?_Move_assign@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@AEAAXAEAV12@U_Equal_allocators@2@@Z	private: void __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::_Move_assign(class std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > > & __ptr64,struct std::_Equal_allocators) __ptr64
// ?_Move_construct@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@AEAAXAEAV12@U?$integral_constant@_N$00@2@@Z	private: void __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::_Move_construct(class std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > > & __ptr64,struct std::integral_constant<bool,1>) __ptr64
// ?_Move_construct@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@AEAAXAEAV12@U?$integral_constant@_N$0A@@2@@Z	private: void __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::_Move_construct(class std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > > & __ptr64,struct std::integral_constant<bool,0>) __ptr64
// ?_Orphan_range@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@AEBAXPEAV?$Rect_@H@cv@@0@Z	private: void __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::_Orphan_range(class cv::Rect_<int> * __ptr64,class cv::Rect_<int> * __ptr64)const __ptr64
// ?_Reallocate_exactly@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@AEAAX_K@Z	private: void __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::_Reallocate_exactly(unsigned __int64) __ptr64
// ?_Tidy@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@AEAAXXZ	private: void __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::_Tidy(void) __ptr64
// ?_Ufill@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@AEAAPEAV?$Rect_@H@cv@@PEAV34@_KAEBV34@@Z	private: class cv::Rect_<int> * __ptr64 __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::_Ufill(class cv::Rect_<int> * __ptr64,unsigned __int64,class cv::Rect_<int> const & __ptr64) __ptr64
// ?_Ufill@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@AEAAPEAV?$Rect_@H@cv@@PEAV34@_KU_Value_init_tag@2@@Z	private: class cv::Rect_<int> * __ptr64 __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::_Ufill(class cv::Rect_<int> * __ptr64,unsigned __int64,struct std::_Value_init_tag) __ptr64
// ?_Umove@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@AEAAPEAV?$Rect_@H@cv@@PEAV34@00@Z	private: class cv::Rect_<int> * __ptr64 __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::_Umove(class cv::Rect_<int> * __ptr64,class cv::Rect_<int> * __ptr64,class cv::Rect_<int> * __ptr64) __ptr64
// ?_Umove_if_noexcept1@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@AEAAXPEAV?$Rect_@H@cv@@00U?$integral_constant@_N$00@2@@Z	private: void __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::_Umove_if_noexcept1(class cv::Rect_<int> * __ptr64,class cv::Rect_<int> * __ptr64,class cv::Rect_<int> * __ptr64,struct std::integral_constant<bool,1>) __ptr64
// ?_Umove_if_noexcept1@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@AEAAXPEAV?$Rect_@H@cv@@00U?$integral_constant@_N$0A@@2@@Z	private: void __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::_Umove_if_noexcept1(class cv::Rect_<int> * __ptr64,class cv::Rect_<int> * __ptr64,class cv::Rect_<int> * __ptr64,struct std::integral_constant<bool,0>) __ptr64
// ?_Umove_if_noexcept@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@AEAAXPEAV?$Rect_@H@cv@@00@Z	private: void __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::_Umove_if_noexcept(class cv::Rect_<int> * __ptr64,class cv::Rect_<int> * __ptr64,class cv::Rect_<int> * __ptr64) __ptr64
// ?_Unchecked_begin@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEAAPEAV?$Rect_@H@cv@@XZ	public: class cv::Rect_<int> * __ptr64 __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::_Unchecked_begin(void) __ptr64
// ?_Unchecked_begin@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEBAPEBV?$Rect_@H@cv@@XZ	public: class cv::Rect_<int> const * __ptr64 __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::_Unchecked_begin(void)const __ptr64
// ?_Unchecked_end@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEAAPEAV?$Rect_@H@cv@@XZ	public: class cv::Rect_<int> * __ptr64 __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::_Unchecked_end(void) __ptr64
// ?_Unchecked_end@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEBAPEBV?$Rect_@H@cv@@XZ	public: class cv::Rect_<int> const * __ptr64 __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::_Unchecked_end(void)const __ptr64
// ?_Xlength@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@CAXXZ	private: static void __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::_Xlength(void)
// ?_Xrange@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@CAXXZ	private: static void __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::_Xrange(void)
// ?__autoclassinit2@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEAAX_K@Z	public: void __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::__autoclassinit2(unsigned __int64) __ptr64
// ?assign@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEAAXV?$initializer_list@V?$Rect_@H@cv@@@2@@Z	public: void __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::assign(class std::initializer_list<class cv::Rect_<int> >) __ptr64
// ?assign@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEAAX_KAEBV?$Rect_@H@cv@@@Z	public: void __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::assign(unsigned __int64,class cv::Rect_<int> const & __ptr64) __ptr64
// ?at@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEAAAEAV?$Rect_@H@cv@@_K@Z	public: class cv::Rect_<int> & __ptr64 __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::at(unsigned __int64) __ptr64
// ?at@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEBAAEBV?$Rect_@H@cv@@_K@Z	public: class cv::Rect_<int> const & __ptr64 __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::at(unsigned __int64)const __ptr64
// ?back@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEAAAEAV?$Rect_@H@cv@@XZ	public: class cv::Rect_<int> & __ptr64 __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::back(void) __ptr64
// ?back@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEBAAEBV?$Rect_@H@cv@@XZ	public: class cv::Rect_<int> const & __ptr64 __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::back(void)const __ptr64
// ?begin@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEAA?AV?$_Vector_iterator@V?$_Vector_val@U?$_Simple_types@V?$Rect_@H@cv@@@std@@@std@@@2@XZ	public: class std::_Vector_iterator<class std::_Vector_val<struct std::_Simple_types<class cv::Rect_<int> > > > __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::begin(void) __ptr64
// ?begin@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEBA?AV?$_Vector_const_iterator@V?$_Vector_val@U?$_Simple_types@V?$Rect_@H@cv@@@std@@@std@@@2@XZ	public: class std::_Vector_const_iterator<class std::_Vector_val<struct std::_Simple_types<class cv::Rect_<int> > > > __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::begin(void)const __ptr64
// ?capacity@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEBA_KXZ	public: unsigned __int64 __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::capacity(void)const __ptr64
// ?cbegin@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEBA?AV?$_Vector_const_iterator@V?$_Vector_val@U?$_Simple_types@V?$Rect_@H@cv@@@std@@@std@@@2@XZ	public: class std::_Vector_const_iterator<class std::_Vector_val<struct std::_Simple_types<class cv::Rect_<int> > > > __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::cbegin(void)const __ptr64
// ?cend@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEBA?AV?$_Vector_const_iterator@V?$_Vector_val@U?$_Simple_types@V?$Rect_@H@cv@@@std@@@std@@@2@XZ	public: class std::_Vector_const_iterator<class std::_Vector_val<struct std::_Simple_types<class cv::Rect_<int> > > > __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::cend(void)const __ptr64
// ?clear@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEAAXXZ	public: void __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::clear(void) __ptr64
// ?crbegin@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEBA?AV?$reverse_iterator@V?$_Vector_const_iterator@V?$_Vector_val@U?$_Simple_types@V?$Rect_@H@cv@@@std@@@std@@@std@@@2@XZ	public: class std::reverse_iterator<class std::_Vector_const_iterator<class std::_Vector_val<struct std::_Simple_types<class cv::Rect_<int> > > > > __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::crbegin(void)const __ptr64
// ?crend@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEBA?AV?$reverse_iterator@V?$_Vector_const_iterator@V?$_Vector_val@U?$_Simple_types@V?$Rect_@H@cv@@@std@@@std@@@std@@@2@XZ	public: class std::reverse_iterator<class std::_Vector_const_iterator<class std::_Vector_val<struct std::_Simple_types<class cv::Rect_<int> > > > > __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::crend(void)const __ptr64
// ?data@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEAAPEAV?$Rect_@H@cv@@XZ	public: class cv::Rect_<int> * __ptr64 __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::data(void) __ptr64
// ?data@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEBAPEBV?$Rect_@H@cv@@XZ	public: class cv::Rect_<int> const * __ptr64 __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::data(void)const __ptr64
// ?empty@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEBA_NXZ	public: bool __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::empty(void)const __ptr64
// ?end@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEAA?AV?$_Vector_iterator@V?$_Vector_val@U?$_Simple_types@V?$Rect_@H@cv@@@std@@@std@@@2@XZ	public: class std::_Vector_iterator<class std::_Vector_val<struct std::_Simple_types<class cv::Rect_<int> > > > __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::end(void) __ptr64
// ?end@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEBA?AV?$_Vector_const_iterator@V?$_Vector_val@U?$_Simple_types@V?$Rect_@H@cv@@@std@@@std@@@2@XZ	public: class std::_Vector_const_iterator<class std::_Vector_val<struct std::_Simple_types<class cv::Rect_<int> > > > __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::end(void)const __ptr64
// ?erase@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEAA?AV?$_Vector_iterator@V?$_Vector_val@U?$_Simple_types@V?$Rect_@H@cv@@@std@@@std@@@2@V?$_Vector_const_iterator@V?$_Vector_val@U?$_Simple_types@V?$Rect_@H@cv@@@std@@@std@@@2@0@Z	public: class std::_Vector_iterator<class std::_Vector_val<struct std::_Simple_types<class cv::Rect_<int> > > > __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::erase(class std::_Vector_const_iterator<class std::_Vector_val<struct std::_Simple_types<class cv::Rect_<int> > > >,class std::_Vector_const_iterator<class std::_Vector_val<struct std::_Simple_types<class cv::Rect_<int> > > >) __ptr64
// ?erase@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEAA?AV?$_Vector_iterator@V?$_Vector_val@U?$_Simple_types@V?$Rect_@H@cv@@@std@@@std@@@2@V?$_Vector_const_iterator@V?$_Vector_val@U?$_Simple_types@V?$Rect_@H@cv@@@std@@@std@@@2@@Z	public: class std::_Vector_iterator<class std::_Vector_val<struct std::_Simple_types<class cv::Rect_<int> > > > __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::erase(class std::_Vector_const_iterator<class std::_Vector_val<struct std::_Simple_types<class cv::Rect_<int> > > >) __ptr64
// ?front@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEAAAEAV?$Rect_@H@cv@@XZ	public: class cv::Rect_<int> & __ptr64 __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::front(void) __ptr64
// ?front@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEBAAEBV?$Rect_@H@cv@@XZ	public: class cv::Rect_<int> const & __ptr64 __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::front(void)const __ptr64
// ?get_allocator@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEBA?AV?$allocator@V?$Rect_@H@cv@@@2@XZ	public: class std::allocator<class cv::Rect_<int> > __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::get_allocator(void)const __ptr64
// ?insert@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEAA?AV?$_Vector_iterator@V?$_Vector_val@U?$_Simple_types@V?$Rect_@H@cv@@@std@@@std@@@2@V?$_Vector_const_iterator@V?$_Vector_val@U?$_Simple_types@V?$Rect_@H@cv@@@std@@@std@@@2@$$QEAV?$Rect_@H@cv@@@Z	public: class std::_Vector_iterator<class std::_Vector_val<struct std::_Simple_types<class cv::Rect_<int> > > > __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::insert(class std::_Vector_const_iterator<class std::_Vector_val<struct std::_Simple_types<class cv::Rect_<int> > > >,class cv::Rect_<int> && __ptr64) __ptr64
// ?insert@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEAA?AV?$_Vector_iterator@V?$_Vector_val@U?$_Simple_types@V?$Rect_@H@cv@@@std@@@std@@@2@V?$_Vector_const_iterator@V?$_Vector_val@U?$_Simple_types@V?$Rect_@H@cv@@@std@@@std@@@2@AEBV?$Rect_@H@cv@@@Z	public: class std::_Vector_iterator<class std::_Vector_val<struct std::_Simple_types<class cv::Rect_<int> > > > __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::insert(class std::_Vector_const_iterator<class std::_Vector_val<struct std::_Simple_types<class cv::Rect_<int> > > >,class cv::Rect_<int> const & __ptr64) __ptr64
// ?insert@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEAA?AV?$_Vector_iterator@V?$_Vector_val@U?$_Simple_types@V?$Rect_@H@cv@@@std@@@std@@@2@V?$_Vector_const_iterator@V?$_Vector_val@U?$_Simple_types@V?$Rect_@H@cv@@@std@@@std@@@2@V?$initializer_list@V?$Rect_@H@cv@@@2@@Z	public: class std::_Vector_iterator<class std::_Vector_val<struct std::_Simple_types<class cv::Rect_<int> > > > __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::insert(class std::_Vector_const_iterator<class std::_Vector_val<struct std::_Simple_types<class cv::Rect_<int> > > >,class std::initializer_list<class cv::Rect_<int> >) __ptr64
// ?insert@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEAA?AV?$_Vector_iterator@V?$_Vector_val@U?$_Simple_types@V?$Rect_@H@cv@@@std@@@std@@@2@V?$_Vector_const_iterator@V?$_Vector_val@U?$_Simple_types@V?$Rect_@H@cv@@@std@@@std@@@2@_KAEBV?$Rect_@H@cv@@@Z	public: class std::_Vector_iterator<class std::_Vector_val<struct std::_Simple_types<class cv::Rect_<int> > > > __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::insert(class std::_Vector_const_iterator<class std::_Vector_val<struct std::_Simple_types<class cv::Rect_<int> > > >,unsigned __int64,class cv::Rect_<int> const & __ptr64) __ptr64
// ?max_size@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEBA_KXZ	public: unsigned __int64 __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::max_size(void)const __ptr64
// ?pop_back@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEAAXXZ	public: void __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::pop_back(void) __ptr64
// ?push_back@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEAAX$$QEAV?$Rect_@H@cv@@@Z	public: void __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::push_back(class cv::Rect_<int> && __ptr64) __ptr64
// ?push_back@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEAAXAEBV?$Rect_@H@cv@@@Z	public: void __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::push_back(class cv::Rect_<int> const & __ptr64) __ptr64
// ?rbegin@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEAA?AV?$reverse_iterator@V?$_Vector_iterator@V?$_Vector_val@U?$_Simple_types@V?$Rect_@H@cv@@@std@@@std@@@std@@@2@XZ	public: class std::reverse_iterator<class std::_Vector_iterator<class std::_Vector_val<struct std::_Simple_types<class cv::Rect_<int> > > > > __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::rbegin(void) __ptr64
// ?rbegin@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEBA?AV?$reverse_iterator@V?$_Vector_const_iterator@V?$_Vector_val@U?$_Simple_types@V?$Rect_@H@cv@@@std@@@std@@@std@@@2@XZ	public: class std::reverse_iterator<class std::_Vector_const_iterator<class std::_Vector_val<struct std::_Simple_types<class cv::Rect_<int> > > > > __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::rbegin(void)const __ptr64
// ?rend@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEAA?AV?$reverse_iterator@V?$_Vector_iterator@V?$_Vector_val@U?$_Simple_types@V?$Rect_@H@cv@@@std@@@std@@@std@@@2@XZ	public: class std::reverse_iterator<class std::_Vector_iterator<class std::_Vector_val<struct std::_Simple_types<class cv::Rect_<int> > > > > __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::rend(void) __ptr64
// ?rend@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEBA?AV?$reverse_iterator@V?$_Vector_const_iterator@V?$_Vector_val@U?$_Simple_types@V?$Rect_@H@cv@@@std@@@std@@@std@@@2@XZ	public: class std::reverse_iterator<class std::_Vector_const_iterator<class std::_Vector_val<struct std::_Simple_types<class cv::Rect_<int> > > > > __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::rend(void)const __ptr64
// ?reserve@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEAAX_K@Z	public: void __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::reserve(unsigned __int64) __ptr64
// ?resize@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEAAX_K@Z	public: void __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::resize(unsigned __int64) __ptr64
// ?resize@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEAAX_KAEBV?$Rect_@H@cv@@@Z	public: void __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::resize(unsigned __int64,class cv::Rect_<int> const & __ptr64) __ptr64
// ?shrink_to_fit@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEAAXXZ	public: void __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::shrink_to_fit(void) __ptr64

// 224
// ?size@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEBA_KXZ
// public: unsigned __int64 __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::size(void)const __ptr64
function size__StdVectorRect(Obj: pStdVectorRect): int64; external opencv_delphi_dll name '?size@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEBA_KXZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// ?swap@?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@QEAAXAEAV12@@Z	public: void __cdecl std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > >::swap(class std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > > & __ptr64) __ptr64

{$ENDREGION}

implementation

{$REGION 'CvStdString'}
{ CppString }

procedure CvStdString.assign(const p: pAnsiChar);
begin
  assign_CppString(@Self, p);
end;

class operator CvStdString.assign(var Dest: CvStdString; const [ref] Src: CvStdString);
begin
  Finalize(Dest);
  assign_CppString(pCppString(@Dest), pCppString(@Src));
end;

procedure CvStdString.erase(const _Off: UInt64);
begin
  erase_CppString(@Self, _Off);
end;

class operator CvStdString.Finalize(var Dest: CvStdString);
begin
  Destructor_CppString(@Dest);
end;

class operator CvStdString.Implicit(const s: string): CvStdString;
begin
  Result.assign(pAnsiChar(AnsiString(s)));
end;

class operator CvStdString.Implicit(const p: pAnsiChar): CvStdString;
begin
  Result.assign(p);
end;

class operator CvStdString.Implicit(const s: CvStdString): string;
var
  r: pAnsiChar;
begin
  r := c_str_CppString(@s);
  Result := string(r);
end;

class operator CvStdString.Initialize(out Dest: CvStdString);
begin
  Constructor_CppString(@Dest);
end;

function CvStdString.length: UInt64;
begin
  Result := length_CppString(@Self);
end;

function CvStdString.size: UInt64;
begin
  Result := size_CppString(@Self);
end;

{$ENDREGION}
//
{$REGION 'std::vector<Rect>'}
{ StdVectorRect }

class operator StdVectorRect.Finalize(var Dest: StdVectorRect);
begin
  Destructor_StdVectorRect(@Dest);
end;

class operator StdVectorRect.Initialize(out Dest: StdVectorRect);
begin
  Constructor_StdVectorRect(@Dest);
end;

function StdVectorRect.size: int64;
begin
  Result := size__StdVectorRect(@Self);
end;

{$ENDREGION}

end.


class sequence final : public object_t, public seekable_i {
  const var next;
  const var data;
public:

  size_t type() const final { return runtime::type::sequence; }

  bool equals(var const & o) const final {
    if(first() != runtime::first(o))
      return false;
    
    for(auto const& it : runtime::range_pair(rest(),runtime::rest(o)))
      if (it.first != it.second)
        return false;

    return true;
  }

#if !defined(FERRET_DISABLE_STD_OUT)
  void stream_console() const final {
    runtime::print("(");
    data.stream_console();
    for(auto const& i : runtime::range(next)){
      runtime::print(" ");
      i.stream_console();
    }
    runtime::print(")");
  }
#endif

  explicit sequence(var const & d = nil(), var const & n = nil()) : next(n), data(d) {} 

  virtual seekable_i* cast_seekable_i() { return this; }

  var cons(var const & x) final {
    return obj<sequence>(x, var(this));
  }
  var first() const final {
    return data;
  }
  var rest() const final {
    return next;
  }
  template <typename T>
  static T to(var const & ){
    T::unimplemented_function;
  }
  template <typename T>
  static var from(T){
    T::unimplemented_function; return nil();
  }

};
namespace runtime {
  inline var list() { 
    return cached::empty_sequence;
  }
  inline var list(var const & v) { 
    return obj<sequence>(v,nil());
  }
                    
  template <typename... Args>
  inline var list(var const & first, Args const & ... args) { 
    return obj<sequence>(first, list(args...));
  }
}

#ifdef FERRET_STD_LIB
typedef ::std::vector<var>  std_vector;

template <> std_vector sequence::to(var const & v) { 
  std_vector ret;
  for(auto const& it : runtime::range(v))
    ret.push_back(it);
  return ret;
}

template <> var sequence::from(std_vector v) { 
  var ret;
  for(auto const& it : v)
    ret = runtime::cons(it,ret);
  return ret;
}
#endif

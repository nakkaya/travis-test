
class number final : public object_t {
  const real_t _word;
public:


  size_t type() const final { return runtime::type::number; }

  bool equals(var const & o) const final {
    if (runtime::abs(_word - o.cast<number>()->word()) < real_epsilon)
      return true;
    else
      return false;
  }

#if !defined(FERRET_DISABLE_STD_OUT)
  void stream_console() const final {
    runtime::print(_word);
  }
#endif

  template<typename T> explicit number(T x) : _word((real_t)x) {} 

  real_t word() const {
    return _word;
  }
  
  template<typename T> T as() const {
    T::unimplemented_function;
  }
  
  var add(var const & v) const {
    return obj<number>(_word + v.cast<number>()->word());
  }
  var sub(var const & v) const {
    return obj<number>(_word - v.cast<number>()->word());
  }
  var mul(var const & v) const {
    return obj<number>(_word * v.cast<number>()->word());
  }
  var div(var const & v) const {
    return obj<number>(_word / v.cast<number>()->word());
  }
  var is_smaller(var const & v) const {
    return obj<boolean>(_word < v.cast<number>()->word());
  }
  var is_smaller_equal(var const & v) const {
    return obj<boolean>(_word <= v.cast<number>()->word());
  }
  var is_bigger(var const & v) const {
    return obj<boolean>(_word > v.cast<number>()->word());
  }
  var is_bigger_equal(var const & v) const {
    return obj<boolean>(_word >= v.cast<number>()->word());
  }
  
  template<typename T> static T to(var const & v){
    return (T)v.cast<number>()->word();
  }
};

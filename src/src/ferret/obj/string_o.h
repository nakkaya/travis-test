
class string final : public object_t, public seekable_i {
  var data;

  void from_char_pointer(const char * str, int length){
    for (int i = --length; i >= 0; i--)
      data = runtime::cons(obj<number>((number_t)str[i]),data);
  }
  
public:

  size_t type() const final { return runtime::type::string; }

  bool equals(var const & other) const final {
    return (container() == other.cast<string>()->container());
  }

#if !defined(FERRET_DISABLE_STD_OUT)
  void stream_console() const final {
    for(auto const& it : runtime::range(data))
      runtime::print(number::to<char>(it));
  }
#endif

  explicit string() : data(nullptr) {} 

  explicit string(var const & s) : data(s) {}

  explicit string(const char * str) {
    int length = 0;
    for (length = 0; str[length] != '\0'; ++length);
    from_char_pointer(str,length);
  }

  explicit string(const char * str,number_t length) { from_char_pointer(str,length); }

  var container() const {
    return data;
  }

  virtual seekable_i* cast_seekable_i() { return this; }

  var cons(var const & x) final {
    return obj<string>(runtime::cons(x,data));
  }

  var first() const final {
    return runtime::first(data);
  }

  var rest() const final {
    if (!runtime::rest(data).is_nil())
      return obj<string>(runtime::rest(data));

    return cached::empty_sequence;
  }

  template <typename T>
  static T to(var const & ){
    T::unimplemented_function;
  }

};

#ifdef FERRET_STD_LIB
template<>
inline var obj<string>(std::string s) {
  void * storage = FERRET_ALLOC_POLICY::allocate<string>();
  return var(new(storage) string(s.c_str(), (number_t)s.size()));
}

template <> ::std::string string::to(var const & v) { 
  ::std::stringstream ss;
  for(auto const& it : runtime::range(v.cast<string>()->container()))
    ss << number::to<char>(it);
  return ss.str();
}
#endif

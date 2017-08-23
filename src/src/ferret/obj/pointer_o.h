
class pointer final : public object_t {
  void * _payload;
public:


  size_t type() const final { return runtime::type::pointer; }

  bool equals(var const & o) const final {
    return (_payload == o.cast<pointer>()->payload());
  }

#if !defined(FERRET_DISABLE_STD_OUT)
  void stream_console() const final {
    runtime::print("pointer<");
    runtime::print(_payload);
    runtime::print(">");
  }
#endif

  explicit pointer(void* p) : _payload(p) {} 

  void* payload() const {
    return _payload;
  }
  template<typename T> static T* to_pointer(var const & v){
    return ((T *) v.cast<pointer>()->payload());
  }
  template<typename T> static T& to_reference(var const & v){
    return (*(pointer::to_pointer<T>(v)));
  }
};

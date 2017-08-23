
template <typename T>
class value final : public object_t {
  T _value;
 public:

  size_t type() const final { return runtime::type::value; }

  bool equals(var const & o) const final {
    return (this == o.get());
  }

#if !defined(FERRET_DISABLE_STD_OUT)
  void stream_console() const final {
    runtime::print("value<");
    const void* addr = this;
    runtime::print(addr);
    runtime::print(">");
  }
#endif

  template <typename... Args>
  explicit value(Args&&... args) : _value(static_cast<Args&&>(args)...) { } 

  T to_value() const {
    return _value;
  }
  
  static T to_value(var const & v){
    return v.cast<value<T>>()->to_value();
  }

  T & to_reference() {
    return _value;
  }
  
  static T & to_reference(var const & v) {
    return v.cast<value<T>>()->to_reference();
  }  
};

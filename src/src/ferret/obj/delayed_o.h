
class delayed final : public deref_i {
  var val;
  mutex lock;
  var fn;
  
  public:

  size_t type() const final { return runtime::type::delayed; }

  bool equals(var const & o) const final {
    return (this == o.cast<delayed>());
  }

#if !defined(FERRET_DISABLE_STD_OUT)
  void stream_console() const final {
    runtime::print("delay");
  }
#endif

  explicit delayed(var const & f) : fn(f) {} 
  
  var deref() {
    lock_guard guard(lock);
    if (!fn.is_nil()){
      val = fn.cast<lambda_i>()->invoke(nil());
      fn = nil();
    }
    return val;
  }
};

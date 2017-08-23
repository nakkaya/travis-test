
class atomic final : public deref_i {
  var data;
  mutex lock;
  public:


  size_t type() const final { return runtime::type::atomic; }

  bool equals(var const & o) const final {
    return (this == o.cast<atomic>());
  }

#if !defined(FERRET_DISABLE_STD_OUT)
  void stream_console() const final {
    runtime::print("atom <");
    data.stream_console();
    runtime::print(">");
  }
#endif

  explicit atomic(var const & d) : data(d) {} 

  var swap(var const & f,var const & args){
    lock_guard guard(lock);
    data = f.cast<lambda_i>()->invoke(runtime::cons(data, args));
    return data;
  }
  var deref() {
    lock_guard guard(lock);
    return data;
  }
};

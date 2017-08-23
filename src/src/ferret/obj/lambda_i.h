
class lambda_i : public object_t {
 public:
  virtual var invoke(var const & args) const = 0;

  size_t type() const { return runtime::type::lambda_i; }

  bool equals(var const & o) const {
    return (this == o.get());
  }

#if !defined(FERRET_DISABLE_STD_OUT)
  void stream_console() const {
    runtime::print("lambda");
  }
#endif
};

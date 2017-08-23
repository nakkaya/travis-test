
class lazy_sequence final : public object_t, public seekable_i {
  const var thunk;
  const var head;
public:

  size_t type() const final { return runtime::type::lazy_sequence; }

  var sval() const {
    if (head.is_nil())
      return runtime::first(run(thunk));
    
    return head;
  }

  bool equals(var const & o) const final {
    if(sval() != runtime::first(o))
      return false;
    
    for(auto const& it : runtime::range_pair(rest(),runtime::rest(o)))
      if (it.first != it.second)
        return false;

    return true;
  }

#if !defined(FERRET_DISABLE_STD_OUT)
  void stream_console() const final {
    runtime::print("(");
    sval().stream_console();
    for(auto const& i : runtime::range(rest())){
      runtime::print(" ");
      i.stream_console();
    }
    runtime::print(")");
  }
#endif

  explicit lazy_sequence(var const & t) : thunk(t) {} 
  explicit lazy_sequence(var const & h, var const & t) : thunk(t), head(h) {} 

  virtual seekable_i* cast_seekable_i() { return this; }

  var cons(var const & x) final {
    return obj<lazy_sequence>(x,thunk);
  }
  var first() const final {
    return sval();
  }
  var rest() const final {
    if (head.is_nil())
      return runtime::rest(run(thunk));
    
    return run(thunk);
  }
};

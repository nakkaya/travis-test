
#ifdef FERRET_STD_LIB
class async final : public deref_i {
  var value;
  mutex lock;
  var fn;
  bool cached;
  std::future<var> task;

  class rc_guard{
    object_t *obj;
  public:
    explicit rc_guard(const rc_guard &) = delete;
    explicit rc_guard(object_t *o) : obj(o) { };
    ~rc_guard() { obj->dec_ref(); }
  };

  var exec() {
    rc_guard g(this);
    return run(fn);
  }
  
  public:

  explicit async(var const & f) :
    value(nil()), fn(f), cached(false),
    task(std::async(std::launch::async, [this](){ return exec(); })){ inc_ref(); }

  size_t type() const final { return runtime::type::async; }

  bool equals(var const & o) const final {
    return (this == o.cast<async>());
  }

#if !defined(FERRET_DISABLE_STD_OUT)
  void stream_console() const final {
    runtime::print("future <");
    fn.stream_console();
    runtime::print(">");
  }
#endif

  bool is_ready(){
    lock_guard guard(lock);
    if (cached)
      return true;
    return task.wait_for(std::chrono::seconds(0)) == std::future_status::ready;
  }

  void get(){
    if (!cached){
      value = task.get();
      cached = true;
    }
  }

  var deref() {
    lock_guard guard(lock);
    get();
    return value;
  }
};
#endif

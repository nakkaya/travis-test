
#if !defined(FERRET_SAFE_MODE)
class elapsed_micros : public object_t {

  unsigned long us;

#if defined(FERRET_HARDWARE_ARDUINO)
  inline unsigned long now() const{
    return ::micros();
  }
#elif defined(FERRET_STD_LIB)
  inline unsigned long now() const{
    auto now = ::std::chrono::high_resolution_clock::now();
    auto epoch = now.time_since_epoch();
    return (unsigned long)::std::chrono::duration_cast<::std::chrono::microseconds>(epoch).count();
  }
#endif

  inline unsigned long _elapsed() const { return (now() - us); }  
  
 public:

  elapsed_micros(void) { us = now(); }
  void reset() { us = now(); }
  
  size_t type() const { return runtime::type::elapsed_micros; }

  bool equals(var const & o) const {
    return (this == o.cast<elapsed_micros>());
  }

#if !defined(FERRET_DISABLE_STD_OUT)
  void stream_console() const {
    runtime::print("elapsed_micros<");
    runtime::print(_elapsed());
    runtime::print(">");
  }
#endif

  inline var elapsed() const { return obj<number>(_elapsed()); }
  inline bool is_elapsed(real_t t) const { return (_elapsed() >= (unsigned long)t); }
};
#endif

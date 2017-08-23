
class keyword final : public lambda_i {
  const number_t _word;

  number_t from_str(const char * str){
    number_t word = 0;
    for (number_t i = 0; str[i] != '\0'; i++){
      word = word + (number_t)str[i];
    }
    
    return word;
  }
  
public:

  size_t type() const final { return runtime::type::keyword; }

  bool equals(var const & o) const final {
    return (_word == o.cast<keyword>()->word());
  }

#if !defined(FERRET_DISABLE_STD_OUT)
  void stream_console() const final {
    runtime::print(_word);
  }
#endif

  explicit keyword(number_t w) : _word(w) {} 
  explicit keyword(const char * str): _word(from_str(str)) { }

  number_t word() const {
    return _word;
  }

  var invoke(var const & args) const {
    var map = runtime::first(args);
    var map_args = runtime::cons(var((object_t*)this), runtime::rest(args));

    if (map.is_type(runtime::type::d_list)){
      return map.cast<d_list>()->val_at(map_args);
    }

    return nil();
  }
};

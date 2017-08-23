
class d_list final : public lambda_i, public seekable_i {

  var data;

  number_t val_index(var const & k) const {
    var keys = runtime::first(data);

    for(auto i : runtime::range_indexed(keys))
      if ( i.value == k )
        return i.index;

    return -1;
  }
  
public:

  size_t type() const final { return runtime::type::d_list; }

  bool equals(var const & o) const final {
    return (this == o.get());
  }

#if !defined(FERRET_DISABLE_STD_OUT)
  void stream_console() const final {
    data.stream_console();
  }
#endif

  explicit d_list() : data(runtime::list(runtime::list())) { }
  explicit d_list(var const & l) : data(l) { }

  var assoc(var const & k, var const & v) const {
    var keys = runtime::first(data);
    var values = runtime::rest(data);

    values = runtime::cons(v,values);
    keys   = runtime::cons(k,keys);
    
    return obj<d_list>(runtime::cons(keys,values));
  }

  var dissoc(var const & k) const {
    number_t idx = val_index(k);
    
    if ( idx == -1 )
      return obj<d_list>(data);

    var keys = runtime::first(data);
    var values = runtime::rest(data);

    var new_keys;
    for(auto i : runtime::range_indexed(keys))
      if ( i.index != idx)
        new_keys = runtime::cons(i.value, new_keys);

    var new_values;
    for(auto i : runtime::range_indexed(values))
      if ( i.index != idx)
        new_values = runtime::cons(i.value, new_values);
    
    return obj<d_list>(runtime::cons(new_keys,new_values));
  }
  
  var val_at(var const & args) const {
    var key = runtime::first(args);
    var not_found = runtime::first(runtime::rest(args));

    var values = runtime::rest(data);
    number_t idx = val_index(key);

    if ( idx == -1 ){
     if ( !not_found.is_nil() ){
      return not_found;
     }else{
      return nil();  
     }
    }

    for(number_t i = 0; i < idx; i++)
      values = runtime::rest(values);
    
    return runtime::first(values);
  }

  var invoke(var const & args) const final {
    return val_at(args);
  }

  var vals () const { return runtime::rest(data);}
  var keys () const { return runtime::first(data);}

  virtual seekable_i* cast_seekable_i() { return this; }
  
  var cons(var const & v) final {
    return runtime::list(v,data);
  }
  
  var first() const final {
    var keys = runtime::first(data);
    var values = runtime::rest(data);
    return runtime::list(runtime::first(keys),runtime::first(values));
  }
  
  var rest() const final {
    var keys = runtime::first(data);
    var values = runtime::rest(data);

    if(runtime::rest(keys) == nil())
      return runtime::list();
    
    return obj<d_list>(runtime::cons(runtime::rest(keys),runtime::rest(values)));
  }
};

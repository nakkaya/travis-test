
namespace ferret{
  namespace runtime{
    var first(var const & coll){
      if (coll.is_nil() || coll.is_type(runtime::type::empty_sequence))
        return nil();
      else
        return coll.cast<seekable_i>()->first();
    }
  
    var rest(var const & coll){
      if (coll.is_nil())
        return runtime::list();
      if (coll.is_type(runtime::type::empty_sequence))
        return nil();
      return coll.cast<seekable_i>()->rest();
    }
  
    var cons(var const & x, var const & coll){
      if (coll.is_nil() || coll == runtime::list())
        return runtime::list(x);
  
      return coll.cast<seekable_i>()->cons(x);
    }
  
    var nth(var const & seq, number_t index){
      for(auto const& i : range_indexed(seq))
        if (index == i.index)
          return i.value;
  
      return nil();
    }
  
    var nthrest(var const & seq, number_t index){
      var ret = seq;
      for(number_t i = 0; i < index; i++)
        ret = runtime::rest(ret);
  
      if (ret.is_nil())
        return runtime::list(); 
  
      return ret;
    }
    
    size_t count(var const & seq){
      size_t acc = 0;
      for(auto const& v : runtime::range(seq)){
        (void)v;
        acc++;
      }
      return acc;
    }
  
    bool is_seqable(var const & seq){
      if(seq.cast<seekable_i>())
        return true;
      else
        return false;
    }
  }
  template<typename T, typename... Args>
  inline var run(T const & fn, Args const & ... args) {
    return fn.invoke(runtime::list(args...));
  }
  
  template<typename T>
  inline var run(T const & fn) {
    return fn.invoke(nil());
  }
  
  template<>
  inline var run(var const & fn) {
    return fn.cast<lambda_i>()->invoke(nil());
  }
  
  template<typename... Args>
  inline var run(var const & fn, Args const & ... args) {
    return fn.cast<lambda_i>()->invoke(runtime::list(args...));
  }
}

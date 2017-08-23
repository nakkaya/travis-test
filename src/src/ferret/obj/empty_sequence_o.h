
class empty_sequence final : public object_t {
public:

  size_t type() const final { return runtime::type::empty_sequence; }

  bool equals(var const & ) const final {
    return true;
  }

#if !defined(FERRET_DISABLE_STD_OUT)
  void stream_console() const final {
    runtime::print("()");
  }
#endif
};

namespace cached{
  const var empty_sequence = obj<ferret::empty_sequence>();
}

namespace runtime {
  struct range{
    var p;

    explicit range(var const & v) : p(v) { }
    inline range begin() const { return range(p); }
    inline range end()   const { return range(cached::empty_sequence); }

    inline bool operator!=(const range& other){
      return !p.is_nil() && (p != other.p);
    }

    inline const range& operator++(){
      p = runtime::rest(p);
      return *this;
    }

    inline var operator*(){
      return runtime::first(p);
    }
  };
}

namespace runtime {
  struct range_indexed_pair{
    number_t index;
    var value;

    explicit range_indexed_pair(number_t i = 0, var const & v = nil()) : index(i) , value(v) { }
  };
  
  struct range_indexed{
    var p;
    number_t index;

    explicit range_indexed(var const & v) : p(v) , index(0) { }
    inline range_indexed begin() const { return range_indexed(p); }
    inline range_indexed end()   const { return range_indexed(cached::empty_sequence); }

    inline bool operator!=(const range_indexed& other){
      return !p.is_nil() && (p != other.p);
    }

    inline const range_indexed& operator++(){
      p = runtime::rest(p);
      index++;
      return *this;
    }

    inline range_indexed_pair operator*(){
      return range_indexed_pair(index, runtime::first(p));
    }
  };
}

namespace runtime {
  struct range_pair_pair{
    var first;
    var second;

    explicit range_pair_pair(var const & a = nil(), var const & b = nil()) : first(a) , second(b) { }
  };
    
  struct range_pair{
    var first;
    var second;

    explicit range_pair(var const & a = nil(), var const & b = nil()) : first(a) , second(b) { }
    
    inline range_pair begin() const { return range_pair(first, second); }
    inline range_pair end()   const { return range_pair(cached::empty_sequence,cached::empty_sequence); }

    inline bool operator!=(const range_pair& other){
      return (first != other.first) && (second != other.second);
    }

    inline const range_pair& operator++(){
      first = runtime::rest(first);
      second = runtime::rest(second);
      return *this;
    }

    inline range_pair_pair operator*(){
      return range_pair_pair(runtime::first(first), runtime::first(second));
    }
  };
}

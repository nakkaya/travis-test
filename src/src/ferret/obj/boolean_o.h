
class boolean final : public object_t {
  const bool value;
public:

  size_t type() const final { return runtime::type::boolean; }

  bool equals(var const & o) const final {
    return (value == o.cast<boolean>()->container());
  }

#if !defined(FERRET_DISABLE_STD_OUT)
  void stream_console() const final {
    if (value)
      runtime::print("true");
    else
      runtime::print("false");
  }
#endif

  explicit boolean(bool b) : value(b) {} 

  bool container() const {
    return value;
  }
};

namespace cached{
  const var true_t = obj<ferret::boolean>(true);
  const var false_t = obj<ferret::boolean>(false);
}

var::operator bool() const {
  if (obj == nullptr)
    return false;
  else if (obj->type() == runtime::type::boolean)
    return static_cast<boolean*>(obj)->container();
  else
    return true;
}

bool var::equals (var const & other) const {
  if ( ( is_nil() && !other.is_nil()) ||
       (!is_nil() &&  other.is_nil()))
    return false;

  if (get() == other.get())
    return true;
  
  if (runtime::is_seqable(*this) && runtime::is_seqable(other))
    return get()->equals(other);
  else if (obj->type() != other.cast<object_t>()->type())
    return false;
  else
    return get()->equals(other);
}

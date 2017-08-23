
// Literals
namespace ferret{
  constexpr auto operator "" _MB( unsigned long long const x ) -> long { return 1024L * 1024L * (long)x; }
  constexpr auto operator "" _KB( unsigned long long const x ) -> long { return 1024L * (long)x; }
}

// Detect Hardware
# define FERRET_CONFIG_SAFE_MODE TRUE

#if !defined(FERRET_SAFE_MODE)
  #if defined(__APPLE__) ||                       \
    defined(_WIN32) ||                            \
    defined(__linux__) ||                         \
    defined(__unix__) ||                          \
    defined(_POSIX_VERSION)
  
    # undef  FERRET_CONFIG_SAFE_MODE
    # define FERRET_STD_LIB TRUE
  #endif
  
  #if defined(ARDUINO)

    # define FERRET_HARDWARE_ARDUINO TRUE

    #if !defined(FERRET_HARDWARE_ARDUINO_UART_PORT)
      # define FERRET_HARDWARE_ARDUINO_UART_PORT Serial
    #endif
  #endif
  
  #if defined(FERRET_HARDWARE_ARDUINO)
    # undef  FERRET_CONFIG_SAFE_MODE
    # define FERRET_DISABLE_MULTI_THREADING TRUE
    # define FERRET_DISABLE_STD_MAIN TRUE


    #if defined(__AVR__)
      # undef  FERRET_MEMORY_POOL_PAGE_TYPE
      # define FERRET_MEMORY_POOL_PAGE_TYPE uint8_t
    #endif

  #endif
#endif

#if defined(FERRET_CONFIG_SAFE_MODE)
  # define FERRET_DISABLE_MULTI_THREADING TRUE
  # define FERRET_DISABLE_STD_OUT TRUE
#endif
#ifdef FERRET_STD_LIB
 #include <iostream>
 #include <iomanip>
 #include <sstream>
 #include <cstdio>
 #include <cstdlib>
 #include <cstddef>
 #include <cmath>
 #include <vector>
 #include <algorithm>
 #include <chrono>
 #include <atomic>
 #include <mutex>
 #include <thread>
 #include <future>
#endif

#ifdef FERRET_HARDWARE_ARDUINO
 #include <Arduino.h>
 #include <stdio.h>
 #include <stdlib.h>
 #include <stdint.h>
#endif

#ifdef FERRET_CONFIG_SAFE_MODE
 #include <stdio.h>
 #include <stdlib.h>
 #include <stdint.h>
 #include <math.h>
#endif

namespace ferret {
#if defined(FERRET_STD_LIB) && !defined(FERRET_DISABLE_MULTI_THREADING)
  class mutex {
    ::std::mutex m;
  public:
    void lock()   { m.lock(); } 
    void unlock() { m.unlock(); }
  };
#else
  class mutex {
  public:
    void lock()   {} 
    void unlock() {} 
  };
#endif
}

namespace ferret {
  class lock_guard{
    mutex & _ref;
  public:
    explicit lock_guard(const lock_guard &) = delete;
    explicit lock_guard(mutex & mutex) : _ref(mutex) { _ref.lock(); };
    ~lock_guard() { _ref.unlock(); }
  };
}

// Fixed Point Real
namespace ferret{
  #if !defined(__clang__)
  constexpr auto operator "" _QN(long double x) -> int {
    return (int)::floor(::log(1.0/(double)x)/::log(2));
  }
  #endif
  
  template<int bits> struct fixed_real_container;
  template<> struct fixed_real_container<8>  { typedef int8_t  base_type;
                                               typedef int16_t next_type; };
  template<> struct fixed_real_container<16> { typedef int16_t base_type;
                                               typedef int32_t next_type; };
  template<> struct fixed_real_container<32> { typedef int32_t base_type;
                                               typedef int64_t next_type; };
  template<> struct fixed_real_container<64> { typedef int64_t base_type;
                                               typedef int64_t next_type; };
  
  template<int bits, int exp>
  class fixed_real{
    typedef fixed_real fixed;
    typedef typename fixed_real_container<bits>::base_type base;
    typedef typename fixed_real_container<bits>::next_type next;
  
    base m;
    static const int N      = (exp - 1);
    static const int factor = 1 << N;
  
    template<typename T>
    inline T to_rational() const { return T(m) / factor; }
  
    template<typename T>
    inline base from_rational(T d) const { return (base)(d * factor); }
      
    template<typename T>
    inline base from_whole(T i) const { return ((base)i << N); }
  
    template<typename T>
    inline T to_whole() const { return (T)(m >> N); }
      
  public:
  
    //from types
    explicit fixed_real( )           : m(0) { }
    template<typename T>
    explicit fixed_real(T v)         : m(from_whole<T>(v)) {}
    explicit fixed_real(double d)    : m(from_rational<double>(d)) { }
  
    template<typename T>
    fixed& operator=(T v)        { m = from_whole<T>(v); return *this; }
    fixed& operator=(double v)   { m = from_rational<double>(v); return *this; }
      
    //to types
    template<typename T>
    operator T()           const { return to_whole<T>();    }
    operator double()      const { return to_rational<double>(); }
      
    // operations
    fixed& operator+= (const fixed& x) { m += x.m; return *this; }
    fixed& operator-= (const fixed& x) { m -= x.m; return *this; }
    fixed& operator*= (const fixed& x) { m = (base)(((next)m * (next)x.m) >> N); return *this; }
    fixed& operator/= (const fixed& x) { m = (base)(((next)m << N) / x.m); return *this; }
    fixed& operator*= (int x)          { m *= x; return *this; }
    fixed& operator/= (int x)          { m /= x; return *this; }
    fixed  operator-  ( )              { return fixed(-m); }
      
    // friend functions
    friend fixed operator+ (fixed x, const fixed& y) { return x += y; }
    friend fixed operator- (fixed x, const fixed& y) { return x -= y; }
    friend fixed operator* (fixed x, const fixed& y) { return x *= y; }
    friend fixed operator/ (fixed x, const fixed& y) { return x /= y; }
      
    // comparison operators
    friend bool operator== (const fixed& x, const fixed& y) { return x.m == y.m; }
    friend bool operator!= (const fixed& x, const fixed& y) { return x.m != y.m; }
    friend bool operator>  (const fixed& x, const fixed& y) { return x.m > y.m; }
    friend bool operator<  (const fixed& x, const fixed& y) { return x.m < y.m; }
    friend bool operator>= (const fixed& x, const fixed& y) { return x.m >= y.m; }
    friend bool operator<= (const fixed& x, const fixed& y) { return x.m <= y.m; }
  
  #if defined(FERRET_STD_LIB)
    friend std::ostream& operator<< (std::ostream& stream, const fixed& x) {
      stream << (double)x;
      return stream;
    }
  #endif
  };
}

// Number Configuration
namespace ferret{
#if !defined(FERRET_NUMBER_TYPE)
   #define FERRET_NUMBER_TYPE int
#endif

#if !defined(FERRET_REAL_TYPE)
   #define FERRET_REAL_TYPE   double
#endif

#if !defined(FERRET_REAL_EPSILON)
   #define FERRET_REAL_EPSILON   0.00001
#endif
  
  typedef FERRET_NUMBER_TYPE           number_t;                   // Whole number Container.
  typedef FERRET_REAL_TYPE             real_t;                     // Real number Container.
  const   real_t                       real_epsilon(FERRET_REAL_EPSILON);
#if !defined(FERRET_DISABLE_STD_OUT)
  const   size_t                       number_precision = 4;       // number Format String (fprinf)
#endif

  constexpr auto operator "" _pi(long double x) -> double {
    return 3.14159265358979323846 * (double)x;
  }

  constexpr auto operator "" _pi(unsigned long long int  x) -> double {
    return 1.0_pi * (double)x;
  }

  constexpr auto operator "" _deg(long double x) -> double {
    return (1.0_pi * (double)x) / 180;
  }

  constexpr auto operator "" _deg(unsigned long long int  x) -> double {
    return 1.0_deg * (double)x;
  }
}

// Initialize Hardware
namespace ferret{
  #if !defined(FERRET_UART_RATE)
    # define FERRET_UART_RATE 9600
  #endif
  #if defined(FERRET_DISABLE_STD_OUT)
     namespace runtime{
       void init(){ }
      
       template <typename T>
       void print(T){ }
     }
  #endif
  #if defined(FERRET_STD_LIB) && !defined(FERRET_DISABLE_STD_OUT)
    namespace runtime{
      void init(){}
      
      template <typename T>
      void print(const T t){ std::cout << t; }
  
      template <>
      void print(const real_t n){
        std::cout << std::fixed << std::setprecision(number_precision) << n;
      }
    }
  #endif
  #if defined(FERRET_HARDWARE_ARDUINO) && !defined(FERRET_DISABLE_STD_OUT) 
    namespace runtime{
      void init(){ FERRET_HARDWARE_ARDUINO_UART_PORT.begin(FERRET_UART_RATE); }
  
      template <typename T>
      void print(const T t){ FERRET_HARDWARE_ARDUINO_UART_PORT.print(t); }
  
      template <>
      void print(const real_t d){ FERRET_HARDWARE_ARDUINO_UART_PORT.print(double(d)); }
      
      template <>
      void print(void *p){
        FERRET_HARDWARE_ARDUINO_UART_PORT.print((size_t)p,HEX);
      }
  
      template <> void print(const void * const p){
        FERRET_HARDWARE_ARDUINO_UART_PORT.print((size_t)p, HEX);
      }
     }
  #endif
}

// Object System Base
namespace ferret{
  #ifdef FERRET_MEMORY_POOL_SIZE
  namespace mem{
    namespace allocator{
      template<size_t pool_size>
      class bit_array {
      private:
        uint8_t bits[pool_size / 8 + 1];
  
        inline size_t index (size_t i) { return i / 8; }
        inline size_t offset(size_t i) { return i % 8; }
      public:
        bit_array() : bits{ false } { }
  
        inline void set   (size_t b){         bits[index(b)] = (uint8_t)(bits[index(b)] |  (1 << (offset(b))));}
        inline void reset (size_t b){         bits[index(b)] = (uint8_t)(bits[index(b)] & ~(1 << (offset(b))));}
        inline bool test  (size_t b){ return (bits[index(b)] & (1 << (offset(b))));}
      };
  
      template<typename page_size, size_t pool_size>
      class memory_pool{
      public:
        mutex lock;
        bit_array<pool_size> used;
        page_size pool[pool_size];
        size_t offset;
        size_t page_not_found;
  
        memory_pool() : pool{0}, offset(0), page_not_found(pool_size + 1) { }
  
        inline size_t chunk_length(size_t size){
          size_t d = (size / sizeof(page_size));
          size_t f = (size % sizeof(page_size));
  
          if (f == 0)
            return d;
          else
            return (d + 1);
        }
  
        inline bool chunk_usable(size_t begin, size_t end){
          for(size_t i=begin; i < end; i++)
            if (used.test(i))
              return false;
          return true;
        }
  
        inline size_t next_page(size_t begin){
          for(size_t i=begin; i < pool_size; i++)
            if (!used.test(i))
              return i;
          return pool_size;
        }
  
        inline size_t scan_pool(size_t pages_needed, size_t offset = 0){
          for(;;){
            size_t begin = next_page(offset);
            size_t end   = begin + pages_needed;
    
            if (end > pool_size)
              return page_not_found;
          
            if (chunk_usable(begin, end))
              return begin;
    
            offset = end;
          }
        }
  
        void *allocate(size_t req_size){
          lock_guard guard(lock);
  
          size_t length = chunk_length(++req_size);
          size_t page   = scan_pool(length, offset);
  
          if (page == page_not_found){
            page = scan_pool(length);
            if (page == page_not_found)
              return nullptr;
          }
          
          pool[page] = length;
          offset = page + length;
          for(size_t i = page; i < offset; i++)
            used.set(i);
  
          return &pool[++page];
        }
  
        void free(void *p){
          lock_guard guard(lock);
  
          ptrdiff_t begin = (static_cast<page_size *>(p) - pool) - 1;
          ptrdiff_t end = begin + (ptrdiff_t)pool[begin];
  
          for (ptrdiff_t i = begin; i < end; i++)
            used.reset((size_t)i);
        }
      };
    }
  }
  #endif
  #ifdef FERRET_MEMORY_POOL_SIZE
  
   #define FERRET_ALLOC_POLICY mem::allocator::pool
  
   #if !defined(FERRET_MEMORY_POOL_PAGE_TYPE)
    #define FERRET_MEMORY_POOL_PAGE_TYPE size_t
    #define FERRET_MEMORY_POOL_PAGE_COUNT                                   \
      (FERRET_MEMORY_POOL_SIZE / sizeof(FERRET_MEMORY_POOL_PAGE_TYPE))
   #else
    #define FERRET_MEMORY_POOL_PAGE_COUNT FERRET_MEMORY_POOL_SIZE
   #endif
  
  namespace mem{
    namespace allocator{
  
      memory_pool<FERRET_MEMORY_POOL_PAGE_TYPE, FERRET_MEMORY_POOL_PAGE_COUNT> program_memory;
  
      class pool{
      public:
  
        static void init(){ }
        
        template<typename FT>
        static inline void*  allocate(){ return program_memory.allocate(sizeof(FT)); }
        
        static inline void   free(void * ptr){ program_memory.free(ptr); }
      };
    }
  }
  #endif
  #ifdef FERRET_MEMORY_BOEHM_GC
  
  #define FERRET_ALLOC_POLICY mem::allocator::gc
  #define FERRET_DISABLE_RC true
  
  #include <gc.h>
  
  namespace mem{
    namespace allocator{
      
      class gc{
      public:
  
        static void init(){ GC_INIT(); }
        
        template<typename FT>
        static inline void* allocate(){
  #ifdef FERRET_DISABLE_MULTI_THREADING
          return GC_MALLOC(sizeof(FT));
  #else
          return GC_MALLOC_ATOMIC(sizeof(FT));
  #endif
        }
      
        static inline void  free(void * ptr){ }
      };
    }
  }
  #endif
  #if !defined(FERRET_ALLOC_POLICY)
  
  #define FERRET_ALLOC_POLICY mem::allocator::system
  
  namespace mem{
    namespace allocator{
  
      class system{
      public:
  
        static void init(){ }
  
        template<typename FT>
        static inline void* allocate(){ return ::malloc(sizeof(FT)); }
  
        static inline void  free(void * ptr){ ::free(ptr); } 
      };
    }
  }
  #endif
  #if !defined(FERRET_RC_POLICY)
  namespace mem {
    namespace gc {
  #if defined(FERRET_DISABLE_RC)
  
  #define FERRET_RC_POLICY mem::gc::no_rc
      
      class no_rc{
      public:
  
        inline void inc_ref() { }
        inline bool dec_ref() { return false; }
      };
  
  #else
  
      template<typename T>
      class rc{
      public:
        
        inline void inc_ref() { ref_count++; }
        inline bool dec_ref() { return (--ref_count == 0); }
      
      private:
        T ref_count{0};
      };    
  
      #if defined(FERRET_DISABLE_MULTI_THREADING) || !defined(FERRET_STD_LIB)
        #define FERRET_RC_POLICY mem::gc::rc<int>
      #endif
      
      #if defined(FERRET_STD_LIB) && !defined(FERRET_DISABLE_MULTI_THREADING)
        #define FERRET_RC_POLICY mem::gc::rc<::std::atomic<int>>
      #endif
  #endif
    }
  }
  #endif
  class var;
  class seekable_i;
  
  namespace object{
    template <typename rc>
    class base : public rc{
    public:
      base() { }
      virtual ~base() { };
    
      virtual size_t type() const = 0;
    
  #if !defined(FERRET_DISABLE_STD_OUT)
      virtual void stream_console() const = 0;
  #endif
    
      virtual bool equals(var const & o) const = 0;
  
      virtual seekable_i* cast_seekable_i() { return nullptr; }
  
      void* operator new(size_t, void* ptr){ return ptr; }
      void  operator delete(void * ptr){ FERRET_ALLOC_POLICY::free(ptr); }
    };
  }
  
  typedef object::base<FERRET_RC_POLICY> object_t;
  class var{
  public:
    explicit var(object_t* o = nullptr) : obj(o) { inc_ref(); }
  
    var(const var& o) : obj(o.obj) { inc_ref(); }
    var(var&& o) : obj(o.obj) { o.obj = nullptr; }
      
    ~var() { dec_ref(); }
  
    var& operator=(var&& other){
      if (this != &other){
        dec_ref();
        obj = other.obj;
        other.obj = nullptr;
      }
      return *this;
    }
    
    var& operator= (const var& other){
      if (obj != other.obj){
        dec_ref();
        obj = other.obj;
        inc_ref();
      }
      return *this;
    }
  
    bool equals (var const & rhs) const;
  
    bool operator==(const var& other) const { return equals(other); }
  
    bool operator!=(const var& other) const { return !equals(other); }
    
    operator bool() const;
  
  #if !defined(FERRET_DISABLE_STD_OUT)
    void stream_console() const {
      if (obj != nullptr )
        obj->stream_console();
      else
        runtime::print("nil");
    }
  #endif
        
    inline object_t* get() const { return obj; }
    
    template<typename T>
    inline T* cast() const { return static_cast<T*>(obj); }
  
    inline bool is_type(size_t type) const { 
      return (static_cast<object_t*>(obj)->type() == type);
    }
  
    inline bool is_nil() const { return (obj == nullptr); }
  
  private:
    inline void inc_ref(){
  #if !defined(FERRET_DISABLE_RC)
      // Only change if non-null
      if (obj) obj->inc_ref();
  #endif
    }
      
    inline void dec_ref(){
  #if !defined(FERRET_DISABLE_RC)
      // Only change if non-null
      if (obj){
        // Subtract and test if this was the last pointer.
        if (obj->dec_ref()){
          delete obj;
          obj = nullptr;
        }
      }
  #endif
    }
      
    object_t* obj;
  };
  
  template<>
  inline seekable_i* var::cast<seekable_i>() const { return obj->cast_seekable_i(); }
  template<typename FT, typename... Args>
  inline var obj(Args... args) {
    void * storage = FERRET_ALLOC_POLICY::allocate<FT>();
    return var(new(storage) FT(args...));
  }
  
  inline var nil(){
    return var();
  }
}

// Runtime Prototypes
namespace ferret{
  namespace runtime{
    #undef min
    #undef abs
  
    template<typename T>
    constexpr T min(T a, T b){
      return ((a) < (b) ? (a) : (b));
    }
  
    template<typename T>
    constexpr T abs(T a){
      return ((a) < (T)0 ? -(a) : (a));
    }
  }
  namespace runtime {
    var list(var const & v);
    var list(var const & v);
    template <typename... Args>
    var list(var const & first, Args const & ... args);
  
    var first(var const & coll);
    var rest(var const & coll);
    var cons(var const & x, var const & seq);
    var nth(var const & seq, number_t index);
    var nthrest(var const & seq, number_t index);
    size_t count(var const & seq);
    bool is_seqable(var const & seq);
  }
  template<typename T, typename... Args>
  inline var run(T const & fn, Args const & ... args);
        
  template<typename T>
  inline var run(T const & fn);
  
  template<>
  inline var run(var const &);
}

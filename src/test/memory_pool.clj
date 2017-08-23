
(native-define "#define FERRET_MEMORY_POOL_SIZE 4194304") ;; 4 MB
(native-declare "void* ptr;")

(native-declare "ferret::mem::allocator::memory_pool<size_t,14> tiny_pool;")
(let [next-page (fn [idx]
                  "size_t i = number::to<size_t>(idx);
                        __result = obj<number>((number_t)tiny_pool.next_page(i))")
      malloc (fn [size]
               "size_t s = number::to<size_t>(size);;
                     ptr = tiny_pool.allocate(sizeof(size_t) * s);
                     __result = obj<boolean>((ptr != nullptr));")
      free (fn [] "tiny_pool.free(ptr);")]
  
  (assert (= 0 (next-page 0)))
  (assert (malloc 2))
  (assert (= 3 (next-page 0)))
  (assert (malloc 4))
  (assert (= 8 (next-page 2)))
  (free)
  (assert (= 3 (next-page 2)))
  (assert (false? (malloc 40)))
  (assert (malloc 6))
  (assert (malloc 1))
  (assert (malloc 1))
  (assert (false? (malloc 10))))

(native-declare "ferret::mem::allocator::memory_pool<size_t,256> even_pool;")
(let [next-page (fn [idx]
                  "size_t i = (size_t)number::to<size_t>(idx);
                        __result = obj<number>((number_t)even_pool.next_page(i))")
      malloc (fn [size]
               "size_t s = number::to<size_t>(size);
                     ptr = even_pool.allocate(sizeof(size_t) * s);
                     __result = obj<boolean>((ptr != nullptr));")
      free (fn [] "even_pool.free(ptr);")]
  
  (assert (= 0 (next-page 0)))
  (assert (malloc 255))
  (assert (= 256 (next-page 0)))
  (free)
  (assert (= 0 (next-page 0)))
  (assert (false? (malloc 256)))
  (assert (= 0 (next-page 0))))

(native-declare "ferret::mem::allocator::memory_pool<size_t,255> odd_pool;")
(let [next-page (fn [idx]
                  "size_t i = number::to<size_t>(idx);
                       __result = obj<number>((number_t)odd_pool.next_page(i))")
      malloc (fn [size]
               "size_t s = number::to<size_t>(size);
                     ptr = odd_pool.allocate(sizeof(size_t) * s);
                     __result = obj<boolean>((ptr != nullptr));")
      free (fn [] "odd_pool.free(ptr);")]
  
  (assert (= 0 (next-page 0)))
  (assert (malloc 254))
  (assert (= 255 (next-page 0)))
  (free)
  (assert (= 0 (next-page 0)))
  (assert (false? (malloc 255)))
  (assert (= 0 (next-page 0))))

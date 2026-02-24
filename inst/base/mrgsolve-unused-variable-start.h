#if defined(_MSC_VER)
  #pragma warning(push)
  #pragma warning(disable: 4101)
#elif defined(__INTEL_COMPILER)
  #pragma warning(push)
  #pragma warning(disable: 177)
#elif defined(__GNUC__) || defined(__clang__)
  #pragma GCC diagnostic push
  #pragma GCC diagnostic ignored "-Wunused-variable"
#endif

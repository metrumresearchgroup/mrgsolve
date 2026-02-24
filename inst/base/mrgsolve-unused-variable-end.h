#if defined(_MSC_VER)
  #pragma warning(pop)
#elif defined(__INTEL_COMPILER)
  #pragma warning(pop)
#elif defined(__GNUC__) || defined(__clang__)
  #pragma GCC diagnostic pop
#endif

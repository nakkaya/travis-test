
#if !defined(FERRET_DISABLE_STD_MAIN)
 #if defined(FERRET_DISABLE_CLI_ARGS) || !defined(FERRET_STD_LIB)
  int main()
 #else
  int main(int argc, char* argv[])
 #endif
  {     
    using namespace ferret;

    FERRET_ALLOC_POLICY::init();

   #if defined(FERRET_STD_LIB) && !defined(FERRET_DISABLE_CLI_ARGS)
    for (int i = argc - 1; i > -1 ; i--)
      _star_command_line_args_star_ =  runtime::cons(obj<string>(argv[i]),_star_command_line_args_star_);
   #endif

    program::run();

   #if defined(FERRET_PROGRAM_MAIN)
    run(FERRET_PROGRAM_MAIN);
   #endif
     
    return 0;
  }
#endif
#if defined(FERRET_HARDWARE_ARDUINO)
  void setup(){
    using namespace ferret;

      runtime::init();

    #if defined(FERRET_PROGRAM_MAIN)
      program::run();
    #endif
  }
  void loop(){
    using namespace ferret;
    #if !defined(FERRET_PROGRAM_MAIN)
      program::run();
    #endif          

    #if defined(FERRET_PROGRAM_MAIN)
      run(FERRET_PROGRAM_MAIN);
    #endif
  }
#endif

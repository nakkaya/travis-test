
template <typename T>
class pid_controller : public object_t {
  T p;
  T i;
  T d;
  T maximum_output;
  T minimum_output;
  T maximum_input;
  T minimum_input;
  bool continuous;
  T prev_error;
  T total_error;
  T setpoint;
  T error;
  T result;
  T input;
public:

  pid_controller(var const & kp, var const & ki, var const & kd,
                 var const & inMin, var const & inMax, var const & outMin, var const & outMax,
                 var const & cont){
    p = number::to<T>(kp);
    i = number::to<T>(ki);
    d = number::to<T>(kd);
    maximum_output = number::to<T>(outMax);
    minimum_output = number::to<T>(outMin);
    maximum_input = number::to<T>(inMax);
    minimum_input = number::to<T>(inMin);
    continuous = cont.cast<boolean>()->container();
    prev_error = 0;
    total_error = 0;
    setpoint = 0;
    error = 0;
    result = 0;
    input = 0;
  }

  size_t type() const { return runtime::type::pid_controller; }

  bool equals(var const & o) const {
    return (this == o.cast<pid_controller>());
  }

#if !defined(FERRET_DISABLE_STD_OUT)
  void stream_console() const {
    runtime::print("pid_controller");
  }
#endif


  var update(var const & in){
    input = number::to<T>(in);

    // Calculate the error signal
    error = setpoint - input;

    // If continuous is set to true allow wrap around
    if (continuous) {
      if (runtime::abs(error) > ((maximum_input - minimum_input) / (real_t)2)) {
        if (error > (real_t)0) {
          error = (error - maximum_input) + minimum_input;
        } else {
          error = (error + maximum_input) - minimum_input;
        }
      }
    }
                              
    /*
     * Integrate the errors as long as the upcoming integrator does
     * not exceed the minimum and maximum output thresholds
     */
    if ((((total_error + error) * i) < maximum_output) &&
        (((total_error + error) * i) > minimum_output)) {
      total_error += error;
    }
                              
    // Perform the primary PID calculation
    result = ((p * error) + (i * total_error) + (d * (error - prev_error)));
                              
    // Set the current error to the previous error for the next cycle
    prev_error = error;
                              
    // Make sure the final result is within bounds
    if (result > maximum_output) {
      result = maximum_output;
    } else if (result < minimum_output) {
      result = minimum_output;
    }

    return obj<number>(result);
  }
  void set_setpoint(var const & p){
    T sp = number::to<T>(p);
    if (maximum_input > minimum_input) {
      if (sp > maximum_input) {
        setpoint = maximum_input;
      } else if (sp < minimum_input) {
        setpoint = minimum_input;
      } else {
        setpoint = sp;
      }
    } else {
      setpoint = sp;
    }
  }
  void reset(){
    prev_error = 0;
    total_error = 0;
    result = 0;
  }
};

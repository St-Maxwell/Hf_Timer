# Hafnium Timer
a Fortran implementation of timer

## Install

### Fortran package manager

Add fpm dependency declaration in the `fpm.toml` file of your project.
```toml
[dependencies]
hf_timer = { git = "https://github.com/St-Maxwell/hf_timer" }
```

## Usage

```fortran
use Hf_Timer, only: hftimer
!! hftimer type only has four public methods as shown below

type(hftimer) :: clock

call clock%start()

!! do something...

call clock%stop()

write(*,"('CPU Time: ',f8.6,' s')") clock%get_cpu_time()
write(*,"('Elapsed Time: ',f8.6,' s')") clock%get_elapsed_time()

```

More examples and tests can be found in [test](test/).


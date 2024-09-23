# Effect from BMI and Sodium on Diseases

This configuration simulates a minimal example population to demonstrate an instance of issue [227](https://github.com/imperialCHEPI/healthgps/issues/277) of the Health-GPS repository, where sodium and BMI appear to not affect the instance of some diseases relative to the baseline scenario.

In [this comment](https://github.com/imperialCHEPI/healthgps/issues/277#issuecomment-1894621015), it can be seen that both baseline and intervention traces for some diseases appear to follow near identical trajectories.

Run on HPC with `qsub example-jobscript.sh`. Note that you need to also have set the `HGPS_ROOT` environment variable to point to your `healthgps` repo and have built the executable like so:

```sh
cmake --build --preset release-build-linux --target install
```

Output files are placed in `$HOME/healthgps/example_diseases_output`.

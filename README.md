# Health-GPS example model configurations

This repo contains example configurations for the [Health-GPS](../healthgps/) console program, which should cover all of the models currently implented.

## Imperial HPC users

For users of [Imperial College's HPC system](https://www.imperial.ac.uk/computational-methods/hpc/) there is [an example PBS job script](example-jobscript.sh) in this repository.

To get started, follow these instructions after logging into the HPC:

1. Clone the `healthgps` repository:

    ```sh
    git clone https://github.com/imperialCHEPI/healthgps.git
    ```

1. Install [CMake] using the [binary distribution on the CMake website] [^1]

1. Load the build tools

    ```sh
    module load tools/dev
    module load GCCcore/12.3.0
    ```

1. Install [vcpkg] following [these instructions] and set the `VCPKG_ROOT` environment variable to point to the `vcpkg` repo in your `.profile`. E.g. if `vcpkg` is downloaded to `~/vcpkg`:

    ```sh
    echo export VCPKG_ROOT=~/vcpkg >> ~/.profile
    ```

    (You will need to log out and back in again for the change to take effect.)

1. Configure the project

    ```sh
    cmake --preset linux-release
    ```

1. Build the project in release mode

    ```sh
    cmake --build --preset release-build-linux --target install
    ```

1. (Optionally) set the `$HGPS_ROOT` environment variable in your `.profile`, if you have cloned the `healthgps` repo to somewhere other than your home directory

1. Copy `example-jobscript.sh` to the directory containing the model

1. Modify the required resources (memory and number of CPUs) as appropriate

1. Submit the job:

    ```sh
    qsub some/model/folder/example-jobscript.sh
    ```

[CMake]: https://cmake.org
[binary distribution on the CMake website]: https://cmake.org/download/
[vcpkg]: https://vcpkg.io/en/
[these instructions]: https://learn.microsoft.com/en-us/vcpkg/get_started/get-started?pivots=shell-cmd#1---set-up-vcpkg
[^1]: The version installed on the HPC doesn't work at the time of writing.

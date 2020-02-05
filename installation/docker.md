This installation method requires Docker. The program can be found in your distribution's software repository or on the [Docker website](https://docs.docker.com/install/). For institutions, [Rootless Docker](https://medium.com/@tonistiigi/experimenting-with-rootless-docker-416c9ad8c0d6) may be the preferred choice since it does not allow users to execute programs as a root user.

To set up GP 2 via Docker, use the command below as a superuser to run a GP 2 program. The first time you run the command, Docker will download the necessary files for executing GP 2 programs. Subsequently, programs can be run offline.
```
docker run -v ${PWD}:/data registry.gitlab.com/yorkcs/batman/gp2i:latest <program> <input graph>
```
Replace ``<program>`` with the relative path to your program from your working directory, and ``<input graph>`` with the relative path to your input graph.

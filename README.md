# Money Maker

Unfinished trading bot based on the strategy described in [this video](https://youtu.be/otyIt5l0vjc?t=506).


# How to

## Build

If you want to have an optimised executable then run the following command

```bash
stack build
```

If you want to have faster compilation times at the expense of having a less optimised executable then run this

```bash
stack build --fast
```

## Run

First you need to start the database. Make sure you have `docker` and `docker-compose` installed and the `docker` service is running. Then run the following command

```bash
docker-compose up
```

If you don't want `docker-compose` to occupy your terminal then you can run it detached

```bash
docker-compose up -d
```

Then, assuming you have built the project, you can run the executable

```bash
stack exec make-money
```

## Test

```bash
stack test --no-keep-going --fast --file-watch --test-arguments "--color"
```

`--no-keep-going` stops tests from running as soon as one fails, `--fast` improves compilation time by disabling optimisations, `--file-watch` rebuilds and retests the project when a tracked file changes, and `--test-arguments "--color"` makes HSpec (the used testing library) print tests output with colors

Add `--ghc-options -Wwarn` at the end of that if you want to temporarily ignore non-critical warnings. The project is configured in a way that turns warnings into errors, this flag turns them back into warnings.

Add `--force-dirty` to force stack to rebuild everything.
